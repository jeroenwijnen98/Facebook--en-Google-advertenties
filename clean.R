setwd('/Users/marcwijnen/DutchElectionObservatory')
pacman::p_load(tidyverse, janitor, highcharter, httr, furrr, lubridate)

dir.create("data")

#functions
get_mid <- function(spend_upper_bound, spend_lower_bound) {
  # (spend_upper_bound-spend_lower_bound)/2+spend_lower_bound
  (spend_upper_bound+spend_lower_bound)/2
}

assign_colors <- function(dat, n = 12) {
  
  color_sample <- colorspace::divergingx_hcl(n)
  
  lenght <- dat$color[is.na(dat$color)] %>% length
  
  if(lenght==0) return(invisible())
  
  cols <- sample(color_sample, lenght, replace = T)
  
  dat$color[is.na(dat$color)] <- cols
  
  return(dat)
  
}

unnest_geos <- function(x) {
  # cat(glue::glue("{x$row_number} out of {nrow(age_gender_targeted_raw)} ({round(100*x$row_number/nrow(age_gender_targeted_raw), 2)}%)\n\n"))
  x %>% 
    pull(region_distribution) %>% 
    flatten() %>% 
    map_dfr(flatten) %>% 
    mutate(id = x$id)%>% 
    mutate(start_time = x$start_time)%>% 
    mutate(advertiser_name = x$advertiser_name)
}

unnest_dems <- function(x) {
  # cat(glue::glue("{x$row_number} out of {nrow(age_gender_targeted_raw)} ({round(100*x$row_number/nrow(age_gender_targeted_raw), 2)}%)\n\n"))
  x %>% 
    pull(demographic_distribution) %>% 
    flatten() %>% 
    map_dfr(flatten) %>% 
    mutate(id = x$id)
}

# Set a "plan" for how the code should run.
# plan(multisession, workers = 4)

# get_ggl_data <- function() {

last_updated_time <- as.character(Sys.time())

#######################################################################################################################
################################################### Google ############################################################
#######################################################################################################################

ggl_link <- "https://storage.googleapis.com/transparencyreport/google-political-ads-transparency-bundle.zip"
ggl_file <- "data/ggl.zip"

download.file(ggl_link, ggl_file, mode="wb")
unzip(ggl_file, exdir = "data")
unlink(ggl_file)

color_dat <- tibble(
  color = c("#00b13d", "#80c31c", "#cd503e", "#008067", "#e01003", "#e3101c", "#6f2421"),
  advertiser_name = c("D66", "GroenLinks", "VVD", "CDA", "SP", "PvdA", "FvD"))


ggl_ads <- data.table::fread("data/google-political-ads-transparency-bundle/google-political-ads-creative-stats.csv") %>% 
  filter(str_detect(Regions, "NL")) %>%
  janitor::clean_names() %>% 
  # filter(advertiser_name %in% dutch_parties) %>%
  filter(date_range_start >= as.Date("2020-01-06")) %>% 
  mutate(advertiser_name = case_when(
    advertiser_name == 'Christen Democratisch Appèl' ~ "CDA",
    advertiser_name == 'SP (Socialistische Partij)' ~ "SP",
    advertiser_name == 'Partij van de Arbeid' ~ "PvdA",
    advertiser_name == 'Forum voor Democratie' ~ "FvD",
    T ~ advertiser_name
  ))

advertiser_emp_ggl <- ggl_ads %>% 
  pull(advertiser_name) %>% 
  unique()

advertiser_emp_ggl

weekly_spend <- read_csv("data/google-political-ads-transparency-bundle/google-political-ads-advertiser-weekly-spend.csv")

weekly_spend_ggl <- weekly_spend  %>% 
   janitor::clean_names() %>%
   mutate(advertiser_name = case_when(
     advertiser_name == 'Christen Democratisch Appèl' ~ "CDA",
     advertiser_name == 'SP (Socialistische Partij)' ~ "SP",
     advertiser_name == 'Partij van de Arbeid' ~ "PvdA",
     advertiser_name == 'Forum voor Democratie' ~ "FvD",
     T ~ advertiser_name
    )) %>%
   filter(week_start_date >= as.Date("2020-09-06")) %>%
   filter(advertiser_name %in% advertiser_emp_ggl) %>%
   mutate(spend_eur = ifelse(spend_eur == 0, 0.01, spend_eur)) %>%
   complete(advertiser_name,
            week_start_date = seq.Date(min(week_start_date), max(week_start_date), by="week"),
            fill = list(spend_eur = 0)) #%>%
   #select(advertiser_name, date_range_start = week_start_date, spend_eur)

total_spend_ggl <- weekly_spend_ggl %>%
  group_by(advertiser_name) %>%
  summarize(spend_eur = sum(spend_eur))

dutch_parties <- c("VVD", "D66", "FvD", "SP", "GroenLinks", "Volt Nederland", "PvdA", "CDA", "PvdD", "ChristenUnie","50Plus","PVV")

ggl_ads <- ggl_ads[which(ggl_ads$advertiser_name %in% dutch_parties),]
weekly_spend_ggl <- weekly_spend_ggl[which(weekly_spend_ggl$advertiser_name %in% dutch_parties),]

ggl_ads <- ggl_ads %>% 
  mutate(impressions_lower_bound = case_when(
    impressions == "≤ 10k" ~ 1,
    impressions == "10k-100k" ~ 10000,
    impressions == "100k-1M" ~ 100000,
    impressions == "1M-10M" ~ 1000000,
    # impressions == "> 10M" ~ 10000000,
    T ~ 0
  )) %>% 
  mutate(impressions_upper_bound = case_when(
    impressions == "≤ 10k" ~ 10000,
    impressions == "10k-100k" ~ 100000,
    impressions == "100k-1M" ~ 1000000,
    impressions == "1M-10M" ~ 10000000,
    # impressions == "> 10M" ~ 20000000,
    T ~ 0
  )) 


#Gender targeting
ggl_ads <- ggl_ads %>%
  mutate(gender_targeting = ifelse(str_detect(gender_targeting, "Male") & str_detect(gender_targeting, "Female"), "Not targeted", gender_targeting))

#Age targeting
ggl_ads <- ggl_ads %>%
  mutate(age_targeting2 = case_when(
    str_detect(age_targeting, "18-24, 25-34, 35-44, 45-54, 55-64, ≥65") ~ "Not targeted",
    T ~ age_targeting
  ))


#######################################################################################################################
################################################### Facebook ##########################################################
#######################################################################################################################
readRenviron(".Renviron")
token <- 'EAAO3V3HJa24BAGkiP6kiCsk9pIXFoxHZAm5apZARt7oBRBT1oBJLfSzeFAJwifnEXy3VxEgaZAZCkqDQucGRxmZCXTPbzywoBZAUTdzbmLqVqrbSsqidssyE2q697dYs3aKUcRWsEbVogAi7g2vWdu9b5FVZBt3ltFNhBDGY4QFULVQt5eKBJu8'

#link to fb api
my_link<- "https://graph.facebook.com"

#define fields you are interested in
search_fields=c("ad_creation_time", 
                "ad_delivery_start_time",
                "ad_delivery_stop_time",
                "ad_creative_link_caption",
                "ad_creative_link_description",
                "ad_creative_link_title",
                "currency",
                "ad_creative_body", 
                "page_id",
                "page_name",
                "spend",
                "ad_snapshot_url",
                "demographic_distribution",
                "funding_entity",
                "impressions",
                "region_distribution") %>% 
  stringr::str_c(., collapse=", ")

#get the data from the first 'page' of data the api provides
page_one_response <- GET(my_link,
                         path = "/ads_archive",
                         query = list(access_token = token,
                                      limit=100,
                                      ad_active_status="ALL",
                                      search_terms="''",
                                      ad_delivery_date_min = "2020-09-01",
                                      fields=search_fields,
                                      ad_reached_countries="NL"))
page_one_content<- content(page_one_response)

x <- tibble(data=page_one_content$data)
df_imp <- x %>% 
  unnest_wider(data) 

#get the link refering to the next page
next_link <- page_one_content$paging$`next`

page <- 1

#iterate over all pages until there is no further page
while(length(next_link)>0) {
  # while(T) {
  
  print(page)
  
  next_response <- GET(next_link)
  next_content<- content(next_response)
  
  y <- tibble(data=next_content$data)
  df_next <- y %>% 
    unnest_wider(data) 
  
  df_imp <- bind_rows(df_imp, df_next)  
  
  next_link <- next_content$paging$`next`
  
  page <- page + 1
  
}


fb_dat <- df_imp %>% 
  rename(advertiser_name = page_name) %>% 
  rename(advertiser_id = page_id) %>% 
  # bind_rows(fb_dat) %>% 
  # distinct(id, .keep_all = T) %>% 
  mutate(advertiser_name = case_when(
    advertiser_name == 'Partij voor de Dieren' ~ "PvdD",
    advertiser_name == 'Partij van de Arbeid (PvdA)' ~ "PvdA",
    advertiser_name == 'Forum voor Democratie -FVD' ~ "FvD",
    advertiser_name == '50PLUSpartij' ~ "50Plus",
    T ~ advertiser_name
  ))

color_dat <- tibble(
  color = c("#00b13d", "#80c31c", "#cd503e", "#008067", "#6f2421", "#e3101c", "#e01003", "#036b2c", "#02a6e9", "#562883"),
  advertiser_name = c("D66", "GroenLinks", "VVD", "CDA", "FvD", "PvdA", "SP", "PvdD", "ChristenUnie", "Volt Nederland"))

partijen <- c(
  609816282477420,  #FVD
  121264564551002, #VVD
  175740570505, #GL
  113895885305052, #PvdA
  128393027527, #SP
  1178933415531366, #PVV
  320374518118, #CDA
  52985377549, #D66
  102287806490622, #PvdD
  347623668624706, #50Plus
  433611050005553, #SGP
  211661062254003, #CU
  1550088745275913, #DENK
)
fb_dat <- subset(fb_dat, fb_dat$advertiser_id %in% partijen)

fb_dat <- fb_dat %>% 
  mutate(date_range_start = as.Date(ad_delivery_start_time)) %>%
  filter(date_range_start >= as.Date("2020-09-01")) %>% 
  unnest_wider(spend, names_sep = "_") %>%
  unnest_wider(impressions, names_sep = "_") %>%
  mutate_at(vars(spend_lower_bound, spend_upper_bound, impressions_lower_bound, impressions_upper_bound), as.numeric) %>% 
  # drop_na(spend_lower_bound, spend_upper_bound, impressions_lower_bound, impressions_upper_bound) %>% 
  mutate(impressions_lower_bound = case_when(
    # is.na(impressions_upper_bound) ~ 0, 
    # is.na(impressions_lower_bound) ~ 0,
    impressions_lower_bound == 0 ~ 0.01, 
    T ~ impressions_lower_bound)) %>% 
  mutate(spend_lower_bound = case_when(
    spend_lower_bound == 0 ~ 0.01, 
    T ~ spend_lower_bound)) %>% 
  drop_na(impressions_lower_bound, impressions_upper_bound)


batch_id_dat <- total_times  %>% 
  # filter(is.na(advertiser_id)) %>% View
  mutate(unique_advertiser_id = as.numeric(as.factor(advertiser_name))) %>% 
  group_by(ad_creative_body, ad_creative_link_title, advertiser_name, advertiser_id) %>% 
  mutate(batch_id = paste0(unique_advertiser_id, "_", n(), "_", sample(10000:10000000000, size = 1)))%>% 
  ungroup() %>% 
  select(id, batch_id)

fb_total <- fb_dat  %>% 
  group_by(ad_creative_body, ad_creative_link_title, advertiser_name, advertiser_id) %>% 
  summarise(spend_range_min = median(spend_lower_bound),
            spend_range_max = median(spend_upper_bound),
            spend_range_mid = median(get_mid(spend_lower_bound, spend_upper_bound)),
            impressions_range_min = median(impressions_lower_bound),
            impressions_range_max = median(impressions_upper_bound),
            impressions_range_mid = median(get_mid(impressions_lower_bound, impressions_upper_bound)),
            n_ids = n()) %>% 
  ungroup() %>% 
  group_by(advertiser_name, advertiser_id) %>%# View
  summarise(spend_range_min = sum(spend_range_min),
            spend_range_max = sum(spend_range_max),
            spend_range_mid = sum(spend_range_mid),
            impressions_range_min = sum(impressions_range_min),
            impressions_range_max = sum(impressions_range_max),
            impressions_range_mid = sum(impressions_range_mid),
            n = n()) %>% 
  ungroup() %>% 
  left_join(color_dat)  %>% 
  assign_colors()



fb_times <- fb_dat %>%
  group_by(date_range_start, ad_creative_body, ad_creative_link_title, advertiser_name, advertiser_id) %>% 
  summarise(spend_range_min = median(spend_lower_bound),
            spend_range_max = median(spend_upper_bound),
            spend_range_mid = median(get_mid(spend_lower_bound, spend_upper_bound)),
            impressions_range_min = median(impressions_lower_bound),
            impressions_range_max = median(impressions_upper_bound),
            impressions_range_mid = median(get_mid(impressions_lower_bound, impressions_upper_bound)),
            n_ids = n()) %>% 
  ungroup() %>% 
  group_by(date_range_start, advertiser_name, advertiser_id) %>%# View
  summarise(spend_range_min = sum(spend_range_min),
            spend_range_max = sum(spend_range_max),
            spend_range_mid = sum(spend_range_mid),
            impressions_range_min = sum(impressions_range_min),
            impressions_range_max = sum(impressions_range_max),
            impressions_range_mid = sum(impressions_range_mid),
            n = n()) %>% 
  ungroup() %>% 
  complete(advertiser_name, date_range_start = seq.Date(min(date_range_start), max(date_range_start), by="day"), fill = list(n = 0, spend_range_min = 0, spend_range_mid = 0, spend_range_max = 0, impressions_range_min = 0, impressions_range_mid = 0, impressions_range_max = 0)) %>% 
  left_join(color_dat) %>% 
  assign_colors() %>% 
  left_join(fb_total)

age_gender_targeted_raw <- fb_dat %>% 
  mutate(start_time = lubridate::as_datetime(ad_delivery_start_time) %>% lubridate::floor_date("day")) %>% 
  mutate(start_time = as.Date(start_time)) %>% 
  mutate(date_range_start = as.Date(ad_delivery_start_time)) %>%
  filter(date_range_start >= as.Date("2020-09-01")) 

age_gender_targeted <- age_gender_targeted_raw %>% 
  mutate(row_number = 1:n()) %>% 
  split(1:nrow(.)) %>% 
  map_dfr(unnest_dems) %>% 
  right_join(age_gender_targeted_raw)

geo_targeted <- age_gender_targeted_raw %>% 
  mutate(row_number = 1:n()) %>% 
  # slice(1:10) %>% 
  split(1:nrow(.)) %>% 
  map_dfr(unnest_geos) %>% 
  right_join(age_gender_targeted_raw)

