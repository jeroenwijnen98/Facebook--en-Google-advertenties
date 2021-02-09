library(ggplot2) 
library(tidyr) #voor herrankschikken dataset
library(tidyverse)
library(openxlsx)
library(gsubfn)
library(cat)
library(base)
library(anchors)
library(data.table)
library(stringr)
library(janitor) #voor maken column totals
library(fastDummies)
library(pmdplyr)
library(sjmisc)
library(webshot)
library(wdman)
library(zoo)
library(schoolmath)
library(googleLanguageR)
library(progress)

#scrapen
library(rvest)
library(rjson)
library(gtools)
library(xml2)
library(RSelenium)
library(httr)
library(Rfacebook)
library(XML)
library(googlesheets4)
library(rvest)

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
            fill = list(spend_eur = 0))

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
token <- 'INSERT YOUR OWN ACCESS TOKEN HERE'

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
#while(page < 50) {
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
  1550088745275913 #DENK
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

#getargete groepen 'uitpakken'
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

#PVV toevoegen
pvv <- fb_dat[1,]
pvv[,8] <- 123456789
pvv[,9] <- 'PVV'
pvv[,c(10:11,15:16)] <- 0

fb_dat <- rbind(pvv,fb_dat)

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



################################## 
###### Analyseren ads data #######
##################################


#meest bekeken ad per partij (bij gelijkspel de duurste/meest recente)
fb_top <- fb_dat %>% group_by(advertiser_id) %>% filter(date_range_start >=  Sys.Date()-7 & advertiser_name != 'PVV') 
fb_top <- fb_top %>% group_by(advertiser_id) %>% top_n(1, impressions_upper_bound)
fb_top <- fb_top %>% group_by(advertiser_id) %>% top_n(1, spend_upper_bound)
fb_top <- fb_top %>% group_by(advertiser_id) %>% top_n(1, date_range_start)
fb_top <- fb_top %>% group_by(advertiser_id) %>% filter(row_number()==1)

ggl_top <- ggl_ads %>% group_by(advertiser_id) %>% filter(date_range_start >=  Sys.Date()-7)
ggl_top <- ggl_top %>% group_by(advertiser_id) %>% top_n(1, impressions_upper_bound)
ggl_top <- ggl_top %>% group_by(advertiser_id) %>% top_n(1, spend_range_max_eur)
ggl_top <- ggl_top %>% group_by(advertiser_id) %>% top_n(1, date_range_start)
ggl_top <- ggl_top %>% group_by(advertiser_id) %>% filter(row_number()==1)

top_ads <- merge(fb_top[,c('advertiser_name','ad_snapshot_url')],ggl_top[,c('advertiser_name','ad_url')], all = T)
names(top_ads)[2:3] <- c('Facebook','Google')

#######################################################################################################################
################################################### Datum #############################################################
#######################################################################################################################

fb_datum <- fb_times %>%
  group_by(advertiser_name) %>%
  mutate(spend_min_cum = cumsum(spend_range_min)) %>%
  mutate(spend_mid_cum = cumsum(spend_range_mid)) %>%
  mutate(spend_max_cum = cumsum(spend_range_max)) %>%
  mutate(count_cum = cumsum(n))


fb_datum <- fb_datum[,which(names(fb_datum) %in% c("advertiser_name","date_range_start","spend_min_cum","spend_mid_cum","spend_max_cum","count_cum",'spend_range_min','spend_range_mid','spend_range_max'))]

ggl_datum <- weekly_spend_ggl %>% arrange(weekly_spend_ggl$week_start_date) %>% arrange(weekly_spend_ggl$advertiser_name)
ggl_datum <- ggl_datum[,c(1,2,6)];
names(ggl_datum) <- c("partij","datum", "spend")
ggl_datum <- ggl_datum %>%
  complete(datum = seq.Date(min(ggl_datum$datum), max(ggl_datum$datum), by='day', fill = list(impressions_min = 0)), partij)

ggl_datum[is.na(ggl_datum)] <- 0
ggl_datum <- ggl_datum %>%
    group_by(partij) %>%
    mutate(spend_google_cum = cumsum(spend))


#######################################################################################################################
################################################### Combineren ########################################################
#######################################################################################################################

#samenvoegen cumulatieve grafieken
datum <- 
  merge(x=ggl_datum, y=fb_datum, by.x = c("datum","partij"), by.y = c("date_range_start","advertiser_name"), all = T)

#afronden
datum[,3:10] <- round(datum[,3:10]) #afronden

#vul de laatste stand in voor dagen dat we nog geen data hebben voor Google
inconsistent_df <- pibble(i = datum$partij,t = as.numeric(datum$datum),x = datum$spend_google_cum,.i = i,.t = t)
inconsistent_df <- inconsistent_df %>% mutate(x_filled = panel_locf(x, .resolve = mean))
datum$spend_google_cum <- inconsistent_df$x_filled

#totale uitgaven kolom
datum <- datum %>% 
  rowwise() %>% 
  mutate(maximum = sum(spend_max_cum, spend_google_cum, na.rm = T))  %>%
  mutate(minimum = sum(spend_min_cum, spend_google_cum, na.rm = T)) %>%
  mutate(midden = sum(spend_mid_cum, spend_google_cum, na.rm = T)) %>%
  mutate(spend_max_tot = sum(spend_range_max, spend, na.rm = T))  %>%
  mutate(spend_min_tot = sum(spend_range_min, spend, na.rm = T)) %>%
  mutate(spend_mid_tot = sum(spend_range_mid, spend, na.rm = T))
  
#aggregeren per week/maand
datum$week <- format(datum$datum, "%Y-%W")
datum$maand <- format(datum$datum, "%Y-%m")

week <- aggregate(datum[,c(3:17)], by = list('partij'=datum$partij,'week'=datum$week), FUN = function(x){sum(x, na.rm = T)})
maand <- aggregate(datum[,c(3:17)], by = list('partij'=datum$partij,'maand'=datum$maand), FUN = function(x){sum(x, na.rm = T)})

#uitgaven over hele periode alle maanden
totaal <- datum[which(datum$datum == max(datum$datum)),]
names(totaal)[4] <- 'Google'
names(totaal)[9]<- 'Facebook'


#######################################################################################################################
################################################### Doelgroepen #######################################################
#######################################################################################################################

## Facebook
#regios
geo_targeted
geo_targeted2 <- spread(geo_targeted,key = region,value = percentage)
geo_targeted3 <- geo_targeted2[, !names(geo_targeted2) %in% c("Flemish Region","Niedersachsen","Nordrhein-Westfalen","Unknown","Wallonia")]
geo_targeted3 <- geo_targeted3[,c(1,3,22:33)]
for (col in c(3:14)){
  geo_targeted3[which(is.na(geo_targeted3[,col])),col] <- '0'
} #Na's verwijderen

#age-gender fill
age_gender_targeted$percentage <- as.numeric(age_gender_targeted$percentage)
age_gender_targeted2 <- age_gender_targeted %>% complete(age,gender,id, fill = list(percentage=0))

id_partij <- geo_targeted2[,c('id','advertiser_name')]
for (row in which(is.na(age_gender_targeted2$advertiser_name))){
  age_gender_targeted2$advertiser_name[row] <- id_partij$advertiser_name[which(id_partij$id == age_gender_targeted2$id[row])]
} #partijnnamen invullen in lege velden

#samenvoegen
targeted <- merge(x = age_gender_targeted2[,c(1:4)], y = geo_targeted3, all.x = T, all.y = F)
targeted2 <- gather(targeted,'geo','geo_perc',-names(targeted[1:5]))

targeted2$geo_perc <- as.numeric(targeted2$geo_perc)
targeted2$tot_perc <- targeted2$percentage * targeted2$geo_perc

#aggregeren
targeted_sum <- aggregate(targeted2[,c('percentage','geo_perc','tot_perc')],by = list('partij'=targeted2$advertiser_name,'age'=targeted2$age,'gender'=targeted2$gender,'geo'=targeted2$geo),FUN = function(x){mean(x,na.rm = T)})
names(targeted_sum)[5:7] <- c("percentage geslacht/leeftijd", "percentage regio","percentage totaal")
targeted_sum[,5] <- targeted_sum[,5]*100;targeted_sum[,6] <- targeted_sum[,6]*100;targeted_sum[,7] <- targeted_sum[,7]*100

#testen: telt het op naar 100%
aggregate(targeted_sum[c(5:7)], by=list('partij'=targeted_sum$partij),FUN=function(x){sum(x,na.rm = T)})

#gender NL maken
targeted_sum$gender <- str_replace_all(targeted_sum$gender, 'female','vrouw')
targeted_sum$gender <- str_replace_all(targeted_sum$gender, 'unknown','onbekend')
targeted_sum$gender <- str_replace_all(targeted_sum$gender, 'male','man')

#afronden
targeted_sum$`percentage geslacht/leeftijd` <- round(targeted_sum$`percentage geslacht/leeftijd`,2)
targeted_sum$`percentage regio` <- round(targeted_sum$`percentage regio`,2)
targeted_sum$`percentage totaal` <- round(targeted_sum$`percentage totaal`,2)



#######################################################################################################################
################################################### Printscreens ########################################################
#######################################################################################################################
namen_fb <- paste(top_ads$advertiser_name[which(!is.na(top_ads$Facebook))], rep(c(' fb.png'),length(which(!is.na(top_ads$Facebook)))), sep = '')#partijnamen
urls_fb <- top_ads$Facebook[which(!is.na(top_ads$Facebook))]#urls
namen_ggl <- paste(top_ads$advertiser_name[which(!is.na(top_ads$Google))], rep(c(' ggl.png'),length(which(!is.na(top_ads$Google)))), sep = '')
urls_ggl <- top_ads$Google[which(!is.na(top_ads$Google))]

## Rselenium ##
remDr <- remoteDriver(remoteServerAddr = "127.0.0.1", port = 4444L, browserName="chrome")
remDr$open(TRUE)

#functies
maak_screenshot_fb <- function(url, file_name){
  #go to page
  remDr$navigate(url)
  
  # click cookies toestaan
  try(webElem <- remDr$findElement(using = 'css selector','button[title="Accept All"]'), silent = T)
  # try(webElem <- remDr$findElement(using = 'css selector',"#u_0_j"), silent = T)
  try(webElem$clickElement(), silent = T)
  # try(webElem <- remDr$findElement(using = 'css selector',"#u_0_g"), silent = T)
  # try(webElem$clickElement(), silent = T)
  
  #scrollen
  remDr$executeScript(paste("scroll(0,65);"))
  
  #neem screenshot
  remDr$maxWindowSize()
  remDr$screenshot(file = paste('screenshots', file_name, sep = '/'))

  print(file_name)
}
maak_screenshot_ggl <- function(url, file_name){
  #go to page
  remDr$navigate(url)
  
  # #click cookies toestaan
  # try(webElem <- remDr$findElement(using = 'css selector',"#u_0_g"), silent = T)
  # try(webElem$clickElement(), silent = T)
  # 
  # #scrollen
  # for(i in 1:5) {
  #   remDr$executeScript(paste("scroll(0,",i*10000,");"))
  #   Sys.sleep(1)
  # }
  # #neem screenshot
  Sys.sleep(3)
  remDr$findElement("css", "div.detail-box:nth-child(1) > p:nth-child(1)")$clickElement()
  remDr$maxWindowSize()
  remDr$screenshot(file = paste('screenshots', file_name, sep = '/'))

  print(file_name)
}

#screenshots maken
# setwd('/var/folders/t9/mpvzds097dg0rgn1txtmvf180000gn/T//Rtmpf2bEWV/') #working directory gelijkzetten aan plek die bereikbaar is voor dedicated upload
# do.call(file.remove, list(list.files('/var/folders/t9/mpvzds097dg0rgn1txtmvf180000gn/T//Rtmpf2bEWV/', pattern = '.png'))) #oude screenshots verwijderen
# do.call(file.remove, list(list.files('screenshots', pattern = '.png'))) #oude screenshots verwijderen
f <- list.files('screenshots', pattern = '.png', full.names = TRUE)
file.remove(f);

for (num in c(1:length(urls_fb))){
  maak_screenshot_fb(urls_fb[num],namen_fb[num])
}#Facebook

#Google
if (length(urls_ggl) > 0){
  for (num in c(1:length(urls_ggl))){
    maak_screenshot_ggl(urls_ggl[num],namen_ggl[num])
  }
}

# screenshots_fb <- list.files('/var/folders/t9/mpvzds097dg0rgn1txtmvf180000gn/T//Rtmpf2bEWV/', pattern = 'fb.png') #lijst filenames facebook
screenshots_fb <- list.files('screenshots', pattern = 'fb.png') #lijst filenames facebook
# screenshots_ggl <- list.files('/var/folders/t9/mpvzds097dg0rgn1txtmvf180000gn/T//Rtmpf2bEWV/', pattern = 'ggl.png') #lijst filenames facebook
screenshots_ggl <- list.files('screenshots', pattern = 'ggl.png') #lijst filenames facebook

screenshots <- append(screenshots_fb,screenshots_ggl)
urls <-  append(urls_fb,urls_ggl)
