setwd('/Users/marcwijnen/DutchElectionObservatory')

#meest bekeken ad per partij (bij gelijkspel de duurste)
fb_top <- fb_dat %>% group_by(advertiser_id) %>% top_n(1, impressions_upper_bound)
fb_top <- fb_top %>% group_by(advertiser_id) %>% top_n(1, spend_upper_bound)
fb_top <- fb_top %>% group_by(advertiser_id) %>% top_n(1, date_range_start)
fb_top <- fb_top %>% group_by(advertiser_id) %>% filter(row_number()==1)

ggl_top <- ggl_ads %>% group_by(advertiser_id) %>% top_n(1, impressions_upper_bound)
ggl_top <- ggl_top %>% group_by(advertiser_id) %>% top_n(1, spend_range_max_eur)
ggl_top <- ggl_top %>% group_by(advertiser_id) %>% top_n(1, date_range_start)
ggl_top <- ggl_top %>% group_by(advertiser_id) %>% filter(row_number()==1)

top_ads <- merge(fb_top[,c('advertiser_name','ad_snapshot_url')],ggl_top[,c('advertiser_name','ad_url')], all = T)

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
datum$week <- format(datum$datum, "%Y-%V")
datum$maand <- format(datum$datum, "%Y-%m")

week <- aggregate(datum[,c(3:16)], by = list('partij'=datum$partij,'week'=datum$week), FUN = function(x){sum(x, na.rm = T)})
maand <- aggregate(datum[,c(3:16)], by = list('partij'=datum$partij,'maand'=datum$maand), FUN = function(x){sum(x, na.rm = T)})


#uitgaven over hele periode alle maanden
totaal <- datum[which(datum$datum == max(datum$datum)),]
names(totaal)[4] <- 'Google'
names(totaal)[9]<- 'Facebook'

#######################################################################################################################
################################################### Doelgroepen #######################################################
#######################################################################################################################

## Google
ggl_target <- ggl_ads[,c(1,6,15:17)]

ggl_target$age_targeting <- 
  str_replace(ggl_target$age_targeting,'Not targeted',"18-24, 25-34, 35-44, 45-54, 55-64, ≥65, Unknown age")
ggl_target$age_targeting <- as.list(strsplit(ggl_target$age_targeting, ", |,"))
leeftijden <- unique(unlist(ggl_target$age_targeting))
ggl_target <- cbind(ggl_target,sapply(leeftijden, function(x) as.integer(grepl(x, ggl_target$age_targeting))))

ggl_target$geo_targeting_included <- str_replace(ggl_target$geo_targeting_included,"Netherlands","Drenthe, Flevoland, Friesland, Gelderland, Groningen, Limburg, North Brabant, North Holland, Overijssel, Utrecht, Zeeland, South Holland")
ggl_target$geo_targeting_included <- as.list(strsplit(ggl_target$geo_targeting_included, ", |,"))
locaties <- unique(unlist(ggl_target$geo_targeting_included))
ggl_target <- cbind(ggl_target,sapply(locaties, function(x) as.integer(grepl(x, ggl_target$geo_targeting_included))))

ggl_target$gender_targeting <- str_replace(ggl_target$gender,'Not targeted','Men, Women, Unknown')
ggl_target$gender_targeting <- as.list(strsplit(ggl_target$gender_targeting, ", |,"))
genders <- unique(unlist(ggl_target$gender_targeting))
ggl_target <- cbind(ggl_target,sapply(genders, function(x) as.integer(grepl(x, ggl_target$gender_targeting))))


ggl_target_sum <- aggregate(ggl_target[,6:39], by = list('partij'=ggl_ads$advertiser_name),FUN = function(x){round(mean(x),3)})
names(ggl_target_sum)
ggl_target_sum2 <- gather(ggl_target_sum,'geo','perc_geo',-c("partij", "18-24","25-34","35-44","45-54","55-64","≥65","Unknown age","Men","Women"))
ggl_target_sum2 <- gather(ggl_target_sum2,'leeftijd','perc_leeftijd',-c("partij","Men","Women","geo","perc_geo"))
ggl_target_sum2 <- gather(ggl_target_sum2,'gender','perc_gender',-c("partij","geo","perc_geo",'leeftijd','perc_leeftijd'))
ggl_target_sum2 <- ggl_target_sum2 %>%
  mutate(perc_combi = perc_geo*perc_leeftijd*perc_gender)
ggl_target_sum2 <- ggl_target_sum2[,c(1,2,4,6,3,5,7,8)]



## Facebook
#regios
geo_targeted
geo_targeted2 <- spread(geo_targeted,key = region,value = percentage)
geo_targeted3 <- geo_targeted2[, !names(geo_targeted2) %in% c("Flemish Region","Niedersachsen","Nordrhein-Westfalen","Unknown","Wallonia")]
geo_targeted3 <- geo_targeted3[,c(1,3,22:33)]
for (col in c(3:14)){
  geo_targeted3[which(is.na(geo_targeted3[,col])),col] <- 0
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
################################################### Exporteren ########################################################
#######################################################################################################################


#export lokaal
sheets <- list('dag'=datum, 'week'=week,'maand'=maand, 'totaal'=totaal,'google_target'= ggl_target_sum2,'fb_target'=targeted_sum)
write.xlsx(sheets,'ads.xlsx')

sheets <- list('top_ads'=top_ads)
write.xlsx(sheets,'top ads.xlsx')

sheets <- list('dag'=datum)
write.xlsx(sheets,'dag.xlsx')

sheets <- list('week'=week)
write.xlsx(sheets,'week.xlsx')

sheets <- list('totaal'=totaal)
write.xlsx(sheets,'totaal.xlsx')

sheets <- list('fb_target'=targeted_sum)
write.xlsx(sheets,'fb target.xlsx')


#######################################################################################################################
################################################### Printscreens ########################################################
#######################################################################################################################

                                      namen_fb <- paste(top_ads$advertiser_name[which(!is.na(top_ads$Facebook))], rep(c(' fb.png'),length(which(!is.na(top_ads$Facebook)))), sep = '')
urls_fb <- top_ads$Facebook[which(!is.na(top_ads$Facebook))]
namen_ggl <- paste(top_ads$advertiser_name[which(!is.na(top_ads$Google))], rep(c(' ggl.png'),length(which(!is.na(top_ads$Google)))), sep = '')
urls_ggl <- top_ads$Google[which(!is.na(top_ads$Google))]


## Rselenium ##
#start Rselenium
rD <- rsDriver(browser = 'firefox', port = 4592L)
remDr <- rD[["client"]]

#functies
maak_screenshot_fb <- function(url, file_name){
  #go to page
  remDr$navigate(url)
  
  #click cookies toestaan
  try(webElem <- remDr$findElement(using = 'css selector',"#u_0_g"), silent = T)
  try(webElem$clickElement(), silent = T)
  
  #scrollen
  remDr$executeScript(paste("scroll(0,65);"))
  
  
  #neem screenshot
  remDr$maxWindowSize()
  remDr$screenshot(file = file_name)
}
#### WERKT NOG NIET #####
maak_screenshot_ggl <- function(url, file_name){
  #go to page
  remDr$navigate(url)
  
  # #click cookies toestaan
  # try(webElem <- remDr$findElement(using = 'css selector',"#u_0_g"), silent = T)
  # try(webElem$clickElement(), silent = T)
  # 
  # #scrollen
  for(i in 1:20) {
    remDr$executeScript(paste("scroll(0,",i*10000,");"))
    Sys.sleep(1)
  }
  # #neem screenshot
  Sys.sleep(2)
  remDr$maxWindowSize()
  remDr$screenshot(file = file_name)
}

#screenshots maken
for (num in c(1:length(urls))){
  maak_screenshot_fb(urls_fb[num],namen_fb[num])
}#Facebook
for (num in c(1:length(urls))){
  maak_screenshot_ggl(urls_ggl[num],namen_ggl[num])
}#Google



