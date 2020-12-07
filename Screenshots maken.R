top_ads <- read_sheet('https://docs.google.com/spreadsheets/d/1K9s8J1XyK5tHggGkG6MmIdSu_nuNH-vJAINnIpu1VCY/edit#gid=966467927')
url <- 'https://www.facebook.com/ads/archive/render_ad/?id=898765480658179&access_token=EAAO3V3HJa24BAGkiP6kiCsk9pIXFoxHZAm5apZARt7oBRBT1oBJLfSzeFAJwifnEXy3VxEgaZAZCkqDQucGRxmZCXTPbzywoBZAUTdzbmLqVqrbSsqidssyE2q697dYs3aKUcRWsEbVogAi7g2vWdu9b5FVZBt3ltFNhBDGY4QFULVQt5eKBJu8'

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






