#! /usr/bin/Rscript

###this script aggregates, cleans and compiles data ffrom the raw data
library(broom)
library(dplyr)
library(stringr)
library(magrittr)
library(lubridate)


if(Sys.info()["sysname"]=="Darwin"){
  files=list.files('rawData/',pattern = '*')
  files = data.frame(fullpath=paste0('~/githubrepo/realEstateMapperLogger/realEstateMapperLoggerR/rawData/', files),
                     fileName=files)
}else{
  files=list.files('/home/production/realEstateMapperLogger/realEstateMapperLoggerR/rawData/',pattern = '*')
  files = data.frame(fullpath=paste0('/home/production/realEstateMapperLogger/realEstateMapperLoggerR/rawData/', files),
                     fileName=files) 
}

readData = function(file){
  return(data.frame(filename=file$fileName, read.csv(file=file$file)))
}
data = files %>% 
  group_by(fullpath) %>% 
  mutate(file = as.character(fullpath)) %>%
  do(data = readData(.)) 
  
dataAll = data %>% extract2(2) %>% bind_rows()

dataAll[["groupName"]] = str_replace(dataAll$filename,pattern = '\\_201[0-9]-.*$', '') 
dataAll[["Date"]] = ymd(dataAll$date)

### high level summary at the group, property level for actively selling (not recently sold)
dataActiveSummary = dataAll %>% 
  filter(!str_detect(groupName, 'recentlysold')) %>%
  arrange(Date) %>%
  group_by(groupName, zpid, lat, long,sqft,lot, baths, beds, type) %>%
  summarize(lengthOnMarket=max(Date)-min(Date),
            avgPrice=mean(price),
            maxPrice=max(price),
            minPrice=min(price),
            currentPrice=price[length(price)],
            priceTrend=(price[1]-price[length(price)])/lengthOnMarket,
            mostRecentListingDate=max(Date)) %>%
  filter(lengthOnMarket>0)

dataSoldSummary = dataAll %>% 
  filter(str_detect(groupName, 'recentlysold')) %>%
  arrange(desc(Date)) %>%
  group_by(groupName, zpid,lat, long,sqft,lot, baths, beds, type) %>%
  summarize(price=price[1]) %>%
  left_join((dataActiveSummary %>% select(lengthOnMarket, avgPrice, maxPrice, minPrice, priceTrend)),
            by = 'zpid')


#write the summary data

if(Sys.info()["sysname"]=="Darwin"){
  
  write.csv(dataActiveSummary, 
            '~/githubrepo/realEstateMapperLogger/realEstateMapperLoggerR/rawData/dataActiveSummary.csv')
  write.csv(dataSoldSummary, 
            '~/githubrepo/realEstateMapperLogger/realEstateMapperLoggerR/rawData/dataSoldSummary.csv')
  
}else{
  write.csv(dataActiveSummary, 
            '/home/production/realEstateMapperLogger/realEstateMapperLoggerR/rawData/dataActiveSummary.csv')
  write.csv(dataSoldSummary, 
            '/home/production/realEstateMapperLogger/realEstateMapperLoggerR/rawData/dataSoldSummary.csv')
  
}



