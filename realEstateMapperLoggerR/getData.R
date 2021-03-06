#! /usr/bin/Rscript

library(rvest)
library(httr)
library(RJSONIO)
library(dplyr)
library(stringr)


## get some data
if(Sys.info()["sysname"]=="Darwin"){
  fetches = read.csv('searchList.csv', colClasses = c("character","integer","integer","integer","integer",rep("character",4)))
}else{
  fetches = read.csv('/home/production/realEstateMapperLogger/realEstateMapperLoggerR/searchList.csv', colClasses = c("character","integer","integer","integer","integer",rep("character",4)))
}

options(digits = 6)

uagent="Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.130 Safari/537.36"


for(j in 1:nrow(fetches)){
  sleep=rexp(n = 1,rate = 1/30)
  print(fetches[j,])
  cat("sleeping: ", sleep, '\n')
  Sys.sleep(sleep)  
  basedata = html_session(
    with(fetches[j,], 
         paste0("http://www.zillow.com/search/GetResults.htm?spt=homes&status=",status,"&lt=",lt,"&ht=",ht,
                "&pr=,&mp=,&bd=0,&ba=0,&sf=,&lot=,&yr=,&pho=0&pets=0&parking=0&laundry=0&pnd=0&red=0&zso=0&att=",att ,"&days=any&ds=all&pmf=0&pf=0&zoom=9&rect=",latStart,",",longStart,
                ",",latStop,",",longStop,"&p=1&sort=days&zpid=13214794&search=maplist&disp=1&listright=true&isMapSearch=true&zoom=11"
         ))
    ,user_agent(uagent)
    )
  
  baseJSON = fromJSON(rawToChar(basedata$response$content))
  
  
  properties=baseJSON$map$properties
  
  rawData = data.frame(zpid=rep(0, length(baseJSON$map$properties)), 
                       lat=rep(0, length(baseJSON$map$properties)), 
                       long=rep(0, length(baseJSON$map$properties)), 
                       price=rep(0, length(baseJSON$map$properties)),
                       sqft=rep(0, length(baseJSON$map$properties)),
                       lot=rep(0, length(baseJSON$map$properties)),
                       baths=rep(0, length(baseJSON$map$properties)),
                       beds=rep(0, length(baseJSON$map$properties)),
                       type=rep(0, length(baseJSON$map$properties)))
  
  if(length(properties)>0){
    
    
    for(i in 1:length(baseJSON$map$properties)){
      rawData$zpid[i] = properties[[i]][[1]]
      rawData$lat[i] = properties[[i]][[2]]/1e6
      rawData$long[i] = properties[[i]][[3]]/1e6
      rawData$price[i] = properties[[i]][[4]] %>% str_replace('[\\$]','') %>%
        str_replace('[M]','') %>%
        str_replace('[K]', '000') %>% 
        as.character() %>% as.numeric()
      rawData$sqft[i] = ifelse(is.null(properties[[i]][[9]][[4]]), NA, properties[[i]][[9]][[4]])
      lotsize = properties[[i]][[9]][[7]]
      rawData$lot[i] = ifelse(str_detect(lotsize," ac lot"),
                              as.numeric(str_replace(lotsize, " ac lot", replacement = ""))*43560,
                              as.numeric(str_replace(lotsize, " sqft lot", replacement = "") %>%
                                           str_replace(",","")))
      rawData$baths[i] = ifelse(is.null(properties[[i]][[9]][[3]]), NA, properties[[i]][[9]][[3]])
      rawData$beds[i] = ifelse(is.null(properties[[i]][[9]][[2]]), NA,properties[[i]][[9]][[2]])
      rawData$type[i] = properties[[i]][[5]]
    }
    
    
    rawData$price = ifelse(rawData$price<100, as.numeric(rawData$price)*1e6, rawData$price)
    
    if(Sys.info()["sysname"]=="Darwin"){
      write.csv(data.frame(rawData, date=Sys.Date()), file = paste0('rawData/',fetches$name[j],'_',Sys.Date()))
    }else{
      write.csv(data.frame(rawData, date=Sys.Date()), file = paste0('/home/production/realEstateMapperLogger/realEstateMapperLoggerR/rawData/',fetches$name[j],'_',Sys.Date()))
    }
  }
}

