# get the base page
library(rvest)
library(RJSONIO)
library(OpenStreetMapR)
library(stringr)
library(ggplot2)
library(ggmap)
library(maptools)

options(digits = 6)

uagent="Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.130 Safari/537.36"

se = html_session("https://httpbin.org/user-agent", user_agent(uagent))
se$response$request

#ht=100010 = houses and lots, ht=100000 = houses
basedata = html_session("http://www.zillow.com/search/GetResults.htm?spt=homes&status=100000&lt=110000&ht=100000&pr=,&mp=,&bd=0,&ba=0,&sf=,&lot=,&yr=,&pho=0&pets=0&parking=0&laundry=0&pnd=0&red=0&zso=0&days=any&ds=all&pmf=0&pf=0&zoom=12&rect=-105397854,39931986,-104907589,40039312&p=1&sort=days&zpid=13214794&search=maplist&disp=1&listright=true&isMapSearch=true&zoom=11")

baseJSON = fromJSON(rawToChar(basedata$response$content))


rawData = data.frame(zpid=rep(0, length(baseJSON$map$properties)), lat=rep(0, length(baseJSON$map$properties)), long=rep(0, length(baseJSON$map$properties)), price=rep(0, length(baseJSON$map$properties)))

properties=baseJSON$map$properties
for(i in 1:length(baseJSON$map$properties)){
  rawData$zpid[i] = properties[[i]][[1]]
  rawData$lat[i] = properties[[i]][[2]]/1e6
  rawData$long[i] = properties[[i]][[3]]/1e6
  rawData$price[i] = properties[[i]][[4]]
}

rawData$priceNum = rawData$price %>% str_replace('[\\$]','') %>%
  str_replace('[M]','') %>%
  str_replace('[K]', '000') %>% 
  as.character() %>% as.numeric()
  
rawData$priceNum = ifelse(rawData$priceNum<100, rawData$priceNum*1e6, rawData$priceNum)



bb = c(min(rawData$long), min(rawData$lat), max(rawData$long), max(rawData$lat))
ggmap(get_map(bb))+geom_density2d(data=rawData, aes(x=long, y=lat))+
  geom_point(data=rawData, aes(x=long, y=lat, size=priceNum))
  
#grab web pages for each zpid

for(i in 1:3){
  Sys.sleep(2)
  url = "http://www.zillow.com/jsonp/Hdp.htm?zpid=RzpidR&fad=false&hc=true&lhdp=false&callback=YUI.Env.JSONP.handleHomeDetailPageRzpidR"
  
  str_replace(url, 'RzpidR', rawData$zpid[i])
  html_session()
}


)