# get the base page
library(rvest)
library(RJSONIO)
library(OpenStreetMapR)
library(stringr)
library(ggplot2)
library(ggmap)
library(maptools)
library(httr)
library(RJSONIO)
library(dplyr)

options(digits = 6)

uagent="Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.130 Safari/537.36"

se = html_session("https://httpbin.org/user-agent", user_agent(uagent))
se$response$request

#ht=100010 = houses and lots, ht=100000 = houses
basedata = html_session("http://www.zillow.com/search/GetResults.htm?spt=homes&status=100000&lt=110000&ht=100000&pr=,&mp=,&bd=0,&ba=0,&sf=,&lot=,&yr=,&pho=0&pets=0&parking=0&laundry=0&pnd=0&red=0&zso=0&days=any&ds=all&pmf=0&pf=0&zoom=12&rect=-105157854,39951986,-105007589,40075312&p=1&sort=days&zpid=13214794&search=maplist&disp=1&listright=true&isMapSearch=true&zoom=11")

baseJSON = fromJSON(rawToChar(basedata$response$content))


rawData = data.frame(zpid=rep(0, length(baseJSON$map$properties)), 
                     lat=rep(0, length(baseJSON$map$properties)), 
                     long=rep(0, length(baseJSON$map$properties)), 
                     price=rep(0, length(baseJSON$map$properties)),
                     sqft=rep(0, length(baseJSON$map$properties)),
                     lot=rep(0, length(baseJSON$map$properties)),
                     baths=rep(0, length(baseJSON$map$properties)),
                     beds=rep(0, length(baseJSON$map$properties)),
                     type=rep(0, length(baseJSON$map$properties)))

properties=baseJSON$map$properties
for(i in 1:length(baseJSON$map$properties)){
  rawData$zpid[i] = properties[[i]][[1]]
  rawData$lat[i] = properties[[i]][[2]]/1e6
  rawData$long[i] = properties[[i]][[3]]/1e6
  rawData$price[i] = properties[[i]][[4]] %>% str_replace('[\\$]','') %>%
    str_replace('[M]','') %>%
    str_replace('[K]', '000') %>% 
    as.character() %>% as.numeric()
  rawData$sqft[i] = ifelse(is.null(properties[[i]][[8]][[4]]), NA, properties[[i]][[8]][[4]])
  lotsize = properties[[i]][[8]][[7]]
  rawData$lot[i] = ifelse(str_detect(lotsize," ac lot"),
                          as.numeric(str_replace(lotsize, " ac lot", replacement = ""))*43560,
                          as.numeric(str_replace(lotsize, " sqft lot", replacement = "") %>%
                                       str_replace(",","")))
  rawData$baths[i] = ifelse(is.null(properties[[i]][[8]][[3]]), NA, properties[[i]][[8]][[3]])
  rawData$beds[i] = ifelse(is.null(properties[[i]][[8]][[2]]), NA,properties[[i]][[8]][[2]])
  rawData$type[i] = properties[[i]][[5]]
}
 
  
rawData$price = ifelse(rawData$price<100, as.numeric(rawData$price)*1e6, rawData$price)

rawData = na.omit(rawData)
rawData[["cluster"]] = kmeans(rawData[,c("lat", "long", "price", "beds", "baths","sqft","lot")], centers = 3)$cluster

bb = c(min(rawData$long), min(rawData$lat), max(rawData$long), max(rawData$lat))
ggmap(get_map(location=bb, maptype="toner", source="stamen"), extent="normal", 
      base_layer=ggplot(rawData, aes(x=long, y=lat, color=factor(cluster))))+#geom_density2d(data=rawData, aes(x=long, y=lat))+
  geom_point(size=5)
  
ggplot(rawData, aes(x=log(lot*sqft*baths*beds), y=log(price)))+geom_point()+geom_smooth(method="lm")
ggplot(rawData, aes(x=log(lot*sqft*baths*beds), y=(price), color=factor(cluster)))+geom_point()+coord_trans(y="log2")+facet_wrap(~cluster)
ggplot(rawData, aes(x=log(lot*sqft*baths*beds), y=log(price),color=factor(cluster)))+geom_point()+geom_smooth(method="lm")+facet_wrap(~cluster)



rawData$residuals = lm(log(price)~log(lot*sqft*baths*beds), data=rawData)$residuals

View(rawData %>% arrange(desc(residuals)))

#grab web pages for each zpid - not as necessary

# for(i in 1:3){
#   Sys.sleep(2)
#   url = "http://www.zillow.com/jsonp/Hdp.htm?zpid=RzpidR&fad=false&hc=true&lhdp=false&callback=YUI.Env.JSONP.handleHomeDetailPageRzpidR"
#   
#   propRaw = str_replace(url, 'RzpidR', rawData$zpid[i]) %>%
#   html_session(user_agent(uagent))
#   
#   property = rawToChar(propRaw$response$content)
#   
#   propSplit = str_split(property,pattern = ":")[[1]] 
#   propSplit[str_detect(propSplit,"www.zillow.com/homedetails")]
# }


