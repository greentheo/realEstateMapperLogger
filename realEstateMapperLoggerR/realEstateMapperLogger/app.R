#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(OpenStreetMapR)
library(ggplot2)
library(lubridate)
library(broom)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  tags$head(tags$script(src="http://cdn.leafletjs.com/leaflet/v0.7.7/leaflet.js"),
            tags$link(rel = "stylesheet", type = "text/css", href = "http://cdn.leafletjs.com/leaflet/v0.7.7/leaflet.css"),
            tags$script(src="https://code.jquery.com/jquery-1.12.2.min.js"),
            tags$script(src="https://code.jquery.com/jquery-migrate-1.2.1.min.js")),
   # Application title
   titlePanel("realEstateDataMapper"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("regionSelect",label = "Region",choices = c("populating..."),multiple = F),
        dateInput("modelStart", label="Model Start Date",value = '2016-01-01'),
        dateInput("displayStart", label="Display Start Date",value = Sys.Date()-7),
        numericInput("lowerPrice", label="Low Price Filter", 0),
        numericInput("upperPrice", label="Upper Price Filter",1e7),
        checkboxInput("modelSold",label="Model from Historical (sold) data",value = F)
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
          htmlOutput("propertyPlot")
         ,dataTableOutput("propertyTable")
         ,plotOutput("activePlot")
         ,plotOutput("timePlot")
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  if(Sys.info()["sysname"]=="Darwin"){
     dataSold = read.csv('~/githubrepo/realEstateMapperLogger/realEstateMapperLoggerR/rawData/dataSoldSummary.csv')
     dataActive = read.csv('~/githubrepo/realEstateMapperLogger/realEstateMapperLoggerR/rawData/dataActiveSummary.csv')
  }else{
    dataSold = read.csv('/home/production/realEstateMapperLogger/realEstateMapperLoggerR/rawData/dataSoldSummary.csv')
    dataActive = read.csv('/home/production/realEstateMapperLogger/realEstateMapperLoggerR/rawData/dataActiveSummary.csv')
  }
   data = reactiveValues()
   
   updateSelectInput(session, "regionSelect",choices = as.character(unique(dataActive$groupName)),
                     selected = dataActive$groupName[3])

   observe({
     
     
     # if(!is.null(dataSold) & !is.null(input$regionSelect) & input$regionSelect!="populating..."){
     #   
     #   dataS = dataSold %>%
     #     filter(groupName.x==paste0(input$regionSelect,"_recentlysold"))
     #   
     #   dataS[is.na(dataS)]=0
     #   
     #   
     #   #dataS[["cluster"]] = kmeans(dataS[,c("lat.x", "long.x", "price", "beds.x", "baths.x","sqft.x","lot.x")], centers = 2)$cluster
     #   data$dataSold=dataS
     #   
     # }
     
     if(!is.null(dataActive) & !is.null(input$regionSelect)
        & input$regionSelect!="populating..." & nrow(dataActive%>%filter(groupName==input$regionSelect))>10){
        # browser()
       #do some modelling per region 
       dataA = dataActive %>%
         filter(groupName==input$regionSelect) %>%
         mutate(price=currentPrice) 
       
       
        if(grepl('land',input$regionSelect)){
          dataACluster =  1
        }else{
          dataA = na.omit(dataA)
         dataACluster =  try(kmeans((dataA[,c("lat", "long", "price", "beds", "baths","sqft","lot")]), centers = 2)$cluster)  
        }
         if(class(dataACluster)!="try-error"){
           dataA[["cluster"]]=dataACluster
         }else{
           dataA[["cluster"]]=1
         }
       
       ##need to differentiate between a land and a home model
        if(grepl('land',input$regionSelect)){
          
           dataA=dataA %>%
             filter(ymd(mostRecentListingDate)>ymd(input$modelStart), price>input$lowerPrice, price<input$upperPrice, log(price)>7.5, !is.na(lot)) %>%
             mutate(loglot = ifelse(lot>0, log(lot), 0)) %>%
             do(cbind(., augment(lm(log(price)~log(lot), data=.)))) %>%
              mutate(predPrice=exp(.fitted),
                     res=predPrice-price)
        }else{
          dataA=dataA %>%
            filter(ymd(mostRecentListingDate)>ymd(input$modelStart), price>input$lowerPrice, price<input$upperPrice, log(price)>7.5) %>%
            group_by(cluster) %>%
            do(cbind(., augment(lm(log(price)~log(lot)+log(sqft)+baths+beds, data=.)))) %>%
            mutate(predPrice=exp(.fitted),
                   res=predPrice-price)
        }
       
       data$dataActive = dataA
                  # res0 = ifelse(is.na(res),0,res)) #,
                  # color=ifelse(res<quantile(res0, .25), "#00FF00", "#000000")) 
         
     }
     
   })
   
   output$propertyTable = renderDataTable({
     if(!is.null(data$dataActive)){
       # return(data$dataActive)
       return( data$dataActive %>%
                 filter(ymd(mostRecentListingDate)>ymd(input$displayStart)) %>%
                 mutate(link=paste0("http://www.zillow.com/homedetails/",zpid,"_zpid/?view=public")) %>%
                select(mostRecentListingDate,zpid, sqft, lot, beds, baths, currentPrice,
                       res,lengthOnMarket,
                       priceTrend, cluster,link, .resid) %>%
                  arrange(desc(ymd(mostRecentListingDate)), .resid)
       )
       }
   })
   
   output$propertyPlot = renderText({
     if(!is.null(data$dataActive)){
       # browser()
       dataplot = data$dataActive %>% 
         filter(ymd(mostRecentListingDate)>ymd(input$displayStart)) %>% data.frame()
       dataplot[["color"]] = ifelse(dataplot$res<quantile(dataplot$res, probs = .25, na.rm = T), "#00ff00", "#000000")
       dataplot[["popup"]]= paste0("<h3>",dataplot$zpid,
                                      "</h3><p>Under-Over Value: ",dataplot$res,"</p>",
                                      "<p>Price: ", dataplot$price,"</p>",
                                      "<a href='http://www.zillow.com/homedetails/",dataplot$zpid,"_zpid/?view=public'>link</a>",
                                      "<p>Beds:",dataplot$beds ,"</p>",
                                      "<p>Baths: ",dataplot$baths,"</p>",
                                      "<p>sqft: ",dataplot$sqft,"</p>",
                                      "<p>lot: ",dataplot$lot,"</p>")
       dataplot["size"]=log(dataplot$price)
       return(print(OSMMap(df = dataplot,lat = "lat",long="long", size="size",
                           popup="popup",color = "color",colorByFactor = F
                           #need to do some more work on the popup... good for now


       ), returnText=T, title='REMap',subtitle=''))
     }

   })
   output$timePlot = renderPlot({
     if(!is.null(data$dataActive)){
       # browser()
     # dataPlot = data$dataActive %>%
     #   group_by(mostRecentListingDate,) %>%
     #   summarize(price=mean(log(price)),
     #             numSold=count(price)
     #             )
     
     ggplot(data$dataActive , aes(x=mostRecentListingDate, y=log(price), color=factor(cluster)))+
       geom_point(size=3)+
       # geom_bar(aes(x=mostRecentListingDate, y=log(numSold), fill=factor(cluster)))
       geom_smooth(method="lm")+
       labs(title='Active Properties Over Time')
     }
   })
   output$activePlot = renderPlot({
     
     if(!is.null(data$dataActive)){
       if(grepl('land',input$regionSelect)){  
         ggplot(data$dataActive, aes(x=log(lot),y=log(price), color=factor(cluster)))+
           geom_point(size=3)+
           geom_smooth(method="lm")+
           labs(title='Active Properties')
         
       }else{
         ggplot(data$dataActive, aes(x=log(lot*sqft*baths*beds),y=log(price), color=factor(cluster)))+
           geom_point(size=3)+
           geom_smooth(method="lm")+
           labs(title='Active Properties')
       }
     }
   })
})

# Run the application 
shinyApp(ui = ui, server = server)

