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
        selectInput("regionSelect",label = "Region",choices = c("populating..."),multiple = F)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
          htmlOutput("propertyPlot")
         ,dataTableOutput("propertyTable")
         ,plotOutput("modelPlot")
         ,plotOutput("activePlot")
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
   dataSold = read.csv('~/githubrepo/realEstateMapperLogger/realEstateMapperLoggerR/rawData/dataSoldSummary.csv')
   dataActive = read.csv('~/githubrepo/realEstateMapperLogger/realEstateMapperLoggerR/rawData/dataActiveSummary.csv')
   
   data = reactiveValues()
   
   updateSelectInput(session, "regionSelect",choices = as.character(unique(dataActive$groupName)),
                     selected = as.character(dataActive$groupName[3]))

   observe({
     
     
     if(!is.null(dataSold) & !is.null(input$regionSelect) & input$regionSelect!="populating..."){
       
       dataS = dataSold %>%
         filter(groupName.x==paste0(input$regionSelect,"_recentlysold"))
       
       dataS[is.na(dataS)]=0
       
       
       dataS[["cluster"]] = kmeans(dataS[,c("lat.x", "long.x", "price", "beds.x", "baths.x","sqft.x","lot.x")], centers = 2)$cluster
       data$dataSold=dataS
       data$salemodel = lm(log(price)~log(lot.x*sqft.x*baths.x*beds.x),
                           data=dataS %>% filter(log(lot.x*sqft.x*baths.x*beds.x)>10, log(price)>7.5 ) ) 
     }
     
     if(!is.null(dataActive) & !is.null(input$regionSelect)
        & input$regionSelect!="populating..." & !is.null(data$salemodel)){
       # browser()
       #do some modelling per region 
       dataA = dataActive %>%
         filter(groupName==input$regionSelect) %>%
         mutate(lot.x=lot,
                sqft.x=sqft,
                baths.x=baths,
                beds.x=beds,
                price=currentPrice) %>%
         mutate(predPrice=exp(predict(data$salemodel,newdata=.)),
                res=price - predPrice)
       dataA = na.omit(dataA)
       dataACluster =  try(kmeans((dataA[,c("lat", "long", "price", "beds.x", "baths.x","sqft.x","lot.x")]), centers = 2)$cluster)
       if(class(dataACluster)!="try-error"){
         dataA[["cluster"]]=dataACluster
       }else{
         dataA[["cluster"]]=1
       }
       data$dataActive = dataA
                  # res0 = ifelse(is.na(res),0,res)) #,
                  # color=ifelse(res<quantile(res0, .25), "#00FF00", "#000000")) 
         
     }
     
   })
   
   output$propertyTable = renderDataTable({
     if(!is.null(data$dataActive)){
       return( data$dataActive %>%
                 mutate(link=paste0("http://www.zillow.com/homedetails/",zpid,"_zpid/?view=public")) %>%
                select(zpid, sqft, lot, beds, baths, currentPrice, 
                       predPrice,res,lengthOnMarket, avgPrice, 
                       priceTrend, cluster,link) %>%
                  arrange(res)
       )
       }
   })
   
   output$propertyPlot = renderText({
     if(!is.null(data$dataActive)){
       dataplot = data$dataActive
       dataplot[["color"]] = ifelse(dataplot$res<quantile(dataplot$res, probs = .25, na.rm = T), "#00ff00", "#000000")
       dataplot[["popup"]]= paste0("<h3>",data$dataActive$zpid
                                      ,"</h3><p>",data$dataActive$res,"</p><a href='http://www.zillow.com/homedetails/",data$dataActive$zpid,"_zpid/?view=public'>link</a>")
       return(print(OSMMap(df = dataplot,lat = "lat",long="long",
                           popup="popup",color = "color",colorByFactor = F
                           #need to do some more work on the popup... good for now


       ), returnText=T))
     }

   })
   output$modelPlot = renderPlot({
     ggplot(data$dataSold %>% filter(log(price)>7.5,log(lot.x*sqft.x*baths.x*beds.x)>10), aes(x=log(lot.x*sqft.x*baths.x*beds.x),y=log(price), color=factor(cluster)))+
       geom_point()+
       geom_smooth(method="lm")+
       labs(title='Sold Properties')
   })
   output$activePlot = renderPlot({
     ggplot(data$dataActive %>% filter(log(price)>7.5,log(lot.x*sqft.x*baths.x*beds.x)>10), aes(x=log(lot.x*sqft.x*baths.x*beds.x),y=log(price), color=factor(cluster)))+
       geom_point()+
       geom_smooth(method="lm")+
       labs(title='Active Properties')
   })
})

# Run the application 
shinyApp(ui = ui, server = server)

