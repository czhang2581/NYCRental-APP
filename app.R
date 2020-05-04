library(psych)
library(stringr)
library(tmap)
library(leaflet)
library(rgdal)
library(purrr)
library(readr)
library(dplyr)
library(shiny)
library(stringr)
library(tidyr)
library(reshape)
library(zoo)
library(xts)
library(dygraphs)
library(shinythemes)
library(ggplot2)
library(tidyverse)




dateInput <- function(inputId, label, value, min, max, format="yyyy-mm-dd", minview = "days", maxview = "decades", ...) {
  d <- shiny::dateInput(inputId, label, value, min, max, format, ...)
  d$children[[2L]]$attribs[["data-date-min-view-mode"]] <- minview
  d$children[[2L]]$attribs[["data-date-max-view-mode"]] <- maxview
  d
}


clean_up_rent<-function(rent){

  rent <- rent%>%subset(areaType == "neighborhood"&Borough=="Manhattan")

  rent$areaName <- rent$areaName%>%as.character()
  rent$areaName[rent$areaName == "Soho"] = "SoHo"
  rent$areaName[rent$areaName == "Central Harlem"] = "Harlem"
  rent$areaName[rent$areaName == "Flatiron"] = "Flatiron District"
  rent$areaName[rent$areaName == "Gramercy Park"] = "Gramercy"
  rent$areaName[rent$areaName == "Stuyvesant Town/PCV"] = "Stuyvesant Town"
  rent$areaName[rent$areaName == "Midtown West"] = "Hell's Kitchen"
  rent$areaName[rent$areaName == "Midtown"] = "Theater District"
  rent$areaName[rent$areaName == "Midtown East"] = "Midtown"
  kp<-rep(rent[rent$areaName == "Midtown South",])%>%as.data.frame()
  rent$areaName[rent$areaName == "Midtown South"] = "Murray Hill"

  kp$areaName <- as.character(kp$areaName)
  kp$areaName <- "Kips Bay"
  rent <- rbind(rent, kp)
  return (rent)

}

clean_up_invent <- function(invent){
  invent <- invent%>%subset(areaType == "neighborhood"&Borough=="Manhattan")

  invent$areaName <- invent$areaName%>%as.character()
  invent$areaName[invent$areaName == "Soho"] = "SoHo"
  invent$areaName[invent$areaName == "Central Harlem"] = "Harlem"
  invent$areaName[invent$areaName == "Flatiron"] = "Flatiron District"
  invent$areaName[invent$areaName == "Gramercy Park"] = "Gramercy"
  invent$areaName[invent$areaName == "Stuyvesant Town/PCV"] = "Stuyvesant Town"
  invent$areaName[invent$areaName == "Midtown West"] = "Hell's Kitchen"
  invent$areaName[invent$areaName == "Midtown"] = "Theater District"
  invent$areaName[invent$areaName == "Midtown East"] = "Midtown"
  kp<-rep(invent[invent$areaName == "Midtown South",])%>%as.data.frame()
  invent$areaName[invent$areaName == "Midtown South"] = "Murray Hill"

  kp$areaName <- as.character(kp$areaName)
  kp$areaName <- "Kips Bay"
  invent <- rbind(invent, kp)
  return (invent)
}

mydata <- read.csv('medianRent.csv')
year_data <- mydata[,c(4:126)]
plotdata_month <- year_data%>%gather(time,value)
plotdata_month$time <- gsub("X","",plotdata_month$time)

plotdata_month$month <- lapply(plotdata_month$time,function(x){
  temp <- unlist(strsplit(x,split = ".",fixed = TRUE))
  temp[2]
})

plotdata_month$year <- lapply(plotdata_month$time,function(x){
  temp <- unlist(strsplit(x,split = ".",fixed = TRUE))
  temp[1]
})
plotdata_month$month <- as.numeric(plotdata_month$month)
plotdata_month$year <- as.numeric(plotdata_month$year)

average <- plotdata_month %>% group_by(year,month) %>% 
  summarise(price = mean(value,na.rm = TRUE))


nybr <- readOGR("neigborhoods_bound.geojson", verbose= FALSE)
nybr<-nybr%>%subset(as.numeric(nybr@data$borough)==3)
nybr@data$neighborhood<- as.character(nybr@data$neighborhood)
nybr<-nybr%>%subset(!nybr@data$neighborhood%in%c("Randall's Island", "Ellis Island", "Liberty Island", "Governors Island"))

rent_all <- read.csv("medianRent.csv")
invent_all <- read.csv("rentalInventory_All.csv")
rent_all<-clean_up_rent(rent_all)
invent_all <- clean_up_invent(invent_all)

rent_studio <- read.csv("medianAskingRent_Studio.csv")
invent_studio <- read.csv("rentalInventory_Studio.csv")
rent_studio<-clean_up_rent(rent_studio)
invent_studio <- clean_up_invent(invent_studio)

rent_1b <- read.csv("medianAskingRent_OneBd.csv")
invent_1b <- read.csv("rentalInventory_OneBd.csv")
rent_1b<-clean_up_rent(rent_1b)
invent_1b <- clean_up_invent(invent_1b)


rent_2b <- read.csv("medianAskingRent_TwoBd.csv")
invent_2b <- read.csv("rentalInventory_TwoBd.csv")
rent_2b<-clean_up_rent(rent_2b)
invent_2b <- clean_up_invent(invent_2b)


rent_3b <- read.csv("medianAskingRent_ThreePlusBd.csv")
invent_3b <- read.csv("rentalInventory_ThreePlusBd.csv")
rent_3b <-clean_up_rent(rent_3b)
invent_3b <- clean_up_invent(invent_3b)

PAGE_TITLE <- "Rental Genius|Manhattan,NY"


ui =  navbarPage(
  title = div(PAGE_TITLE, img(src = "city.png", height="35px", 
                              style = "position: relative; top: -10px")),
  theme = shinytheme("sandstone"),
  
  tabPanel("About",
            fluidRow(
             column(width=6, h2(strong("Apartment Rental in Manhattan"), style = "font-size:20px; color:black"))
           ),
            fluidRow(
             column(width=10, textOutput("preface")),
               column(width=2, tags$img(src = "map.png", height=300, width=100)
                      )
             ),
            fluidRow(
             column(width=6, h2(strong("Authors"), style = "font-size:20px; color:black")
                    )
              ),
            fluidRow(
             column(width=6, uiOutput("author")),
             column(width=3,tags$img(src = "nyc.png", height=200, width=280)
              )
            )
           
              
           
           ),
  
  tabPanel( "Overview",
  
  
    tags$head(tags$style(
    type = "text/css",
      "#controlPanel {background-color: rgba(255,255,255,0.8);}",
      ".leaflet-top.leaflet-right .leaflet-control {
        margin-top: 500px;
        margin-right: 685px
      }",
      ".dygraph-title {
      color: white;
      font-weight: bold;
      }",
      ".dygraph-axis-label {
      color:white;
      }", 
      ".dygraph-legend {
      color:black;
      }",
      "#date{
      color:white
      }",
      ".h1 {
      margin-bottom: 100px;
      }",
      "#h1 {
      margin-bottom: 100px;
      }"
    )),


  
    fixedPanel(
      id = "fullscreen",
      top = 60,
      left = 0,
      width = "100%",
      height = "100%",
      leafletOutput("nyplot", width = "100%", height = "100%")
    ),
  
    absolutePanel(
      id = "controls",
      draggable = FALSE,
      top = 65,
      right = 2,
      width = 400,
      height = "auto",
      id = "input_panel",
      style = "background-color:rgba(0, 0, 0, 0.5); padding:10px;",
    
    
      tabsetPanel(
        tabPanel(
          shiny::HTML("<p><span style='color: black'>Overview</span></p>"),
          dateInput("date", 
                    shiny::HTML("<p><span style='color: white'>Select a Date</span></p>"),
                    "2010-01","2010-01","2020-03", 
                    startview = "year", minview = "months", maxview = "decades"),
          selectInput(
            "bedroom",
            shiny::HTML("<p><span style='color: white'>Bedroom Count</span></p>"),
            c("All", "Studio", "One Bedroom", "Two Bedroom", "Three Bedroom+"),
            selected = "All"
          ),
        
          textOutput("date")
          ),
        
        tabPanel(
          shiny::HTML("<p><span style='color: black'>Neighborhood</span></p>"),
          selectInput(
            'neighborhood',
            shiny::HTML("<p><span style='color: white'>Select a Neighborhood</span></p>"),
            rent_all$areaName,
            selected = 'Midtown'
          ),
          selectInput(
            "bedroom2",
            shiny::HTML("<p><span style='color: white'>Bedroom Count</span></p>"),
            c("All", "Studio", "One Bedroom", "Two Bedroom", "Three Bedroom+"),
            selected = "All"
          ),
          dygraphOutput("chart", width = "100%")
        )
      )
    ),
  
  ),
  tabPanel("Time Series",
             titlePanel("Manhattan NYC"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("input1","Select year:",choices = c(2010:2020))
               ),
               mainPanel(
                 plotOutput(outputId = "plot1")
               )
             )
           ),
  tabPanel("Renter", htmlOutput("hc")),
  tabPanel("Review", htmlOutput("rr"))
)


  
server = function(input, output) {
  
  rent <- reactive({
    if (input$bedroom =="All"){
      rent <- rent_all
    }
    else if (input$bedroom =="Studio"){
      rent <- rent_studio
    }
    else if (input$bedroom =="One Bedroom"){
      rent <- rent_1b
    }
    else if (input$bedroom =="Two Bedroom"){
      rent <- rent_2b
    }
    else if (input$bedroom =="Three Bedroom+"){
      rent <- rent_3b
    }
  })
  
  rent2 <- reactive({
    if (input$bedroom2 =="All"){
      rent <- rent_all
    }
    else if (input$bedroom2 =="Studio"){
      rent <- rent_studio
    }
    else if (input$bedroom2 =="One Bedroom"){
      rent <- rent_1b
    }
    else if (input$bedroom2 =="Two Bedroom"){
      rent <- rent_2b
    }
    else if (input$bedroom2 =="Three Bedroom+"){
      rent <- rent_3b
    }
  })
  
  invent <- reactive({
    if (input$bedroom =="All"){
      invent <- invent_all
    }
    else if (input$bedroom =="Studio"){
      invent <- invent_studio
    }
    else if (input$bedroom =="One Bedroom"){
      invent <- invent_1b
    }
    else if (input$bedroom =="Two Bedroom"){
      invent <- invent_2b
    }
    else if (input$bedroom =="Three Bedroom+"){
      invent <- invent_3b
    }
  })

  output$nyplot <- renderLeaflet({
    
    rent <- rent()
    invent <- invent()

    dt <- input$date%>%format("%Y.%m")%>%as.character()
    dt <- str_c("X", dt)
    df <- rent%>%select(c(areaName, Borough, areaType, dt))
    
    colnames(df)[1] = "neighborhood"
    avg <- df%>%group_by(neighborhood)%>%summarise_at(.vars = names(.)[4], mean)
    avg <- avg%>%subset(neighborhood%in%nybr@data$neighborhood)%>%na.omit()
    colnames(avg)[2]<-"mean"
    
    df2 <- invent%>%select(c(areaName, Borough, areaType, dt))
    
    colnames(df2)[1] = "neighborhood"
    summ <- df2%>%group_by(neighborhood)%>%summarise_at(.vars = names(.)[4], sum, round=1)
    summ <- summ%>%subset(neighborhood%in%nybr@data$neighborhood)%>%na.omit()
    colnames(summ)[2]<-"sum"
    
    nybr@data<-merge(nybr@data, avg, by = "neighborhood", all = T)
    nybr@data<-merge(nybr@data, summ, by = "neighborhood", all = T)
    
    tm1 <- tm_shape(nybr) + tm_fill("mean", title = str_c("Median Rent"," [",input$date%>%format("%B, %Y")%>%as.character(),"]"),
                                            popup.vars=c("Median:$"="mean", "Inventory: "="sum"), style = "pretty") +
      tm_text("neighborhood", shadow=TRUE, bg.color="white", bg.alpha=.25,size=.6, remove.overlap=TRUE) 
    
    tmap_leaflet(tm1)
    
    
    })
  
    output$date <- renderText({
      input$date%>%format("%B, %Y")%>%as.character()
    })
    
    output$author <- renderUI({
      HTML('Group E <br> cz2581@columbia.edu <br> xx2337@columbia.edu <br> zh2411@columbia.edu <br> yz3667@columbia.edu')
    })
    
    output$preface <- renderText({
      ("We use multiple datasets of rental information of Manhattan, NY, to closely\n
      examine several different aspects of these apartments. Hopefully, this\n 
      website can help future renters to get a clear picture of the overall rental\n 
      market in Manhattan, NY, and select the one that suits them the most with ease.\n
      In OVERVIEW, future renters can access the median rent of different neighborhoods\n
      and across time. Median rent of different room types are available in the ROOM\n
      TYPE section. The RENTER tab contains rental housing characteristics and renter\n
      characteristics. The REVIEW tab includes text analysis of renter reviews."                   
              )
    })
    
    output$nycmap <- renderUI({
      tags$img(src = "images/map.png")
    })
    
    plotdata <- reactive({
      temp <- subset(average,average$year==input$input1)
      temp$label <- paste0(temp$year,'/',temp$month)
      temp
    })
    
    output$plot1 <- renderPlot({
      plotdata1 <-plotdata()
      ggplot(plotdata1,aes(month,price))+geom_bar(stat = "identity",fill="skyblue")+
        scale_x_continuous(breaks = plotdata1$month,labels = plotdata1$label)+xlab("")+
        theme(axis.text.x = element_text(angle =45))
    })
    
    
    
    output$chart <- renderDygraph({
      rent <- rent2()
      
      nb <- input$neighborhood
      df3 <- rent%>%subset(areaName==nb)
      df3 <- df3%>%select(-c(Borough, areaType))

      df3<-melt(df3)
      df3$variable <- df3$variable%>%as.character()%>%str_replace("X","")%>%str_replace("\\.","-")
      df3$variable <- as.Date(as.yearmon(df3$variable))
      colnames(df3)[3] <- "Median Asking Rent"

      xts(df3, order.by = df3$variable)

      dygraph(xts(df3, order.by = df3$variable),
        main = str_c("Median Asking Rent in ", input$neighborhood),
        xlab = "Year")%>%
        dyOptions(colors = c("red", "black")) %>%
        dyRangeSelector(height = 20) %>%
        dyHighlight(
          highlightCircleSize = 5,
          highlightSeriesBackgroundAlpha = 0.4,
          hideOnMouseOut = FALSE
          )
      
    })
    
    getPage<-function() {
      return(includeHTML("DV_GP_HC.html"))
    }
    output$hc<-renderUI({getPage()})
    
    getPage2<-function() {
      return(includeHTML("DV_GP_RR.html"))
    }
    output$rr<-renderUI({getPage2()})

}

shinyApp(ui=ui, server=server)


























