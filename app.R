library(shinydashboard)
library(jsonlite)
library(tidyverse)
library(DT)

rawstationdata <- fromJSON(txt="./data/stations.json", flatten = TRUE)
stationdata <- select(rawstationdata, railway, title.en, title.ja)
colnames(stationdata)=c("Railway", "Name-EN", "Name-JP")

rawtimedatawkd <- fromJSON(txt="./data/timetable-weekday.json", flatten = TRUE)
timedatawkd <- select(rawtimedatawkd, t, r, d, tt)
colnames(timedatawkd)=c("Train", "Railway", "Direction", "TimeTable")

rail_lines <- names(split(timedatawkd,timedatawkd$Railway))

header <- dashboardHeader(title = "Train Selector")

sidebar <- dashboardSidebar(
  tags$aside(tags$style(HTML('.well {background-color:#222d32}'))),
  sidebarPanel(
    selectizeInput(
      "line", 
      label = "Choose a train line",
      choices = data.frame(names(rail_lines)),
      options = list(
        placeholder = 'Select a line',
        onInitialize = I('function() { this.setValue("");}')
      )
    ),
    selectizeInput(
      "station", 
      label = "Enter station",
      choices = stationdata$"Name-EN",
      options = list(
        placeholder = 'Select a station',
        onInitialize = I('function() { this.setValue("");}')
      )
    ),
    width = 20
  )
)

body <- dashboardBody(
  fluidRow( 
    box(
      width = 6,
      h2("Station List"), 
      dataTableOutput("selected_line_stations")
    ),
    box(
      width = 6,
      h3("Timetable"),
      textOutput("selected_line"),
      textOutput("selected_station"),
      dataTableOutput("station")
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
  
  observe(
    output$station <- renderDataTable(
      {
        req(input$line)
        df1 <- subset(timedatawkd, timedatawkd$Railway == input$line)
        
        req(df1)
        dir <- dir <- unique(df1[, 3])
        
        inb <- select(subset(df1, df1$Direction == dir[1]), Train, TimeTable)
        rownames(inb) <- NULL
        oub <- select(subset(df1, df1$Direction == dir[2]), Train, TimeTable)
        rownames(inb) <- NULL
        
        inb_list <- lapply(inb[[2]], select, dt, ds)
        inb_data = do.call(rbind, inb_list)
        inb_data <- na.omit(inb_data)
        
        oub_list <- lapply(oub[[2]], select, dt, ds)
        oub_data = do.call(rbind, oub_list)
        oub_data <- na.omit(oub_data)
        
        inb_table <- split(inb_data,inb_data$ds)
        oub_table <- split(oub_data,oub_data$ds)
        
        req(input$station)
        
        ttinb <- subset(inb_data, inb_data$ds == paste(input$line, input$station, sep="."))
        ttinb2 <- ttinb[order(ttinb$dt), ]
        rownames(ttinb) <- NULL
        
        ttoub <- subset(oub_data, oub_data$ds == paste(input$line, input$station, sep="."))
        ttoub <- ttoub[order(ttoub$dt), ]
        rownames(ttoub) <- NULL
        
        max <-max(length(ttoub[,1]), length(ttinb[,1]))
        Inbound <- c(ttinb[,1], rep(NA, max - length(ttinb[,1])))
        Outbound <- c(ttoub[,1], rep(NA, max - length(ttoub[,1])))
        sched <- data.frame(cbind(Outbound, Inbound))
        colnames(sched) <- c(dir[2], dir[1])
        return (sched)
      }
    )
  )
  
  output$selected_line_stations <- renderDataTable(
    {stationdata}, filter = 'top')
  
  output$selected_line <- renderText(input$line)
  output$selected_station <- renderText(input$station)
  
  observeEvent(input$line, {
    req(input$line)
    df1 <- subset(timedatawkd, timedatawkd$Railway == input$line)
    req(df1)
    dir <- dir <- unique(df1[, 3])
    
    inb <- select(subset(df1, df1$Direction == dir[1]), Train, TimeTable)
    rownames(inb) <- NULL
    oub <- select(subset(df1, df1$Direction == dir[2]), Train, TimeTable)
    rownames(inb) <- NULL
    
    inb_list <- lapply(inb[[2]], select, dt, ds)
    inb_data = do.call(rbind, inb_list)
    inb_data <- na.omit(inb_data)
    
    oub_list <- lapply(oub[[2]], select, dt, ds)
    oub_data = do.call(rbind, oub_list)
    oub_data <- na.omit(oub_data)
    
    inb_table <- split(inb_data,inb_data$ds)
    oub_table <- split(oub_data,oub_data$ds)
    
    stations <- names(oub_table)
    stations <- sapply(strsplit(stations, split='.', fixed=TRUE), function(x) (x[[3]]))
    st_names <- data.frame(stations)
   
    req(st_names)
    updateSelectizeInput(
      session, 
      "station",
      choices = st_names,
      options = list(
        placeholder = 'Select a station',
        onInitialize = I('function() { this.setValue("");}')
        )
      )
    
    }
  )
}

shinyApp(ui = ui, server = server)

