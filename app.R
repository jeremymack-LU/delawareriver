library(shiny)
library(tidyverse)
library(dataRetrieval)
library(lubridate)
library(shinydashboard)
library(leaflet)

url <- "https://raw.githubusercontent.com/jeremymack-LU/delawareriver/master/data/dr_hourly.csv"
df  <- read_csv(url)
df  <- df %>% mutate(dateTime=dateTime-(3600*8))

df <- df %>%
  mutate(station_nm=case_when(
    station_nm == "DELAWARE RIVER AT LORDVILLE NY" ~ "Delaware River at Lordville NY",
    station_nm == "DELAWARE RIVER AT CALLICOON NY"  ~ "Delaware River at Callicoon NY",
    station_nm == "DELAWARE RIVER AT BARRYVILLE NY"  ~ "Delaware River at Barryville NY",
    station_nm == "DELAWARE RIVER AT PORT JERVIS NY"  ~ "Delaware River at Port Jervis NY",
    station_nm == "DELAWARE RIVER AT NEW CASTLE, DE"  ~ "Delaware River at New Castle DE",
    station_nm == "Delaware River near Delaware Water Gap Pa"  ~ "Delaware River at Delaware Water Gap PA",
    TRUE ~ station_nm
  ))

df <- df %>%
  mutate(station_nm=str_remove(station_nm,"Delaware River at")) %>%
  mutate(station_nm=str_remove(station_nm,"US Route 22 at")) %>%
  mutate(station_nm=str_squish(station_nm))

df.sites <- df %>%
  group_by(station_nm) %>%
  slice(n()) %>%
  arrange(site_id)

time <- df %>% slice(n())
time <- as.character(time[['dateTime']])


ui <- dashboardPage(
  skin="black",
  dashboardHeader(title="Delaware River - USGS streamgages",
                  titleWidth=350),
  dashboardSidebar(
    width=350,
    fluidRow(
             column(12,
                    align='center',
    selectInput(inputId="window",
                label=HTML(paste(p(HTML('&nbsp;'),strong("Select time window:*")))),
                choices=c("Entire time series", "14-day", "7-day", "1-day")))),
    fluidRow(
             column(12,
                    align='center',
                    selectInput(inputId="site",
                                label=HTML(paste(p(HTML('&nbsp;'),strong("Select site:*")))),
                                choices=df.sites[['station_nm']]))),
    br(),
    fluidRow(column(12,
                    align='center',
                    #offset=1,
                    tableOutput(outputId="table")))
    ),
  dashboardBody(
    fluidRow(
      column(6, box(leafletOutput("map",height="800px"), width = NULL, solidHeader=TRUE)),
      column(6, box(plotOutput("plot",height="800px"), width = NULL, solidHeader=TRUE))),
    fluidRow(
      column(12,
             align='center',
             paste("*Data updated at",time,"ET. Data provided by USGS dataRetrieval package in R")))))

server <- function(input, output) {
  # Create window variable based on selection
  window <- reactive({
    req(input$window)
    window <- case_when(
      input$window=="Entire time series" ~ 1000,
      input$window=="14-day" ~ 14,
      input$window=="7-day" ~ 7,
      TRUE ~ 1)
  })
  # Subset data based on date and window selection
  df2 <- reactive({
    df %>%
      filter(dateTime >= Sys.Date()-window())
  })
  
  sum.table <- reactive({
    df2b <- df2() %>%
      filter(station_nm==input$site)
    
    tibble(
      'Variable'=c('Site','Length','Avg. Max. Height','Max. Height'),
      'Value'=c(input$site,
                input$window,
                round(mean(df2b$max, na.rm=TRUE),1),
                round(max(df2b$max, na.rm=TRUE),1))
    )
  })

  # Table
  output$table <- renderTable(sum.table(),bordered=TRUE,colnames=FALSE)
  # Map output
  output$map <- renderLeaflet({
    # Default view location
    lat <- 40.8
    lng <- -74.70186
    zoom <- 8
    # Leaflet map
    leaflet() %>%
      setView(lat = lat,
              lng = lng,
              zoom = zoom) %>%
      addTiles("https://tiles.stadiamaps.com/tiles/alidade_smooth/{z}/{x}/{y}{r}.png") %>%
      addCircleMarkers(data=df.sites,
                       lng=~longitude,
                       lat=~latitude,
                       label=~site_id,
                       popup=~station_nm,
                       labelOptions=labelOptions(
                         #textOnly=TRUE,
                         direction="right",
                         permanent=TRUE,
                         offset=c(15,0)),
                       stroke=FALSE,
                       fillOpacity=0.5)
  })
  # Plot output
  output$plot <- renderPlot({
    df2() %>% 
      ggplot() + 
      geom_line(aes(x=dateTime,y=max)) +
      labs(x="Date & Time",
           y="Maximum streamgage height (ft.)") +
      lemon::facet_rep_grid(row=vars(site_id)) +
      theme(panel.background=element_blank(),
            panel.grid=element_blank(),
            plot.title=element_text(size=8, color="black"),
            strip.background=element_rect(color="black", size=0.25),
            axis.line=element_line(size=0.25),
            axis.ticks=element_line(size=0.25),
            axis.text=element_text(size=7, color="black"),
            axis.title=element_text(size=12, color="black"),
            legend.key=element_blank(),
            legend.title=element_text(size=8, color="black"),
            legend.text=element_text(size=7))
  })
}

shinyApp(ui, server)