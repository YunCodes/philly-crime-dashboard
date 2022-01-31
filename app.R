library(tidyverse)
library(sf)
library(rgdal)
library(shiny)
library(readr)
library(plotly)
library(leaflet)
library(DT)
library(lubridate)
library(rgeos)
library(tibble)

# After the link becoming available
X2020_crimes <- read_csv("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272020-01-01%27%20AND%20dispatch_date_time%20%3C%20%272021-01-01%27") 
X2019_crimes <- read_csv("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272019-01-01%27%20AND%20dispatch_date_time%20%3C%20%272020-01-01%27")
X2018_crimes <- read_csv("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272018-01-01%27%20AND%20dispatch_date_time%20%3C%20%272019-01-01%27")
X2017_crimes <- read_csv("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272017-01-01%27%20AND%20dispatch_date_time%20%3C%20%272018-01-01%27")
X2016_crimes <- read_csv("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272016-01-01%27%20AND%20dispatch_date_time%20%3C%20%272017-01-01%27")
X2015_crimes <- read_csv("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272015-01-01%27%20AND%20dispatch_date_time%20%3C%20%272016-01-01%27")
X2014_crimes <- read_csv("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272014-01-01%27%20AND%20dispatch_date_time%20%3C%20%272015-01-01%27")
X2013_crimes <- read_csv("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272013-01-01%27%20AND%20dispatch_date_time%20%3C%20%272014-01-01%27")
X2012_crimes <- read_csv("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272012-01-01%27%20AND%20dispatch_date_time%20%3C%20%272013-01-01%27")
X2011_crimes <- read_csv("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272011-01-01%27%20AND%20dispatch_date_time%20%3C%20%272012-01-01%27")

all_crimes <- rbind(X2019_crimes, X2019_crimes, X2018_crimes, X2017_crimes, X2016_crimes, X2015_crimes, X2014_crimes, X2013_crimes, X2012_crimes, X2011_crimes) %>%
    janitor::clean_names() %>%
    select("dc_key", "location_block", "dc_dist", "psa", "dispatch_date_time", "ucr_general",         
           "text_general_code", "point_x", "point_y") %>%
    filter(!str_detect(text_general_code, "Other")) %>%
    mutate(crime_type = case_when(str_detect(text_general_code, "Aggravated Assault") ~ "Aggravated Assault",
                                  str_detect(text_general_code, "Burglary") ~ "Burglary",
                                  str_detect(text_general_code, "Homicide") ~ "Homicide",
                                  str_detect(text_general_code, "Robbery") ~ "Robbery",
                                  str_detect(text_general_code, "Thefts") ~ "Thefts",
                                  TRUE ~ text_general_code))

# UIyes
ui <- fluidPage(
    titlePanel("6abc Action News - Crime Data Dashboard"),
    theme = shinythemes::shinytheme("slate"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("crime_type", "Select Crime Type", choices = unique(all_crimes$crime_type)),
            dateRangeInput("date_range", "Select Date Range", 
                           start = "2020-01-01", 
                           end = max(date(all_crimes$dispatch_date_time)),
                           min = "2006-01-01",
                           max = max(date(all_crimes$dispatch_date_time)))),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Trend",
                         plotly::plotlyOutput("trend")),
                tabPanel("Map",
                         leaflet::leafletOutput("map")),
                tabPanel("Top/Bottom Neighborhoods",
                         DT::DTOutput("neighborhoods"))
            )
        )
    )
)

# Server
server <- function(input, output) {
    selected_crime <- reactive({
        all_crimes %>%
            filter(crime_type == input$crime_type) %>%
            filter(date(dispatch_date_time) >= input$date_range[1] & 
                       date(dispatch_date_time) <= input$date_range[2])
    })
    
    output$trend <- plotly::renderPlotly({
        selected_crime() %>%
            group_by(date = date(dispatch_date_time)) %>%
            tally() %>%
            plot_ly(x = ~date, y = ~n, hoverinfo = "text",
                    text = ~paste("Date:", date, "<br>",
                                  "Count:", n)) %>%
            add_bars() %>%
            layout(title = list(title = "Trend"),
                   xaxis = list(title = "Count"),
                   yaxis = list(title = "Date"))
    })
    
    output$map <- leaflet::renderLeaflet({
        selected_crime() %>%
            leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%
            addMarkers(lng = ~point_x, lat = ~point_y,
                       clusterOptions = markerClusterOptions())
    })
    
    output$neighborhoods <- DT::renderDT({
        selected_crime() %>%
            group_by(dc_dist) %>%
            tally() %>%
            arrange(desc(n)) %>%
            mutate("Police District" = dc_dist,
                   "Count" = n) %>%
            select(-dc_dist, -n) %>%
            datatable()
    })
}

shinyApp(ui = ui, server = server)