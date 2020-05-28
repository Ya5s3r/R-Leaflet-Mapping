library(dplyr)
library(readr)
library(shiny)
library(leaflet)
library(rgdal)
library(rmapshaper)
library(spdplyr) # allows dplyr to work with spatial data frames
library(DT)
library(shinyWidgets)
###import acorn data####
acorn_profiles <- read_csv("acorn_profile_data.csv")
###import HIGH RESOLUTION lsoa shape file and transform type####
Lancs_lsoa_HR <- readOGR("Lower_Layer_Super_Output_Areas_December_2011_Full_Extent__Boundaries_in_England_and_Wales.shp", 
                         "Lower_Layer_Super_Output_Areas_December_2011_Full_Extent__Boundaries_in_England_and_Wales",
                         stringsAsFactors = FALSE) %>%
                 spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))
###colour pal####
pal2 <- colorNumeric("viridis", NULL)
###UI for app####
ui <- fluidPage(titlePanel("Lancashire and South Cumbria STP - Acorn Data"),
                p(),
                sidebarLayout(
                    sidebarPanel(
                pickerInput("CCG", "CCG Name", unique(acorn_profiles$CCGName), options = list(`actions-box` = TRUE), multiple = TRUE),
                pickerInput("Measure", "Measure", unique(acorn_profiles$PropensityMeasure), options = list(`actions-box` = TRUE), multiple = TRUE),
                pickerInput("Wellbeing", "Wellbeing", unique(acorn_profiles$WellbeingAcornFullDescription), options = list(`actions-box` = TRUE), multiple = TRUE),
                pickerInput("HouseholdType", "Household Type", unique(acorn_profiles$AcornHouseholdType), options = list(`actions-box` = TRUE), multiple = TRUE),
                selectInput("Base", "Basemap", c("OpenStreetMap.Mapnik","Esri.WorldStreetMap","Esri.WorldImagery"), selected = "OpenStreetMap.Mapnik", multiple = FALSE)),
                mainPanel(
                    tabsetPanel(type = "tabs",
                                tabPanel("Map",leafletOutput("mymap", height = 600)),
                                tabPanel("Acorn Data",dataTableOutput("test_table")),
                                tabPanel("LSOA Summary",dataTableOutput("LSOA_summary"))
                    )
                )
                ))

###server for app####
server <- function(input, output, session) {
    output$mymap <- renderLeaflet({
        Lancs_lsoa_acorn <- Lancs_lsoa_HR %>%
            inner_join(acorn_profiles %>%
                           filter(
                               CCGName %in% input$CCG,
                               PropensityMeasure %in% input$Measure,
                               WellbeingAcornFullDescription %in% input$Wellbeing,
                               AcornHouseholdType %in% input$HouseholdType
                           ) %>%
                           group_by(LSOA) %>%
                           summarise(
                               total_population = sum(Population),
                               total_propensity_pop = sum(PropensityPopulation, na.rm = TRUE),
                               rate = round(total_propensity_pop/total_population *100, 2)
                           ),c("lsoa11cd" = "LSOA")
            )
        
        leaflet(Lancs_lsoa_acorn) %>% addProviderTiles(input$Base) %>%
            addPolygons(group = "Propensity Population", weight = 1, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5, color = "#444444", 
                        label = ~paste0(Lancs_lsoa_acorn$lsoa11cd, ": ", formatC(Lancs_lsoa_acorn$total_propensity_pop)),
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = TRUE),
                        fillColor = ~pal2(Lancs_lsoa_acorn$total_propensity_pop)) %>%
            addLegend("bottomleft",pal = pal2, values = Lancs_lsoa_acorn$total_propensity_pop,
                      opacity = 0.7, group = "Propensity Population", title ="Propensity Population" ) %>%
            addPolygons(group = "Rate per Population", weight = 1, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5, color = "#444444", 
                        label = ~paste0(Lancs_lsoa_acorn$lsoa11cd, ": ", formatC(Lancs_lsoa_acorn$rate)),
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = TRUE),
                        fillColor = ~pal2(Lancs_lsoa_acorn$rate)) %>%
            addLegend("bottomright",pal = pal2, values = Lancs_lsoa_acorn$rate,
                      opacity = 0.7, group = "Rate per Population", title = "Rate per Population") %>%
            addLayersControl(baseGroups = c("Propensity Population","Rate per Population"), options = 
                                 layersControlOptions(collapsed = TRUE))
    })
    
    output$test_table <- renderDataTable({
        acorn_profiles %>%
                 subset(
                     CCGName %in% input$CCG &
                         PropensityMeasure %in% input$Measure &
                         WellbeingAcornFullDescription %in% input$Wellbeing &
                         AcornHouseholdType %in% input$HouseholdType
                 )
        
    })
    
    output$LSOA_summary <- renderDataTable({
        acorn_profiles %>%
            filter(
                CCGName %in% input$CCG,
                PropensityMeasure %in% input$Measure,
                WellbeingAcornFullDescription %in% input$Wellbeing,
                AcornHouseholdType %in% input$HouseholdType
            ) %>%
            group_by(LSOA) %>%
            summarise(
                total_population = sum(Population),
                total_propensity_pop = sum(PropensityPopulation, na.rm = TRUE),
                rate = round(total_propensity_pop/total_population *100, 2)
            )
    })
}

###run app####
shinyApp(ui, server)