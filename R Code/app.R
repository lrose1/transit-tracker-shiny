library(shiny)
library(tidyverse)
library(shinymaterial)
library(ggpubr)
library(Cairo)
library(shinyWidgets)
library(sf)
library(leaflet)
options(shiny.usecairo=T)

#### Data ####


if (!exists("transit_data")) {
    transit_data <- read_csv("transit_data.csv") 
}
if (!exists("agg1")) {
    agg1 <- read_csv("origin_mode2.csv") %>% 
        st_as_sf(wkt = "WKT")
}


#### UI #####


ui <- material_page( primary_theme_color = "#64b5f6",
                     
                     tags$head(tags$style(HTML(' p {
        font-size: 16px;
      }
    '))),
    #theme = shinytheme("sandstone"),
                 title ="Comparing driving and transit in the San Francisco Bay Area",   
    br(),
     material_row(
         material_column(
             width = 12,
             material_card(
             strong("Key Takeaway: Taking transit is 30-40 minutes slower than driving for trips under 15 miles.", style = "bold") 
         ))
    ),
    h4("Background", align='center'),
    material_row(style='padding-left:5%;padding-right:5%',
        material_column(
            width = 12,
                HTML("<p>Traffic is a big issue in the Bay Area, 
    with millions going into infrasturucture projects each year and still some of the longest commute times in the country before the COVID-19 pandemic.<a href='https://www.statista.com/statistics/578402/metro-networks-in-us-longest-commuting-time/'><sup>1</sup></a>
      Yet most individuals choose to drive, with less than 20% commuting by public transit in the San Francisco-Oakland-Berkeley
      metro area and less than 5% in the San Jose-Sunnyvale-Santa Clara metro area.</p>"),
                
                HTML("<p>Surveys indicate that 
      frequency and reliability are needed to convince people to take transit.<a href='https://transitcenter.org/publication/whos-on-board-2019/#executive-summary'><sup>2</sup></a>
      Making this a reality is difficult, as the region has 27 transit agencies that have to work with 9 county governments and 101 municipal governments. 
      Making the task even harder, transit has been much slower to rebound from the pandemic and faces a perilous financial future.<a href='https://www.sfchronicle.com/projects/2021/future-of-transit/'><sup>3</sup></a>
                </p>"),
                
                p("These data measure how much slower transit is compared to driving in the Bay Area. To simulate what a typical trip would look like,
    Google Maps API was queried between two random 
    points that are relatively close together (<15 miles) at random points throughout the day (7AM - 7PM on weekdays). 
    The time for both the fastest driving route and the fastest transit route were recorded as if an individual were to leave at that time. 
    While not perfectly
    indicative, this gives a decent view of the time cost of taking transit in a given area. 
      Full methodological details are below.")
            )
    ),
    
    
    material_parallax(
        image_source = "oak.jpg"
    ),
    ## All trips
    h4("All trips", align='center'),
    
    material_row(
        material_column(
            width = 12,
            material_card(
                plotOutput("basic_histogram", width = "70%"),
            ))
    ),
    material_row(style='padding-left:5%;padding-right:5%',
        material_column(
            width = 12,p("This shows the distribution of time to destination for all trips in the sample. 
                         While there is some overlap, transit times are clearly substantially higher and with more variance."))
    ),
    
    material_row(
        material_column(
            width = 12,
            material_card(
                div(DT::dataTableOutput("summary_table"), style = "font-size: 75%; width: 75%"),
            ))
    ),

    br(),
    
    
    ## Census tract
    h4("Trips by Census Tract", align= "center"),
    material_row(style='padding-left:5%;',
                 material_column(
                     width = 12,    p("Here, we want to know the average trip time for a given census tract. 
      That is, for all trips in the sample that start in this census tract, what was the average time for driving and transit?"))
    ),
    material_row(
        material_column(
            width = 12,
            material_card(
                plotOutput("basic_histogram_tract", width = "70%"),
            ))
    ),
    material_row(
        material_column(
            width = 12,
            material_card(
                leafletOutput("mymap"),
            ))
    ),

    material_row(
        material_column(
            width = 12,
            material_card(
                div(DT::dataTableOutput("summary_table_tract"), style = "font-size: 75%"),
            ))
    ),
    # material_row(
    #     material_column(
    #         width = 12,
    #         material_card(
    #             div(DT::dataTableOutput("summary_table_tract_diff"), style = "font-size: 75%"),
    #         ))
    # ),



    br(),
    

    ## Time of Day
    h4("Trips by Time of the Day", align="center"),
    material_row(style='padding-left:5%;padding-right:5%',
                 material_column(
                     width = 12,    p("For all trips in the sample, what was the average time for driving and transit at a given time of the day?
                                      This shows the distribution of time to destination for all trips in the sample split by time of day."))
    ),
    material_row(
        material_column(
            width = 12,
            material_card(
                shinyWidgets::sliderTextInput("time_of_day", "Time of Day:",
                                              choices=c("7-10", "10-3", "3-7"),
                                              selected="7-10", grid = F),
                plotOutput("histogram_time_of_day", width = "70%"),
                
            ))
    ),


    p(""),
    
    
    ## Distance
    h4("Trips by Distance", align="center"),
    material_row(style='padding-left:5%;padding-right:5%',
                 material_column(
                     width = 12,   p("For all trips in the sample, what was the average time for driving and transit for different trip lengths?
                                     This shows the distribution of time to destination for all trips in the sample split by (straight-line) distance from origin to destination"))
    ),
    material_row(
        material_column(
            width = 12,
            material_card(
                shinyWidgets::sliderTextInput("dist_pick", "Distance between origin and destination (miles):",
                                              choices=c("0-2", "2-5", "5-10", "10-15"),
                                              selected="2-5", grid = F),
                plotOutput("histogram_distance", width = "70%"),
                
            ))
    ),
    
    br(),
    
    
    ## City
    h4("Trips by City", align="center"),
    material_row(style='padding-left:5%;padding-right:5%',
                 material_column(
                     width = 12,   p("For all trips in the sample, what was the average time for driving and transit for each city?
                                     Please note this is an inexact measure, as census tracts do not map perfectly to cities."))
    ),
    material_row(
        material_column(
            width = 12,
            material_card(
                pickerInput(inputId = 'city_pick',
                            label = 'City',
                            choices = unique((transit_data$City)),
                            selected = (transit_data$City)[[1]],
                            options = pickerOptions(size = 8)),
                #             options = list(`style` = "btn-warning")),
                # shinyWidgets::sliderTextInput("c_pick", "Distance between origin and destination (miles):",
                #                               choices=c("0-2", "2-5", "5-10", "10-15"),
                #                               selected="2-5", grid = F),
                plotOutput("histogram_city", width = "70%"),
                
            ))
    ),
    material_row(
        material_column(
            width = 12,
            material_card(
                div(DT::dataTableOutput("summary_table_city_diff"), style = "font-size: 75%"),
            ))
    ),
    
    
    
    
    ## County
    h4("Trips by County", align="center"),
    material_row(style='padding-left:5%;padding-right:5%',
                 material_column(
                     width = 12,   p("For all trips in the sample, what was the average time for driving and transit for each county?  "))
    ),
    material_row(
        material_column(
            width = 12,
            material_card(
                pickerInput(inputId = 'c_pick',
                            label = 'County',
                            choices = unique((transit_data$c_name)),
                            selected = (transit_data$c_name)[[1]]),
                #             options = list(`style` = "btn-warning")),
                # shinyWidgets::sliderTextInput("c_pick", "Distance between origin and destination (miles):",
                #                               choices=c("0-2", "2-5", "5-10", "10-15"),
                #                               selected="2-5", grid = F),
                plotOutput("histogram_county", width = "70%"),
                
            ))
    ),
    material_row(
        material_column(
            width = 12,
            material_card(
                div(DT::dataTableOutput("summary_table_county_diff"), style = "font-size: 75%"),
            ))
    ),
    
    
    
    
    br(),
    material_parallax(
        image_source = "genericbartart1.webp"
    ),
    ## Details
    h4("Details", align="center"),
    material_row(style='padding-left:5%;padding-right:5%',
                 material_column(
                     width = 12,
                     p("This analysis includes census tracts from the SF Bay Area as defined in 2016.
    Low-density census tracts (<3500 per square mile as of the 2010 census) were not included in the analysis, as these areas are much less 
    likely to have substantial transit access. The analysis was run in Febuary-March 2022."),
                     
                     p(" On select weekday mornings a script was run that selected two random points from every census tract in the sample. Every 5 minutes
    from 7AM-7PM, the script would randomly select 10 pairs of points that were within 15 miles of each other. The Google Maps API
    was called to get the distance and time for both driving and transit for each of these origin-destination pairs."),
                     
                     p("  There no claim that this a perfect indicator of service adequacy. The random points in each census tract do not have any correlation
      with residences or businesses, and may disproportionally suffer from 'last-mile' issues. A small number of origin-destination pairs
      did not have a transit rout that Google could find at that time, and these were dropped from the analysis.
      More data would give more precise estimates, but
      the Google Maps API has limits on free account usage. ")
                 )
    ),
     
)



#### Server ####


server <- function(input, output, session) {
    
    # Plot historgram of all observations 
    output$basic_histogram <- renderPlot({
        transit_data %>% 
        ggplot(., aes(x = duration_s/60, group = mode, fill= mode)) + geom_density( alpha = .3, position = "identity") +
            xlim(0,120)+
            scale_fill_manual(name = "", labels = c("Driving", "Transit"), values = c( "grey20", "dodgerblue")) +
            xlab("Time to Destination (Minutes)") +
            ylab("Proportion of Trips (Density)") + 
            theme_pubclean()
    }, res = 96)
    
    agg_sum1 <- transit_data %>% 
        group_by(mode) %>% 
        summarise(`Trip Length Mean`=  round(mean(duration_s/60, na.rm=T)), 
                  `Trip Length Median`=  round(median(duration_s/60, na.rm=T)),
                  `Trip Length SD`=  round(sd(duration_s/60, na.rm=T)), `Number of Trips`=n()) %>% 
        rename(Mode = mode)
    output$summary_table <- DT::renderDataTable(agg_sum1,
                                                options = list(paging = FALSE,    ## paginate the output
                                                               scrollX = FALSE,   ## enable scrolling on X axis
                                                               scrollY = FALSE,   ## enable scrolling on Y axis
                                                               autoWidth = TRUE, ## use smart column width handling
                                                               server = FALSE,   ## use client-side processing
                                                               dom = 't',
                                                               searching = FALSE,
                                                              
                                                               columnDefs = list(list(targets = '_all', className = 'dt-center'))
                                                ),
                                          rownames = FALSE)
    
    
    # Plot histogram by census tract
    output$basic_histogram_tract <- renderPlot({
        agg1 %>% 
            ggplot(., aes(x = time_median_driving/60)) + geom_density(fill = "grey20", color = "grey20", alpha = .3) +
            geom_density(aes(x = time_median_transit/60), fill = "dodgerblue", color = "dodgerblue", alpha = .3) +
            xlim(0,120)+
            xlab("Median Time to Destination (Minutes) in Census Tract") +
            ylab("Proportion of Census Tracts (Density)") +
            theme_pubclean()
    }, res = 96)
    agg_sum_tract <- transit_data %>% 
        group_by(origin, mode) %>% 
        summarise(c_name = first(c_name), `Trip Length Mean`=  round(mean(duration_s/60, na.rm=T)), 
                  `Trip Length Median`=  round(median(duration_s/60, na.rm=T)),
                  `Trip Length SD`=  round(sd(duration_s/60, na.rm=T)), `Number of Trips`=n()) %>% ungroup() %>% 
        rename(Mode = mode, `Census Tract ID` = origin, County = c_name)
    output$summary_table_tract <- DT::renderDataTable(agg_sum_tract,
                                                options = list(paging = TRUE,    ## paginate the output
                                                               scrollX = FALSE,   ## enable scrolling on X axis
                                                               scrollY = FALSE,   ## enable scrolling on Y axis
                                                               autoWidth = TRUE, ## use smart column width handling
                                                               server = TRUE,   ## use client-side processing
                                                               # dom = 't',
                                                               searching = FALSE,
                                                               
                                                               columnDefs = list(list(targets = '_all', className = 'dt-center'))
                                                ),
                                                rownames = FALSE)
    # agg_sum_tract2 <- agg_sum_tract %>% 
    #     group_by(`Census Tract ID` ) %>% 
    #     mutate(`Median Difference` = `Trip Length Median` - lag(`Trip Length Median`),
    #            `Mean Difference` = `Trip Length Mean` - lag(`Trip Length Mean`)) %>% 
    #     na.omit() %>% select(`Census Tract ID`, County, `Median Difference`, `Mean Difference`)
    # output$summary_table_tract_diff <- DT::renderDataTable(agg_sum_tract2,
    #                                                   options = list(paging = TRUE,    ## paginate the output
    #                                                                  scrollX = FALSE,   ## enable scrolling on X axis
    #                                                                  scrollY = FALSE,   ## enable scrolling on Y axis
    #                                                                  autoWidth = TRUE, ## use smart column width handling
    #                                                                  server = TRUE,   ## use client-side processing
    #                                                                  # dom = 't',
    #                                                                  searching = FALSE,
    #                                                                  
    #                                                                  columnDefs = list(list(targets = '_all', className = 'dt-center'))
    #                                                   ),
    #                                                   rownames = FALSE)
    
    # Plot histogram by time of day
    agg_time <- transit_data %>% 
        mutate(time_floor = case_when(hour >=7 & hour <10 ~ "7-10", 
                                      hour >=10 & hour <15 ~ "10-3",
                                      hour >=15  ~"3-7")) 
        # group_by(origin, mode, time_floor) %>% 
        # summarise(time = median(duration_s), time_sd = sd(duration_s), n=n())
    
    output$histogram_time_of_day <- renderPlot({
        agg_time %>% 
            filter(time_floor == input$time_of_day) %>% 
            ggplot(., aes(x = duration_s/60, group = mode, fill = mode, color = mode)) + geom_density(alpha = .3, position = "identity") +
            xlim(0,120)+
            ylim(0,.075)+
            xlab("Time to Destination (Minutes)") +
            scale_fill_manual(name = "", labels = c("Driving", "Transit"), values = c("grey20", "dodgerblue")) +
            scale_color_manual(name = "", labels = c("Driving", "Transit"), values = c("grey20", "dodgerblue")) +
            ylab("Proportion of Trips (Density)") +
            theme_pubclean()
    }, res = 96)
    
    # Plot histogram by distance
    agg_dist <- transit_data %>% 
        mutate(distance = (as.numeric(distance_m)/1000) * 0.621371) %>% 
        mutate(distance_floor = case_when(distance <2 ~ "0-2", 
                                          distance >=2 & distance <5 ~ "2-5",
                                          distance >=5 & distance <10 ~"5-10",
                                          distance >=10 & distance <=15 ~ "10-15")) 
        # group_by(origin, mode, distance_floor) %>% 
        # summarise(time = median(duration_s), time_sd = sd(duration_s), n=n())
    
    output$histogram_distance <- renderPlot({
        agg_dist %>% 
            filter(distance_floor == input$dist_pick) %>%
            # na.omit() %>%
            ggplot(., aes(x = duration_s/60, group = mode, fill = mode, color = mode)) + geom_density(alpha = .3, position = "identity") +
            xlim(0,120)+
            ylim(0,0.2) +
            xlab("Time to Destination (Minutes)") +
            scale_fill_manual(name = "", labels = c("Driving", "Transit"), values = c("grey20", "dodgerblue")) +
            scale_color_manual(name = "", labels = c("Driving", "Transit"), values = c("grey20", "dodgerblue")) +
            ylab("Proportion of Trips (Density)") +
            theme_pubclean()
    }, res = 96)
    
    # By city
    output$histogram_city <- renderPlot({
        transit_data %>% 
            filter(City == input$city_pick) %>%
            
            # na.omit() %>%
            ggplot(., aes(x = duration_s/60, group = mode, fill = mode, color = mode)) + geom_density(alpha = .3, position = "identity") +
            xlim(0,120)+
            ylim(0,0.1) +
            xlab("Time to Destination (Minutes)") +
            scale_fill_manual(name = "", labels = c("Driving", "Transit"), values = c("grey20", "dodgerblue")) +
            scale_color_manual(name = "", labels = c("Driving", "Transit"), values = c("grey20", "dodgerblue")) +
            ylab("Proportion of Trips (Density)") +
            theme_pubclean()
    }, res = 96)
    
    agg_sum_city <- transit_data %>% 
        group_by(City, mode) %>% 
        summarise(City = first(City), mean_length = mean(duration_s/60, na.rm=T),
                  median_length = median(duration_s/60, na.rm=T)) %>% 
        mutate(`Mean Difference` = round(mean_length - lag(mean_length)),
               `Median Difference` = round(median_length - lag(median_length))) %>% 
        na.omit() %>% select( City = City, `Median Difference`, `Mean Difference`)
    output$summary_table_city_diff <- DT::renderDataTable(agg_sum_city,
                                                            options = list(paging = TRUE,    ## paginate the output
                                                                           scrollX = FALSE,   ## enable scrolling on X axis
                                                                           scrollY = FALSE,   ## enable scrolling on Y axis
                                                                           autoWidth = TRUE, ## use smart column width handling
                                                                           server = TRUE,   ## use client-side processing
                                                                           dom = 't',
                                                                           searching = FALSE,
                                                                           
                                                                           columnDefs = list(list(targets = '_all', className = 'dt-center'))
                                                            ),
                                                            rownames = FALSE)
    
    # By county
    output$histogram_county <- renderPlot({
        transit_data %>% 
            filter(c_name == input$c_pick) %>%
            
            # na.omit() %>%
            ggplot(., aes(x = duration_s/60, group = mode, fill = mode, color = mode)) + geom_density(alpha = .3, position = "identity") +
            xlim(0,120)+
            ylim(0,0.09) +
            xlab("Time to Destination (Minutes)") +
            scale_fill_manual(name = "", labels = c("Driving", "Transit"), values = c("grey20", "dodgerblue")) +
            scale_color_manual(name = "", labels = c("Driving", "Transit"), values = c("grey20", "dodgerblue")) +
            ylab("Proportion of Trips (Density)") +
            theme_pubclean()
    }, res = 96)
    
    agg_sum_county <- transit_data %>% 
        group_by(c_name, mode) %>% 
        summarise(c_name = first(c_name), mean_length = mean(duration_s/60, na.rm=T),
               median_length = median(duration_s/60, na.rm=T)) %>% 
        mutate(`Mean Difference` = round(mean_length - lag(mean_length)),
               `Median Difference` = round(median_length - lag(median_length))) %>% 
        na.omit() %>% select( County = c_name, `Median Difference`, `Mean Difference`)
    output$summary_table_county_diff <- DT::renderDataTable(agg_sum_county,
                                                           options = list(paging = TRUE,    ## paginate the output
                                                                          scrollX = FALSE,   ## enable scrolling on X axis
                                                                          scrollY = FALSE,   ## enable scrolling on Y axis
                                                                          autoWidth = TRUE, ## use smart column width handling
                                                                          server = TRUE,   ## use client-side processing
                                                                          dom = 't',
                                                                          searching = FALSE,
                                                                          
                                                                          columnDefs = list(list(targets = '_all', className = 'dt-center'))
                                                           ),
                                                           rownames = FALSE)
    
    # Maps
    temp <- agg1 %>%
        st_as_sf()
    
    popup1 <- paste0("<span style='color: #7f0000'><strong>Census Tract Info</strong></span>",
                     "<br><span style='color: salmon;'><strong>Tract ID: </strong></span>", 
                     temp$origin, 
                     "<br><span style='color: salmon;'><strong>Median Drive Time: </strong></span>", 
                     floor(as.numeric(temp$time_driving)/60),
                     "<br><span style='color: salmon;'><strong>Median Transit Time: </strong></span>", 
                     floor(as.numeric(temp$time_transit)/60)
    )
    
    # colors
    qpal <- colorQuantile("RdYlBu", temp$time_diff, n = 5, reverse = T)
    qpal_colors <- unique(qpal(sort(temp$time_diff))) # hex codes
    qpal_labs <- quantile(temp$time_diff, seq(0, 1, .2)) # depends on n from pal, need to change ".2" if more breaks
    qpal_labs <- paste(floor(lag(qpal_labs/60)), floor(qpal_labs/60), sep = " - ")[-1] 
    
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) %>% 
            addPolygons(data = temp, color = "#444444", weight = 1, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5,
                        fillColor = ~qpal(temp$time_diff),
                        highlightOptions = highlightOptions(color = "red", weight = 2,
                                                            bringToFront = TRUE),
                        popup = popup1)  %>% 
            addLegend("bottomleft", colors = qpal_colors, labels = qpal_labs, 
                      title = "How much longer is it </br> to take transit rather than driving?</br>(minutes)",
                      opacity = 1)
    })
    
}








shinyApp(ui, server)