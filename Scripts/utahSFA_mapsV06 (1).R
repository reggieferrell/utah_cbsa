#Analyst - Reginald Ferrell 
#Project - Mapping Utah Schools   

#Load packages
library("leaflet")
library("dplyr")
library("tidyverse")
library("rgdal") 
library("sf") 
library("htmltools")
library("janitor")
library("readxl")
library("reshape2")
library("RColorBrewer")
library("viridis")     
library("tidycensus")
library("htmlwidgets")
library("tigris")
library("broom")
library("ggsn")
library("plotly")
library("tmap")
library("ggrepel")
library("reshape2")
library("unikn")

#Set working directory & API key
setwd("/Users/rferrel/Documents/Research Methodologies/SFA/Scripts/")

#Color Palette
cp_SFA <- c("#0D3259","#1F7BE2","#D1E5FA","#E66100","#4B0092")

#Import Shapefile 
utah_counties <- tigris::counties("Utah") %>% #FIPS code - 49
rename(longitude = INTPTLON,latitude = INTPTLAT) %>% clean_names() %>%
  mutate(longitude=as.numeric(longitude),
         latitude=as.numeric(latitude))
# utah <- map_data("state") %>% filter(region=="utah")

utah_schooldistricts <- tigris::school_districts("Utah") %>% #FIPS code - 49
  rename(longitude = INTPTLON,latitude = INTPTLAT) %>% clean_names() %>%
  mutate(longitude=as.numeric(longitude),
         latitude=as.numeric(latitude)) 

core <- tigris::core_based_statistical_areas("Utah")

utah_sd <- readOGR("/Users/rferrel/Documents/My Code/Data Visualization/Maps/Utah/unified school districts/tl_2021_49_unsd.shp") #Utah School District Boundaries 
fortify_utahsd <- tidy(utah_sd, region = "NAME")

#Distance Measures 
distance <- read_excel("/Users/rferrel/Documents/Research Methodologies/SFA/Inputs/p-2_schloc_border.xlsx") %>% clean_names()

#Distance 
cbsa <- readOGR("/Users/rferrel/Downloads/Utah_Metro_Micro_Statistical_Areas/MetroMicroStatisticalAreas.shp")
fortify_cbsa <- tidy(cbsa, region = "NAME")

#Distance measures (clean)
distance_measures <- distance %>% 
  dplyr::select(district_name, school_name, school_locale,
                school_latitude, school_longitude,
                charter_school_indicator,distance_in_miles_between_school_and_school_in_a_neighboring_state,
                miles_to_the_center_of_the_nearest_metro_or_micro_area,miles_to_the_center_of_the_nearest_metro_area,
                miles_to_nearest_school_with_a_comparable_grade_band,miles_to_the_nearest_weather_station) %>%
  rename(latitude = school_latitude, longitude = school_longitude, 
         distance_near_neighborhood = distance_in_miles_between_school_and_school_in_a_neighboring_state,
         nearest_metro_micro = miles_to_the_center_of_the_nearest_metro_or_micro_area,
         nearest_metro = miles_to_the_center_of_the_nearest_metro_area,
         nearest_grade = miles_to_nearest_school_with_a_comparable_grade_band,
         nearest_weatherstation = miles_to_the_nearest_weather_station) %>%
  group_by(district_name) %>% 
mutate(charter_school_indicator = as.character(charter_school_indicator),
        school_locale = as.character(school_locale),
       distance_near_neighborhood = round(distance_near_neighborhood,2),
       nearest_metro_micro = round(nearest_metro_micro,2),
       nearest_metro = round(nearest_metro,2),
       nearest_grade = round(nearest_grade,2)) %>%
mutate(text = paste0("School Name: ", school_name,"\n",
                     "Distance Away (in miles): ",nearest_metro_micro, sep=""))

measures_long <- distance_measures %>% melt(measure.vars=c("distance_near_neighborhood","nearest_metro_micro",
                                                                          "nearest_metro","nearest_grade","nearest_weatherstation"))

######### Choropleth - Distance 
#Create bubble plot 

#Color - Look at using viridis, reduce points to 3 categories

remote <- measures_long %>% 
  filter(variable=="nearest_metro_micro") %>%
  mutate(label = ifelse(value < 45, "Less than 45 Miles","")) %>%
  mutate(label = ifelse(value > 45 & value < 90, "Between 45-90 Miles",label)) %>%
  mutate(label = ifelse(value > 90, "Greater than 90 Miles",label))

remote_plot <- ggplot() +
geom_polygon(data = fortify_utahsd, aes(x=long, y=lat, group=group),
  fill="gray95", color="gray50", size=1) +
  geom_point(data = remote, 
  aes(x=longitude, y=latitude,
  size = value, 
  colour=value)) +
  scale_color_manual(values = cp_SFA) +
with(utah_schooldistricts, 
     annotate(geom = "text", x=longitude, y=latitude,
     label=name, size=2.5))+
  geom_text_repel() +
theme_void() +
  scale_size_continuous(name = "Distance:",
  breaks = c(45,90,120),
  labels = c("Less than 45 Miles",
  "Between 45-90 Miles",
  "Greater than 90 Miles")) +
  scale_fill_gradient(low = "#0D3259", high = "#D1E5FA")
  
  
  scale_color_viridis(trans="log") +
  guides(size=guide_legend(override.aes = list(color = viridis(3)))) +
  # values = c("#0D3259","#1F7BE2","#D1E5FA")) +
  # scale_color_viridis(name = "Distance:",
  # breaks = c(45,90,120),
  # labels = c("Less than 45 Miles",
  #            "Between 45-90 Miles",
  #            "Greater than 90 Miles")) +
  # values = c("#0D3259","#1F7BE2","#D1E5FA")) +
guides(color = guide_legend(reverse =T),
size = guide_legend(reverse = T)) +
labs(title = "Utah Department of Education",
     subtitle = "Distance from Nearest Metro-Micro Area")+
theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
      legend.position = "right",
      plot.subtitle = element_text(hjust = .5)) +
  north(fortify_utahsd, location = "topright", symbol = 3) 
remote_plot

remote_plot <- remote_plot + scale_fill_manual(values = cp_SFA)

ggsave(remote_plot, filename = paste("Remoteness Plot",".png",sep = "")
       , width = 8
       , height = 8
       , type = "cairo-png")


######################################### Leaflet Map
# utah_shp <- "/Users/rferrel/Documents/My Code/Data Visualization/Maps/Utah/unified school districts/tl_2021_49_unsd.shp" #Utah School District Boundaries 
# utah_sd2 <- st_read(utah_sd2) %>% rename(district_name = NAME)

remote <- measures_long %>% 
  filter(variable=="nearest_metro_micro") %>%
  mutate(label = ifelse(value < 45, "Less than 45 Miles","")) %>%
  mutate(label = ifelse(value > 45 & value < 90, "Between 45-90 Miles",label)) %>%
  mutate(label = ifelse(value > 90, "Greater than 90 Miles",label))

remote_under30 <- remote %>% filter(label == "Less than 45 Miles")
remote_under90 <- remote %>% filter(label == "Between 45-90 Miles")
remote_over90 <- remote %>% filter(label == "Greater than 90 Miles")

remote_pal <- c("#0D3259","#1F7BE2","#FE6100")
pal_remote <- newpal(remote_pal)

remote_bins <- c(0,45,90,135)
remote_pal <- colorBin(pal_remote, domain = remote$value, bins = remote_bins,reverse = F)

# enrollment_regions <- leaflet() %>% 
#   addProviderTiles(providers$OpenStreetMap.DE) %>% #addProvider allows one to choose the proider of the basefile (Esri, Carto, etc.)
#   setView(lng = -119.417931, lat = 36.778259, zoom=6) %>%
#   addPolygons(data = california_counties_shp,
#               weight = 1,
#               fillColor = "grey",
#               color = "black",
#               smoothFactor = .25,
#               highlightOptions = highlightOptions(color="black", weight=5, bringToFront = FALSE),
#               label = ~ NAME) %>%

utahremoteness_map <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldTerrain) %>% #addProvider allows one to choose the proider of the basefile (Esri, Carto, etc.)
  setView(lng = -111.950684, lat = 39.419220, zoom=6) %>%
  addPolygons(data = utah_sd,
              weight = 1,
              fillColor = "gray50",
              color = "white",
              # color = "#3C4048",
              smoothFactor = .25) %>% 
  addCircleMarkers(lng = remote_under30$longitude, 
                   lat = remote_under30$latitude,
                   color = "#0D3259",weight = 1,
                   radius = 5,stroke = FALSE, fillOpacity = 1,
                   group = "Less than 45 Miles",
                   label = lapply(remote_under30$label, HTML)) %>%
  addCircleMarkers(lng = remote_under90$longitude, 
                   lat = remote_under90$latitude,
                   color = "#1F7BE2",weight = 1,
                   radius = 5, stroke = FALSE, fillOpacity = 1,
                   group = "Between 45-90 Miles",
                   label = lapply(remote_under90$label, HTML)) %>% 
  addCircleMarkers(lng = remote_over90$longitude, 
                   lat = remote_over90$latitude,
                   color = "#FE6100",weight = 1,
                   radius = 5,stroke = FALSE, fillOpacity = 1,
                   group = "Greater than 90 Miles",
                   label = lapply(remote_over90$label, HTML)) %>%
  addLabelOnlyMarkers(data = utah_schooldistricts,
                      lng = ~longitude, lat = ~latitude, label = ~name,
                      labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE)) %>%
  addLegend(pal = remote_pal,
            values  = remote$label,
            position = "topright",
            title = "Distance to Nearest Metro-Micro Area",
            labFormat = labelFormat(suffix = " miles away")) %>% 
  htmlwidgets::prependContent(backg)
utahremoteness_map



backg <- htmltools::tags$style(".leaflet-container { background: white; }" )









cbsa <- readOGR("/Users/rferrel/Downloads/tl_2019_us_cbsa (2)/tl_2019_us_cbsa.shp")
cbsa_utahsd <- tidy(cbsa, region = "NAME")


utahremoteness_map <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.PositronOnlyLabels) %>% #addProvider allows one to choose the proider of the basefile (Esri, Carto, etc.)
  setView(lng = -111.950684, lat = 39.419220, zoom=6) %>%
  addPolygons(data = utah_sd,
              weight = 1,
              # color = "#3C4048",
              smoothFactor = 1) %>% 
  addCircleMarkers(lng = remote_under30$longitude, 
                   lat = remote_under30$latitude,
                   color = "#0D3259",weight = 1,
                   radius = 5,stroke = FALSE, fillOpacity = 1,
                   group = "Less than 45 Miles",
                   label = lapply(remote_under30$label, HTML)) %>%
  addCircleMarkers(lng = remote_under90$longitude, 
                   lat = remote_under90$latitude,
                   color = "#1F7BE2",weight = 1,
                   radius = 5, stroke = FALSE, fillOpacity = 1,
                   group = "Between 45-90 Miles",
                   label = lapply(remote_under90$label, HTML)) %>% 
  addCircleMarkers(lng = remote_over90$longitude, 
                   lat = remote_over90$latitude,
                   color = "#D1E5FA",weight = 1,
                   radius = 5,stroke = FALSE, fillOpacity = 1,
                   group = "Greater than 90 Miles",
                   label = lapply(remote_over90$label, HTML)) %>%
  addLabelOnlyMarkers(data = utah_schooldistricts,
                      lng = ~longitude, lat = ~latitude, label = ~name,
                      labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE)) %>%
  addLegend(pal = remote_pal,
            values  = remote$label,
            position = "topright",
            title = "Distance to Nearest Metro-Micro Area",
            labFormat = labelFormat(suffix = " miles away"))
utahremoteness_map
