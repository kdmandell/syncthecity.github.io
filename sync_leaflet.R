library(tidyverse)
library(leaflet)
library(htmlwidgets)


# data manipulation & prep ------------------------------------------------


# load organizations csv  
geo_data <- read_csv("geo_all.csv") 

# create table of codes for map definition
codes <- table(geo_data$codes) %>% 
  as.data.frame()

# create list of codes for legend and filter 
codes_list <- c('Animal-Related',
                'Arts, Culture and Humanities', 	
                'Civil Rights, Social Action, Advocacy',
                'Community Improvement, Capacity Building',
                'Crime, Legal-Related',
                'Diseases, Disorders, Medical Disciplines',
                'Educational Institutions and Related Activities',
                'Employment, Job-Related',
                'Environmental Quality, Protection and Beautification',
                'Food, Agriculture and Nutrition',
                'Health - General and Rehabilitative',
                'Housing, Shelter',
                'Human Services - Multipurpose and Other',
                'International, Foreign Affairs and National Security',
                'Medical Research',
                'Mental Health, Crisis Intervention',
                'Mutual/Membership Benefit Organizations, Other',
                'Philanthropy, Voluntarism and Grantmaking Foundations',
                'Public Safety, Disaster Preparedness and Relief',
                'Public, Society Benefit - Multipurpose and Other',
                'Recreation, Sports, Leisure, Athletics',
                'Religion-Related, Spiritual Development',
                'Science and Technology Research Institutes, Services',
                'Social Science Research Institutes, Services',
                'Youth Development',
                'Unknown')

# define colors for each group
AR <-"#ff0000"  # Red - Animal-Related
ACH <-  "#008080"  # Teal - Arts, Culture, and Humanities
U <- "#000000"  # Black - Unknown

# define palette for circle markers
#pal <- colorFactor(c(AR, ACH, U), domain= codes_list)

# define palette for circle markers -- one color for all 
pal <- colorFactor(rep('blue', 26), domain = codes_list)

# map definition ---------------------------------------------------------


map <- leaflet(data = geo_data, options = leafletOptions(minZoom = 11, maxZoom = 18)) %>%
  
  addTiles(group = 'Color') %>% 
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group = 'Grayscale') %>% 
  addProviderTiles(providers$CartoDB.DarkMatter, group = 'Dark') %>% 
  
  # restrict boundaries to around Baltimore (doesn't seem to work as expected)
  fitBounds(lng1 = min(geo_data$lon) - 0.11, 
            lat1 = min(geo_data$lat) - 0.11,
            lng2 = max(geo_data$lon) + 0.11, 
            lat2 = max(geo_data$lat) + 0.11) %>% 
  
  # set default view to downtown Baltimore
  setView(lng= -76.62, lat=39.29,zoom=12) %>% 
  
  # add legend to bottom right of map
  # addLegend(
  #   title="Tax-Exempt Organizations in Baltimore",
  #   position = 'bottomright',
  #   #colors = c(AR,ACH, U),
  #   colors = rep('blue', 26),
  #   labels = codes_list)  %>%
  
# add legend to bottom left of map
addLegend(
  position = 'bottomleft',
  #colors = c(AR,ACH, U),
  colors = 'blue',
  group = 'Animal-Related',
  labels = 'Animal-Related')  %>%
  
  
  ## Animal-Related group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Animal-Related',],~lon, ~lat, stroke=FALSE,
                 radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                 #create pop-up window with information for each marker
                 popup = ~ paste(NAME, "<br/>",
                                 "Address:", STREET,"<br/>",
                                 "Assets:", ASSET_AMT, "<br/>",
                                 "Income:", INCOME_AMT, "<br/>",
                                 "Revenue:", REVENUE_AMT, "<br/>",
                                 "Category:", codes),
                 
                 group="Animal-Related") %>% 
  
  ## Arts, Culture, & Humanities group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Arts, Culture and Humanities',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Arts, Culture and Humanities") %>% 
  
  ## Civil Rights, Social Action, Advocacy group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Civil Rights, Social Action, Advocacy',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Civil Rights, Social Action, Advocacy") %>% 
  
  ## Community Improvement, Capacity Building group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Community Improvement, Capacity Building',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Community Improvement, Capacity Building") %>% 
  
  ## Crime, Legal-Related group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Crime, Legal-Related',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Crime, Legal-Related") %>% 
  
  ## Diseases, Disorders, Medical Disciplines group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Diseases, Disorders, Medical Disciplines',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Diseases, Disorders, Medical Disciplines") %>% 
  
  ## Educational Institutions and Related Activities group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Educational Institutions and Related Activities',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Educational Institutions and Related Activities") %>% 
  
  ## Employment, Job-Related group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Employment, Job-Related',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Employment, Job-Related") %>% 
  
  ## Environmental Quality, Protection and Beautification group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Environmental Quality, Protection and Beautification',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Environmental Quality, Protection and Beautification") %>% 
  
  ## Food, Agriculture and Nutrition group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Food, Agriculture and Nutrition',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Food, Agriculture and Nutrition") %>% 
  
  ## Health - General and Rehabilitative group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Health - General and Rehabilitative',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Health - General and Rehabilitative") %>% 
  
  ## Housing, Shelter group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Housing, Shelter',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Housing, Shelter") %>% 
  
  ## Human Services - Multipurpose and Other group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Human Services - Multipurpose and Other',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Human Services - Multipurpose and Other") %>% 
  
  ## International, Foreign Affairs and National Security group
  addCircleMarkers(data=geo_data[geo_data$codes == 'International, Foreign Affairs and National Security',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="International, Foreign Affairs and National Security") %>% 
  
  ## Medical Research group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Medical Research',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Medical Research") %>% 
  
  ## Mental Health, Crisis Intervention group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Mental Health, Crisis Intervention',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Mental Health, Crisis Intervention") %>%
  
  ## Mutual/Membership Benefit Organizations, Other group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Mutual/Membership Benefit Organizations, Other',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Mutual/Membership Benefit Organizations, Other") %>%
  
  ## Philanthropy, Voluntarism and Grantmaking Foundations group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Philanthropy, Voluntarism and Grantmaking Foundations',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Philanthropy, Voluntarism and Grantmaking Foundations") %>%
  
  ## Public Safety, Disaster Preparedness and Relief group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Public Safety, Disaster Preparedness and Relief',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Public Safety, Disaster Preparedness and Relief") %>%
  
  ## Public, Society Benefit - Multipurpose and Other group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Public, Society Benefit - Multipurpose and Other',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Public, Society Benefit - Multipurpose and Other") %>%
  
  ## Recreation, Sports, Leisure, Athletics group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Recreation, Sports, Leisure, Athletics',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Recreation, Sports, Leisure, Athletics") %>%
  
  ## Religion-Related, Spiritual Development group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Religion-Related, Spiritual Development',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Religion-Related, Spiritual Development") %>%
  
  ## Science and Technology Research Institutes, Services group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Science and Technology Research Institutes, Services',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Science and Technology Research Institutes, Services") %>%
  
  ## Social Science Research Institutes, Services group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Social Science Research Institutes, Services',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Social Science Research Institutes, Services") %>%
  
  ## Youth Development group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Youth Development',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), #color defined above
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Youth Development") %>%
  
  ## Unknown group
  addCircleMarkers(data=geo_data[geo_data$codes == 'Unknown',],~lon, ~lat, stroke=FALSE, 
                   radius = ~log(ASSET_AMT + 100), fillOpacity = .5, color = ~pal(codes), 
                   #create pop-up windows with information for each marker
                   popup = ~ paste(NAME, "<br/>",
                                   "Address:", STREET,"<br/>",
                                   "Assets:", ASSET_AMT, "<br/>",
                                   "Income:", INCOME_AMT, "<br/>",
                                   "Revenue:", REVENUE_AMT, "<br/>",
                                   "Category:", codes),
                   
                   group="Unknown") %>% 

  # Add user controls to toggle groups displayed
  addLayersControl(
    baseGroups = c('Color', 'Grayscale', 'Dark'),
    overlayGroups = codes_list,
    options = layersControlOptions(collapsed = TRUE)
  ) %>% 
  
  # Hide all groups by default 
  hideGroup(codes_list)
  
map

# map export --------------------------------------------------------------


# save map as html document 
sav.file <- "/Users/jbjrV/OneDrive/Code for Baltimore/index.html"
saveWidget(map, file=sav.file, selfcontained = F)

# notebook ----------------------------------------------------------------

# # Baltimore Neighborhood Indicators
# Housing and Community Development
# Children and Family Health
# Crime and Safety
# Workforce and Economic Development
# Sustainability
# Education and Youth
# Arts and Culture

# change groups added in markers to rollup groups and add legend for each rollup group with different colors 
