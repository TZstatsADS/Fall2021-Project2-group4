setwd('C:\\Users\\tuyum\\Desktop\\study\\Columbia\\2021fall\\Applied DS\\project2')
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
#dag data processing
df <- read.csv("C:\\Users\\tuyum\\Desktop\\study\\Columbia\\2021fall\\Applied DS\\project2\\dog\\NYC_Dog_Licensing_Dataset.csv")
df <- df[,-c(1,6,10,11)]

#transfer to date dtype
df$LicenseIssuedDate<-as.Date(df$LicenseIssuedDate,"%m/%d/%Y")
df$LicenseExpiredDate<-as.Date(df$LicenseExpiredDate,"%m/%d/%Y")

#We define the covid date as 2020-03-01; 
#Here df1 only contains pre-covid dogs
#dogs which have multiple entries, take the earliest issued date
df1 <- df %>% 
  group_by(AnimalName,AnimalGender,AnimalBirthMonth,BreedName) %>% 
  slice(which.min(LicenseIssuedDate)) %>%
  filter( LicenseIssuedDate < as.Date("2020-03-01"))
#make old dog list
old_dogs1 <- right_join(df,df1,by = c("AnimalName"="AnimalName",
                                      "AnimalGender"="AnimalGender",
                                      "AnimalBirthMonth"="AnimalBirthMonth",
                                      "BreedName"="BreedName","ZipCode"="ZipCode"))[,-c(8,9)]

#dogs registed before covid
old_dogs2 <- df %>% filter(LicenseIssuedDate < as.Date("2020-03-01"))

#new dogs registed since covid
new_dogs <- anti_join(df,old_dogs1)
new_dogs <- anti_join(new_dogs,old_dogs2)%>% 
  group_by(AnimalName,AnimalGender,AnimalBirthMonth,BreedName,ZipCode) %>% 
  slice(which.min(LicenseIssuedDate))

#dogs registered from 2019/03-2020/03
old_dogs <- df %>% filter(LicenseIssuedDate < as.Date("2020-03-01") & LicenseIssuedDate > as.Date("2019-03-01"))

#dogs registered before 2019/03
df1 <- df %>% filter(LicenseIssuedDate < as.Date("2019-03-01")) %>% 
  group_by(AnimalName,AnimalGender,AnimalBirthMonth,BreedName) %>% 
  slice(which.min(LicenseIssuedDate))
#make before 19/03 old dog list
old_dogs1 <- right_join(df,df1,by = c("AnimalName"="AnimalName","AnimalGender"="AnimalGender","AnimalBirthMonth"="AnimalBirthMonth","BreedName"="BreedName","ZipCode"="ZipCode"))[,-c(8,9)]

#anti-join old_dog
old_dogs <- anti_join(old_dogs,old_dogs1)


#read in geo-zip file
zip<-read.csv('C:\\Users\\tuyum\\Desktop\\study\\Columbia\\2021fall\\Applied DS\\project2\\dog\\zc_geo.csv',sep=";")
new_dogs<-left_join(new_dogs,zip,by= c("ZipCode" = "Zip"))%>%
  drop_na()

old_dogs<-left_join(old_dogs,zip,by= c("ZipCode" = "Zip"))%>%
  drop_na()


#Emergency visits:

#Data Processing

data<-read.csv('C:/Users/tuyum/Desktop/study/Columbia/2021fall/Applied DS/project2/emergency/Emergency_Department_Visits_and_Admissions_for_Influenza-like_Illness_and_or_Pneumonia.csv')
zip<-read.csv('C:/Users/tuyum/Desktop/study/Columbia/2021fall/Applied DS/project2/emergency/zc_geo.csv',sep=";")
zip<-zip%>%
  mutate(mod_zcta=Zip)
set.seed(0)
data<-data[sample(nrow(data),200000),]
data<-left_join(data,zip,by="mod_zcta")%>%
  drop_na()

data1<-data%>%
  select(-extract_date)

data1$date<-as.Date(data1$date,"%m/%d/%Y")
data1<-data1%>%
  arrange(date)
Sys.setlocale("LC_TIME", "English")

weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
data1$days <- factor((weekdays(data1$date) %in% weekdays1), 
                     levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))

a<-as.Date("2020-03-01")
b<-as.Date("2020-09-30")
c<-as.Date("2021-02-28")
d<-as.Date("2021-10-31")
head(data1)
data1 <- data1 %>% mutate(seasons = ifelse(date >= a & date <= b,'20-Spring & Summer',
                                           ifelse(date > b & date <= c,'20-Autumn & Winter',
                                                  '21-Spring & Summer')))
emergency_info <- zip[,c('Zip','Latitude','Longitude')]


shinyServer(function(input, output) {
  
  output$Dog_left_map <- renderLeaflet({
    #adjust for weekday/weekend effect
    if (input$Genders =='Overall') {
      leaflet_plt_df <- old_dogs %>% 
        group_by(ZipCode) %>%
        summarise(total_count = n()
        ) %>% left_join(zip,by= c("ZipCode" = "Zip"))
      
    } else {
      leaflet_plt_df <- old_dogs %>% 
        filter(AnimalGender == as.character(input$Genders)) %>%
        group_by(ZipCode) %>%
        summarise(total_count = n()) %>% 
        left_join(zip,by= c("ZipCode" = "Zip"))
      
    }
    map_2019 <- leaflet_plt_df %>%
      leaflet(options = leafletOptions(minZoom = 11, maxZoom = 13)) %>%
      addTiles() %>%
      addProviderTiles("CartoDB.Positron",
                       options = providerTileOptions(noWrap = TRUE)) %>%
      setView(-73.9834,40.7504,zoom = 12)
    
    map_2019 %>%
      addHeatmap(
        lng=~Longitude,
        lat=~Latitude,
        intensity=~total_count,
        max=700,
        radius=16,
        blur=8)
    
  }) #left map plot
  
  output$Dog_right_map <- renderLeaflet({
    
    #adjust for weekday/weekend effect
    if (input$Genders =='Overall') {
      leaflet_plt_df <- new_dogs %>% 
        group_by(ZipCode) %>%
        summarise(total_count = n()
        )%>% left_join(zip,by= c("ZipCode" = "Zip"))
      
    } else {
      leaflet_plt_df <- new_dogs %>% 
        filter(AnimalGender == as.character(input$Genders)) %>%
        group_by(ZipCode) %>%
        summarise(total_count = n())%>% 
        left_join(zip,by= c("ZipCode" = "Zip"))
    }
    #initial the map to plot on
    map_2020 <- leaflet_plt_df %>%
      leaflet(options = leafletOptions(minZoom = 11, maxZoom = 13)) %>%
      addTiles() %>%
      addProviderTiles("CartoDB.Positron",
                       options = providerTileOptions(noWrap = TRUE)) %>%
      setView(-73.9834,40.7504,zoom = 12)
    
    map_2020 %>%
      addHeatmap(
        lng=~Longitude,
        lat=~Latitude,
        intensity=~total_count,
        max=700,
        radius=16,
        blur=8)
    
    
    
  }) #right map plot
  
  output$Emergency<- renderLeaflet({
    
    #adjust for weekday/weekend effect
    if (input$adjust_time =='Overall') {
      leaflet_plt_df <- data1 %>% 
        group_by(Zip) %>%
        summarise(
          total=sum(total_ed_visits),
          total_pne = sum(ili_pne_visits),
          total_pneadm = sum(ili_pne_admissions),
          
          adm_percentage=sum(ili_pne_admissions)/sum(ili_pne_visits)
        ) %>% left_join(emergency_info,by='Zip')
    } else {
      leaflet_plt_df <- data1 %>% 
        filter(days == input$adjust_time) %>%
        group_by(Zip) %>%
        summarise(
          total=sum(total_ed_visits),
          total_pne = sum(ili_pne_visits),
          total_pneadm = sum(ili_pne_admissions),
          
          adm_percentage=sum(ili_pne_admissions)/sum(ili_pne_visits)
        ) %>% left_join(emergency_info,by='Zip')
    } 
    
    
    map <- leaflet_plt_df %>%
      leaflet(options = leafletOptions(minZoom = 11, maxZoom = 13)) %>%
      addTiles() %>%
      addProviderTiles("CartoDB.Positron",
                       options = providerTileOptions(noWrap = TRUE)) %>%
      setView(-73.9834,40.7504,zoom = 12)
    
    if (input$adjust_score == 'total visit') {
      map %>%
        
        addHeatmap(
          lng=~Longitude,
          lat=~Latitude,
          intensity=~total_pne,
          max=4000,
          radius=8,
          blur=10)
    }else if (input$adjust_score == 'admission') {
      map %>%
        
        addHeatmap(
          lng=~Longitude,
          lat=~Latitude,
          intensity=~total_pneadm,
          max=4000,
          radius=8,
          blur=10)
    } else if (input$adjust_score == 'total'){
      map %>%
        
        addHeatmap(
          lng=~Longitude,
          lat=~Latitude,
          intensity=~total,
          max=50,
          radius=8,
          blur=10)
      
    }
  })#left map plot
})