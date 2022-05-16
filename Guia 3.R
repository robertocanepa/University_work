
#GUIA 3

#Necesario descargar bases de datos, la cuales se extraen con los siguientes paquetes 
library(dplyr)
install.packages("WDI")
library(WDI)

#Población - SP.POP.TOTL
#Expectativa de vida al nacer (Años) - SP.DYN.LE00.IN
#PIB per cápita - NY.GDP.PCAP.PP.CD

wbData = WDI(indicator=c("SP.POP.TOTL", "SP.DYN.LE00.IN", 
                         "NY.GDP.PCAP.PP.CD")
             , country="all", extra = TRUE, start=1960, end=2016)
saveRDS(wbData, "C:/Users/rcane/Desktop/Electivo R/wbData.rds" )
View(wbData2)

wbData2 <- wbData%>% rename(population=SP.POP.TOTL,lifeExpectancy = SP.DYN.LE00.IN, gdppc=NY.GDP.PCAP.PP.CD)  #cambiar los nombres de las bases de datos 
#los signos de porcentaje son debido a que se utiliza el paquete dplyr 
saveRDS(wbData2,"C:/Users/rcane/Desktop/Electivo R/wbData2.rds")
#para borrar el ambiente 
rm(list=ls())


#luego se agrega la base nuevamente, si esta guardada
wbData2 <-readRDS("C:/Users/rcane/Desktop/Electivo R/wbData2.rds")

#para ordenar los datos:
wbData2 = wbData2 %>% arrange(country,year)


#pregunta 3
#cambiar nombres de datos
library(tidyverse)
wbData2$region <-wbData2$region %>% 
  fct_recode("Este Asiatico y Pacifico"= "East Asia & Pacific",
             "Europa y Asia Central"= "Europe & Central Asia",
             "America Latina y el Caribe"="Latin America & Caribbean",
             "Medio Oriente y Norte de Africa" = "Middle East & North Africa",
             "Norteamerica"=" North America",
             "Asia del Sur"= "South Asia",
             "Africa Subsahariana"= "Sub-Saharan Africa")


wbData2$region <- wbData2$region %>% 
  fct_recode("America Latina y el Caribe" ="Latin America & Caribbean ",
             "Africa Sub Sahariana"="Sub-Saharan Africa ")

#select tiene ver con las columnas, para seleccionar ciertas variables dentro de la base de datos 
#filter es para escoger un rango de valores de la variable 

wbData2 <- wbData2%>% filter(region != "Aggregates")
#las elimina, pero sigue conservando el nombre de las variables, si se quisiera borrar el nombre
wbData2 <- wbData2%>% filter(region != "Aggregates") %>%droplevels()

table(wbData2$region) #para ver el numero de observaciones por cada variable 

#para contar las observaciones
country <- wbData2 %>%count(country)

#Pregunta 4 

  wbData3 <- wbData2 %>% filter(region !="Aggregates") %>%droplevels()
length (unique(wbData3$country))

n_distinct(wbData3$country)
wbData3 %>% count(country)

wbData3 %>% group_by(country) %>% summarise(n=n())

#Pregunta 5
#para guardarlo en una base
poblacion <- wbData3 %>%filter(year==2015)%>%
  arrange(desc(population))

#para que entregue el resultado en la consola
wbData3 %>%filter(year==2015)%>%
  select(country, population) %>%
  arrange(desc(population)) %>% head(5)


#Pregunta 6
wbData3 %>%filter(year==2015)%>%
  select(country,gdppc)%>%
  arrange(gdppc)%>%head(10)


#Pregunta 7
wbData3 %>% filter(year==2015)%>% 
  group_by(region)%>%
  summarize(gdppc_min=min(gdppc,na.rm = TRUE),gdppc_max=max(gdppc,na.rm = TRUE))
#dentro del summarize pueden ir distintos estadisticos


#Pregunta 8
wbData3 %>% filter((year==1990 | year==2015)& (country %in% 
  c("Chile", "United States")))%>%
  select(country,year,gdppc)



#Pregunta 9 
wbData3 %>% filter(region=="America Latina y el Caribe" & year==2015)%>%
  summarise(gdppc_prom=mean(gdppc,na.rm = TRUE),
            gdppc_sd=sd(gdppc,na.rm = TRUE))


#Pregunta 10
wbData4 <- wbData3 %>% mutate(pop1000=population/1000)




wb <- wbData3 %>% select(country,year, population,gdppc,lifeExpectancy)
summary(wb)


g <- wb%>%
  filter(is.na(population)|is.na(gdppc)|is.na(lifeExpectancy))
