library(readr)
ventas <- read.csv()

install.packages("readxl")
#Guia N° 2

#pregunta 2

#eliminar la base anterior
rm=(list=ls())
library(tidyverse)
library(readxl)

excel_sheets("C:/Users/rcane/Desktop/Electivo R/mortality_under_five.xls")

mortality <- read_excel("C:/Users/rcane/Desktop/Electivo R/mortality_under_five.xls", sheet="Data")
View(mortality)
mortalidad5 <- mortality%*% gather( year, mortalidad, -country, convert=TRUE)
mortalidad5 <- mortalidad5 %*% arrange(country, year)

#pregunta 3
 
library(haven)
library(foreign)

hprice2 =read.dta("C:/Users/rcane/Desktop/Electivo R/hprice2.dta")
View(hprice2)

#tipo "describe" en stata
str(hprice2)

#pregunta 4
rm(list= ls())


install.packages("Quandl")
Sys.setenv(TZ="Etc/GRT-4")
library(zoo)
library(xts)
library(Quandl)
library(dplyr)
library(ggplot2)
library(ggthemes)

oil_prices1 = Quandl("FRED/DCOILWTICO")
View(oil_prices1)


#pregunta 5
library(tidyverse)
install.packages("pdfetch")
library(pdfetch)
library(ggplot2)
install.packages("ggthemes")
library(ggthemes)

aapl=pdfetch_YAHOO("AAPL",fields="adjclose",from="2003-01-01")
