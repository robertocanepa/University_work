library(foreign)
library(dplyr)
library(tidyverse)
library(forcats)
library(ggplot2)
library(ggthemes)
library(gridExtra)


nsw <- read.dta("C:/Users/rcane/Desktop/Electivo R/nsw.dta")

#pregunta 1
 nsw$treat <- as.factor(nsw$treat)
 nsw$treat <- nsw$treat %>% fct_recode(Tratamiento ="1" , Control ="0")
 
 
 table (nsw$treat)
 
 attach(nsw)
 table (treat)
 
#Pregunta 2
nsw %>% group_by(treat) %>% summarise(
   avg_age = mean(age, na.rm = TRUE),
   avg_educ =mean(education,na.rm= TRUE),
   avg_black= mean(black, na.rm = TRUE),
   avg_hisp =mean( hispanic, na.rm=TRUE,
   avg_mar= mean(married, na.rm = TRUE),
   avg_nodeg =mean(nodegree, na.rm=TRUE)
   )
  
 
 nsw %>% group_by(treat) %>% summarise_at(2:7 , mean)

 #test de diferencia de medias
 t.test()
 
 #Pregunta 3
 avg_earnigns <-nsw %>% group_by(treat) %>% summarise(
   avg_re75=mean(re75,na.rm = TRUE),
   avg_re78=mean(re78, na.rm=TRUE))
avg_earnigns 

avg_earnigns %>% ggplot(aes(x=treat , y=avg_re78))+
  geom_bar(stat = "identify", width = 0.4),position="dodge",color="black",fill="red")+
  geom_text(aes(label=round(avg_re78,0)),position = position_dodge(0.9),vjust=1.5)+
  xlab("Grupo")+
  ylab("Ingreso promedio en 1978 (dolares)")+
  ggtitle("Ingreso promedio de grupos tratamiento y control")
            )
