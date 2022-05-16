########GUIA N°4################


#paquetes a usar
install.packages("gridExtra")
install.packages("ggstance")
install.packages("ggthemes")
install.packages("ggrepel")
install.packages("plotly")
library(plotly)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggstance)
library(ggthemes)
library(ggrepel)

#sintaxis de comandos de graficos 


#ggplot(var1)+geom_line( ), grafico de lineas
#ggplot(var1)+geom_boxplot(), grafico de caja
#ggplot(var1)+geom_histogram(), histograma
#ggplot(var1)+stat_ecdf(), empirical cumulative distributive function
##ggplot(var1)+geom_point(), grafico de puntos 

#Pregunta 1
wbData3 %>% filter(country%in% c("Chile","Japan","United States"))%>%
  ggplot(aes(year,lifeExpectancy,col=country))+geom_line()

#otra forma mas estetica:
wbData3 %>% filter(country%in% c("Chile","Japan","United States"))%>%
  ggplot(aes(year,lifeExpectancy,col=country))+geom_line(size=1.3)+
  scale_x_continuous(name="Año",breaks = seq(1960,2016,10))+
  ylab("Expectativa de vida al nacer 1960-2016")+
  scale_color_discrete(name="",labels=c("Chile","Japon","Estados Unidos"))+
  theme_bw()+
  ggtitle("Expectativa de Vida al nacer 1960-2016")

# dentro de Theme:
#theme_economist
#theme_bw
#theme_classic




wbData3 %>% filter(country%in% c("Chile","Japan","United States"))%>%
  ggplot(aes(year,lifeExpectancy,col=country))+geom_line(size=1.3)+
  scale_x_continuous(name="A?o",breaks = seq(1960,2016,10))+
  ylab("Expectativa de vida al nacer 1960-2016")+
  ggtitle("Expectativa de Vida al nacer 1960-2016")+
  scale_linetype_manual(name="",values = c("dotted","twodash","solid"),labels=c("Chile","Jap?n","Estados Unidos")+
                 

  #Pregunta 2
wbData3%>%filter(year==2015)%>% 
  ggplot(aes(x=region, y=lifeExpectancy,fill=region))+
  geom_boxplot(show.legend = FALSE)+
  ylab("Expectativa de vida al nacer(a?os)")+
  xlab("")+
  ggtitle("Expectativa de vida al nacer por regiones del mundo(2015)")




wbData3 %>% filter(year== 2015)%>%
  ggplot(aes(X=region,y=lifeExpectancy, fill=region)) +
  geom_boxplot(show.legend = FALSE) +
  ylab("Expectativa de vida al nacer") +
  xlab("") +
  ggtitle("Expectativa de vida al nacer por regiones del mundo (2015)") +
  scale_color_discrete(name="") +
  coord_flip() + theme_bw()


#Pregunta 3
wbData3 %>% filter(year==2015) %>%
  ggplot(aes(gdppc))+
  geom_histogram(bins = 50,col="black",fill="blue")+
  scale_x_continuous(labels=function(x) x/1000,
                     breaks = c(0,25000,50000,10000,125000))+
  ylab("PIB per capita (miles de dolares)")+
  ggtitle("PIB per capita alrededor del mundo(2015)")+
  theme_bw()


#Pregunta 4
wbData3 %>% filter(year==2015)%>%
  ggplot(aes(x=gdppc))+
  stat_ecdf(geom="step")+
  scale_x_continuous(labels=function(x) x/1000,
                     breaks = c(0,25000,50000,10000,125000))+
  ylab("")+
  xlab("PIB per capita (miles de dolares)")+
  ggtitle("distribucion acumulada del PIB per capita (2015)")+
  theme_bw()


#Pregunta 5
wbData3%>% filter(year==2015)%>%
  ggplot(aes(x=gdppc,y=lifeExpectancy,col=region))+
  geom_point()+
  xlab("PIB per capita (miles de dolares)")+
  ylab("Expetativa de vida al nacer (a?os)")+
  scale_x_continuous(labels=function(x) x/1000,
                     breaks = c(0,25000,50000,10000,125000))+
  ggtitle("Expectativa de vida al nacer vs PIB per capita (2015)")+
  scale_color_brewer(name="",palette = "Set2")+
  theme_bw()



#Pregunta 6
library(plotly)

p1 <- wbData3 %>% filter(year==2015) %>% 
  ggplot(aes(x= gdppc,y=lifeExpectancy, col=region, group = country))+
  geom_point(aes(size = population), alpha = 0.7) +
  xlab("PIB per c?pita (miles de d?lares)") + 
  ylab("Expectativa de vida al nacer (a?os)") +
  ggtitle("Expectativa de vida al nacer versus PIB per c?pita (2015)") +
  scale_x_continuous(labels = function(x) x/1000, 
                     breaks = c(0,25000,50000,75000,100000, 125000)) +
  scale_size_area(name="", guide = FALSE, max_size = 12) +
  scale_color_brewer(name ="", palette = "Set2") +
  theme_bw(base_size = 10)

ggplotly(p1, height = 500, width = 890)


#Pregunta 7

library(plotly)
p2 <- wbData3 %>% filter(year>=1990) %>% 
  ggplot(aes(x= gdppc,y=lifeExpectancy, frame=year, col=region, group = country))+
  geom_point(aes(size = population), alpha = 0.7) +
  xlab("PIB per c?pita (miles de d?lares)") + 
  ylab("Expectativa de vida al nacer (a?os)") +
  ggtitle("Expectativa de vida al nacer versus PIB per c?pita 1990-2016") +
  scale_x_continuous(labels = function(x) x/1000, 
                     breaks = c(0,25000,50000,75000,100000, 125000)) +
  scale_size_area(name="", guide = FALSE, max_size = 12) +
  scale_color_brewer(name ="", palette = "Set2") +
  theme_bw(base_size = 13) 


ggplotly(p2, height = 500, width = 890) %>%
  animation_opts(frame = 300, 
                 easing = "linear",
                 redraw = TRUE)



