library(ggplot2)
library(ggthemes)
library(dplyr)

#Data Proyect semana 5:

#datos:
load("brfss2013.RData")
load("data.Rdata")

table(brfss2013$sleptim1)

#Variables que pueden servir:
# menthlth (dias de mala salud mental)
# sleeptim1 (tiempo de sueño)
# exerany2 (ejercicio ultimos 30 días)
# isatisfy (satisfacción con la vida)
# emtsuprt (cada cuanto necesita apoyo emocional)
# colghous (residencia universitaria)
# pvtresd1 (residencia privada)
# educa (nivel educacional)
# employ1 (situacion ocupacional)
# decide (dificultad para concentrarse o recordar)
# smokeday2 (frecuencia fumador)
# avedrnk2 (promedio dias que bebe)


#Pregunta 1

#Seleccionar variables

p1<-brfss2013%>%select(educa,employ1,marital,children,renthom1,cstate,X_age_g)

#N° de hijos

Table1 <-brfss2013%>%summarise(child_mean=mean(children,na.rm=TRUE),child_sd=sd(children,na.rm=TRUE),
                        child_max=max(children,na.rm=TRUE),child_min=min(children,na.rm=TRUE))
#estado laboral
t1 <-table(brfss2013$employ1)
t1<- prop.table(t1)
a = as.data.frame(t1)
names(a)[1]="Employ Status"
names(a)[2]="Frequency"
a

#educación
t2 <-table(brfss2013$educa)
t2<-prop.table(t2)
b = as.data.frame(t2)
names(b)[1]="Educational level"
names(b)[2]="Frequency"
b

#Estado marital
t3 <-table(brfss2013$marital)
t3 <-prop.table(t3)
c= as.data.frame(t3)
names(c)[1]="Marital Status"
names(c)[2]="Frequency"
c

#Rango de edad
g1 <-ggplot(data=p1,aes(X_age_g))+geom_bar()+
  xlab("Age range")+ylab("Frecuency")+
  theme_light()+scale_x_discrete(na.translate = FALSE)+
  ggtitle("Age of the surveyed sample")
g1

freq <-c(0.055,0.102,0.122,0.17,0.222,0.326)
#Dejarlo como frecuencia
t4 <- table(brfss2013$X_age_g)
t4<- prop.table(t4)
z= as.data.frame(t4)



#Pregunta 2:

#Filtrar por las variables que se usarán
p2 <-brfss2013%>%select(fruit1,fvbeans,fvgreen,ssbsugar)

#Consumo de azucar
d <-data.frame(p2$ssbsugar)
d<-d%>%filter(p2.ssbsugar>300 | p2.ssbsugar==0)
d <-d%>%mutate(fsugar=ifelse(p2.ssbsugar>0,p2.ssbsugar-300,0))
t1_2 <-d%>%summarise(mean_=mean(fsugar),sd_=sd(fsugar),max_=max(fsugar),
              min_=min(fsugar),median_=median(fsugar))
t1_2 <-t1_2%>%mutate(name="Sugar")
t1_2 <-t1_2[,c(6,1,2,3,4,5)]

#Consumo de Frutas
e <-data.frame(p2$fruit1)
e <-e%>%filter(p2.fruit1>300|p2.fruit1==0)
e <-e%>%mutate(fruit = ifelse(p2.fruit1>0,p2.fruit1-300,0))
t2_2 <-e%>%summarise(mean_=mean(fruit),sd_=sd(fruit),max_=max(fruit),
              min_=min(fruit),median_=median(fruit))
t2_2 <-t2_2%>%mutate(name="Fruit")
t2_2 <-t2_2[,c(6,1,2,3,4,5)]

#consumo de legumbres
f <-data.frame(p2$fvbeans)
f <-f%>%filter(p2.fvbeans>300|p2.fvbeans==0)
f <-f%>%mutate(beans = ifelse(p2.fvbeans>0,p2.fvbeans-300,0))
t3_2 <-f%>%summarise(mean_=mean(beans),sd_=sd(beans),max_=max(beans),
              min_=min(beans),median_=median(beans))
t3_2 <-t3_2%>%mutate(name="Beans")
t3_2 <-t3_2[,c(6,1,2,3,4,5)]

#Consumo de verduras
g <-data.frame(p2$fvgreen)
g <-g%>%filter(p2.fvgreen>300|p2.fvgreen==0)
g <-g%>%mutate(vgreen = ifelse(p2.fvgreen>0,p2.fvgreen-300,0))
t4_2 <-g%>%summarise(mean_=mean(vgreen),sd_=sd(vgreen),
              max_=max(vgreen),min_=min(vgreen),median_=median(vgreen))
t4_2 <-t4_2%>%mutate(name="vegetables")
t4_2 <-t4_2[,c(6,1,2,3,4,5)]

desc_stat <-rbind(t1_2,t2_2,t3_2,t4_2)


#grafico de los 10 estados con mas consumo de 

graph_p2 <-data.frame(d,e,f,g)
#Paquete stargazer hace tablas bonitas en formato latex (utiles para regresiones)

install.packages("stargazer")
library(stargazer)



# Pregunta 3

p3 <-brfss2013%>%select(menthlth,addepev2,misnervs,misdeprd,lsatisfy,X_state)

# variables addepev es si o no, lsatisfy es 4 categorías.

table3_1 <-p3%>%summarise(mean_=mean(menthlth,na.rm=TRUE),
                          median_=median(menthlth,na.rm=TRUE),
                          sd_=sd(menthlth,na.rm=TRUE))

#Grafico de depresión
depress <-p3%>%select(X_state,addepev2)%>%filter(addepev2=="Yes")%>%
  group_by(X_state)%>%count(addepev2,sort = TRUE)

Table3_2 <-depress%>%select(X_state,n)%>%head(10)
names(Table3_2)[1]="State"

g2<- ggplot(data = depress,aes(x=reorder(X_state,n),y=n))+geom_bar(stat = "Identity")+
      coord_flip()+theme_minimal()+xlab("States")+ylab("Number of depressive disorder happened")
  
#Grafico de Frecuencia nerviosa
g3 <-ggplot(data=p3,aes(misnervs))+geom_bar()+scale_x_discrete(na.translate = FALSE)+
  theme_minimal()+xlab("Types of nervious sensations")+ylab("Frequency")

#Grafico de satisfaccion con la vida

g4<- ggplot(data=p3,aes(lsatisfy))+geom_bar()+scale_x_discrete(na.translate=FALSE)+
  theme_minimal()+xlab("Satisfaction with life")+ylab("Frequency")



