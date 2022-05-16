
###GUIA 10

#Pregunta 1

library(ggplot2)
library(ggthemes)

ggplot(data.frame(x=c(-6,6)),aes(x))+
  stat_function(fun=dnorm,args=list(mean=0,sd=1),
                geom="line",colour="red",size=1.2)+
  stat_function(fun=dnorm,args=list(mean=0,sd=1.5),
                geom="line",colour="black",size=1.2)+
  stat_function(fun=dnorm,args=list(mean=0,sd=2),
                geom="line",colour="blue",size=1.2)+
  ylab("f(x)") + theme_economist_white() +
  ggtitle("Distribucion normal")


#Pregunta 2

ggplot(data.frame(x=c(-6,6)),aes(x))+
  stat_function(fun=pnorm,
                geom="line",colour="red",size=1.2)+
  stat_function(fun=pt,args=list(5),
                geom="line",colour="black",size=1.2)+
  stat_function(fun=plogis,
                geom="line",colour="green",size=1.2)+
  ylab("Cummulative Distribution") + theme_hc() +
  ggtitle("Distribucion normal, t-student y logistica")


#Pregunta 3 

#a)
quad <-function(x) {x^3+2^x}
ggplot(data.frame(x=c(-2,2)),aes(x))+
  stat_function(fun=quad, geom="line", size=1.2)+ 
  theme_hc()


#otra forma
ggplot(data.frame(x=c(-2,2)),aes(x)) +
  stat_function(fun=function(x) x^3+2^x, geom="line", size=1.2)+ 
  theme_hc()

#forma mas lenta
library(dplyr)
x=seq(-2,2,length=1000)
y=x^3+2^x
data=data.frame(x,y)
data %>% ggplot(aes(x,y))+geom_line(size=1.2)+theme_bw()

#b)

ggplot(data.frame(x=c(-10,10)),aes(x))+
  stat_function(fun=function(x){(x^2+x)*cos(x)},geom="line",size=1.2)+
  theme_bw()

#c)

x=seq(-6,4,length=1000)
y=x^2+2*x-1
data=data.frame(x,y)
data%>%ggplot(aes(x,y))+geom_line(size=1.3)+
  geom_text(aes(2.8,20,label=(paste(expression("y=x"^2"+2x-1")))),parse=TRUE) +
  theme_bw()


#Pregunta 4

A=5
eps=1
x=seq(1,10,length=1000)
y=(A/x)^(1/eps)

data=data.frame(x,y)
data%>% ggplot(aes(x,y))+geom_line(size=1.2)+xlab("Cantidad demandada")+
  ylab("Precio")+theme_bw()


#Pregunta 5

Q=seq(1,9,length=1000)
CT= 3*Q^3-15*Q^2+50*Q+100
Cmg=9*Q^2-30*Q+50
Cme=3*Q^2-15*Q+50+(100/Q)
CVme=3*Q^2-15*Q+50

data=data.frame(Q,CT,Cme,Cmg,CVme)
data%>% ggplot()+
  geom_line(aes(x=Q,y=Cmg),linetype = "dashed", size=1.3)+
  geom_line(aes(x=Q,y=Cme),linetype = "solid", size=1.3)+
  geom_line(aes(x=Q,y=CVme),linetype = "dashed", size=1.3)+
  xlab("Produccion")+ ylab("Costo($)")+
  ylim(0,150)+
  geom_text(x=5.6,y=140,label="CMg")+
  geom_text(x=1.1,y=142,label="CMe")+
  geom_text(x=8.7,y=140,label="CVme")+
  theme_bw()
  
#Pregunta 6

y<- rep(NA,1000)
e<- rep(NA,1000)
e[1]=rnorm(1)
y[1]=0
rho=0.8
for(t in 2:2000) {
e[t]=rnorm(1)
y[t]<-rho* y[t-1]+e[t]
}

time <-1:1000
data <-data.frame(time,y)
data %>% ggplot(aes(time,y))+ geom_line(size=0.8)+
  theme_bw()