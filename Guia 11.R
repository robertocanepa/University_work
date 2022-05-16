
###GUIA 11



#Pregunta 1

#(a) Probabilidad de observar 4 sucesos en 1 hora, tasa promedio 1.8
dpois(4,1.8)

#(b) Pr de dos o mas sucesos por hora
1-dpois(0,1.8)-dpois(1,1.8)
1-ppois(1,1.8)

#Pregunta 2

(1-punif(1400,1000,2000))*5000

#Pregunta 3

#Experimento de montecarlo
set.seed(123) # al fijar la semilla no cambian las proporciones 
b = rep(c("roja", "azul"),times=c(4,6))
b

R=10000 #Número de replicaciones
n=1     #Tamaño muestral


eventos = replicate(R,sample(b,n,replace= TRUE))
table(eventos)
prop.table(table(eventos))

#Pregunta 4

#(a)
R =10000 #N° observaciones
n=30     # tamaño curso

bdays = sample(1:365,n,replace = TRUE)
bdays
duplicated (bdays)
any(duplicated(bdays))

results = replicate(R,
                    {bdays = sample(1:135,n,replace =TRUE)
                     any(duplicated(bdays))})
mean(results)

#(b)

bday_problem = function(n, R=10000) {
  mismo_dia = replicate(R,
                        {bdays = sample(1:365,n,replace =TRUE)
                        any(duplicated(bdays))})
  mean(mismo_dia)
}

bday_problem(30,500)

#(c)

library(dplyr)
library(ggplot2)

n=seq(1,60)
prob = sapply(n,bday_problem)
p<- data.frame(n,prob)
 p%>% ggplot (aes(x=n,y=prob))+ geom_line(size=0.8)+
   xlab("Tamaño del curso")+ylab("Probabilidad")+
   theme_bw()

#parte (d)
 
rm(list = ls())

library(dplyr)
library(ggplot2)
 
bday_prob = function(R, n=30) {
  mismo_dia = replicate(R,
                        {bdays =sample(1:365,n,replace=TRUE)
                        any(duplicated(bdays))})
  mean(mismo_dia)
}

R = 10^seq(1.5,len =1000)
probR = sapply(R, bday_prob)

logR = log10(R)

p <- data.frame(logR, probR)
p %>% ggplot(aes(x =logR, y=probR))+ geom_line(size=0.8)+
  xlab("Logaritmo (base10) numero de repeticiones")+ ylab ("Probabilidad")+
  theme_bw()



#Pregunta 5
Y <-sample(c(-1,1),10,replace = TRUE,prob = c(9/19,10/19))
Y
sum(Y)

rep <- 10000
n <- 1000 #numero de lanzamientos

S <- replicate(rep, {
  x <- sample(c(-1,1),n,replace = TRUE, prob = c(9/19,10/19))
  Z<- sum(x)
  Z
})

mean(S<0)
mean(S)
sd(S)


data <- data.frame(S)
ggplot(data)+
  geom_histogram(aes(S, ..density..), col="black", binwidth = 8)+
  stat_function(fun=dnorm, args = list(mean=mean(S), sd=sd(S)),
                geom= "line", colour= "red", size=1.2)+
  xlab("S")+
  ylab("Probabilidad")+ theme_bw()



#Pregunta 6

prev <- 0.01 #prevalencia
N <- 10000
resultado <- sample(c("enfermo","sano"),N, replace = TRUE, prob = c(prev, 1-prev))
E <- sum(resultado=="enfermo")
S <- sum(resultado=="sano")
E
S

accurracy <- 0.95
test <- vector ("character",N)

test [resultado =="enfermo"] <- sample(c("+","-"),E, replace = TRUE , prob =c(accurracy, 1- accurracy))

test [resultado =="sano"] <- sample(c("-","+"),S, replace = TRUE , prob =c(accurracy, 1- accurracy))

table( test, resultado)

# Pregunta 7

library(ggplot2)
library(dplyr)
library(gridExtra)

#Teorema central del limite

set.seed(1234567)

reps = 10000
medias = matrix(NA, nrow = reps, ncol=4)

n = c(50,100,500,1000) # tamaño muestral

for(j in 1:length (n)){
  for(i in 1:reps){
    x= runif(n[j],0.1)
    medias [i,j]= mean(x)
  }
}


medias <- data.frame(medias)

g1 <- ggplot(medias)+
  geom_histogram(aes(medias[,1], y= ..density..), col="black", bins= 30)+
  stat_function(fun=dnorm, args = list (mean= mean(medias[,1]),sd=sd(medias[,1])),
                geom= "line",colour = "red",size=1.2)+
  ylab("densidad")+ggtitle("n=50")+xlab(expression(bar(x)))+
  theme_bw()

g2 <- ggplot(medias)+
  geom_histogram(aes(medias[,2], y= ..density..), col="black", bins= 30)+
  stat_function(fun=dnorm, args = list (mean= mean(medias[,2]),sd=sd(medias[,2])),
                geom= "line",colour = "red",size=1.2)+
  ylab("densidad")+ggtitle("n=100")+xlab(expression(bar(x)))+
  theme_bw()

g3 <- ggplot(medias)+
  geom_histogram(aes(medias[,3], y= ..density..), col="black", bins= 30)+
  stat_function(fun=dnorm, args = list (mean= mean(medias[,3]),sd=sd(medias[,3])),
                geom= "line",colour = "red",size=1.2)+
  ylab("densidad")+ggtitle("n=500")+xlab(expression(bar(x)))+
  theme_bw()

g4 <- ggplot(medias)+
  geom_histogram(aes(medias[,4], y= ..density..), col="black", bins= 30)+
  stat_function(fun=dnorm, args = list (mean= mean(medias[,4]),sd=sd(medias[,4])),
                geom= "line",colour = "red",size=1.2)+
  ylab("densidad")+ggtitle("n=1000")+xlab(expression(bar(x)))+
  theme_bw()




grid.arrange(g1, g2,g3,g4, nrow=2, ncol=2)