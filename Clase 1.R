#Programa general del curso:
#Asistencia >= 80%
#Tareas: 60%
#Examen: 25%
#Controles:15%

####CLASE 1 #####

#pregunta 1: generar un vector que genere una frecuencia de numeros del 1 al 10 
seq(1,10,1)
#aca se pueden hacer comentarios, el ejercicio anterior se puede hacer de varias formas distintas 
seq(10)
1:10

#Para guardar la informacion en un vector cualquiera (recordar que el == significa igual, el otro significa asignacion)
x=1:10
x <- 1:10

#b
seq(1,9,2)
#c 
seq(1,5,0.5) #otra forma de hacerlo
seq(1,5,length=9)

#pregunta 2
rep ("A",5)
#pregunta 4

x= c("pedro"=17,"Ana"=NA,"Maya"=23,"Max"=NA,"Paula"=20)
x
#para que aparezcan ciertos valores
x[c(2,5)]


#para eliminar los missing values
x[c(1,3,5)]
x[-c(2,4)]
x[!is.na(x)]

#para saber cual de los valores se cumple la condicion 
!is.na(x)
# en R, cuando existen missing values la estadistica descriptiva no puede ser calculada a menos que se indique remover dichos valores
mean(x,na.rm = TRUE)
mean(x)
mean(x<=20,na.rm = TRUE)


#CLASE 2 (30/07)
mean(x<=20,na.rm = TRUE) #aca calcula la proporcion de alumnos que cumple esa condicion
mean(x[x<=20],na.rm = TRUE)

#F 
sum(x<=20,na.rm=TRUE)
#G
sum(x[x<=20],na.rm = TRUE)

#H
sum(x[!is.na(x)])

#PREGUNTA 5
#crear el vector x que sea una secuencia de 1 a 100, para luego sumar los cuadrados de los reciprocos 
x=1:100
#PREGUNTA 6
z=sum((1/x)^2)
cumsum(z) #entrega la secuencia completa, en lugar de solo el resultado de la suma 

#PREGUNTA 7
a=c(0,10,-5)
b=c(5,8,-4)
c=c(0,1,-1)
#definir un vector d que identifique a los elementos del vector a que son menores a los elementos del vector b
d=a<b
d
#b) asignar el segundo elemento de b hacia a
a[2]<-b[2]
a

#c) definir un nuevo vector


#d) nombrar los elementos de un vector 
names(a)<-c("L1","L2","L3")
#e) definir una matriz a partir de ciertos vectores 
A<- rbind(a,b,c)
A

A<-cbind(a,b,c)
A


#PREGUNTA 8
#a partir de los siguientes vectores
x=c(3,2)
y=c(1,0)

#a) 
W<-cbind(x,y)
W

#b)asignar nombres a la matriz 
rownames(W)<- c("fila1","fila2")
colnames(W)<- c("Columna1","columna 2")


#PREGUNTA 9
e=c(-1,0,2,4,-2,3)
K1 <-matrix(e,nrow=3,ncol=2)
K1
K2<-matrix(e,nrow =2,ncol=3,byrow = TRUE)
K2



#pregunta 10
#construir una matriz identidad 
Z<-diag(4)

#pregunta 11
z<-c(1,4,-1,3,-2)
G<-diag(z)
G

#pregunta 12

fila1=c(1,1,1)
fila2=c(3,-2,1)
fila3=c(2,1,-1)

X=rbind(fila1, fila2, fila3)
X
#calcular la transpuesta e inversa de x
t(X)#transpuesto
solve(X)#inversa
solve(X)%*% X
round (solve(X)%*% X,0)#construir la identidad a partir de la multiplicacion de la inversa y la original 



#PREGUNTA 13
#resolver sistemas de ecuaciones

fila1 =c(3,4,-5,1)
fila2=c(2,2,2,-1)
fila3=c(1,-1,5,-5)
fila4=c(5,0,0,1)
k=c(10,5,7,4)

X=rbind(fila1,fila2,fila3,fila4)
solve(X)%*%k



#Pregunta 14
hola=list(A=seq(8,36,4),B="hola",C=diag(3))
hola

s=hola[[1]] #dataframe
s


#Pregunta 15

mes <- c("ENE", "FEB","MAR","ABR","MAY","JUN","JUL")
producto1 <-c("$1,000","$3,000","$6,000","$9,000","$7,000","$8,000","$10000")
producto2 <-c("$1,000","$2,000","$3,000","$5,000","$9,000","$6,000","$4000")
producto3 <-c("$2,000", "$4,000","$4,000","$2,000","$3,000","$2,000","$5,000")
ventas = data.frame (mes,producto1,producto2,producto3)
ventas

View(ventas)
#para guardar el dataframe
write.csv(ventas, path = "C:/Users/rcane/Desktop/Electivo R/ventas.csv")

#para instalar paquetes
install.packages("tidyverse")
install.packages("dyplr")


ventas<- ventas%*% mutate_at(2:4,parse_number)

summary(ventas)
str(ventas)

#para ver cierta cantidad de datos, ya sea los n primeros o n ultimos 
head(ventas,n=3)
tail(ventas)


#para actualizar R
install.packages("installr")
library(installr)
updateR()


ventas3<- ventas2 %*% select(mes,producto2)


