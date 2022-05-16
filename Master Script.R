################################################################################
################################################################################
####### Script base para tener comandos y funciones a mano #####################
################################################################################
################################################################################

#1. Trabajar con vectores

# En R se puede hacer un vector con el simbolo "<-" o "=" indicando su composi-
# ción. Para mayor orden y claridad le damos un nombre a ese vector:

a <-seq(1,10,1) # a es un vector de 10 valores del 1 al 10.

# si quiero darle un rango definido se una length al final de la secuencia
# el vector pueden ser valores repetidos, para esto usar rep(valor,N° de veces)

#Dar etiquetas al vector y su valor: Las etiquetas van con " " y pueden haber NA

b <-c("A"=1,"B"=NA,"C"=3,"D"=4,"E"=5)
b

# Operaciones con vectores

length(v) #Determina el número de elementos del vector v
min(v) #Determina el mínimo valor del vector v
max(v) #Determina el máximo valor del vector v
sum(v) #Suma elementos del vector v
prod(v) #Multiplica elementos del vector v
sort(v) #Ordena elementos del vector v


#Identificar cual de los valores es un missing value.
!is.na(b)

#Hacer estadistica descriptiva cuando hay missing values (debo indicarlos, ya
# ya que si hay no lo va a calcular)
mean(b,na.rm = TRUE)
#Darle ciertas condiciones al comando mean
mean(b<=3,na.rm = TRUE) #proporcion
mean(b[b<=3],na.rm = TRUE) #cantidad que cumplen con la condicion

sum(a>3,na.rm = TRUE)
cumsum(a) #Es la suma acumulada de los elementos del vector.

#Matrices
# se crea un vector c primero

c <-c(1,3,5,7,9,12,15)

#Se nombran con letras mayúsculas
A <-cbind(a,b,c) #cuando no tienen el mismo rango las columnas, repite valores.
A

#Nombrar filas y columnas de la matriz
colnames(A) <-c("Columna 1", "Columna 2","Columna 3")
rownames(A) <-c("Fila 1", "Fila 2","Fila 3","Fila 4","Fila 5", "Fila 6", "Fila 7","Fila 8","Fila 9","Fila 10")

e=c(-1,0,2,4,-2,3)
K1 <-matrix(e,nrow=3,ncol=3) #Crea una matriz a partir de un vector, asignando 
K1                           #  numero de filas y columnas

#Matriz identidad
B <-diag(3)

#Operaciones maticiales
t(K1)       #Transpuesta
solve(K1)   #Inversa, si no es cuadrada o es singular no entregará la inversa.
diag(A)     #Captura elementos de la diagonal principal de A
A + B       #Suma de matrices A y B
A %*% B     #Multiplicación de matrices A y B
A*B         #Multiplica elemento (i, j) de A y B, respectivamente
rowSums(A)  #Suma elementos de cada ﬁla de A
colSums(A)  #Suma elementos de cada columna de A

#Sistemas de ecuaciones

#Se construye una matriz cuadrada y mediante operaciones matriciales se calculan
# los x's
f1 =c(3,4,-5,1)
f2=c(2,2,2,-1)
f3=c(1,-1,5,-5)
f4=c(5,0,0,1)
k=c(10,5,7,4)

B=rbind(f1,f2,f3,f4)
solve(B)%*%k #Entrega cada una de las incognitas.

#Guardar vectores o matrices en data frames y trabajar con el data frame

mes <- c("ENE", "FEB","MAR","ABR","MAY","JUN","JUL")
producto1 <-c("$1,000","$3,000","$6,000","$9,000","$7,000","$8,000","$10000")
producto2 <-c("$1,000","$2,000","$3,000","$5,000","$9,000","$6,000","$4000")
producto3 <-c("$2,000", "$4,000","$4,000","$2,000","$3,000","$2,000","$5,000")
ventas = data.frame (mes,producto1,producto2,producto3)
ventas #lo muestra en la consola
View(ventas) #lo muestra en una ventana aparte, como base de datos.

#Teniendo un data frame como base de datos, es posible operar ciertas funciones
# para lo cual es muy util utilizar paquetes de funciones, tanto para graficar
# tablas de estadistica, crear variables nuevas, etc.

library(tidyverse)
library(dplyr)

summary(ventas) #entrega una breve descripción de la variable.
str(ventas)     #muestra las observaciones en formato string.

#Seleccionar una parte de la base de datos:
ventas2<- ventas %*% select(mes,producto2) #Para tidyverse se usa %*%

#Borrar base de datos usada anteriomente
rm=(list=ls())

#####IMPORTACION DE DATOS#####

# El comando tidyverse tiene muchos paquetes incluidos, dentro de los cuales se
# pueden importar datos de diversos formatos, pudiendo utilizar estas funciones:

# comando readr y readxl:
read_excel() #Para archivos .xls y .xlsx
read_csv() #Para archivos .csv
read_tsv() #Para archivos separados por tab
read_delim() #Para archivos delimitados
read_fwf() #Para archivos de ancho fijo

# El paquete foreign permite importar datos en formato de otros programas
# estadísticos, como:

read.dta() #Stata
read.spss() #SPSS
read.xport() read.ssd() #SAS

#El paquete Quandl permite acceder a bases de datos financieras y económicas, el
# paquete quantmod permite modelar series de tiempo 
library(Quandl)

#El paquete pdfetch permite importar datos de estas fuentes:
pdfetch_YAHOO #Yahoo Finance
pdfetch_BLS   #Oficina de estadísticas laborales de Estados Unidos
pdfetch_BOE   #Banco de Inglaterra
pdfetch_ECB   #Banco Central Europeo
pdfetch_EUROSTAT #EUROSTAT
pdfetch_WB    #Banco Mundial


###PAQUETE DPLYR (muy utilizado para edición de bases de datos)###

library(dplyr)

#A partir de una base de datos, es posible aplicar funciones, usando %>%.

#Cambiar nombre de variables
basenueva <- baseusada%>%rename(var_nueva=var_a_cambiar)
#Ordenar los datos
basenueva <- basenueva%>%arrange(var_a_ordenar)

#cambiar nombre de datos de una variable
basenueva$var_cambiar <-basenueva$var_cambiar%>%
  fct_recode("nombrenuevo"="nombre antiguo")

#Selecionar variables de la base de datos (muestra)
basenueva <-baseusada%>%select(Variable1,variable2,variable3)

#Filtrar la base por ciertas observaciones de una variable, pudiendo eliminar el
# resto o solo ocultarlas
basenueva <- basenueva%>% filter(var_filtrar != "dato que no quiero")
basenueva <- basenueva%>% filter(var_filtrar != "dato que no quiero") %>%
  droplevels() #Con esto se eliminan los datos que no filtré
basenueva <- basenueva%>%filter(is.na(var)) #elimina todas las observaciones que
                                            # que tienen NA de la variable.

#Contar las observaciones
variable <-baseusada%>%count(variable)

#Ordenar los datos de mayor a menor (o viceversa), usando filtros
#Tomo una base de datos, selecciono las var. a utilizar, filtro por año y ordeno
# de mayor a menor, pudiendo mostrar las que me interesan o todas.

poblacion <- wbData3 %>%filter(year==2015)%>%
  arrange(desc(population))
#para que entregue el resultado en la consola
wbData3 %>%filter(year==2015)%>%
  select(country, population) %>%
  arrange(desc(population)) %>% head(5) #desc es para mostrar de mayor a menor
                                        # si no lo pongo es de menor a mayor.

#Agrupar por categoria y generar estadística descriptiva
wbData3 %>% filter(year==2015)%>% #filtra por año, pueden ser varios con "&"
  group_by(region)%>%
  summarize(gdppc_min=min(gdppc,na.rm = TRUE),gdppc_max=max(gdppc,na.rm = TRUE))
#El comando summarize sirve para generar estadistica descriptiva, se le da el 
# nombre del estadistico (puede ser cualquiera) y se indica cual es de cual
# variable.

#Crear variables a partir de otras de la base.
base <-base%>%mutate(Var_nueva=Var1+var)

########GRAFICOS##########

#Paquetes útiles para graficos:
library(plotly)
library(ggplot2)
library(gridExtra)
library(ggstance)
library(ggthemes)
library(ggrepel)

#Graficos mas usados:

ggplot(var1)+geom_line( )          #grafico de lineas
ggplot(var1)+geom_boxplot()        #grafico de caja
ggplot(var1)+geom_histogram()      #histograma
ggplot(var1)+stat_ecdf()           #empirical cumulative distributive function
ggplot(var1)+geom_point()          #grafico de puntos 

#Antes del comando del grafico, como dentro tambien de la función ggplot se 
# pueden aplicar filtros y elegir cuales serán los ejes.

base <- base%>%filter(var%in%c("cat1","cat2","cat3"))%>% #aplico el filtro en
  ggplot(aes(year,lifeExpectancy,col=country))+          #una variable
  geom_line() 
#aes es para indicar cuales son las variables que se graficarán, se puede poner
# eje x e y, categorias en el caso de lineas como las que se filtraron.

#Ajustes estéticos:

scale_x_continuous(name="Año",breaks = seq(1960,2016,10)) #puntos del eje x
scale_x_continuous(labels=function(x) x/1000,             #Definir los puntos
                   breaks = c(0,25000,50000,10000,125000))#en base a la funcion
ylab("Expectativa de vida al nacer 1960-2016")     #Nombre eje y
xlab("variable ")   "Nombre eje x"
#nombres y color de las lineas del grafico
scale_color_discrete(name="",labels=c("Chile","Jap?n","Estados Unidos"))
scale_linetype_manual(name="",values = c("dotted","twodash","solid") #manualmente
theme_economist_white() # Tema del grafico
ggtitle("Expectativa de Vida al nacer 1960-2016") #titulo del grafico
coord_flip() #cambiar los ejes de posicion 


#Ejemplo de grafico de disperción:
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
























