#pregunta 1

for(i in 1:10){
  print(i)
}


#otra forma
i=1
while(i<=10){
  print(i)
  i=i+1
}

#Pregunta 2
x=LETTERS[1:5]
for i (i in x){
  #print(i)
  cat(i, "")
}

x=LETTERS[1:5]
i=1
While(i<=lenght(x)){
  cat(x[i], "")
  i=i+1 
}

#PRegunta 3


for (i in 1:5) {
  cat(i-1, "+",i,"=",2=i-1,"\n")
}


#Pregunta 5

for (n in 1:15) {
  if (n%%2==0%n%% 3==0){
    print(i^2)
  }
  else{
    cat(n,"No es divisible por 6","\n")
  }
}


#otra forma

for(n in 1:15){
  if(n%%6 ==0 ){
    cat(n,"es divisible por 6","\n")
  }
  else{
    cat(n,"No es divisible por 6,"\n"")
  }
}


#Pregunta 6


library(dplyr)
library(tidyverse)
df <-tibble(
  x=1:5,
  y=c(2,0,1,4,NA)
)


#otra forma

for (i in seq_along(df)) {
  x_bar= mean(df[i]), na.rm = TRUE)
cat(x_bar,"el promedio de la columna", i, "\n")
}

apply(df,2,mean, na.rm=TRUE)

apply(df, 2,sd,na.rm=TRUE)





