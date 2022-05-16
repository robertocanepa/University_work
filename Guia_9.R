# guia 9


# Pregunta 1

div2 = function(x){
  if (x %%2 ==0){
    print(paste(x, "es divisible por 2"))
  }
  else {
    print(paste(x,"no es divisible por 2"))
  }
}

div2(44)


#Pregunta 2

suma2 <- function(n){
  x <- 1:n
  x2 <- x^2
  s <- sum(x2)
  return(s)
}

suma2(45)


#Pregunta 3 

doble <-function(x){
  t <- log(2)/log(1+x)
  return(t)
}

doble(0.01)
doble(12)

#Pregunta 4 
avg <- function(x, arithmetic = TRUE){
  n= length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}

v = c(4,5,6,1,2)

avg(v, arithmetic = FALSE)

avg(v,arithmetic = TRUE)

avg(v)


#Pregunta 5

des_stat <- function(x){
  x = x[!is.na(x)]
  media = sum(x)/length(x)
  sdesv = sqrt(sum((x-media)^2)/(length(x)- 1))
 return(list(MEDIA = media, DESVIACION_ESTANDAR = sdesv))
}

v = c(1,10,6,4,3,NA)

des_stat(v)

sd(v, na.rm = TRUE)

# Pregunta 6

cobb <- function(alpha, beta, u){
  x <- 1:30
  y <- (u/(x)^alpha)^(1/beta)
  plot (x,y,type = "l")
}

cobb (0.4, 0.8, 10)




