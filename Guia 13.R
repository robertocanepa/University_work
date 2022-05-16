
####Guia 13####

library(ggplot2)
library(gridExtra)

set.seed(123)
reps = 10000
betas = matrix(NA, nrow = reps, ncol= 8)
beta0=1
beta1=0.5
su = 1
n = c(50,100,500,1000)
for (j in 1:length(n)){
  X=rnorm(n[j],4,1)
  
  for(i in 1:reps){
    u=rnorm(n[j],0,su)
    Y=beta0+beta1*X+u
    model = lm(Y~X)
    betas[i,j]=model$coef[1]
    betas[i,j+4]=model$coef[2]
  }
}

betas_df <- data.frame(betas)
apply(betas_df,2,mean )
apply(betas_df,2,var)

g11 <- ggplot(betas_df)+
  geom_histogram(aes(betas_df[,5],y = ..density..),col="black",bins = )

ggplot(betas_df)+
  geom_histogram(aes(betas_df[,8],y = ..density..),col="black",bins = )



###pregunta 2

n =1000
X = rnorm(n,4,1)

for(i in 1:reps){
  u0 = rnorm(n,0,1)
  varu1= X^2
  u1= rnorm(n,0,srqt(varu1))
  
  Y0 = beta0 +beta1*X + u0
  Y1 = beta0 +beta1*X + u1
  
  model0 = lm(Y0~X)
  betas[i,1] =model0$coef[1]
  betas[i,2] =model0$coef[2]
  
  model1 = lm(Y1~X)
  betas[i,3] = model1$coef[1]
  betas[i,4] = model1$coef[2]
  
  
}


####pregunta 3

set.seed(123)
reps = 10000
betas = matrix(NA,nrow = reps, ncol = 4)

beta0=1
beta1=0.5





###Pregunta 4

library(ggplot2)
library(dplyr)
library(gridExtra)

set.seed(123)
reps=10000
betas = matrix(NA,nrow = reps, ncol =4)

beta0= 2
beta1 = 2.5
beta2 = 1


su = 1
n = c(100,1000)
for (j in 1:length(n)){
  
  x1=rnorm(n[j],20,1)
  x2=(2*x1)+rnorm(n[j],3,1)
  
  for (i in 1:reps){
    
    u=rnorm(n[j],0,su)
    
    Y0 = beta0 +beta1*x1 +u
    Y1 = beta0 +beta1*x1 +beta2*x2+u
    
    model0 = lm(Y0~x1)
    betas[i,j]= model0$coef[2]
    
    model1 = lm(Y1~x1 + x2)
    betas[i,j+2] = model1$coef[2]
    }
}


betas_df <- data.frame(betas)


# https://econcoding.shinyapps.io/apps/
