

##Guia 12

#Pregunta 1
rm(list = ls())

library(dplyr)
library(ggplot2)
library(stargazer)

ads = read.table("C:/Users/rcane/Desktop/Electivo R/Advertising.csv",sep = ",", header = TRUE)
ads <-ads%>%select(~X)

regTV = lm(Sales~TV ,data = ads) # estima OLS
summary(regTV)


regTV$coefficients
regTV$coefficients[[1]]
regTV$coefficients[[2]]


## intervalo de confianza

confint(regTV)

confint(regTV, level=0.90)

#### R^2 y el error estandar

ads$Sales_hat = fitted(regTV)
ads$u_hat  = ads$Sales-ads$Sales_hat

SCR = sum(ads$u_hat^2)#suma de cuadrados de residuos
SCT = sum((ads$Sales-mean(ads$Sales))^2)

R2= 1-(SCR/SCT)




#Grafico de ventas


ads%>% ggplot(aes(x=TV, y=Sales))+geom_smooth(method = "lm",se=FALSE, color="black")+
  geom_point(color="blue")+theme_bw()

##tabla

model1 = lm(Sales~TV, data=ads)
model2 = lm(Sales~Radio, data=ads)
model3 = lm(Sales~TV+Radio, data=ads)
model4 = lm(Sales~TV+Radio+Newspaper, data=ads)


stargazer(list(model1,model2,model3,model4),
          type = "text",keep.stat = c("n","rsq", "adj.rsq"),
          font.size = "small", align = TRUE, no.space = TRUE)

#g
u_hat_mat = y -x%>%bhat
sigsqhat = as.numeric(t(u_hat_mat) %>% u_hat_mat/ (200-3-1))

varcov_beta_mat = sigsqhat * solve(t(x)%>%x)

round(varcov_beta_mat,4)
round(vcov(model4),4)


#H

library(car)
varRadioTV = varcov_beta_mat[3,3]+(9*varcov_beta_mat[2,2])-(6*varcov_beta_mat[3,2])
((bhat[3,1]-(3*bhat[2,1]))/sqrt(varRadioTV))^2

myH0 = c("Radio=3*TV")
linearHypothesis(model4,myH0)



####PREGUNTA 2

library(foreign)
library(gridExtra)
anscombe <-read.dta("C:/Users/rcane/Desktop/Electivo R/anscombe.dta")
 
m1 <- lm(y1~ x1, data = anscombe)
m2 <- lm(y2~x2, data = anscombe)
m3 <- lm(y3~x3, data = anscombe)
m4 <- lm(y4~x4, data = anscombe)

stargazer(list(m1,m2,m3,m4),
          type="text", keep.stat = c("n","rsq","adj.rsq"),
          font.size = "small",align = TRUE,no.space = TRUE)

#b grafique la relacion

g1 <-anscombe%>% ggplot(aes(x=x1, y=y1))+
  geom_smooth(method = "lm", se =FALSE , color = "black", formula = y ~x)+
  geom_point(color="red")+xlim(0,NA)+theme_bw()

g2 <- anscombe%>% ggplot(aes(x=x2, y=y2))+
  geom_smooth(method = "lm", se =FALSE , color = "black", formula = y ~x)+
  geom_point(color="red")+xlim(0,NA)+theme_bw()

g3 <- anscombe%>% ggplot(aes(x=x3, y=y3))+
  geom_smooth(method = "lm", se =FALSE , color = "black", formula = y ~x)+
  geom_point(color="red")+xlim(0,NA)+theme_bw()

g4 <- anscombe%>% ggplot(aes(x=x4, y=y4))+
  geom_smooth(method = "lm", se =FALSE , color = "black", formula = y ~x)+
  geom_point(color="red")+xlim(0,NA)+theme_bw()

grid.arrange(g1,g2,g3,g4)



## regresion por cuantiles


library(quantreg)
data(engel)

engel%>% ggplot(aes(x=income,y=foodexp))+
  geom_smooth(method = "lm",se = FALSE,color="black", formula = y~x)+
  geom_point(color="red")+
  geom_quantile(quantiles=0.5, linetype= "dashed", size= 1, colour ="black")+
  xlab("Income")+ylab("Food Expenditure")+theme_bw()

  

