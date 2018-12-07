#pregunta 2

library(tidyverse)
library(tidyquant)
library(ggplot2)
library(RJSONIO)

#2.1
Microsof_precio <- tq_get("MSFT",
                     get = "stock.prices",
                     from = "2000-01-01",
                     to = "2018-08-31",
                     periodicity = "monthly")

Apple_precio <- tq_get("AAPL",
                      get = "stock.prices",
                      from = "2000-01-01",
                      to = "2018-08-31",
                      periodicity = "monthly")

#2.A


tickers <- c("MSFT", "AAPL")

data_activos <- tq_get(tickers,
                       get = "stock.prices",
                       from = "2000-01-01",
                       to = "2018-10-31",
                       periodicity = "monthly")

#calculo de retornos

retornos_activos <- data_activos %>%
  group_by(symbol) %>%
  tq_transmute(select = close,
               mutate_fun = periodReturn,
               period = "daily",
               type = "log",
               col_rename = "retornos.mensuales")

#retornos acumulados

retornos_acum <- retornos_activos %>%
  group_by(symbol) %>%
  mutate(ret.cum = cumsum(retornos.mensuales))

retornos_activos %>%
  ggplot(mapping = aes(x = retornos.mensuales, fill = symbol))+
  geom_density(alpha = 0.5) +
  labs(title = "Retornos Activos",
       subtitle = "MICROSOFT (MSFT), APPLE (AAPL)",
       x = "Retornos mensuales", y = "Densidad") +
  theme_tq() +
  scale_fill_tq() +
  facet_wrap(~ symbol, ncol = 2) +
  guides(fill=guide_legend(title="Activos:"))

retornos_acum %>%
  ggplot(mapping = aes(x = ret.cum, fill = symbol))+
  geom_density(alpha = 0.5) +
  labs(title = "Retornos Acumulados Activos",
       subtitle = "MICROSOFT (MSFT), APPLE (AAPL)",
       x = "Retornos mensuales", y = "Densidad") +
  theme_tq() +
  scale_fill_tq() +
  facet_wrap(~ symbol, ncol = 2) +
  guides(fill=guide_legend(title="Activos:"))

#Jarque Bera

n = 452
promedio_acciones <- retornos_activos %>% filter(retornos.mensuales)



#Pregunta 3

library(readr)
library(dplyr)
library(ggplot2)

set.seed(123)

reps = 10000

betas = matrix(NA, nrow = reps, ncol = 12)

beta0 = 2

beta1 = 2.5

beta2 = 1

su = 1

n = c(50, 100, 500, 1000)  #tamagno muestral

for (j in 1:length(n)) {
  
  X1=rnorm(n[j],20,1)
  
  e=rnorm(n[j],3,1)
  
  X2=0.8*X1 + e
  
  
  for (i in 1:reps) {
    
    u= rnorm(n[j],0,su)
    
    Y = beta0 + beta1*X1 + beta2*X2 + u 
    
    model = lm(Y~X1 + X2)  
    
    betas[i,j] = model$coef[1]
    
    betas[i,j+4] = model$coef[2]
    
    betas[i,j+8] = model$coef[3]
  }
  
}

betas_df <- data.frame(betas)

apply(betas_df,2,mean)

apply(betas_df,2,var)

summary(model)

# EXPLICACION, si existe sesgo en los betas cuando se comparan con el modelo, 
#el sesgo disminuye cuando la muestra llega a n=500, (como se puede ver en el grafico, el promedio se acerca mucho a la muestra, linea roja), 
#pero vuelve a aumentar cuando la muestra alcanza n=1000

#3.b

library(ggplot2)

library(gridExtra)

library(dplyr)

g10 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,1], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,1]), sd=sd(betas_df[,1])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=50") + xlab(expression(hat(beta)[0])) +
  
  theme_bw()

g20 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,2], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,2]), sd=sd(betas_df[,2])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=100") + xlab(expression(hat(beta)[0])) +
  
  theme_bw()

g30 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,3], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,3]), sd=sd(betas_df[,3])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=500") + xlab(expression(hat(beta)[0])) +
  
  theme_bw()

g40 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,4], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,4]), sd=sd(betas_df[,4])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=1000") + xlab(expression(hat(beta)[0])) +
  
  theme_bw()

grid.arrange(g10, g20, g30, g40, nrow=2, ncol=2)

g11 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,5], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,5]), sd=sd(betas_df[,5])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=50") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

g21 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,6], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,6]), sd=sd(betas_df[,6])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=100") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

g31 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,7], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,7]), sd=sd(betas_df[,7])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=500") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

g41 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,8], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,8]), sd=sd(betas_df[,8])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=1000") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

grid.arrange(g11, g21, g31, g41, nrow=2, ncol=2)

#3.c

set.seed(123)

reps = 10000

betas = matrix(NA, nrow = reps, ncol = 12)

beta0 = 2

beta1 = 2.5

beta2 = 1

su = 1

n = c(50, 100, 500, 1000)  #tamagno muestral

for (j in 1:length(n)) {
  
  X1=rnorm(n[j],20,1)
  
  e=rnorm(n[j],3,1)
  
  X2=runif(n[j],0,1)
  
  
  for (i in 1:reps) {
    
    u= rnorm(n[j],0,su)
    
    Y = beta0 + beta1*X1 + beta2*X2 + u 
    
    model = lm(Y~X1 + X2)  
    
    betas[i,j] = model$coef[1]
    
    betas[i,j+4] = model$coef[2]
    
    betas[i,j+8] = model$coef[3]
  }
  
}

betas_df <- data.frame(betas)

apply(betas_df,2,mean)

apply(betas_df,2,var)

summary(model)

#En este caso, podemos presenciar que se mantiene el mismo patrón respecto a que cuando aumenta el tamaño de la muestra,
#pero aqui el sesgo es el menor posible hasta el tamaño 100, luego va aumentando el sesgo, esto se presencia mucho mejor en los gráficos,
#ya que en el panel de n=100 el estimador se acerca mucho al parametro poblacional

g10 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,1], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,1]), sd=sd(betas_df[,1])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=50") + xlab(expression(hat(beta)[0])) +
  
  theme_bw()

g20 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,2], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,2]), sd=sd(betas_df[,2])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=100") + xlab(expression(hat(beta)[0])) +
  
  theme_bw()

g30 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,3], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,3]), sd=sd(betas_df[,3])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=500") + xlab(expression(hat(beta)[0])) +
  
  theme_bw()

g40 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,4], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,4]), sd=sd(betas_df[,4])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=1000") + xlab(expression(hat(beta)[0])) +
  
  theme_bw()

grid.arrange(g10, g20, g30, g40, nrow=2, ncol=2)

g11 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,5], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,5]), sd=sd(betas_df[,5])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=50") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

g21 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,6], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,6]), sd=sd(betas_df[,6])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=100") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

g31 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,7], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,7]), sd=sd(betas_df[,7])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=500") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

g41 <- ggplot(betas_df) + 
  
  geom_histogram(aes(betas_df[,8], y=..density..), col="black", bins = 30) +
  
  stat_function(fun=dnorm, args=list(mean=mean(betas_df[,8]), sd=sd(betas_df[,8])), 
                
                geom="line", colour="red", size=1) +
  
  ylab("Densidad") +   ggtitle("n=1000") + xlab(expression(hat(beta)[1])) +
  
  theme_bw()

grid.arrange(g11, g21, g31, g41, nrow=2, ncol=2)