library(ggplot2)

data <- read.table('ozone.txt', header=TRUE, sep=';', dec = ',')

ggplot(data, aes(x=T12, y=maxO3))+
         geom_point()+
         xlab('Temperature à midi (12h)')+
         ylab('maxO3')


# regression lineaire simple

reg_lin = lm(maxO3~T12, data = data)
# print(reg_lin)

summary(reg_lin)

ggplot(data, aes(x=T12, y=maxO3))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  xlab('Temperature à midi (12h)')+
  ylab('maxO3')
  
# valeurs ajustées en fonction des valeurs observées 
data$maxO3_ajust_s <- reg_lin$fit

ggplot(data, aes(x=maxO3,y=maxO3_ajust_s))+
  geom_point()+
  geom_abline(intercept=0,slope=1, color = 'red')+ # (Y=X --> Modele parfait)
  xlab("MaxO3")+
  ylab("MaxO3 ajusté")

# Représentez les résidus du modèle
data$residu_s = reg_lin$residuals

ggplot(data, aes(x=residu_s))+
  geom_histogram(bins = 10)+
  ggtitle('Histogramme des résidus')+
  xlab("Résidus")

# Predictions
a_prevoir <- data.frame(T12=19)
print(a_prevoir)
maxO3_prev <- predict(reg_lin,a_prevoir)
cat("Prediction pour une T12 de 19 :",round(maxO3_prev, digits=2))

