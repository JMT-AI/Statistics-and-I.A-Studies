library(ggplot2)

ble = read.table('ble.txt', sep = ';', dec = ',', header = TRUE)


# Observation intuitive
# phyto
ggplot(ble, aes(x=ble$phyto, y=ble$rdt))+
  geom_boxplot()
# variete
ggplot(ble, aes(x=ble$variete, y=ble$rdt))+
  geom_boxplot()

# ------------------------------------------------------------------
# Confirmation ou infirmation des intuitions via ANOVA
model1 = lm(rdt~phyto, data=ble)
summary(model1) # on regarde ici la statistique de Fisher (F-statistic)
# ou avec 
anova(model1) 
# p-valeur ~ 0.8 > 5% ==> On ne regette pas H0 : "pesticide pas d'effet
# - au moins significatif - sur le rendement"
# ------------------------------------------------------------------

# Confirmation ou infirmation via ANOVA pour la variete
model2 = lm(rdt~variete, data=ble)
summary(model2) 
anova(model2)
# p-value: 7.674e-10 << 5% ==> On rejette HO donc la variete a un effet 
# significatif sur le rendement
# ------------------------------------------------------------------

# ------------------------------------------------------------------
# ANOVA A 2 FACTEURS
# ------------------------------------------------------------------
model3 = lm(rdt~variete*phyto, data = ble)
summary(model3)
anova(model3)
