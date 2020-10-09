# setting working directory
# getwd()
 setwd("/Users/jean-martial/Desktop/UTT/MS_BDAAD_FORMATION/RStudio")

essence = read.table("essence.txt",header=TRUE)
seuil = 31 # 31L/100km

moyenne = mean(essence$conso)
cat("Moyenne empirique :",round(moyenne, digits = 2))

ecart_type = sd(essence$conso)
cat("Ecart-type empirique :",round(ecart_type, digits = 2))

variable_no_biais_1 = var(essence$conso)
variance_no_biais_2 = ecart_type**2
cat("Variance no biais 1 :",round(variable_no_biais_1, digits = 2))
cat("Variance no biais 2 :",round(variance_no_biais_2, digits = 2))

nb_echant = dim(essence)[1] # 1 : indice de la 1ere colonne : nb lignes
variance_biais = variable_no_biais_1*(nb_echant-1)/nb_echant
cat("Variance biaisée :",round(variance_biais, digits = 2))

# Histogramme de l'echantillon
hist(essence$conso, prob=TRUE, main = "Histogramme et densité normale", ylim = c(0, 0.25))

# Ajout de la moyenne empiriquesur l'histo
abline(v=moyenne, col="blue",lwd=3)
abline(v=seuil, col="red",lwd=3)
legend("topright", bty = 'n',text.width = 3, legend=c("Moyenne empirique", "Seuil testé"), col=c("blue", "red"), lwd=3)
curve(dnorm(x,mean=moyenne,sd=ecart_type),col="black",lwd=2,add=TRUE)

# -----------------------------------------------
## Intervalle de confiance de la moyenne theorique
# -----------------------------------------------
alpha <- 0.05

# icinf <- moyenne-qt(p=1-alpha/2,df=n_essence-1)*ecart_type/sqrt(nb_echant)
# round(icinf,digits=2)
# 
# icsup <- moyenne+qt(p=1-alpha/2,df=n_essence-1)*ecart_type/sqrt(nb_echant)
# round(icsup,digits=2)

t.test(essence, conf.level = 1-alpha)

# -------------------------------------------------
## Intervalle de confiance de la variance theorique
# -------------------------------------------------

binf_var_estim = (nb_echant-1)*(ecart_type**2)/qchisq(p=1-alpha/2, df=nb_echant-1)
bsup_var_estim = (nb_echant-1)*(ecart_type**2)/qchisq(p=alpha/2, df=nb_echant-1)

# on retrouve l'intervalle avec la formule 

# -----> installation et importation de EnvStats 
# install.packages('EnvStats')
# library(EnvStats)

varTest(essence$conso,conf.level=1-alpha)

# ------------------------------
# Test d'Hypothese Moyenne et Variance : 
# Moyenne p : H0: p=p0=31 / H1: p<>p0 alternative avec p0 = 31 --> loi de student
# Variance v : H0 : v=v0=4.5 / H1: v>v0
# ------------------------------

t.test(essence$conso, mu = 31, alternative = 'two.sided')
varTest(essence$conso, sigma.squared = 4.5, alternative = 'greater')

