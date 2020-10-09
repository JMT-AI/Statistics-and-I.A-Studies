# setting working directory
# getwd()
# setwd("/Users/jean-martial/Desktop/UTT/MS_BDAAD_FORMATION/RStudio")

guerison = read.table("guerison.txt",header=TRUE)

# proportion empirique
n_echantillon_g = dim(guerison)[1]
n_gueris = sum(guerison$guerison==1)
proportion_emp = n_gueris/n_echantillon_g # --> proportion estimée
#print(mean(guerison$guerison)) 

alpha = 0.1

# Intervalle de confiance asymptotique - Bornes inf et sup
icinf <- proportion_emp-qnorm(p=1-alpha/2)*sqrt(proportion_emp*(1-proportion_emp)/n_gueris)
round(icinf,digits=2)
icsup <- proportion_emp+qnorm(p=1-alpha/2)*sqrt(proportion_emp*(1-proportion_emp)/n_gueris)
round(icsup,digits=2)

cat("Intervalle de confiance asymptotique : [",round(icinf,digits=2),", ",round(icsup,digits=2),"]")

# --- ou 2e methode ---
prop.test(n_gueris, n_echantillon_g, conf.level = 1-alpha)

# Intervalle de confiance exacte - X loi binomiale (~ petits echantillons)
binom.test(n_gueris, n_echantillon_g, conf.level = 1-alpha)


# ------------------------------
# Test d'Hypothese Proportion : H0 = p0=31 / H1>p0 alternative avec p0 = 0.75 %
# ------------------------------

prop.test(n_gueris, n_echantillon_g, p = 0.75, alternative = 'greater')


