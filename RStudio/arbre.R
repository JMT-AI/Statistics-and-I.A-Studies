library(ggplot2)

arbre = read.csv('les-arbres.csv', header=TRUE, sep=';')

# conversions des circonferences en m
arbre$CIRCONFERENCE..m <- arbre$CIRCONFERENCE..cm.*10**(-2)

ggplot(arbre, aes(x=arbre$CIRCONFERENCE..m, y=arbre$HAUTEUR..m.))+
  geom_point()+
  ggtitle('Dispersion de la hauteur en fonction de la circonference')+
  xlab("Circonference de l'arbre")+
  ylab("Hauteur")

#
ggplot(arbre, aes(x=arbre$CIRCONFERENCE..m))+
  xlim(0,1)+
  geom_histogram(binwidth=30)

ggplot(arbre,aes(x=CIRCONFERENCE..m,y=HAUTEUR..m.))+ geom_point()+
  xlab("circ")+
  ylab("haut")
