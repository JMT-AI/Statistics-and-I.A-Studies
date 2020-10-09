library(ggplot2)

data = read.table('ozone.txt', sep = ';', header = TRUE, dec = ',')

ggplot(data, aes(x=data$T12, y=data$maxO3))+
  geom_point()

# multiple linear model
regressor = lm(maxO3~T9+T12+T15+Ne9+Ne12+maxO3v,data=data)

summary(regressor)
