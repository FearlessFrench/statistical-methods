size=rep(rep(c("Small","Median","Large"), each=6))
style=rep(c("1.5fin","1Story","2.5Fin","2Story","SFoyer","SLvl"),3)
price=c( 122113.310924, 155444.838710, 111250.000000, 139868.935961, 130063.612903, 129661.538462, 127633.404255, 154586.404959, 179500.000000, 196876.092527, 148430.357143, 159739.000000, 164904.673267, 211214.744000, 271600.000000, 249323.478149, 155008.708333, 177153.153846)
dataB=data.frame(size, style, price)
dataB

########## ANOVA Table #################
ANOVA2 <- aov(price ~ size * style, data=dataB)
ANOVA2 <- aov(price ~ size + style, data=dataB)
summary(ANOVA2)

model2 = lm(price ~ size + style, data=dataB)
summary(model2)

########## Multiple Comparison LSD Test #################
install.packages("agricolae")
library(agricolae)
LSD_Test2 <- LSD.test(ANOVA2,"style")
LSD_Test2

plot(LSD_Test2)