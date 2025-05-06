
########################### Question 1 ##################################

score <-c(74,68,73,62,78,65,98,75,83,69,76,64,75,70,91,86,78,58,53,65,63,63,59,68,78,81,79,85,88,90)

A <-c(74,68,73,62,78,65,98,75,83,69,76,64,75,70,91)
summary(A)
B <-c(63,63,59,68,78,81,79,85,88,90,86,78,58,53,65)
summary(B)

summary(score)

boxplot(A, horizontal = TRUE, main="The score of group A", xlab="score")
boxplot(B, horizontal = TRUE, main="The score of group B", xlab="score")

hist(score)

sqrt(var(A))
sqrt(var(B))
########################### Question 2 ##################################

#Exponential distribution 
prob <- rexp(500,0.5)            # The mean of the exponential distribution is 1/lambda
X <- matrix(ncol = 4, nrow = 10000)  # 10,000 repetitions

prob <- rexp(10000,0.1) 
X <- matrix(ncol = 4, nrow = 10000)

samp_sizes <- c(5, 10, 36, 50)

for (j in 1:ncol(X)) {
  for (i in 1:nrow(X)) {
    X[i, j] <- mean(sample(prob, size = samp_sizes[j]))
  }
}

par(mfrow = c(2, 2))

for (j in 1:ncol(X)) {
  hist(X[ ,j], 
       breaks = 20, 
       xlim = c(0, 20), 
       col = "orange", 
       xlab = "", 
       main = paste("Sample Size =", samp_sizes[j]))
}

########################### Question 3 ##################################

accident=c(87, 89, 95, 90, 91, 92, 93, 95, 90, 89, 88, 87, 92, 93, 89)
mean(accident)
var(accident)
sqrt(var(accident))
t.test(accident, conf.level=0.95)

accident=c(10,9,10,11,16,15,8,6,18,17,4,12,15,14,15,9,7,8,16,14)
mean(accident)
var(accident)
sqrt(var(accident))
t.test(accident, conf.level=0.95)

########################### Question 4 ##################################

xbar = 14.6            # sample mean 
mu0 = 15.4             # hypothesized value 
s = 2.5                # sample standard deviation 
n = 35                 # sample size 
t = (xbar-mu0)/(s/sqrt(n)) 
t                      # test statistic

alpha = .05 
t.half.alpha = qt(1-alpha/2, df=n-1) 
c(-t.half.alpha, t.half.alpha) 

pval = 2*pt(t, df=n-1)  # lower tail 
pval                    # two−tailed p−value 

######

xbar = 3.09            # sample mean 
mu0 = 0             # hypothesized value 
s = 2.8513                # sample standard deviation 
n = 24           # sample size 
t = (xbar-mu0)/(s/sqrt(n)) 
t                      # test statistic

alpha = .05 
t.half.alpha = qt(1-alpha/2, df=22) 
c(-t.half.alpha, t.half.alpha) 

pval = 2*pt(t, df=n-1)  # lower tail 
pval                    # two−tailed p−value 


########################### Question 5 ##################################

########## Data Set ##########################################
Gas=rep(rep(c("A","B","C","D"), each=5))
Distance=c(25, 23, 20, 27, 20, 28, 31, 27, 28, 26, 32, 33, 30, 28, 32, 24, 24, 23, 27, 22)
dataA=data.frame(Gas, Distance)
dataA

Gas=rep(rep(c("A","B","C"), each=5))
Distance=c(70,66,79,68,70,64,71,81,72,71,87,92,91,90,83)
dataA=data.frame(Gas, Distance)
dataA
########## ANOVA Table #################
ANOVA <- aov(Distance ~ Gas, data=dataA)
summary(ANOVA)

model = lm(Distance ~ Gas, data=dataA)
summary(model)

########## Multiple Comparison LSD Test #################
install.packages("agricolae")
library(agricolae)
LSD_Test<- LSD.test(ANOVA,"Gas")
LSD_Test

plot(LSD_Test)

##print(LSD_Test$statistics)
##print(LSD_Test$groups)

#par(mfrow=c(1,2))
#plot(model, which=1)
#plot(model, which=2)

####### Create boxplots #######################
boxplot(Distance ~ Gas,
        data = dataA,
        main = "Type of Gas",
        xlab = "Gas",
        ylab = "Distance",
        col = "orange",
        border = "black")

boxplot(Distance ~ Gas, data=dataA, main="Type of Gas")

############################# Take Home 250 ########################################

Restaurants = rep(rep(c("X","Y","Z"), each=5))
Inspectors = rep(c("A","B","C","D","E"))
Cleanliness = c(71,65,70,72,76,55,57,65,69,64,84,86,77,70,85)
investigate = data.frame(Restaurants, Inspectors, Cleanliness)
investigate

########## ANOVA Table #################
ANOVA <- aov(Cleanliness ~ Restaurants * Inspectors, data=investigate)
ANOVA <- aov(Cleanliness ~ Restaurants + Inspectors, data=investigate)
summary(ANOVA)

