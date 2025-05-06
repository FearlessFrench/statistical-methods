
###########################Question1#############################

heart.rate=c(87, 89, 95, 90, 91, 92, 93, 95, 90, 89, 88, 87, 92, 93, 89)
mean(heart.rate)
var(heart.rate)
sqrt(var(heart.rate))
t.test(heart.rate, conf.level=0.9)

###########################Question2#############################

A = c(89, 90, 95, 85, 95, 97, 93, 96, 98, 99)
mean(A)
sqrt(var(A)) # Standard Deviation (S)
var(A)                # Variance (S^2)
B = c(93, 90, 85, 80, 86, 92, 91, 91, 90, 82, 81, 99)
mean(B)
sqrt(var(B)) # Standard Deviation (S)
var(B)                # Variance (S^2)
t.test(A, B, conf.level=0.95)	

###########################Question3#############################

radial=c(5.2,4.7,6.6,7.0,6.7,5.5,5.7,6.0,7.4,5.9)
normal=c(4.1,4.9,5.2,6.8,4.4,5.7,5.8,6.9,6.0,4.7)
t.test(radial, normal, conf.level = 0.9, alternative = "greater")

###########################Question4#############################

car=c(12,9,11,7,7,7,8,9,10,12,13,12,7,6,8,9,10,11,7,6,8,9,8,9,11,8,7,7,6,7,8,10,14,12,11,9)
t.test(car, mu=10, conf.level = 1-0.05, alternative = "two.sided")

###########################Question5#############################

xbar = 0.22            # sample mean 
mu0 = 0.25             # hypothesized value 
sigma = 0.07            # population standard deviation 
n = 60                 # sample size 
z = (xbar-mu0)/(sigma/sqrt(n)) 
z                      # test statistic 

alpha = .01 
z.alpha = qnorm(1-alpha) 
-z.alpha               # critical value 

pval = pnorm(z) 
pval                   # lower tail p−value 

#########################Question6###########################

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

##############################################################



















