
#################### One-Way ANOVA ###########################

########## Data Set ##########################################
# define 4 treatment, ให้แต่ละ treatment มี 5 ตัว
Gas=rep(rep(c("A","B","C","D"), each=5))
Distance=c(25, 23, 20, 27, 20, 28, 31, 27, 28, 26, 32, 33, 30, 28, 32, 24, 24, 23, 27, 22)
dataA=data.frame(Gas, Distance)
dataA

########## ANOVA Table #################
ANOVA <- aov(Distance ~ Gas, data=dataA)
summary(ANOVA) # ขาดช่อง Sum Sq total (n-1)
# สังเกตได้ว่า p-value = 0.000128 ซึ่ง p-value < 0.05 => ปฎิเสธ H0

model = lm(Distance ~ Gas, data=dataA)
summary(model)

########## Multiple Comparison LSD Test #################
install.packages("agricolae")
library(agricolae)
LSD_Test<- LSD.test(ANOVA,"Gas")
LSD_Test

plot(LSD_Test)

# LCL กับ UCL คือ ช่วงความเชื่อมั่นแบบประมาณ
print(LSD_Test$statistics)
print(LSD_Test$groups)

par(mfrow=c(1,2))
plot(model, which=1)
plot(model, which=2)

####### Create boxplots #######################
boxplot(Distance ~ Gas,
        data = dataA,
        main = "Type of Gas",
        xlab = "Gas",
        ylab = "Distance",
        col = "orange",
        border = "black")

boxplot(Distance ~ Gas, data=dataA, main="Type of Gas")

############### Test Assumption ###############
graphics.off()
par("mar")
par(mar=c(1,1,1,1))

#####(1) fitted vs residuals #####
plot(ANOVA, which=1)
#####(2) Normal Q-Q plot #####
plot(ANOVA, which=2)

######(3) Histogram #####
library(rcompanion)
x = residuals(model)
plotNormalHistogram(x)

#plot(fitted(model),residuals(model))

########(4) normality of distribution ############
shapiro.test(x)


########(5) equal variance ############
leveneTest(Distance ~ Gas, data=dataA)

# Install the car package
library(car)
#################################################################################


#################### Two-Way ANOVA ###########################

########## Data Set ##########################################
Food=rep(rep(c("A","B","C","D"), each=5))
Block=rep(c("I","II","III","IV","V"),4)  # จะตั้งชื่อ Block เป็น Pig ก็ได้
Weight=c( 29, 29, 28, 28, 26, 31, 35, 32, 36, 34, 32, 34, 35, 36, 34, 33, 36, 37, 34, 33)
dataB=data.frame(Food, Block, Weight)
dataB

########## ANOVA Table #################
ANOVA2 <- aov(Weight ~ Food + factor(Block), data=dataB)
summary(ANOVA2)
# p-value ของ Food มีผลเพราะ < 0.05
# p-value ของ Block ไม่มีผลเพราะ > 0.05
### ตอนสอบ Final อันหนึ่ง reject (หา LSD ต่อ) และอีกข้อหนึ่ง accept

model2 = lm(Weight ~ Food + factor(Block), data=dataB)
summary(model2)

########## Multiple Comparison LSD Test #################
install.packages("agricolae")
library(agricolae)
LSD_Test2 <- LSD.test(ANOVA2,"Food")
LSD_Test2

plot(LSD_Test2)
# เส้น คือ ช่วงความเชื่อมั่น
# จุด คือ ค่าเฉลี่ย

####### Create boxplots #######################
boxplot(Weight ~ Food,
        data = dataB,
        main = "Type of Food",
        xlab = "Food",
        ylab = "Weight",
        col = "orange",
        border = "black")

boxplot(Weight ~ Food, data=dataB, main="Type of Food")

############### Test Assumption ###############

#####(1) fitted vs residuals #####

# ดูว่าค่า y กับ ค่า Error แตกต่างกันมากน้อยขนาดไหน
# ค่า Residual ที่ดีควรจะกระจายสม่ำเสมอรอบๆหรือใกล้หรือเท่ากับ 0
plot(ANOVA2, which=1)

#####(2) Normal Q-Q plot #####

# plot ค่าจริงกับ ค่าตัว Standardized ของ Error
plot(ANOVA2, which=2)

######(3) Histogram #####
library(rcompanion)
x = residuals(model2)
plotNormalHistogram(x)

#plot(fitted(model),residuals(model))

########(4) normality of distribution ############

# test normal ของ Error
shapiro.test(x)
# H0 : ค่าความคลาดเคลื่อนสุ่มมีการแจกแจงปกติ
# H1 : ค่าความคลาดเคลื่อนสุ่มมีการแจกแจงไม่ปกติ หรือแบบอื่น

########(5) equal variance ############
leveneTest(Weight ~ Food, data=dataB)











