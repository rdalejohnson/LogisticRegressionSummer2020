library(ggplot2)

obesitySleep <- read.table("Obesity_Sleep.csv", sep=",", header=T)

##### Descriptive Stats for entire data set/sample
##### and by Obesity BMI categories

#AGE: in years
#RACE: 
#      0 = black not hispanic
#      1 = white not hispanic
#SLEEP QUALITY RATING:
#      1 = BAD
#      2 = OK
#      3 = GOOD
#SLEEP DURATION: average hours of sleep per day over the last month
#OBESITY:
#      0 = Not obese; BMI of 29.9 or less
#      1 = Obese; BMI >= 30.0

frequencyDistribution= function(dataSet, ascending=F)
{
  row.counts = table(dataSet)
  n = sum(row.counts)
  if (ascending) ord = order(-row.counts)
  else ord = 1:length(row.counts)
  data.frame(
    row.names = row.names(row.counts[ord]),
    Counts = as.vector(row.counts),
    Percents = 100*as.vector(row.counts[ord]/n),
    CumCount = cumsum(as.vector(row.counts[ord])),
    CumPercent = 100*cumsum(as.vector(row.counts[ord]))/n
  )
  
}


str(obesitySleep)

############################################ AGE ###############################
############################################ AGE ###############################
############################################ AGE ###############################
############################################ AGE ###############################


###### AGE ALONE ######## AGE ALONE ##############  AGE ALONE ##############

summary(obesitySleep[,c(2)])
sd(obesitySleep$Age)

#MODE OF AGE
tab <- table(obesitySleep$Age) # number of occurrences for each unique value
sort(tab, decreasing = TRUE) # sort highest to lowest

#Coefficient of Variation
sd(obesitySleep$Age, na.rm=TRUE) / mean(obesitySleep$Age, na.rm=TRUE)
#NORMALITY TESTS ON AGE
hist(obesitySleep$Age)
age.boxplot <- boxplot(obesitySleep$Age)
shapiro.test(obesitySleep$Age)
qqnorm(obesitySleep$Age)
qqline(obesitySleep$Age)

##### AGE BY OBESE CATEGORY #########
by(obesitySleep[,c(2)], obesitySleep$Obese, summary)
# NUMBER OF AGE VALUES BY OBESE CATEGORY ###
by(obesitySleep[,c(2)], obesitySleep$Obese, length)


#MODE OF AGE when Obese = NOT OBESE
tab <- table(obesitySleep$Age[obesitySleep$Obese=='Not Obese'])   # number of occurrences for each unique value
sort(tab, decreasing = TRUE) # sort highest to lowest

#MODE OF AGE when Obese = OBESE
tab <- table(obesitySleep$Age[obesitySleep$Obese=='Obese'])   # number of occurrences for each unique value
sort(tab, decreasing = TRUE) # sort highest to lowest


#SD/MEAN on AGE when Obese = NOT OBESE
with(obesitySleep, sd(Age[Obese=='Not Obese']))
with(obesitySleep, mean(Age[Obese=='Not Obese']))


#SD/MEAN on AGE when Obese =  OBESE
with(obesitySleep, sd(Age[Obese=='Obese']))
with(obesitySleep, mean(Age[Obese=='Obese']))


############################################ RACE ###############################
############################################ RACE ###############################
############################################ RACE ###############################
############################################ RACE ###############################

summary(obesitySleep[,c(4)])
prop.table(table(obesitySleep$Race))

by(obesitySleep[,c(4)], obesitySleep$Obese, summary)


raceTab <- table(obesitySleep$Race[obesitySleep$Obese=='Obese'])   
cbind(raceTab, prop.table(raceTab))

raceTab2 <- table(obesitySleep$Race[obesitySleep$Obese=='Not Obese']) 
cbind(raceTab2, prop.table(raceTab2))




############################################ SLEEP QUALITY ###############################
############################################ SLEEP QUALITY ###############################
############################################ SLEEP QUALITY ###############################
############################################ SLEEP QUALITY ###############################


summary(obesitySleep[,c(5)])
prop.table(table(obesitySleep$SLEEP.QUALITY.RATING))
by(obesitySleep[,c(5)], obesitySleep$SLEEP.QUALITY.RATING, summary)


sleepTab <- table(obesitySleep$SLEEP.QUALITY.RATING[obesitySleep$Obese=='Obese'])   
cbind(sleepTab, prop.table(sleepTab))

sleepTab2 <- table(obesitySleep$SLEEP.QUALITY.RATING[obesitySleep$Obese=='Not Obese']) 
cbind(sleepTab2, prop.table(sleepTab2))


############################################ SLEEP DURATION ###############################
############################################ SLEEP DURATION ###############################
############################################ SLEEP DURATION ###############################
############################################ SLEEP DURATION ###############################


summary(obesitySleep[,c(6)])
sd(obesitySleep$SLEEP.DURATION, na.rm=TRUE)

#MODE 
tab <- table(obesitySleep$SLEEP.DURATION) # number of occurrences for each unique value
sort(tab, decreasing = TRUE) # sort highest to lowest

#Coefficient of Variation
sd(obesitySleep$SLEEP.DURATION, na.rm=TRUE) / mean(obesitySleep$SLEEP.DURATION, na.rm=TRUE)
#NORMALITY TESTS
hist(obesitySleep$SLEEP.DURATION)
sleepduration.boxplot <- boxplot(obesitySleep$SLEEP.DURATION)
shapiro.test(obesitySleep$SLEEP.DURATION)
qqnorm(obesitySleep$SLEEP.DURATION)
qqline(obesitySleep$SLEEP.DURATION)




##### BY OBESE CATEGORY #########
by(obesitySleep[,c(6)], obesitySleep$Obese, summary)
# NUMBER OF  VALUES BY OBESE CATEGORY ###
by(obesitySleep[,c(6)], obesitySleep$Obese, length)


#MODE  when Obese = NOT OBESE
tab <- table(obesitySleep$SLEEP.DURATION[obesitySleep$Obese=='Not Obese'])   # number of occurrences for each unique value
sort(tab, decreasing = TRUE) # sort highest to lowest

#MODE  when Obese = OBESE
tab <- table(obesitySleep$SLEEP.DURATION[obesitySleep$Obese=='Obese'])   # number of occurrences for each unique value
sort(tab, decreasing = TRUE) # sort highest to lowest


#SD/MEAN  when Obese = NOT OBESE
with(obesitySleep, sd(SLEEP.DURATION[Obese=='Not Obese'],na.rm=TRUE))
with(obesitySleep, mean(SLEEP.DURATION[Obese=='Not Obese'] , na.rm=TRUE))


#SD/MEAN when Obese =  OBESE
with(obesitySleep, sd(SLEEP.DURATION[Obese=='Obese'], na.rm=TRUE))
with(obesitySleep, mean(SLEEP.DURATION[Obese=='Obese'], na.rm=TRUE))



###### BMI ALONE ######## BMI ALONE ##############  BMI ALONE ##############
###### BMI ALONE ######## BMI ALONE ##############  BMI ALONE ##############
###### BMI ALONE ######## BMI ALONE ##############  BMI ALONE ##############
###### BMI ALONE ######## BMI ALONE ##############  BMI ALONE ##############


summary(obesitySleep$BMI)
sd(obesitySleep$BMI)

#Coefficient of Variation
sd(obesitySleep$BMI, na.rm=TRUE) / mean(obesitySleep$BMI, na.rm=TRUE)
#NORMALITY TESTS ON BMI
hist(obesitySleep$BMI)
bmi.boxplot <- boxplot(obesitySleep$BMI)
shapiro.test(obesitySleep$BMI)
qqnorm(obesitySleep$BMI)
qqline(obesitySleep$BMI)

nrow(obesitySleep[obesitySleep$BMI>=30, ])
nrow(obesitySleep[obesitySleep$BMI<30, ])






############################################ OBESITY CATEGORY ###############################
############################################ OBESITY CATEGORY ###############################
############################################ OBESITY CATEGORY ###############################
############################################ OBESITY CATEGORY ###############################


summary(obesitySleep[,c(11)])
prop.table(table(obesitySleep$Obese))
by(obesitySleep[,c(11)], obesitySleep$Obese, summary)



###################### BIVARIATE ANALYSIS ################ ###################### BIVARIATE ANALYSIS ##############################
###################### BIVARIATE ANALYSIS ################ ###################### BIVARIATE ANALYSIS ##############################
###################### BIVARIATE ANALYSIS ################ ###################### BIVARIATE ANALYSIS ##############################
###################### BIVARIATE ANALYSIS ################ ###################### BIVARIATE ANALYSIS ##############################


##### Obesity Category by Age, Race, Sleep Quality, Sleep Duration

############# OBESITY BY AGE ############## OBESITY BY AGE ##############
Age=obesitySleep$Age
obesityCategory=obesitySleep$Obese
t.test(Age~obesityCategory)


############# OBESITY BY SLEEP DURATION ############## OBESITY BY SLEEP DURATION ##############
sleepDuration=obesitySleep$SLEEP.DURATION
obesityCategory=obesitySleep$Obese
t.test(sleepDuration~obesityCategory)



############ OBESITY BY RACE ########################### OBESITY BY RACE ###################

library(gmodels)
library(MASS)
with(obesitySleep, CrossTable(Obese, Race))

#chiRaceObesity <- 
chisq.test(table(obesitySleep$Obese, obesitySleep$Race))


############ OBESITY BY SLEEP QUALITY ########################### OBESITY BY SLEEP QUALITY ###################

with(obesitySleep, CrossTable(Obese, Bad))
chisq.test(table(obesitySleep$Obese, obesitySleep$Bad))

with(obesitySleep, CrossTable(Obese, Good))
chisq.test(table(obesitySleep$Obese, obesitySleep$Good))

with(obesitySleep, CrossTable(Obese, OK))
chisq.test(table(obesitySleep$Obese, obesitySleep$OK))

####### AGE BY RACE #########

Race=obesitySleep$Race
Age=obesitySleep$Age

t.test(Age~Race)


####### AGE BY SLEEP QUALITY ########

#sleepQualityBad=obesitySleep$Bad
#Age=obesitySleep$Age

anova.sleepqualbad.age <- aov(Age ~ Bad, data = obesitySleep)
summary(anova.sleepqualbad.age)
plot(anova.sleepqualbad.age, 1)

anova.sleepqualgood.age <- aov(Age ~ Good, data = obesitySleep)
summary(anova.sleepqualgood.age)
plot(anova.sleepqualgood.age, 1)

anova.sleepqualok.age <- aov(Age ~ OK, data = obesitySleep)
summary(anova.sleepqualok.age)
plot(anova.sleepqualok.age, 1)

library(car)
leveneTest(Age ~ Bad, data = obesitySleep)

leveneTest(Age ~ Good, data = obesitySleep)

leveneTest(Age ~ OK, data = obesitySleep)





#### AGE BY SLEEP DURATION ########


scatterplot(SLEEP.DURATION ~ Age, data = obesitySleep)
plot(x=obesitySleep$Age,y=obesitySleep$SLEEP.DURATION)
Hmisc::rcorr(x=obesitySleep$Age,y=obesitySleep$SLEEP.DURATION, type=c("spearman"))
Hmisc::rcorr(x=obesitySleep$SLEEP.DURATION,y=obesitySleep$Age, type=c("spearman"))

Hmisc::rcorr(x=obesitySleep$Age,y=obesitySleep$SLEEP.DURATION, type=c("pearson"))

#complete.obs means only complete rows, ignore NA values
cor(obesitySleep$Age, obesitySleep$SLEEP.DURATION, use = "complete.obs", method = "pearson")

cor.test(obesitySleep$Age, obesitySleep$SLEEP.DURATION)

library(rms)
library(ggpubr)


ggs = ggscatter(obesitySleep, x = "Age", y = "SLEEP.DURATION", 
                add = "reg.line", conf.int = TRUE, 
                cor.coef = TRUE, cor.method = "pearson",
                xlab = "Age", ylab = "Sleep Duration") 
ggs



### RACE BY SLEEP QUALITY #####
tableBad= table(obesitySleep$Race,obesitySleep$Bad)
tableBad
chisq.test(tableBad)

tablegood= table(obesitySleep$Race,obesitySleep$Good)
tablegood
chisq.test(tablegood)

tableOk= table(obesitySleep$Race,obesitySleep$OK)
tableOk
chisq.test(tableOk)




### RACE BY SLEEP DURATION ####

sleepDuration=obesitySleep$SLEEP.DURATION
raceCategory=obesitySleep$Race
t.test(sleepDuration~raceCategory)


### SLEEP QUALITY BY SLEEP DURATION ####

anova.sleepqualbad.dur <- aov(SLEEP.DURATION ~ Bad, data = obesitySleep)
summary(anova.sleepqualbad.dur)
plot(anova.sleepqualbad.dur, 1)

anova.sleepqualgood.dur <- aov(SLEEP.DURATION ~ Good, data = obesitySleep)
summary(anova.sleepqualgood.dur)
plot(anova.sleepqualgood.dur, 1)

anova.sleepqualok.dur <- aov(SLEEP.DURATION ~ OK, data = obesitySleep)
summary(anova.sleepqualok.dur)
plot(anova.sleepqualok.dur, 1)

library(car)
leveneTest(SLEEP.DURATION ~ Bad, data = obesitySleep)

leveneTest(SLEEP.DURATION ~ Good, data = obesitySleep)

leveneTest(SLEEP.DURATION ~ OK, data = obesitySleep)


#### Step 3:  Conduct Multiple Logistic Regression Analyses


obesitySleep$Race = factor(obesitySleep$Race, levels=c("White, Non-Hispanic","Black, Non-Hispanic"))
obesitySleep$Obese=factor(obesitySleep$Obese, levels = c("Obese","Not Obese"))

Races=relevel(obesitySleep$Race, "White, Non-Hispanic")
Obeses=relevel(obesitySleep$Obese,"Not Obese")
mylogit = glm(Obese ~ Race + Age + SLEEP.DURATION + Bad + OK , data = obesitySleep, family = "binomial")



summary(mylogit)


#comparing full to null model?
null.model <- glm(Obese~1, data = obesitySleep, family = "binomial")
summary(null.model)
anova(null.model, mylogit, test="Chisq");

# As a rule of thumb, a VIF value that exceeds 5  indicates a 
# problematic amount of collinearity. 
# http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/#multicollinearity-logistic-regression
car::vif(mylogit)

#A wald test is used to evaluate the statistical significance of each 
#coefficient in the model and is calculated by taking the ratio of the 
#square of the regression coefficient to the square of the standard error 
#of the coefficient. The idea is to test the hypothesis that the 
#coefficient of an independent variable in the model is significantly 
#different from zero. If the test fails to reject the null hypothesis, 
#this suggests that removing the variable from the model will 
#not substantially harm the fit of that model.
# https://www.r-bloggers.com/evaluating-logistic-regression-models/

regTermTest(mylogit, "Race")
regTermTest(mylogit, "Age")
regTermTest(mylogit, "SLEEP.DURATION")
regTermTest(mylogit, "Bad")
regTermTest(mylogit, "OK")

library(caret)
varImp(mylogit)

library(ResourceSelection)
hoslem.output <- hoslem.test(mylogit$y, fitted(mylogit), g=10)

#NONsignificant result means model fits well

hoslem.output

cbind(hoslem.output$observed,hoslem.output$expected)

#Cox and Snell, Nagelkerge, McFadden outputs:
library(rcompanion)
nagelkerke(mylogit, null=NULL)

###### significance of the overall model
with (mylogit, null.deviance - deviance)
with (mylogit, df.null - df.residual)
with (mylogit, pchisq(null.deviance - deviance, 
                      df.null - df.residual,
                      lower.tail = FALSE))



# Get the AOR and the CI on the AOR with this
require(MASS)
exp(cbind(coef(mylogit), confint(mylogit)))  

exp(coefficients(mylogit))

# percentage correctly classified ????

probabilities <- predict(mylogit, type = "response")
1 - mean(abs(probabilities - mylogit[["y"]]))

table(probabilities>.5, mylogit[["y"]])


##### predict and specific scenarios

predict(mylogit, newdata=data.frame(Race=c('Black, Non-Hispanic'), Age=c(45), SLEEP.DURATION=c(5), Bad=('OK/Good'), OK=('OK')  ), type="response")
exponentOfe <- 1.14563 + (-0.90577*1) + (-0.01510*45) + (0.09677*5) + (0.05401*0) + (-0.07805*1)
probability.example <- exp(exponentOfe)/(1+exp(exponentOfe))

probability.example


predict(mylogit, newdata=data.frame(Race=c('White, Non-Hispanic'), Age=c(35), SLEEP.DURATION=c(9), Bad=('Bad'), OK=('Bad/Good')  ), type="response")

exponentOfe <- 1.14563 + (-0.90577*0) + (-0.01510*35) + (0.09677*9) + (0.05401*1) + (-0.07805*0)
probability.example <- exp(exponentOfe)/(1+exp(exponentOfe))

probability.example


data.frame(Race=c('White, Non-Hispanic'), Age=c(35), SLEEP.DURATION=c(9), Bad=('Bad'), OK=('Bad/Good')  )
# 
# by(obesitySleep[,c(2,4,5,6,11)], obesitySleep$Obese, summary)
# 
# 
# summary(obesitySleep[,c(2,4,5,6,11)])
# 
# lapply(obesitySleep[,c(2,6)], sd, na.rm=TRUE)
# 
# lapply(obesitySleep[,c(2,6)], mean, na.rm=TRUE)
# lapply(obesitySleep[,c(2,6)], IQR, na.rm=TRUE)
# 
# 
# lapply(obesitySleep[,c(2,6)], sd, na.rm=TRUE)


# coefficient.of.variation.age <- sd(obesitySleep$Age, na.rm=TRUE) / mean(obesitySleep$Age, na.rm=TRUE)
# #NORMALITY TESTS ON AGE
# hist(obesitySleep$Age)
# age.boxplot <- boxplot(obesitySleep$Age)
# shapiro.test(obesitySleep$Age)
# qqnorm(obesitySleep$Age)
# qqline(obesitySleep$Age)





#MODE OF AGE
# tab <- table(obesitySleep$Age) # number of occurrences for each unique value
# sort(tab, decreasing = TRUE) # sort highest to lowest



# coefficient.of.variation.sleepduration <- sd(obesitySleep$SLEEP.DURATION, na.rm=TRUE) / mean(obesitySleep$SLEEP.DURATION, na.rm=TRUE)
# hist(obesitySleep$SLEEP.DURATION)
# 
# sleepdur.boxplot <- ggplot(obesitySleep) +
#   aes(x = Obese, y = SLEEP.DURATION) +
#   geom_boxplot()
# 
# sleepdur.boxplot
# 
# qqnorm(obesitySleep$SLEEP.DURATION)
# # Draw the reference line:
# qqline(obesitySleep$SLEEP.DURATION)













