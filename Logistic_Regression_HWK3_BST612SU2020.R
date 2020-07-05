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

chisq.test(table(obesitySleep$Obese, obesitySleep$Race))


############ OBESITY BY SLEEP QUALITY ########################### OBESITY BY SLEEP QUALITY ###################

with(obesitySleep, CrossTable(Obese, SLEEP.QUALITY.RATING))

chisq.test(table(obesitySleep$Obese, obesitySleep$SLEEP.QUALITY.RATING))



####### AGE BY RACE #########


####### AGE BY SLEEP QUALITY ########


#### AGE BY SLEEP DURATION ########


### RACE BY SLEEP QUALITY #####


### RACE BY SLEEP DURATION ####


### SLEEP QUALITY BY SLEEP DURATION ####










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













