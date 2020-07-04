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

by(obesitySleep[,c(2,4,5,6,11)], obesitySleep$Obese, summary)

summary(obesitySleep[,c(2,4,5,6,11)])
lapply(obesitySleep[,c(2,6)], sd, na.rm=TRUE)
lapply(obesitySleep[,c(2,6)], mean, na.rm=TRUE)
lapply(obesitySleep[,c(2,6)], IQR, na.rm=TRUE)


lapply(obesitySleep[,c(2,6)], sd, na.rm=TRUE)


coefficient.of.variation.age <- sd(obesitySleep$Age, na.rm=TRUE) / mean(obesitySleep$Age, na.rm=TRUE)
#NORMALITY TESTS ON AGE
hist(obesitySleep$Age)
age.boxplot <- boxplot(obesitySleep$Age)
shapiro.test(obesitySleep$Age)
qqnorm(obesitySleep$Age)
qqline(obesitySleep$Age)

#SD on AGE when Obese = NOT OBESE
with(obesitySleep, sd(Age[Obese=='Not Obese']))

#SD on AGE when Obese =  OBESE
with(obesitySleep, sd(Age[Obese=='Obese']))

#MODE OF AGE when Obese = NOT OBESE
tab <- table(obesitySleep$Age[obesitySleep$Obese=='Not Obese'])   # number of occurrences for each unique value
sort(tab, decreasing = TRUE) # sort highest to lowest

#MODE OF AGE when Obese = OBESE
tab <- table(obesitySleep$Age[obesitySleep$Obese=='Obese'])   # number of occurrences for each unique value
sort(tab, decreasing = TRUE) # sort highest to lowest

#MODE OF AGE
tab <- table(obesitySleep$Age) # number of occurrences for each unique value
sort(tab, decreasing = TRUE) # sort highest to lowest



coefficient.of.variation.sleepduration <- sd(obesitySleep$SLEEP.DURATION, na.rm=TRUE) / mean(obesitySleep$SLEEP.DURATION, na.rm=TRUE)
hist(obesitySleep$SLEEP.DURATION)

sleepdur.boxplot <- ggplot(obesitySleep) +
  aes(x = Obese, y = SLEEP.DURATION) +
  geom_boxplot()

sleepdur.boxplot

qqnorm(obesitySleep$SLEEP.DURATION)
# Draw the reference line:
qqline(obesitySleep$SLEEP.DURATION)













