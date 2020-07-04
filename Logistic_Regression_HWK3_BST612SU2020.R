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









