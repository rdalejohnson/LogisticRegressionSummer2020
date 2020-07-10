

  
lab <- read.table("Aldosterone.csv", sep=",", header=T)
lab=lab[,-1]
lab=cbind(lab[,c(1:3)],lab[,7])
colnames(lab)=c("Age","Sex","Race","Aldo level")


#####Research Question:  Is there a relationship among a patient's gender, race, and age in predicting their aldosterone level? 

#####Procedure:

####Step 1:  Univariate Analyses:  Descriptive Statistics and Cleaning the Data

lab$Sex = gsub(0, "Female",lab$Sex)
lab$Sex = gsub(1, "Male",lab$Sex)
lab$Race = gsub(0, "White",lab$Race)
lab$Race = gsub(1, "African American",lab$Race)


##### Descriptive Analyses:  


aldo.freq=table(lab$`Aldo level`)
aldo.prop=prop.table(aldo.freq)
aldo.table=cbind(aldo.freq,aldo.prop)
colnames(aldo.table)=c("Frequency","Percent")

sex.freq=table(lab$Sex)
sex.prop=prop.table(sex.freq)
sex.table=as.data.frame(cbind(sex.freq,sex.prop))
colnames(sex.table)=c("Frequency","Percent")

race.freq=table(lab$Race)
race.prop=prop.table(race.freq)
race.table=as.data.frame(cbind(race.freq,race.prop))
colnames(race.table)=c("Frequency","Percent")



library(knitr)
kable(aldo.table,caption = "Aldo level",digits = 5)

library(knitr)
kable(sex.table,caption = "Sex",digits = 5)

library(knitr)
kable(race.table,caption = "Race",digits = 5)


##### There are 66 subjects with High Aldo Levels out of 233. With no other knowledge, the probability of high Aldosterone is 66/233 = 0.28.

##### Check that the Dependent Variable Split is not greater than 80/20 or 20/80. Aldosterone is 71.67% (normal) and 28.33% (high). So the split is OK.



age.table=cbind(233,mean(lab$Age),sd(lab$Age),min(lab$Age),max(lab$Age))
colnames(age.table)=c("N", "Mean","Sed Dev", "Min","Max")
rownames(age.table)=c("age")



library(knitr)
kable(age.table,caption = "Age",digits = 5)




library(ggplot2)
n=233
mean=54.42
sd =11.56
binwidth=2

ggplot(data=lab, aes(lab$Age)) + 
  geom_histogram( breaks = seq(20, 90, binwidth),col="black",fill="green",alpha = 0.2) +
  labs(title="Histogram for Age") +
  labs(x="Age", y="Count")+
  stat_function( 
    fun = function(x, mean, sd, n, bw){ 
      dnorm(x = x, mean = mean, sd = sd) * n * bw
    },args = c(mean = mean, sd = sd, n = n, bw = binwidth))


##### Age appears to be symmetrical with no obvious outliers (Mean=54.42; Median=54.00)

#### Step 2:  Conduct Bivariate Analyses

##### Aldo Level by Sex, Race & Age


table1= table(lab$`Aldo level`,lab$Sex)
freq.table1=cbind(table1,margin.table(table1, 1))
freq.table1=rbind(freq.table1,c(margin.table(table1, 2),233))
colnames(freq.table1)=c("Female","Male","Total")
rownames(freq.table1)=c("High","Normal","Total")



library(knitr)
kable(freq.table1,caption = "Table of Aldo level by Sex",digits = 5)


library(MASS)
chisq.test(table1)



table2= table(lab$`Aldo level`,lab$Race)
freq.table2=cbind(table2,margin.table(table2, 1))
freq.table2=rbind(freq.table2,c(margin.table(table2, 2),233))
colnames(freq.table2)=c("White","African American","Total")
rownames(freq.table2)=c("High","Normal","Total")


library(knitr)
kable(freq.table2,caption = "Table of Aldo level by Race",digits = 5)


library(MASS)
chisq.test(table2)


##### Aldo Level and Sex are significant while Aldo Level and Race is not significant.  Males are much more likely to have high Aldo Levels than Females (42.2% compared to 14.5%).


Age=lab$Age
Aldo=lab$`Aldo level`
t.test(Age~Aldo)


##### Age and Aldo Level is significant.  Younger patients were more likely to have high aldo levels than older patients (51.02 years compared to 55.77 years).

##### Comparing X with X Variables (Sex and Race):



table3= table(lab$Sex,lab$Race)
freq.table3=cbind(table3,margin.table(table3, 1))
freq.table3=rbind(freq.table3,c(margin.table(table3, 2),233))
colnames(freq.table3)=c("Female","Male","Total")
rownames(freq.table3)=c("White","African American","Total")


library(knitr)
kable(freq.table3,caption = "Table of Sex by Race",digits = 5)

library(MASS)
chisq.test(table3)



##### Sex and Race are statistically significant.  Males are more likely to be White than females (56.7% compared to 41.5%).

##### For the bivariate relationship with Age


Sex=lab$Sex
Race=lab$Race
Age=lab$Age

t.test(Age~Sex)
t.test(Age~Race)


##### Sex and Age are not statistically significant.  Race and Age are statistically significant.  White patients are more likely to be older than African American patients (57.37 years compared to 50.89 years).

#### Step 3:  Conduct Multiple Logistic Regression Analyses


lab$Sex= factor(lab$Sex, levels=c("Female","Male"))
lab$Race = factor(lab$Race, levels=c("White","African American"))
lab$`Aldo level`=factor(lab$`Aldo level`, levels = c("High","Normal"))
Sex=relevel(lab$Sex, "Female")
Race=relevel(lab$Race, "White")
Aldo=relevel(lab$`Aldo level`,"Normal")
mylogit = glm(Aldo ~ Sex + Race + Age, data = lab, family = "binomial")

mylogit

summary(mylogit)

#significant compared to NULL model:
1 - pchisq(277.07-245.97, df=231-228)

anova(mylogit, test="Chisq")


require(MASS)
exp(cbind(coef(mylogit), confint(mylogit)))  

library(rcompanion)
nagelkerke(mylogit, null=NULL)



# percentage correctly classified ????

probabilities <- predict(mylogit, type = "response")
1 - mean(abs(probabilities - mylogit[["y"]]))


table(probabilities>.5, mylogit[["y"]])
