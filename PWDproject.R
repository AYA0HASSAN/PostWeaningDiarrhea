
########################################################## Packages

install.packages("hrbrthemes")
install.packages("report")
install.packages("multcomp")
install.packages("dunn.test")
install.packages("readRDS")
library(hrbrthemes)
library(readRDS)
library(car)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(caTools)
library(corrplot)
library(dunn.test)
library(report)
library(multcomp)

########################################################## 1.	Descriptive statistics


data<-get(load("E:/NU/Masters/SA&V/Project/PWD.RData"))

str(PWD)
str(data)
summary(PWD)
# Summarize your data and calculate the following: 
# mean, median, minimum, maximum, first and third quartile (for each variable). 

#Mean
mean(PWD$W0, na.rm =TRUE )
mean(PWD$P0, na.rm =TRUE )
mean(PWD$ ADWG0021, na.rm =TRUE )
mean(PWD$ ADWG2150, na.rm =TRUE )
mean(PWD$ ADWG0050, na.rm =TRUE )

#Median
median(PWD$W0, na.rm =TRUE )
median(PWD$P0, na.rm =TRUE )
median(PWD$ ADWG0021, na.rm =TRUE )
median(PWD$ ADWG2150, na.rm =TRUE )
median(PWD$ ADWG0050, na.rm =TRUE )

#Max
max(PWD$W0, na.rm =TRUE )
max(PWD$P0, na.rm =TRUE )
max(PWD$ ADWG0021, na.rm =TRUE )
max(PWD$ ADWG2150, na.rm =TRUE )
max(PWD$ ADWG0050, na.rm =TRUE )

#Min
min(PWD$W0, na.rm =TRUE )
min(PWD$P0, na.rm =TRUE )
min(PWD$ ADWG0021, na.rm =TRUE )
min(PWD$ ADWG2150, na.rm =TRUE )
min(PWD$ ADWG0050, na.rm =TRUE )

#First & Third Quartile
quantile(PWD$W0, probs =c(0.25,0.75),na.rm =TRUE)
quantile(PWD$P0, probs =c(0.25,0.75),na.rm =TRUE)
quantile(PWD$ ADWG0021, probs =c(0.25,0.75),na.rm =TRUE)
quantile(PWD$ ADWG2150, probs =c(0.25,0.75),na.rm =TRUE)
quantile(PWD$ ADWG0050, probs =c(0.25,0.75),na.rm =TRUE)




# Categorical Features are Treatment & Sex
# Sex
#assume 1= male, 2= female
PWD$Sex<-factor(PWD$Sex,labels=c('Male','Female'))
sexFreq<- table(PWD$Sex)
sexFreq

# Treatment
treatmentFreq <- table(PWD$Treatment)
treatmentFreq

# The correlation coefficient
# ADWG0021 and ADWG2150
# - Pearson
corrPerson21_2150<-cor(PWD$ADWG0021, PWD$ADWG2150, method = "pearson", use = "complete.obs")
#corrPerson21_2150<-cor.test(PWD$ADWG0021, PWD$ADWG2150, method = "pearson")
corrPerson21_2150

# - Spearman
corrSpearman21_2150<-cor(PWD$ADWG0021, PWD$ADWG2150, method = "spearman", use = "complete.obs")
#corrSpearman21_2150<-cor.test(PWD$ADWG0021, PWD$ADWG2150, method = "spearman")
corrSpearman21_2150

# ADWG0021 and ADWG0050
# - Pearson
corrPerson21_50<-cor(PWD$ADWG0021, PWD$ADWG0050, method = "pearson", use = "complete.obs")
#corrPerson21_50<-cor.test(PWD$ADWG0021, PWD$ADWG0050, method = "pearson")
corrPerson21_50

# - Spearman
corrSpearman21_50<-cor(PWD$ADWG0021, PWD$ADWG0050, method = "spearman", use = "complete.obs")
#corrSpearman21_50<-cor.test(PWD$ADWG0021, PWD$ADWG0050, method = "spearman")
corrSpearman21_50

########################################################## 2.	Graphics

barplot(sexFreq, main="Sex Frequency", horiz=FALSE,names.arg=c("Male", "Female"),col = c("lightblue","pink") ,xlab="Gender",ylab="Frequency")

barplot(tapply(PWD$ADWG0021,list(PWD$Sex),mean,na.rm=T),main= "Mean ADWG0021 related to Gender",col = c("lightblue","pink"),xlab="Gender",ylab="Mean ADWG0021")

hist(PWD$ADWG2150,xlab = " ADWG2150", main="Histogram of ADWG2150", col = "orange")
hist(PWD$ADWG0021,xlab = " ADWG0021", main="Histogram of ADWG0021" , col = "limegreen")


plot(PWD$ADWG0021,PWD$ADWG0050 , main = "ADWG0021 Vs ADWG0050 Gender Wise",xlab = "ADWG0021", ylab = "ADWG0050",pch = 19, frame = FALSE) 

points(PWD$ADWG0021[PWD$Sex=="Male"],PWD$ADWG0050[PWD$Sex=="Male"],col="lightblue",pch = 19)
points(PWD$ADWG0021[PWD$Sex=="Female"],PWD$ADWG0050[PWD$Sex=="Female"],col="pink",pch = 19)

abline(lm(PWD$ADWG0050[PWD$Sex=="Male"] ~ PWD$ADWG0021[PWD$Sex=="Male"]),col="lightblue")
abline(lm(PWD$ADWG0050 [PWD$Sex=="Female"] ~ PWD$ ADWG0021 [PWD$Sex=="Female"]),col="pink")

boxplot(PWD$ADWG0021 ~ as.factor(PWD$Treatment), main='Box plot sperated by Treatment group', xlab='Treatment',ylab=' ADWG0021', col = c("yellow","orange","coral","lightgreen","limegreen"))



########################################################## 3.	Outlier detection

outliers21 <- boxplot(PWD$ADWG0021 ~ as.factor(PWD$Treatment), main='Box plot sperated by Treatment group', xlab='Treatment',ylab=' ADWG0021', col = c("yellow","orange","coral","lightgreen","limegreen"))$out
outliers21

outliers50 <- boxplot(PWD$ADWG0050 ~ as.factor(PWD$Treatment), main='Box plot sperated by Treatment group', xlab='Treatment',ylab=' ADWG0021', col = c("yellow","orange","coral","lightgreen","limegreen"))$out
outliers50

# Outliers may provide valuable insights,
# highlight data quality issues, or indicate rare events
# in my opinion we don't remove Outliers but 
# investigate them further to understand their nature and potential impact on the analysis


########################################################## 4.	Testing for Normality/Homoscedasticity
########################################################## 4.1 Normality
par(mfrow=c(1,5))
# Q-Q plot
# Categorical Data
# Treatment 

# A
qqnorm(PWD[PWD$Treatment == "A",]$Feeder, main='Treatment "A" Feeder',col = "yellow",pch =19)
qqline(PWD[PWD$Treatment == "A",]$Feeder)
qqnorm(PWD[PWD$Treatment == "A",]$W0, main='Treatment "A" W0',col = "yellow",pch =19)
qqline(PWD[PWD$Treatment == "A",]$W0)
qqnorm(PWD[PWD$Treatment == "A",]$ADWG0021, main='Treatment "A" ADWG0021' , col= "yellow",pch = 19)
qqline(PWD[PWD$Treatment == "A",]$ADWG0021)
qqnorm(PWD[PWD$Treatment == "A",]$ADWG2150, main='Treatment "A" ADWG2150' , col= "yellow",pch = 19)
qqline(PWD[PWD$Treatment == "A",]$ADWG2150)
qqnorm(PWD[PWD$Treatment == "A",]$ADWG0050, main='Treatment "A" ADWG0050', col = "yellow",pch = 19)
qqline(PWD[PWD$Treatment == "A",]$ADWG0050)

# B
qqnorm(PWD[PWD$Treatment == "B",]$Feeder, main='Treatment "B" Feeder', col= "orange",pch = 19)
qqline(PWD[PWD$Treatment == "B",]$Feeder)
qqnorm(PWD[PWD$Treatment == "B",]$W0, main='Treatment "B" W0', col= "orange",pch = 19)
qqline(PWD[PWD$Treatment == "B",]$W0)
qqnorm(PWD[PWD$Treatment == "B",]$ADWG0021, main='Treatment "B" ADWG0021' , col= "orange",pch = 19)
qqline(PWD[PWD$Treatment == "B",]$ADWG0021)
qqnorm(PWD[PWD$Treatment == "B",]$ADWG2150, main='Treatment "B" ADWG2150' , col= "orange",pch = 19)
qqline(PWD[PWD$Treatment == "B",]$ADWG2150)
qqnorm(PWD[PWD$Treatment == "B",]$ADWG0050, main='Treatment "B" ADWG0050', col = "orange",pch = 19)
qqline(PWD[PWD$Treatment == "B",]$ADWG0050)

# C
qqnorm(PWD[PWD$Treatment == "C",]$Feeder, main='Treatment "C" Feeder', col= "coral",pch = 19)
qqline(PWD[PWD$Treatment == "C",]$Feeder)
qqnorm(PWD[PWD$Treatment == "C",]$W0, main='Treatment "C" W0', col= "coral",pch = 19)
qqline(PWD[PWD$Treatment == "C",]$W0)
qqnorm(PWD[PWD$Treatment == "C",]$ADWG0021, main='Treatment "C" ADWG0021' , col= "coral",pch = 19)
qqline(PWD[PWD$Treatment == "C",]$ADWG0021)
qqnorm(PWD[PWD$Treatment == "C",]$ADWG2150, main='Treatment "C" ADWG2150' , col= "coral",pch = 19)
qqline(PWD[PWD$Treatment == "C",]$ADWG2150)
qqnorm(PWD[PWD$Treatment == "C",]$ADWG0050, main='Treatment "C" ADWG0050', col = "coral",pch = 19)
qqline(PWD[PWD$Treatment == "C",]$ADWG0050)

# D
qqnorm(PWD[PWD$Treatment == "D",]$Feeder, main='Treatment "D" Feeder', col= "lightgreen",pch = 19)
qqline(PWD[PWD$Treatment == "D",]$Feeder)
qqnorm(PWD[PWD$Treatment == "D",]$W0, main='Treatment "D" W0', col= "lightgreen",pch = 19)
qqline(PWD[PWD$Treatment == "D",]$W0)
qqnorm(PWD[PWD$Treatment == "D",]$ADWG0021, main='Treatment "D" ADWG0021' , col= "lightgreen",pch = 19)
qqline(PWD[PWD$Treatment == "D",]$ADWG0021)
qqnorm(PWD[PWD$Treatment == "D",]$ADWG2150, main='Treatment "D" ADWG2150' , col= "lightgreen",pch = 19)
qqline(PWD[PWD$Treatment == "D",]$ADWG2150)
qqnorm(PWD[PWD$Treatment == "D",]$ADWG0050, main='Treatment "D" ADWG0050', col = "lightgreen",pch = 19)
qqline(PWD[PWD$Treatment == "D",]$ADWG0050)

# E
qqnorm(PWD[PWD$Treatment == "E",]$Feeder, main='Treatment "E" Feeder', col= "limegreen",pch = 19)
qqline(PWD[PWD$Treatment == "E",]$Feeder)
qqnorm(PWD[PWD$Treatment == "E",]$W0, main='Treatment "E" W0', col= "limegreen",pch = 19)
qqline(PWD[PWD$Treatment == "E",]$W0)
qqnorm(PWD[PWD$Treatment == "E",]$ADWG0021, main='Treatment "E" ADWG0021' , col= "limegreen",pch = 19)
qqline(PWD[PWD$Treatment == "E",]$ADWG0021)
qqnorm(PWD[PWD$Treatment == "E",]$ADWG2150, main='Treatment "E" ADWG2150' , col= "limegreen",pch = 19)
qqline(PWD[PWD$Treatment == "E",]$ADWG2150)
qqnorm(PWD[PWD$Treatment == "E",]$ADWG0050, main='Treatment "E" ADWG0050', col = "limegreen",pch = 19)
qqline(PWD[PWD$Treatment == "E",]$ADWG0050)

# Sex
# Male
qqnorm(PWD[PWD$Sex == "Male",]$Feeder, main='Male Feeder', col= "lightblue",pch = 19)
qqline(PWD[PWD$Sex == "Male",]$Feeder)
qqnorm(PWD[PWD$Sex == "Male",]$W0, main='Male W0', col= "lightblue",pch = 19)
qqline(PWD[PWD$Sex == "Male",]$W0)
qqnorm(PWD[PWD$Sex == "Male",]$ADWG0021, main='Male ADWG0021', col= "lightblue",pch = 19)
qqline(PWD[PWD$Sex == "Male",]$ADWG0021)
qqnorm(PWD[PWD$Sex == "Male",]$ADWG2150, main='Male ADWG2150', col= "lightblue",pch = 19)
qqline(PWD[PWD$Sex == "Male",]$ADWG2150)
qqnorm(PWD[PWD$Sex == "Male",]$ADWG0050, main='Male ADWG0050', col= "lightblue",pch = 19)
qqline(PWD[PWD$Sex == "Male",]$ADWG0050)

# Female
qqnorm(PWD[PWD$Sex == "Female",]$Feeder, main='Female Feeder', col= "pink",pch = 19)
qqline(PWD[PWD$Sex == "Female",]$Feeder)
qqnorm(PWD[PWD$Sex == "Female",]$W0, main='Female W0', col= "pink",pch = 19)
qqline(PWD[PWD$Sex == "Female",]$W0)
qqnorm(PWD[PWD$Sex == "Female",]$ADWG0021, main='Female ADWG0021', col= "pink",pch = 19)
qqline(PWD[PWD$Sex == "Female",]$ADWG0021)
qqnorm(PWD[PWD$Sex == "Female",]$ADWG2150, main='Female ADWG2150', col= "pink",pch = 19)
qqline(PWD[PWD$Sex == "Female",]$ADWG2150)
qqnorm(PWD[PWD$Sex == "Female",]$ADWG0050, main='Female ADWG0050', col= "pink",pch = 19)
qqline(PWD[PWD$Sex == "Female",]$ADWG0050)

# Numerical Data
qqnorm(PWD$Feeder,main = "Feeder Q-Q Plot ", pch = 19, frame = FALSE, col = "lightblue")
qqline(PWD$Feeder, lwd = 2)

qqnorm(PWD$W0,main = "WO Q-Q Plot ", pch = 19, frame = FALSE, col = "steelblue")
qqline(PWD$W0, lwd = 2)

qqnorm(PWD$ADWG0021,main = "ADW0021 Q-Q Plot ", pch = 19, frame = FALSE, col = "darkcyan")
qqline(PWD$ADWG0021, lwd = 2)

qqnorm(PWD$ADWG2150,main  = "ADW2150 Q-Q Plot ", col = "blue", pch = 19, frame = FALSE)
qqline(PWD$ADWG2150, lwd = 2)

qqnorm(PWD$ADWG0050, main  = "ADW0050 Q-Q Plot ", col = "navy", pch = 19, frame = FALSE)
qqline(PWD$ADWG0050,lwd = 2)

# Shapiro test

# Numerical Data
shapiro.test(PWD$Feeder)
shapiro.test(PWD$W0) 
# P_value = 0.009664, so it is less than 0.05 (alpha), it is not normally distributed
shapiro.test(PWD$ADWG0021)
shapiro.test(PWD$ADWG2150)
shapiro.test(PWD$ADWG0050)

# Categorical Data
# Treatment 
# A
shapiro.test(PWD[PWD$Treatment == "A",]$Feeder)
shapiro.test(PWD[PWD$Treatment == "A",]$W0)
shapiro.test(PWD[PWD$Treatment == "A",]$ADWG0021)
shapiro.test(PWD[PWD$Treatment == "A",]$ADWG2150)
shapiro.test(PWD[PWD$Treatment== "A",]$ADWG0050)

# B
shapiro.test(PWD[PWD$Treatment == "B",]$Feeder)
shapiro.test(PWD[PWD$Treatment == "B",]$W0)
shapiro.test(PWD[PWD$Treatment == "B",]$ADWG0021)
shapiro.test(PWD[PWD$Treatment == "B",]$ADWG2150)
shapiro.test(PWD[PWD$Treatment== "B",]$ADWG0050)

# C
shapiro.test(PWD[PWD$Treatment == "C",]$Feeder)
shapiro.test(PWD[PWD$Treatment == "C",]$W0)
shapiro.test(PWD[PWD$Treatment == "C",]$ADWG0021)
shapiro.test(PWD[PWD$Treatment == "C",]$ADWG2150)
shapiro.test(PWD[PWD$Treatment== "C",]$ADG0050)

# E
shapiro.test(PWD[PWD$Treatment == "E",]$Feeder)
shapiro.test(PWD[PWD$Treatment == "E",]$W0)
shapiro.test(PWD[PWD$Treatment == "E",]$ADWG0021)
shapiro.test(PWD[PWD$Treatment == "E",]$ADWG2150)
shapiro.test(PWD[PWD$Treatment== "E",]$ADWG0050)

# Sex
# Male
shapiro.test(PWD[PWD$Sex == "Male",]$Feeder)
shapiro.test(PWD[PWD$Sex == "Male",]$W0)
shapiro.test(PWD[PWD$Sex == "Male",]$ADWG0021)
shapiro.test(PWD[PWD$Sex == "Male",]$ADWG2150)
shapiro.test(PWD[PWD$Sex == "Male",]$ADWG0050)

# Female
shapiro.test(PWD[PWD$Sex == "Female",]$Feeder)
shapiro.test(PWD[PWD$Sex == "Female",]$W0)
shapiro.test(PWD[PWD$Sex == "Female",]$ADWG0021)
shapiro.test(PWD[PWD$Sex == "Female",]$ADWG2150)
shapiro.test(PWD[PWD$Sex == "Female",]$ADWG0050)

# Histogram
# Numerical Data
par(mfrow=c(1,1))
hist(PWD$Feeder,xlab = "Feeder",main = "Histogram Distribution of Feeder",col = "lightblue")
hist(PWD$W0,xlab = "WO",main = "Histogram Distribution of WO",col = "steelblue")
hist(PWD$ADWG0021,xlab = "ADWG0021",main = "Histogram Distribution of ADWG0021",col = "darkcyan")
hist(PWD$ADWG2150,xlab = "ADWG2150",main = "Histogram Distribution of ADWG2150",col = "blue")
hist(PWD$ADWG0050,xlab = "ADWG0050",main = "Histogram Distribution of ADWG0050",col="navy")

# Categorical Data
# Treatment 
PWD %>%
  ggplot( aes(x=Feeder,fill=Treatment)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  theme_ipsum() +
  labs(fill="")

PWD %>%
  ggplot( aes(x=W0,fill=Treatment)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  theme_ipsum() +
  labs(fill="")

PWD %>%
  ggplot( aes(x=ADWG0021,fill=Treatment)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  theme_ipsum() +
  labs(fill="")

PWD %>%
  ggplot( aes(x=ADWG2150,fill=Treatment)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  theme_ipsum() +
  labs(fill="")

PWD %>%
  ggplot( aes(x=ADWG0050,fill=Treatment)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  theme_ipsum() +
  labs(fill="")

#Sex
par(mfrow=c(1,5))
PWD %>%
  ggplot( aes(x=W0,fill=Sex)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("lightblue", "pink")) +
  theme_ipsum() +
  labs(fill="")

PWD %>%
  ggplot( aes(x=Feeder,fill=Sex)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("lightblue", "pink")) +
  theme_ipsum() +
  labs(fill="")

PWD %>%
  ggplot( aes(x=ADWG0021,fill=Sex)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("lightblue", "pink")) +
  theme_ipsum() +
  labs(fill="")

PWD %>%
  ggplot( aes(x=ADWG2150,fill=Sex)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("lightblue", "pink")) +
  theme_ipsum() +
  labs(fill="")

PWD %>%
  ggplot( aes(x=ADWG0050,fill=Sex)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("lightblue", "pink")) +
  theme_ipsum() +
  labs(fill="")

########################################################## 4.2 Homoscedasticity
# LeveneTest

# Treatment
leveneTest(Feeder~Treatment, data=PWD)
leveneTest(W0~Treatment, data=PWD)
leveneTest(ADWG0021~Treatment, data=PWD)
leveneTest(ADWG2150~Treatment, data=PWD)
leveneTest(ADWG0050~Treatment, data=PWD)

#Sex
leveneTest(Feeder~Sex, data=PWD)
leveneTest(W0~Sex, data=PWD)
leveneTest(ADWG0021~Sex, data=PWD)
leveneTest(ADWG2150~Sex, data=PWD)
leveneTest(ADWG0050~Sex, data=PWD)

# Bartlett test

# Treatment
bartlett.test(Feeder ~ Treatment, data = PWD)
bartlett.test(W0~Treatment, data=PWD)
bartlett.test(ADWG0021~Treatment, data=PWD)
bartlett.test(ADWG2150~Treatment, data=PWD)
bartlett.test(ADWG0050~Treatment, data=PWD)

#Sex
bartlett.test(Feeder~Sex, data=PWD)
bartlett.test(W0~Sex, data=PWD)
bartlett.test(ADWG0021~Sex, data=PWD)
bartlett.test(ADWG2150~Sex, data=PWD)
bartlett.test(ADWG0050~Sex, data=PWD)

# Box Plot
#Treatment
PWD %>% ggplot(aes(x=Treatment, y=Feeder, fill=Treatment)) + 
  geom_boxplot(width=0.5,lwd=1)

PWD %>% ggplot(aes(x=Treatment, y=W0, fill=Treatment)) + 
  geom_boxplot(width=0.5,lwd=1)

PWD %>% ggplot(aes(x=Treatment, y=ADWG0021, fill=Treatment)) + 
  geom_boxplot(width=0.5,lwd=1)

PWD %>% ggplot(aes(x=Treatment, y=ADWG2150, fill=Treatment)) + 
  geom_boxplot(width=0.5,lwd=1)

PWD %>% ggplot(aes(x=Treatment, y=ADWG0050, fill=Treatment)) + 
  geom_boxplot(width=0.5,lwd=1)

# Sex

PWD %>% ggplot(aes(x=Sex, y=Feeder, fill=Sex)) + 
  geom_boxplot(width=0.5,lwd=1)

PWD %>% ggplot(aes(x=Sex, y=W0, fill=Sex)) + 
  geom_boxplot(width=0.5,lwd=1)

PWD %>% ggplot(aes(x=Sex, y=ADWG0021, fill=Sex)) + 
  geom_boxplot(width=0.5,lwd=1)

PWD %>% ggplot(aes(x=Sex, y=ADWG2150, fill=Sex)) + 
  geom_boxplot(width=0.5,lwd=1)

PWD %>% ggplot(aes(x=Sex, y=ADWG0050, fill=Sex)) + 
  geom_boxplot(width=0.5,lwd=1)

# all features are Homoscedastic as all results of p-value are greater than 0.05

########################################################## 5.	Statistical Inference

means <- tapply(PWD$ADWG0021,list(Sex=PWD$Sex),mean,na.rm=T)
sd <- tapply(PWD$ADWG0021,list(Sex=PWD$Sex),sd,na.rm=T)
x = means
s = sd
n = 40

#90%
error <- qnorm(0.951)*s/sqrt(n)
lowerinterval90 <- x - error
upperinterval90 <- x + error

lowerinterval90
upperinterval90

#95%
error <- qnorm(0.975)*s/sqrt(n)
lowerinterval95 <- x - error
upperinterval95 <- x + error

lowerinterval95
upperinterval95

#99%
error <- qnorm(0.9995)*s/sqrt(n)
lowerinterval99 <- x - error
upperinterval99 <- x + error
lowerinterval99
upperinterval99

# When the confidence interval increases the width increase

########################################################## 6.	Hypothesis testing

########################################################## 6.1 ADWG0021is different between male vs female.

t.test(ADWG0021~Sex, data=PWD,alternative = "two.sided", paired = FALSE ,var.equal = TRUE)

# p-value is greater than the significance level alpha 0.05 which means that 
# we do not have enough evidence to reject the null hypothesis in support of alternative hypothesis 
# which means that the mean of the 2 groups males and females is not different from each other,
# and also means that ADWG0021 is not different between male vs female.

# test whether the previous test assumptions have been meet for the test

# 1- Check Normality of data
# Females

qqnorm(PWD[PWD$Sex == "Female",]$ADWG0021, main='Females ADWG0021',col= "pink",pch = 19)
qqline(PWD[PWD$Sex == "Female",]$ADWG0021)
hist(PWD[PWD$Sex == "Female",]$ADWG0021, main='Females ADWG0021',col= "pink")
shapiro.test(PWD[PWD$Sex == "Female",]$ADWG0021)
# Females data is normally distributed

# Male

qqnorm(PWD[PWD$Sex == "Male",]$ADWG0021, main='Males ADWG0021',col= "lightblue",pch = 19)
qqline(PWD[PWD$Sex == "Male",]$ADWG0021)
hist(PWD[PWD$Sex == "Male",]$ADWG0021, main='Males ADWG0021',col= "lightblue")
shapiro.test(PWD[PWD$Sex == "Male",]$ADWG0021)
# Males data is normally distributed

# 2- Check Homogeneity of Variance of data

boxplot(ADWG0021~Sex, data=PWD, col = c("lightblue", "pink"))
#better to use levene because it does not assume normality and more robust
leveneTest(ADWG0021~Sex, data=PWD)
var.test(ADWG0021~Sex, data=PWD)
#the data has homo-variance or equal variance


# Answer: yes, the previous test assumptions have been meet for the test as we tested and the results are that the data is Normal distributed and Homo in variance 


########################################################## 6.2 ADWG0021is “different” in the group receiving Treatment A (normal feed + ZnO)  compared to the Treatment B (normal feed + nutraceuticals).

# Statistical Q: Is the mean different between treatments A and B in ADWG0021?
# Null Hypothesis: mean of treatment A equal to mean of treatment B 
# Alternative Hypothesis: mean of treatment A different from mean of treatment B



par(mfrow=c(1,2))
#QQPLOT for treatment A

qqnorm(PWD[PWD$Treatment == "A",]$ADWG0021, main='A ADWG0021',col= "yellow",pch = 19)
qqline(PWD[PWD$Treatment == "A",]$ADWG0021)

#QQPLOT for treatment B
qqnorm(PWD[PWD$Treatment == "B",]$ADWG0021, main='B ADWG0021',col= "orange",pch = 19)
qqline(PWD[PWD$Treatment == "B",]$ADWG0021)

#Histogram for treatment A
hist(PWD[PWD$Treatment == "A",]$ADWG0021, main='A ADWG0021',col= "yellow",pch = 19)

#Histogram for treatment B
hist(PWD[PWD$Treatment == "B",]$ADWG0021, main='B ADWG0021',col= "orange",pch = 19)

# shapiro test for treatment A
shapiro.test(PWD[PWD$Treatment == "A",]$ADWG0021)

# shapiro test for treatment B
shapiro.test(PWD[PWD$Treatment == "B",]$ADWG0021)

# Treatment A is not normally distributed
# Treatment B is normally distributed
# when we have at least one of our data not normal so we assume that the data is not normally distributed
# so here we gonna use man whitny test (welcox test)
wilcox.test(PWD[PWD$Treatment == "A",]$ADWG0021, PWD[PWD$Treatment == "B",]$ADWG0021, var.equal = FALSE,alternative = "two.sided", paired = FALSE)
# as p-value smaller than alpha we have enough evidence to reject null hypothesis in support of alternative hypothesis

# test hetero-variance of data
var.test(PWD[PWD$Treatment == "A",]$ADWG0021, PWD[PWD$Treatment == "B",]$ADWG0021)
# The variance is homo in this data not hetero as p-value is grater than 0.05
# Answer: No, this do not go with the previous test assumptions
wilcox.test(PWD[PWD$Treatment == "A",]$ADWG0021, PWD[PWD$Treatment == "B",]$ADWG0021, var.equal = TRUE,alternative = "two.sided", paired = FALSE)


########################################################## 6.3 perform comparison between the different groups
# Statistical Q: Is there a difference between treatments in ADWG0021?
# Null Hypothesis: No difference in Treatments between different groups
# Alternative Hypothesis: there is difference between treatments

# Normality
# Q-Q Plot
#QQPLOT for treatment A
qqnorm(PWD[PWD$Treatment == "A",]$ADWG0021, main='A ADWG0021',col= "yellow",pch = 19)
qqline(PWD[PWD$Treatment == "A",]$ADWG0021)

#QQPLOT for treatment B
qqnorm(PWD[PWD$Treatment == "B",]$ADWG0021, main='B ADWG0021',col= "orange",pch = 19)
qqline(PWD[PWD$Treatment == "B",]$ADWG0021)

#QQPLOT for treatment C
qqnorm(PWD[PWD$Treatment == "C",]$ADWG0021, main='C ADWG0021',col = "coral",pch = 19)
qqline(PWD[PWD$Treatment == "C",]$ADWG0021)

#QQPLOT for treatment D
qqnorm(PWD[PWD$Treatment == "D",]$ADWG0021, main='D ADWG0021',col = "lightgreen",pch = 19)
qqline(PWD[PWD$Treatment == "D",]$ADWG0021)
#QQPLOT for treatment B
qqnorm(PWD[PWD$Treatment == "E",]$ADWG0021, main='E ADWG0021',col = "limegreen",pch = 19)
qqline(PWD[PWD$Treatment == "E",]$ADWG0021)

#treatment A is not normally distributed while B,C,D and E are normally distributed

# Histogram
hist(PWD[PWD$Treatment == "A",]$ADWG0021, main='Treatment A',col= "yellow")
hist(PWD[PWD$Treatment == "B",]$ADWG0021, main='Treatment B',col= "orange")
hist(PWD[PWD$Treatment == "C",]$ADWG0021, main='Treatment C',col = "coral")
hist(PWD[PWD$Treatment == "D",]$ADWG0021, main='Treatment D',col = "lightgreen")
hist(PWD[PWD$Treatment == "E",]$ADWG0021, main='Treatment E',col = "limegreen")
#treatment A is not normally distributed while B,C,D and E are normally distributed

# shapiro test for treatment A
shapiro.test(PWD[PWD$Treatment == "A",]$ADWG0021) 
# p-value = 0.0395 
#less than 0.05 => reject null => not normal

# shapiro test for treatment B
shapiro.test(PWD[PWD$Treatment == "B",]$ADWG0021) 
# p-value = 0.8312 
# more than 0.05 => don't reject null => normal

# shapiro test for treatment C
shapiro.test(PWD[PWD$Treatment == "C",]$ADWG0021) 
# p-value = 0.6954 
# more than 0.05 => don't reject null => normal

# shapiro test for treatment D
shapiro.test(PWD[PWD$Treatment == "D",]$ADWG0021)
# p-value = 0.7126 
# more than 0.05 => don't reject null => normal

# shapiro test for treatment E
shapiro.test(PWD[PWD$Treatment == "E",]$ADWG0021) 
# p-value = 0.86 
# more than 0.05 => don't reject null => normal

# we have at least one of the data is not normal (data of treatment A) 
# so we gonna use kruskal walis test ( non-parametric )
kruskal.test(ADWG0021 ~Treatment , data=PWD)

# p-value = 0.09972
# more than 0.05 => don't reject => ADGW0021 is not different between gropus of treatments

#check variance to see wether it met the assumptions or not 
plot(ADWG0021~Treatment, data=PWD, main="Variance",col = c("yellow","orange","coral","lightgreen","limegreen"))
leveneTest(ADWG0021~Treatment, data=PWD) 
# p-value = 0.8968
# more than 0.05 => don't reject => data is homo-variance
oneway.test(ADWG0021~Treatment, data= PWD)
# One-way analysis of means (not assuming equal variances) 
# p-value = 0.2022

#assuming normality and homoscedasticity we gonna use standard ANOVA test 
par(mfrow=c(1,1))
AnovaModel<- aov(ADWG0021~Treatment, var.equal = TRUE,alternative = "two.sided",data= PWD)
summary(AnovaModel) 
coef(AnovaModel)
report(AnovaModel)
# p-value = 0.101
# more than 0.05 => don't reject => ADGW0021 is not different between gropus of treatments

#posthoc using tukey test
#posthoc is performed after annova assuming normality and homoscedasticity (includes p value correction to be p adjusted) 
testing <- TukeyHSD(AnovaModel)
testing 
plot(testing)
# after anova we perform posthoc(tuky test)
# the reults of posthoc of p adjusted value show that all treatments with each other gives
# a p adjusted value greater than the significance level alpha which means that 
# we do not have enough evidence to reject the null so this means that ADWG0021 is not different 

summary(glht(AnovaModel, linfct =mcp(Treatment = "Tukey")))
plot(glht(AnovaModel, linfct =mcp(Treatment = "Tukey")))


summary(glht(AnovaModel, linfct =mcp(Treatment = "Dunnett")))
plot(glht(AnovaModel, linfct =mcp(Treatment = "Dunnett"))) 
# after adjusting p values,treatment B-A gives a p value significant equal to 0.035 which is less than 0.05 
# so we have enough evidence to reject the null 
# so the mix of treatment B with A is different in ADWG0021


# pairwise t test gives adjust p value with benferroni method but also results were not signficant 
pairwise.t.test(PWD$ADWG0021, PWD$Treatment , p.adjust.method = "bonferroni")
# results were not signficant 
#tukey and benferoni assume normality
dunn.test(PWD$ADWG0021, PWD$Treatment, method ="Bonferroni")
# the results of tukey test with adjusted p values with benferoni gives 
# a p adjusted value 0.0437 between treatment A and B (A mixed with B) which is lower than 0.05 
# so we have evidence to reject null so this leads to non normality 
# of data(also then ADWG0021 is different in A with B after adjusting p value with benferoni)
# while the p value of other treatments with each other is greater than 0.05 so 
# we do not have evidence to reject the null so this leads to normality of other treatments
# (also ADWG0021 is not different between other treatment with each others(except A with B))
# also the mean of each pair of treatment is shown in the results. 


########################################################## 7.	Linear model

# Reconvert sex to numerical values
PWD$Sex<-as.integer(PWD$Sex)
plot(PWD$Sex, PWD$ADWG0021,col = c("lightblue","pink"),pch =19)

# Regression Model
regression <- lm(ADWG0021 ~ Sex , data= PWD)
regression
summary(regression)

abline(regression, col="red")

confint(regression, 'Sex', level=0.95)

plot(PWD$ADWG2150, PWD$ADWG0021, col = c("darkcyan","lightgreen"),pch = 19)
regression <- lm(ADWG0021 ~ ADWG2150 , data= PWD)
regression
summary(regression)

abline(regression, col="red")

confint(regression, 'ADWG2150', level=0.95)

# Exchange X(ADWG0021) & Y(ADWG2150)
plot(PWD$ADWG0021, PWD$ADWG2150 , col = c("lightgreen","darkcyan"),pch = 19)
regression <- lm(ADWG2150 ~ ADWG0021 , data= PWD)
regression
summary(regression)

abline(regression, col="red")
confint(regression, 'ADWG0021', level=0.95)


multiple_regression <- lm(ADWG0021~ ADWG2150+Sex, data = PWD)
multiple_regression
summary(multiple_regression)
avPlots(multiple_regression)