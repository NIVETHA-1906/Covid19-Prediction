library(readr)
my_data1<-read.csv("C:/Users/nivet/Downloads/corona_tested_individuals_ver_006.english.csv")
View(my_data1)
str(my_data1)
library(dplyr)
#removing unwanted columns
my_data1<-subset(my_data1,select=-age_60_and_above)#my_data1$age_60_and_above<-NULL
my_data1<-subset(my_data1,select=-gender)
my_data1<-subset(my_data1,select=-test_indication)
View(my_data1)
str(my_data1)
summary(my_data1)
#converting columns to numeric type to perform statistical analysis
my_data1$fever<-ifelse(my_data1$fever=="1",1,0)
my_data1$cough<-ifelse(my_data1$cough=="1",1,0)
my_data1$sore_throat<-ifelse(my_data1$sore_throat=="1",1,0)
my_data1$shortness_of_breath<-ifelse(my_data1$shortness_of_breath=="1",1,0)
my_data1$head_ache<-ifelse(my_data1$head_ache=="1",1,0)
my_data1$corona_result<-ifelse(my_data1$corona_result=="positive",1,0)
str(my_data1)
View(my_data1)
sum(is.na(my_data1))#we get 0=>No NULL values
#Descriptive statistical analysis:
#1.Mean:
mean(my_data1$cough)#we get 0.1514373
mean(my_data1$fever)#we get  0.07800666
mean(my_data1$sore_throat)#we get 0.006906989
mean(my_data1$shortness_of_breath)#we get 0.005655411
mean(my_data1$head_ache)#we get 0.008657046
mean(my_data1$corona_result)#we get 0.05282089
#2.Median:
median(my_data1$cough)#we get 0
mean(my_data1$fever)#we get 0.07800666
mean(my_data1$sore_throat)#we get 0.006906989
mean(my_data1$shortness_of_breath)#we get 0.005655411
mean(my_data1$head_ache)#we get 0.008657046
mean(my_data1$corona_result)#we get 0.05282089
#3. Mode:
table(as.vector(my_data1$cough))
#we can see that 0 occurs 236620 times=>0 is the mode for this column
#4.Variance:
var(my_data1$sore_throat)#var is 0.006859307
#5. Standard deviation:
sqrt(var(my_data1$corona_result))#we get 0.2236762
#6. Range:
range(my_data1$shortness_of_breath)#we get min=0 and max=1
range(my_data1$fever)#we get min=0 and max=1
install.packages("moments")
library(moments)
#7. Skewness:
skewness(my_data1$head_ache)#we get 10.60762=>positively skewed
#8.Kurtosis:
kurtosis(my_data1$cough)#we get 4.781854=>Leptokurtic
#Visualization:
install.packages("tidyverse")
library(tidyverse)
install.packages("ggfortify")
library(ggplot2)
#barplot
ggplot(my_data1,aes(x=corona_result))+
  geom_bar()+
  xlab("Corona result")+
  ylab("Count")+
  ggtitle("Bar plot to find no.of positive and negative cases")#from the graph,we see that only few have been tested positive
#scatter plot
ggplot(data=my_data1,aes(x=head_ache,y=sore_throat))+
  geom_point()#this plot will not be useful
#Histogram:
ggplot(my_data1,aes(x=corona_result))+
  geom_histogram(aes(fill=head_ache))+
  xlab("Corona result")+
  ylab("Count")+
  ggtitle("Histogram of corona_result")
#Histogram for variable cough
ggplot(data=my_data1,aes(x=cough))+
  geom_histogram(aes(col="red"))+
  xlab("Cough")+
  ylab("count")+
  ggtitle("Histogram of cough")
#statistical analysis
#for hypothesis testing,we need these packages:
install.packages("tidyverse")
library(tidyverse)
install.packages("ggpubr")
library(ggpubr)
install.packages("rstatix")
library(rstatix)
#1.t-test
#single t-test

#i)H0:mean of cough column is 0
t.test(my_data1$cough,mu=0,alt="two.sided",var.equal = F,conf.level = 0.95)
#we get p<2/2e-16<0.05=>reject H0=>mean of cough column is not 0
#H0:differnce in Mean of cough and fever is equal to 0
#ii)H0:mean of fever column is 0
t.test(my_data1$fever,mu=0,alt="two.sided",var.equal = F,conf.level = 0.95)
#we get p<2.2e-16 < 0.05 => H0 is rejected => mean of fever column is not 0
#iii)H0:mean of sore throat column is 0
t.test(my_data1$sore_throat,mu=0,alt="two.sided",var.equal = F,conf.level = 0.99)
#we get p<2.2e-16 < 0.05 => H0 is rejected => mean of sore throat column is not 0
#iv)H0:mean of shortness of breath column is 0
t.test(my_data1$shortness_of_breath,mu=0,alt="two.sided",var.equal = F,conf.level = 0.95)
#we get p<2.2e-16 < 0.05 => H0 is rejected => mean of shortness of breath column is not 0

#PAIRED t-test
#i)H0:Difference in mean of cough and fever is 0
t.test(my_data1$cough,my_data1$fever,mu=0,alt="two.sided",var.equal = F,paired=T,conf.level=0.99)
#we get t = 115.32, df = 278847, p-value < 2.2e-16 which is less than 0.05.so H0 rejected ,alternative hypothesis: true difference in means is not equal to 0
#ii)H0: Difference in Mean of cough and shortness of breath is equal to 0
t.test(my_data1$cough,my_data1$shortness_of_breath,mu=0,alt="two.sided",var.equal = F,paired=T,conf.level=0.99)
#we get = 214.84, df = 278847, p-value < 2.2e-16 which is less than 0.05.
#so H0 rejected ,alternative hypothesis: true difference in means is not equal to 0
#iii)H0: Difference in Mean of cough and corona result is equal to 0
t.test(my_data1$cough,my_data1$corona_result,mu=0,alt="two.sided",var.equal = F,paired=T,conf.level=0.99)
#we get t = 135.68, df = 278847, p-value < 2.2e-16 which is less than 0.05.
#so H0 rejected ,alternative hypothesis: true difference in means is not equal to 0
#iv)H0: Difference in Mean of cough and headache is equal to 0
t.test(my_data1$cough,my_data1$head_ache,mu=0,alt="two.sided",var.equal = F,paired=T,conf.level=0.99)
#we get t = 209.63, df = 278847, p-value < 2.2e-16 which is less than 0.05.
#so H0 rejected ,alternative hypothesis: true difference in means is not equal to 0

#CORRELATION:
install.packages("devtools")
library(devtools)
install.packages("ggpubr")
library(ggpubr)
str(my_data1)
#H0:there is no correlation bw fever and cough
cor(my_data1$fever,my_data1$cough,method="pearson") #we get rho=0.454386 which is not equal to 0.so H0 rejected=>there is correlation bw fever and cough
#H0:there is no correlation bw fever and sore throat
cor(my_data1$fever,my_data1$sore_throat,method="pearson")
#we get rho=0.1228317 which is not equal to 0.so H0 rejected=>there is correlation bw fever and sore_throat
#Using agricolae package:
install.packages("agricolae")
library(agricolae)
#H0:there is no correlation bw fever and headache
correl(my_data1$fever,my_data1$head_ache,method="pearson")
#we get rho=0.1688408 which is not equal to 0.so H0 rejected=>there is correlation bw fever and headache
#Using stats package:
install.packages("stats")
library(stats)
#H0:there is no correlation bw fever and corona result
cor.test(my_data1$fever,my_data1$corona_result,method="pearson",conf.level = 0.95)
#We get cor=0.2636491 which is not equal to 0.so H0 rejected=>There is correlation between fever and corona_result

#3.anova
#one-way anova
#i)H0:head_ache has no impact on corona_result
aov1<-aov(my_data1$corona_result~my_data1$head_ache)
summary(aov1)#we get p<2e-16<0.05=>H0 rejected=>head_ache has impact on corona_result
#ii)H0:cough has no impact on corona_Result
aov2<-aov(my_data1$corona_result~my_data1$cough)
summary(aov2)
#we get p<2e-16<0.05=>H0 rejected=>cough has impact on corona_result
#iii)H0:fever has no impact on corona_result
aov3<-aov(my_data1$corona_result~my_data1$fever)
summary(aov3)
#we get p<2e-16<0.05=>H0 rejected=>fever has impact on corona_result
# 2 way anova
#i)H0:fever and sore_throat has no impact on cough
str(my_data1)
aov4<-aov(cough~fever+sore_throat,data=my_data1)
aov4
summary(aov4)#we get p<2e-16<0.05,so H0 rejected=>fever and sore_throat has impact on cough
#ii)H0:fever,sore_throat,head_Ache,shortness_of_breath and cough together have no impact on corona_result
aov5<-aov(corona_result~fever+sore_throat+head_ache+shortness_of_breath+cough,data=my_data1)
summary(aov5)#we get p<2e-16<0.05,so H0 rejected=>fever,sore_throat,head_Ache,shortness_of_breath and cough together have impact on corona_result

#since our dataset contains class label,we perform classification
#Logistic regression
install.packages("caTools")
library(caTools)
#splitting the data into train and test
split<-sample.split(my_data1,SplitRatio = 0.7)
train<-subset(my_data1,split=="TRUE")
test<-subset(my_data1,split=="FALSE")
#Logistic regression model:
model<-glm(formula=corona_result~cough+fever+sore_throat+shortness_of_breath+head_ache,data=my_data1,family="binomial")
res<-predict(model,test,type="response")
res[1:5]#gives predicted result for first 5 records of test data
pred <- ifelse(res > 0.5, 1, 0)
pred
#to find confusion matrix
table(pred,test$corona_result)
#checking accuracy
misclasserror <- mean(pred != test$corona_result)
print(paste('Accuracy=', 1-misclasserror))
#import the dataset containing data to be predicted
library(readxl)#to read excel files
pred_data<-read_excel("C:/Users/nivet/Downloads/Book 1.xlsx")
View(pred_data)
str(pred_data)
predict(model,newdata=pred_data,type="response")#gives predicted value=0.9965705 for new data

