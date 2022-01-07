library(ggpubr)
library(DescTools)
library(ggplot2)
library(plotly)

data <- read.csv("C:/Users/ritik/OneDrive/Desktop/SEM 3/LAB - COMPUTING III/anova.csv", header = TRUE)
data

#Describing Dataset
summary(data)

# 2 sample t - test
t.test(data$count_1, data$count_2, var.equal = FALSE)
t.test(data$count_2, data$count_3, var.equal = FALSE)
t.test(data$count_3, data$count_1, var.equal = FALSE)
#p value is greater than 5%, data is significant to each other(accepts the null hypothesis)

#COUNT_1 analysis

#checking if variable fits a normal distribution 
h<-hist(data$count_1,col="red",xlab="Count 1",main="Checking Normality")
x<-data$count_1
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

#plot
plot_ly(data=data, y = ~data$count_1, x=~data$time, color = I("red"),type="box", 
        alpha = 0.5, boxpoints = "suspectedoutliers",name = "count1")
#show that 48 hours give higher growth than 24 hours
plot_ly(data=data, y = ~data$count_1, x=~data$conc, color = I("red"),type="box", 
        alpha = 0.5, boxpoints = "suspectedoutliers",name = "count1")
#optimal concentration is about 1.2%
plot_ly(data=data, y = ~data$count_1, x=~data$temp, color = I("red"),type="box", 
        alpha = 0.5, boxpoints = "suspectedoutliers",name = "count1")
#optimal temperature is about 35 degrees

#One way anova 
aov.res1<- aov(count_1 ~ time, data = data)
aov.resu1<- aov(count_1 ~ temp, data = data)
aov.resul1<- aov(count_1 ~ conc, data = data)
summary(aov.res1)
summary(aov.resul1)
summary(aov.resu1)
plot(aov.res1,1)#Homogenity(check variation among groups)
plot(aov.res1,2)#Normality
plot(aov.resu1,1)
plot(aov.resu1,2)
plot(aov.resul1,1)
plot(aov.resul1,2)

#Two way anova 
aov2.resul1<- aov(count_1 ~ conc + time, data = data)
summary(aov2.resul1)
aov2.resu1<- aov(count_1 ~ temp + conc, data = data)
summary(aov2.resu1)
aov2.res1<- aov(count_1 ~ temp + time, data = data)
summary(aov2.res1)
plot(aov2.resul1,1)
plot(aov2.resul1,2)

#2 way plot
mean.data1=data %>% group_by(time,conc) %>% summarise(count_1=median(count_1))
two.way.plot1 <- ggplot(data,aes(x=conc, y=count_1, group=time))+geom_point(cex =1.5, pch=1.0,position =position_jitter(w=0.1, h=0))
two.way.plot1 <-two.way.plot1+stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.data1, aes(x=conc, y=count_1)+
               facet_wrap(~ conc))
two.way.plot1 <- two.way.plot1 +
  labs(title = "Count 1 in response to time and concentration",
       x = "CONC of Tryptone by weight",
       y = "COUNT1")
two.way.plot1

#Interactions(3 null hypotheses)
i.resul1<- aov(count_1 ~ conc * time, data = data)
i.resu1<- aov(count_1 ~ temp * time, data = data)
i.res1<- aov(count_1 ~ temp * conc, data = data)
summary(i.resul1)
summary(i.resu1)
summary(i.res1)

#COUNT_2 analysis

#checking if variable fits a normal distribution 
h<-hist(data$count_2,col="magenta",xlab="Count 2",main="Checking Normality")
x<-data$count_2
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

#plot
p1 <- plot_ly(data=data, y = ~data$count_2, x=~data$time, color = I("magenta"),type="box", 
              alpha = 0.5, boxpoints = "suspectedoutliers",name = "count2")
p1 
c1 <- plot_ly(data=data, y = ~data$count_2, x=~data$conc, color = I("magenta"),type="box", 
              alpha = 0.5, boxpoints = "suspectedoutliers",name = "count2")
c1 
T1 <- plot_ly(data=data, y = ~data$count_2, x=~data$temp, color = I("magenta"),type="box", 
              alpha = 0.5, boxpoints = "suspectedoutliers",name = "count2")
T1 


#Oneway anova 
aov.res2<- aov(count_2 ~ time, data = data)
aov.resu2<- aov(count_2 ~ temp, data = data)
aov.resul2<- aov(count_2 ~ conc, data = data)
summary(aov.res2)
summary(aov.resu2)
summary(aov.resul2)
plot(aov.res2,1)
plot(aov.res2,2)
plot(aov.resu2,1)
plot(aov.resu2,2)
plot(aov.resul2,1)
plot(aov.resul2,2)

#Twoway anova 
aov2.resul2<- aov(count_2 ~ conc + time, data = data)
summary(aov2.resul2)
aov2.resu2<- aov(count_2 ~ temp + conc, data = data)
summary(aov2.resu2)
aov2.res2<- aov(count_2 ~ temp + time, data = data)
summary(aov2.res2)
plot(aov2.resul2,1)
plot(aov2.resul2,2)

mean.data2=data %>% group_by(time,conc) %>% summarise(count_2=median(count_2))
two.way.plot2 <- ggplot(data,aes(x=conc, y=count_2, group=time))+geom_point(cex =1.5, pch=1.0,position =position_jitter(w=0.1, h=0))
two.way.plot2 <-two.way.plot2+stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.data2, aes(x=conc, y=count_2)+
               facet_wrap(~ conc))
two.way.plot2 <- two.way.plot2 +
  labs(title = "Count 2 in response to time and concentration",
       x = "CONC of Tryptone by weight",
       y = "COUNT2")
two.way.plot2

#Interactions
i.resul2<- aov(count_2 ~ conc * time, data = data)
i.resu2<- aov(count_2 ~ temp * time, data = data)
i.res2<- aov(count_2 ~ temp * conc, data = data)
summary(i.resul2)
summary(i.resu2)
summary(i.res2)


#COUNT_3 analysis

#checking if variable fits a normal distribution 
h<-hist(data$count_3,col="green",xlab="Count 3",main="Checking Normality")
x<-data$count_3
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

#plot
p3 <- plot_ly(data=data, y = ~data$count_3, x=~data$time, color = I("green"),type="box", 
              alpha = 0.5, boxpoints = "suspectedoutliers",name = "count3")
p3 
c3 <- plot_ly(data=data, y = ~data$count_3, x=~data$conc, color = I("green"),type="box", 
              alpha = 0.5, boxpoints = "suspectedoutliers",name = "count3")
c3 
T3 <- plot_ly(data=data, y = ~data$count_3, x=~data$temp, color = I("green"),type="box", 
              alpha = 0.5, boxpoints = "suspectedoutliers",name = "count3")
T3 

#Oneway anova 
aov.res3<- aov(count_3 ~ time, data = data)
aov.resu3<- aov(count_3 ~ temp, data = data)
aov.resul3<- aov(count_3 ~ conc, data = data)
summary(aov.res3)
summary(aov.resu3)
summary(aov.resul3)
plot(aov.res3,1)
plot(aov.res3,2)
plot(aov.resu3,1)
plot(aov.resu3,2)
plot(aov.resul3,1)
plot(aov.resul3,2)

#Twoway anova 
aov2.resul3<- aov(count_3 ~ conc + time, data = data)
summary(aov2.resul3)
aov2.resu3<- aov(count_3 ~ temp + conc, data = data)
summary(aov2.resu3)
aov2.res3<- aov(count_3 ~ temp + time, data = data)
summary(aov2.res3)
plot(aov2.resul3,1)
plot(aov2.resul3,2)

mean.data3=data %>% group_by(time,conc) %>% summarise(count_3=median(count_3))
two.way.plot3 <- ggplot(data,aes(x=conc, y=count_3, group=time))+geom_point(cex =1.5, pch=1.0,position =position_jitter(w=0.1, h=0))
two.way.plot3 <-two.way.plot3+stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.data3, aes(x=conc, y=count_3)+
               facet_wrap(~ conc))
two.way.plot3 <- two.way.plot3 +
  labs(title = "Count 3 in response to time and concentration",
       x = "CONC of Tryptone by weight",
       y = "COUNT3")
two.way.plot3

#Interactions
i.resul3<- aov(count_3 ~ conc * time, data = data)
i.resu3<- aov(count_3 ~ temp * time, data = data)
i.res3<- aov(count_3 ~ temp * conc, data = data)
summary(i.resul3)
summary(i.resu3)
summary(i.res3)
