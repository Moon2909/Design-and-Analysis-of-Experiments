rm(list=ls())

library(tidyverse)
library(ggplot2)
library(randomForest)
library(caTools)
setwd("~")

data = read.csv("C:/Users/Admin/3D Objects/PTTKTN/DataClear.csv")
table(data$Category)

head(data[data$Category=='ART_AND_DESIGN',c(2,3)],10)
head(data[data$Category=='EDUCATION',c(2,3)],10)

googlePS <-as.data.frame(data)
str(googlePS)

googlePlay <- googlePS[complete.cases(googlePS),]
#Ki�??m tra va` xo??a gia?? tri?? nh�??p l�~i trong dataset
googlePlay<-googlePlay[googlePlay$Rating<=5,]


#Danh m???c(Loa??i) ???ng d???ng c???a h�ng Google Play n�o c� x???p h???ng cao nh???t?
q1<-googlePlay[googlePlay$Category=='GAME'| googlePlay$Category=='BUSINESS' | googlePlay$Category=='EDUCATION'
               |googlePlay$Category=='ENTERTAINMENT'| googlePlay$Category=='LIFESTYLE'|googlePlay$Category=='DATING',]

ggplot(data = q1, aes(x = Category, y = Rating),ylim=c(0,5)) +
  geom_boxplot() +
  stat_boxplot(geom = "errorbar") +
  stat_summary(fun.y = mean, col = "black", geom = "point", size = 3) +
  ggtitle("Bi�??u d�` Boxplots" )


tapply(q1$Rating, q1$Category, summary)

# Bi�??u d�` Histogram
xbar <- tapply(q1$Rating, q1$Category, mean)
s <- tapply(q1$Rating, q1$Category, sd)
q1$normal.density <- apply(q1, 1, function(x){
  dnorm(as.numeric(x["Rating"]),
        xbar[x["Category"]], s[x["Category"]])})
ggplot(q1, aes(x = Rating)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 20,
                 fill = "grey", col = "black") +
  facet_grid(Category ~ .) +
  geom_density(col = "red", lwd = 1) +
  geom_line(aes(y = normal.density), col = "blue", lwd = 1) +
  ggtitle("Bi�??u d�` ha`m m�??t d�?? ph�n ph�??i xa??c su�??t")

# Bi�??u d�` QQ plot
q1$intercept <- apply(q1, 1, function(x){xbar[x["Category"]]})
q1$slope <- apply(q1, 1, function(x){s[x["Category"]]})
ggplot(q1, aes(sample = Rating)) +
  stat_qq() +
  facet_grid(Category ~ .) +
  geom_abline(data = q1, aes(intercept = intercept, slope = slope)) +
  ggtitle("Bi�??u d�` QQPlot so sa??nh ph�n b�?? ca??c m�~u vo??i ph�n ph�??i chu�??n")

# ANOVA test
fit <- aov(Rating ~ Category, data = q1)
summary(fit)
TukeyHSD(fit, conf.level = 0.95)


#C�u 2:Li???u rating c???a m???t ???ng d???ng kh�ng b??? gi???i h???n d??? tu???i c� kh�c bi???t v???i m???t ???ng d???ng b??? gi???i
#h???n d???i tu???i hay kh�ng?

#  boxplot 
googlePlay$Content.Rating<-ifelse(googlePlay$Content.Rating=='Everyone','Non-restricted','Restricted')

ggplot(data = googlePlay, aes(x = Content.Rating, y = Rating)) +
  geom_boxplot() +
  stat_boxplot(geom = "errorbar") +
  stat_summary(fun.y = mean, col = "black", geom = "point", size = 3) +
  ggtitle("Bi�??u d�` Boxplots so sa??nh rating giu~a hai loa??i u??ng du??ng")

tapply(googlePlay$Rating, googlePlay$Content.Rating, summary)

# Histogram 
xbar <- tapply(googlePlay$Rating, googlePlay$Content.Rating, mean)
s <- tapply(googlePlay$Rating, googlePlay$Content.Rating, sd)
googlePlay$normal.density <- apply(googlePlay, 1, function(x){
  dnorm(as.numeric(x["Rating"]),
        xbar[x["Content Rating"]], s[x["Content Rating"]])})
ggplot(googlePlay, aes(x = Rating)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 20,
                 fill = "grey", col = "black") +
  facet_grid(Content.Rating ~ .) +
  geom_density(col = "red", lwd = 1) +
  geom_line(aes(y = normal.density), col = "blue", lwd = 1) +
  ggtitle("Bi�??u d�` ha`m m�??t d�?? ph�n ph�??i xa??c su�??t")

#T�nh test statistic
mu2 = mean(googlePlay[googlePlay$Content.Rating=='Restricted',3])
mu1 = mean(googlePlay[googlePlay$Content.Rating=='Non-restricted',3])
n2 = length(googlePlay[googlePlay$Content.Rating=='Restricted',3])
n1 = length(googlePlay[googlePlay$Content.Rating=='Non-restricted',3])
s2 = sd(googlePlay[googlePlay$Content.Rating=='Restricted',3])
s1 = sd(googlePlay[googlePlay$Content.Rating=='Non-restricted',3])
se = sqrt(s1^2/n1 + s2^2/n2)
t = (mu1 - mu2)/se

#T�nh p-value
v = (s2^2/n2 + s1^2/n1)^2/((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1))
p_value = 2*pt(q = t, df = v) 
p_value

# t-Test
t.test(googlePlay$Rating ~ googlePlay$Content.Rating, mu = 0, conf.level = 0.95,
       paired = FALSE, alternative = "two.sided", var.equal = FALSE)


#3. C� s??? kh�c bi???t d�ng k??? v??? x???p h???ng gi???a ???ng d???ng c???a h�ng Google Play tr??? ph� hay mi???n ph� kh�ng?
#boxplot 
ggplot(data = googlePlay, aes(x = Type, y = Rating)) +
  geom_boxplot() +
  stat_boxplot(geom = "errorbar") +
  stat_summary(fun.y = mean, col = "black", geom = "point", size = 3) +
  ggtitle("Bi�??u d�` boxplots so sa??nh rating giu~a 2 loa??i u??ng du??ng")

tapply(googlePlay$Rating, googlePlay$Type, summary)

# Histogram 
xbar <- tapply(googlePlay$Rating, googlePlay$Type, mean)
s <- tapply(googlePlay$Rating, googlePlay$Type, sd)
googlePlay$normal.density <- apply(googlePlay, 1, function(x){
  dnorm(as.numeric(x["Rating"]),
        xbar[x["Type"]], s[x["Type"]])})
ggplot(googlePlay, aes(x = Rating)) +
  geom_histogram(aes(y = ..density..), 
                 # bins = sqrt(nrow(bike)) + 2,
                 bins = 20,
                 fill = "grey", col = "black") +
  facet_grid(Type ~ .) +
  geom_density(col = "red", lwd = 1) +
  geom_line(aes(y = normal.density), col = "blue", lwd = 1) +
  ggtitle("Bi�??u d�` ha`m m�??t d�?? ph�n ph�??i xa??c su�??t")


#t-test
mu1 = mean(googlePlay[googlePlay$Type=='Free',3])
mu2 = mean(googlePlay[googlePlay$Type=='Paid',3])
n1 = length(googlePlay[googlePlay$Type=='Free',3])
n2 = length(googlePlay[googlePlay$Type=='Paid',3])
s1 = sd(googlePlay[googlePlay$Type=='Free',3])
s2 = sd(googlePlay[googlePlay$Type=='Paid',3])
se = sqrt(s1^2/n1 + s2^2/n2)
t = (mu1 - mu2)/se

#T�nh p-value
v = (s2^2/n2 + s1^2/n1)^2/((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1))
p_value = 2*pt(q = t, df = v) 
p_value


# t-Test
t.test(googlePlay$Rating ~ googlePlay$Type, mu = 0, conf.level = 0.95,
       paired = FALSE, alternative = "two.sided", var.equal = FALSE)



#Random Forest Model
set.seed(1)
data.rf = randomForest(Rating~., data=googlePlay, importance=TRUE)
importance(data.rf)


googlePlay$Type<-ifelse(googlePlay$Type=='Free','0','1')
googlePlay$Type = as.integer(googlePlay$Type)
summary(googlePlay)

#Linear Regression model
str(googlePlay)
set.seed(125)
data_split = sample.split(googlePlay, SplitRatio = 0.75)
train = subset(googlePlay, data_split == TRUE)
test = subset(googlePlay, data_split == FALSE)


model = lm(Rating ~ Reviews + Size + Installs + Type + NameLength, data = googlePlay)
summary(model)
