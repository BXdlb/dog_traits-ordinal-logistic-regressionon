#setwd("C:/Users/xxy/OneDrive/stat 357/project")
setwd("D:/Onedrive/stat 357/project")

library(dplyr)
library(car)
library(ggplot2)
library(leaps)
library(caret)
library(rsample)
library(MASS)
library(sure)


traits <- read.csv("dog_traits_AKC-main/data/breed_traits.csv")
rank <- read.csv("dog_traits_AKC-main/data/breed_rank_all.csv")
descrip <- read.csv("dog_traits_AKC-main/data/trait_description.csv")

summary(rank)
summary(traits)

traits <- traits %>% 
  filter_at(vars(-c("Coat.Type","Coat.Length","Breed")),any_vars(.>=1&.<=5)) 

rank <- na.omit(rank)
traits <- na.omit(traits)

data_all <- inner_join(rank,traits,by="Breed")
data_2020 <- data_all[,c(9,12:27)]

data_2020 <- data_2020 %>% 
  mutate(Level=as.factor(10-X2020.Rank%/%20)) %>% 
  dplyr::select(Family:Level)
table(data_2020$Level)

full_fit <- Level ~ Family + Children + Other.Dogs + Shedding + Coat.Grooming + 
  Drooling + Coat.Type + Coat.Length + Strangers + Playfulness +
  Protective + Adaptability + Trainability + Energy + Barking + Mental
model_0 <-polr(Level~.,data=data_2020,Hess = TRUE)

predict(model_0,data_2020)
data_2020$Level
res=predict(model_0,data_2020)-data_2020$Level
p1 <- ggplot(data.frame(x = data_2020$Level, y = res), aes(x, y)) +
  geom_point(color = "#444444", shape = 19, size = 2, alpha = 0.5)

pres <- presid(model_0)
p1 <- ggplot(data.frame(x = data_2020$Level, y = pres), aes(x, y)) +
  geom_point(color = "#444444", shape = 19, size = 2, alpha = 0.5) +
  geom_smooth(color = "red", se = FALSE) +
  ylab("Probability-scale residual")
p2 <- ggplot(data.frame(y = pres), aes(sample = y)) +
  stat_qq(distribution = qunif, dparams = list(min = -1, max = 1), alpha = 0.5) +
  xlab("Sample quantile") +
  ylab("Theoretical quantile")

set.seed(99) # for reproducibility
autoplot.polr(model_0, what = "qq")
autoplot.polr(model_0, what = "covariate",x = data_2020$Level)
autoplot.polr(model_0, what = "fitted")
autoplot.resid
sres <- resids(model_0)
p1 <- autoplot(sres, what = "covariate", x = data_2020$Level, xlab = "x")
p2 <- autoplot(sres, what = "qq", distribution = qnorm)



s0 <- step(model_0,scope = full_fit,direction = "both")

#fit_1: best 10 variable
fit_1 <- Level ~ Family + Shedding + Coat.Grooming + Drooling + Coat.Length + 
  Strangers + Playfulness + Protective + Barking + Mental
model_1 <- polr(fit_1,data_2020)

#step regression from fit_1 to an interaction model 
s2 <- step(model_1,scope = Level ~ (Family + Shedding + Coat.Grooming + Drooling + Coat.Length + 
             Strangers + Playfulness + Protective + Barking + Mental)^2,direction = "both")
#17 df model
model_2 <- polr(Level ~ Family + Shedding + Coat.Grooming + Drooling + Coat.Length + 
  Strangers + Playfulness + Protective + Barking + Mental + 
  Coat.Grooming:Barking + Shedding:Mental + Family:Playfulness + 
  Drooling:Coat.Length + Drooling:Mental + Strangers:Protective,data_2020)
  

model_1 <-polr(Level~.,data=data_2020[,-7],Hess = TRUE)
summary(model_0)
summary(model_1)
vif(model_0)

#step for ordinal logistic regression
step_OLR <- function(object, scope, steps = 1000, k = 2) {
  fit=object
  AIC_0 <- extractAIC(fit,k=k)[2]
  while(step>0)
  {
    aod <- drop1(fit)
    row.names(aod) <- c(rn[1L], paste("-", rn[-1L]))
    if (AIC_0==min(aod["AIC"]))
      break
    
    aod_min <- filter(aod,AIC==min(aod["AIC"]))
    change <- rev(rownames(aod_min))
    fit <- update(fit,paste("~ .", change), evaluate = F) 
    fit <- eval.parent(fit)
    AIC_0 <- extractAIC(fit,k=k)[2]
    AIC_0
    format(terms(fit))[1]
    if(format(terms(fit))[1]=="Level ~ 1")
      break
    
  }

}

s1 <- step_OLR(polr(Level~(Family + Children + Other.Dogs + Shedding + Coat.Grooming + 
                             Drooling + Coat.Type + Coat.Length + Strangers + Playfulness +
                             Protective + Adaptability + Trainability + Energy + Barking +Mental)^2,data_2020),steps = 10000,k=100)

model_0 <-polr(Level~.,data=data_2020,Hess = TRUE)
model_2<- polr(Level~.^2,data=data_2020[,c(1:6,9:12,17)],Hess = TRUE)
s1 <- step(model_0,data_2020,direction = "backward",steps = 10000,k=2)

set.seed(56)
samp <- sample(1:175,145,replace = F)
samp
test <- data_2020[-samp,]

model_1 <-polr(Level~.,data=train,Hess = TRUE) 
summary(model_1)
pred <- predict(model_1,newdata = test)
pred
test$Level
R2(as.numeric(pred),as.numeric(test$Level))
vif(model_1)
