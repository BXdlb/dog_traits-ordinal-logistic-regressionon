setwd("C:/Users/xxy/OneDrive/stat 357/project")
#setwd("D:/Onedrive/stat 357/project")
library(dplyr)
library(car)
library(ggplot2)
library("GGally")
library(leaps)
library(caret)
library(rsample)
library(Metrics)

traits <- read.csv("dog_traits_AKC-main/data/breed_traits.csv")
traits_l <- read.csv("dog_traits_AKC-main/data/breed_traits_long.csv")
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
#data_2020 <- data_2020 %>% 
#  mutate(Level=10-X2020.Rank%/%20) %>% 
#  dplyr::select(Family:Level)
data_2020$Coat.Type[data_2020$Coat.Type=="Cored"]="Type_Cored"
data_2020$Coat.Type[data_2020$Coat.Type=="Curly"]="Type_Curly"


pairs(data_all[,c(12,13,14,15,16,17,20,21,22,23,24,25,26,27)])
ggpairs(data_all[,c(12,13,14,15,16,17,20,21,22,23,24,25,26,27)])

fit_0 <- lm(Level~.,train)
summary(fit_0)
vif(fit_0)

s0 <- step(lm(X2020.Rank~.,data_2020),direction = "backward")
summary(s0)


#models <- regsubsets(X2020.Rank~(Playfulness*.-Playfulness),data_all[,c(9,12:27)],nvmax = 16,intercept = TRUE,really.big=T)
#reg_sum <- summary(models)
#reg_sum$rsq
#reg_sum$adjr2
#reg_sum$bic
#reg_sum$cp

#glmulti.lm.out <-
#  glmulti(X2020.Rank ~ ., data = data_all,
#          level = 1,               # No interaction considered
#          method = "h",            # Exhaustive approach
#          crit = "aic",            # AIC as criteria
#          confsetsize = 5,         # Keep 5 best models
#          plotty = F, report = T,  # No plot or interim reports
#          fitfunction = "lm")      # lm function

## Show 5 best models (Use @ instead of $ for an S4 object)
#glmulti.lm.out@formulas

#fit_1 <- lm(X2020.Rank~(.)^2,data_2020)
#summary(fit_1)
full_fit<-X2020.Rank ~ Family + Children + Other.Dogs + Shedding + Coat.Grooming + 
  Drooling + Playfulness + Energy + Barking + Mental + Family:Mental + 
  Children:Coat.Grooming + Children:Barking + Other.Dogs:Energy + 
  Shedding:Playfulness + Shedding:Energy + Coat.Grooming:Drooling
s1 <- step(lm(full_fit,data_2020),direction = "backward")
#s1 <- step(lm(X2020.Rank~.^2,data_2020),direction = "backward",steps = 1000,k=300)
s1 <- step(lm(X2020.Rank~1,scope=full_fit,data_2020),direction = "forward")
summary(s1)
plot(s1)

fit_101 <- lm(X2020.Rank ~ Family + Children + Other.Dogs + Shedding + Coat.Grooming + 
                Drooling + Playfulness + Energy + Barking + Mental + Family:Mental + 
                Children:Coat.Grooming + Children:Barking + Other.Dogs:Energy + 
                Shedding:Playfulness + Shedding:Energy + Coat.Grooming:Drooling,data = data_2020)
summary(fit_101)
plot(fit_101)
influencePlot(fit_101)
fit_101$residuals[residuals(fit_101)>50]
cooks.distance(fit_101)[cooks.distance(fit_101)>4/175]
data_2020 <- data_2020[-c(5,8,10,14,59,135,140,167),]
anova(fit_101)

set.seed(34)
x <- sample(1:175,1)
data_x <- data_all[x,c(12:27)]
predict(fit_101, newdata = data_all[x,c(12:27)], interval = "confidence", level = .95)

fit_150 <- lm(X2020.Rank ~ Family + Children + Other.Dogs + Shedding + Coat.Grooming + 
  Drooling + Playfulness + Adaptability + Energy + Barking + 
  Family:Playfulness + Children:Coat.Grooming + Children:Barking + 
  Shedding:Energy + Coat.Grooming:Drooling + Coat.Grooming:Barking + 
  Drooling:Adaptability,data = data_2020)
summary(fit_150)

fit_300 <- lm(X2020.Rank ~ Family + Children + Shedding + Coat.Grooming + Drooling + 
              Playfulness + Protective + Trainability + Energy + Barking + 
              Mental + Family:Playfulness + Children:Coat.Grooming + Shedding:Trainability + 
              Shedding:Energy + Coat.Grooming:Drooling + Drooling:Mental,data = data_2020)
summary(fit_300)

fit_500 <- lm(X2020.Rank ~ Family + Children + Shedding + Coat.Grooming + Drooling + 
              Playfulness + Protective + Trainability + Energy + Barking + 
              Mental + Family:Playfulness + Children:Coat.Grooming + Shedding:Trainability + 
              Shedding:Energy + Coat.Grooming:Drooling + Drooling:Mental,data = data_2020)
summary(fit_500)

anova(fit_101,fit_150,fit_300,fit_500)


#regsubsets
models <- regsubsets(X2020.Rank ~ Family + Children + Other.Dogs + Shedding + Coat.Grooming + 
                       Drooling + Playfulness + Energy + Barking + Mental + Family:Mental + 
                       Children:Coat.Grooming + Children:Barking + Other.Dogs:Energy + 
                       Shedding:Playfulness + Shedding:Energy + Coat.Grooming:Drooling, data_2020, nvmax = 17)
reg_sum<-summary(models)
reg_sum
reg_sum$rsq
reg_sum$adjr2
reg_sum$bic
reg_sum$cp

data.frame(
  R2 = which.max(reg_sum$rsq),
  Adj.R2 = which.max(reg_sum$adjr2),
  CP = which.min(reg_sum$cp),
  BIC = which.min(reg_sum$bic)
)


plot(reg_sum$rsq,xlab="Number of Variables",ylab="R2",type="b")
plot(reg_sum$adjr2,xlab="Number of Variables",ylab="Adj.R2",type="b")
plot(reg_sum$cp,xlab="Number of Variables",ylab="CP",type="b")
plot(reg_sum$bic,xlab="Number of Variables",ylab="BIC",type="b")


#prediction and k-fold
set.seed(56)
samp <- sample(1:175,145,replace = F)
train <- data_2020[samp,]
test <- data_2020[-samp,]
fit_pred_0 <- lm(Level ~ Family + Children + Other.Dogs + Shedding + Coat.Grooming + 
                   Drooling + Playfulness + Energy + Barking + Mental + Family:Mental + 
                   Children:Coat.Grooming + Children:Barking + Other.Dogs:Energy + 
                   Shedding:Playfulness + Shedding:Energy + Coat.Grooming:Drooling,data = train)
summary(fit_pred_0)
pred_0 <- predict(fit_pred_0,newdata = test)
pred_0
R2(pred_0,test$Level)
pred_0_r2 <- R2(pred_0,test$X2020.Rank)
pred_0_rmse <- rmse(pred_0,test$X2020.Rank)
pred_0_mae <- mae(pred_0,test$X2020.Rank)

##k-fold directly
train_contral <- trainControl(method="cv",number=5)
model <- train(X2020.Rank ~ Family + Children + Other.Dogs + Shedding + Coat.Grooming + 
                 Drooling + Playfulness + Adaptability + Energy + Barking + 
                 Family*Playfulness + Children*Coat.Grooming + Children*Barking + 
                 Shedding*Energy + Coat.Grooming*Drooling + Coat.Grooming:Barking + 
                 Drooling:Adaptability,data = data_2020, method ="lm",trControl=train_contral)
model


createFolds(1:145,k=5)
cv <- vfold_cv(train,v=5)

#f1
train_f1 <- data_2020[cv[[1]][[1]][["in_id"]],]
s_f1 <- step(lm(X2020.Rank~.^2,train_f1),direction = "backward",steps = 1000,k=81)
summary(s_f1)
fit_f1 <- lm(X2020.Rank ~ Children + Shedding + Coat.Grooming + Drooling + 
               Playfulness + Trainability + Children:Coat.Grooming + Shedding:Playfulness + 
               Shedding:Trainability + Coat.Grooming:Drooling + Coat.Grooming:Trainability,train_f1)
summary(fit_f1)
pred_f1 <- predict(fit_f1,newdata = test)
pred_f1
R2(pred_f1,test$X2020.Rank)

#f2
train_f2 <- data_2020[cv[[1]][[2]][["in_id"]],]
s_f2 <- step(lm(X2020.Rank~.^2,train_f2),direction = "backward",steps = 1000,k=75)
fit_f2 <- lm(X2020.Rank ~ Family + Children + Shedding + Coat.Grooming + Energy + 
               Barking + Family:Children + Children:Coat.Grooming + Children:Barking + 
               Shedding:Energy + Coat.Grooming:Barking,train_f2)
summary(fit_f2)
pred_f2 <- predict(fit_f2,newdata = test)
pred_f2
R2(pred_f2,test$X2020.Rank)

#f3
train_f3 <- data_2020[cv[[1]][[3]][["in_id"]],]
s_f3 <- step(lm(X2020.Rank~.^2,train_f3),direction = "backward",steps = 1000,k=74)
fit_f3 <- lm(X2020.Rank ~ Family + Children + Other.Dogs + Shedding + Coat.Grooming + 
               Energy + Barking + Family:Other.Dogs + Children:Barking + 
               Shedding:Energy + Coat.Grooming:Barking,train_f3)
summary(fit_f3)
pred_f3 <- predict(fit_f3,newdata = test)
pred_f3
R2(pred_f3,test$X2020.Rank)

#f4
train_f4 <- data_2020[cv[[1]][[4]][["in_id"]],]
s_f4 <- step(lm(X2020.Rank~.^2,train_f4),direction = "backward",steps = 1000,k=82)
fit_f4 <- lm(X2020.Rank ~ Children + Shedding + Coat.Grooming + Drooling + 
               Barking + Mental + Children:Barking + Shedding:Drooling + 
               Shedding:Mental + Coat.Grooming:Drooling + Coat.Grooming:Barking,train_f4)
summary(fit_f4)
pred_f4 <- predict(fit_f4,newdata = test)
pred_f4
R2(pred_f4,test$X2020.Rank)


#f5
train_f5 <- data_2020[cv[[1]][[5]][["in_id"]],]
s_f5 <- step(lm(X2020.Rank~.^2,train_f5),direction = "backward",steps = 1000,k=88)
fit_f5 <- lm(X2020.Rank ~ Family + Children + Shedding + Coat.Grooming + Playfulness + 
               Protective + Energy + Barking + Children:Barking + Shedding:Energy + 
               Coat.Grooming:Protective,train_f5)
summary(fit_f5)
pred_f5 <- predict(fit_f5,newdata = test)
pred_f5
R2(pred_f5,test$X2020.Rank)

pred_kf <- (pred_f1+pred_f2+pred_f3+pred_f4+pred_f5)/5
R2(pred_f5,test$X2020.Rank)


#same model in all kf

fit_f1 <- lm(X2020.Rank ~ Family + Children + Other.Dogs + Shedding + Coat.Grooming + 
               Drooling + Playfulness + Energy + Barking + Mental + Family:Mental + 
               Children:Coat.Grooming + Children:Barking + Other.Dogs:Energy + 
               Shedding:Playfulness + Shedding:Energy + Coat.Grooming:Drooling,train_f1)
summary(fit_f1)
pred_f1 <- predict(fit_f1,newdata = test)
pred_f1
f1_r2 <- R2(pred_f1,test$X2020.Rank)
f1_rmse <- rmse(pred_f1,test$X2020.Rank)
f1_mae <- mae(pred_f1,test$X2020.Rank)

fit_f2 <- lm(X2020.Rank ~ Family + Children + Other.Dogs + Shedding + Coat.Grooming + 
               Drooling + Playfulness + Energy + Barking + Mental + Family:Mental + 
               Children:Coat.Grooming + Children:Barking + Other.Dogs:Energy + 
               Shedding:Playfulness + Shedding:Energy + Coat.Grooming:Drooling,train_f2)
summary(fit_f2)
pred_f2 <- predict(fit_f2,newdata = test)
pred_f2
f2_r2 <- R2(pred_f2,test$X2020.Rank)
f2_rmse <- rmse(pred_f2,test$X2020.Rank)
f2_mae <- mae(pred_f2,test$X2020.Rank)

fit_f3 <- lm(X2020.Rank ~ Family + Children + Other.Dogs + Shedding + Coat.Grooming + 
               Drooling + Playfulness + Energy + Barking + Mental + Family:Mental + 
               Children:Coat.Grooming + Children:Barking + Other.Dogs:Energy + 
               Shedding:Playfulness + Shedding:Energy + Coat.Grooming:Drooling,train_f3)
summary(fit_f3)
pred_f3 <- predict(fit_f3,newdata = test)
pred_f3
f3_r2 <- R2(pred_f3,test$X2020.Rank)
f3_rmse <- rmse(pred_f3,test$X2020.Rank)
f3_mae <- mae(pred_f3,test$X2020.Rank)

fit_f4 <- lm(X2020.Rank ~ Family + Children + Other.Dogs + Shedding + Coat.Grooming + 
               Drooling + Playfulness + Energy + Barking + Mental + Family:Mental + 
               Children:Coat.Grooming + Children:Barking + Other.Dogs:Energy + 
               Shedding:Playfulness + Shedding:Energy + Coat.Grooming:Drooling,train_f4)
summary(fit_f4)
pred_f4 <- predict(fit_f4,newdata = test)
pred_f4
f4_r2 <- R2(pred_f4,test$X2020.Rank)
f4_rmse <- rmse(pred_f4,test$X2020.Rank)
f4_mae <- mae(pred_f4,test$X2020.Rank)

fit_f5 <- lm(X2020.Rank ~ Family + Children + Other.Dogs + Shedding + Coat.Grooming + 
               Drooling + Playfulness + Energy + Barking + Mental + Family:Mental + 
               Children:Coat.Grooming + Children:Barking + Other.Dogs:Energy + 
               Shedding:Playfulness + Shedding:Energy + Coat.Grooming:Drooling,train_f5)
summary(fit_f5)
pred_f5 <- predict(fit_f5,newdata = test)
pred_f5
f5_r2 <- R2(pred_f5,test$X2020.Rank)
f5_rmse <- rmse(pred_f5,test$X2020.Rank)
f5_mae <- mae(pred_f5,test$X2020.Rank)

pred_kf <- (pred_f1+pred_f2+pred_f3+pred_f4+pred_f5)/5
R2(pred_kf,test$X2020.Rank)
real_rank <- test$X2020.Rank
kf_r2 <- R2(pred_kf,test$X2020.Rank)
kf_rmse <- rmse(pred_kf,test$X2020.Rank)
kf_mae <- mae(pred_kf,test$X2020.Rank)

