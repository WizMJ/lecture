##Exploring the Titanic Dataset
## Megan L. Risdal 6 March 2016

setwd("d:\\R/titanic")
library(ggplot2)
library(ggthemes)
library(scales)
train<-read.csv("train.csv",stringsAsFactors=FALSE)
test<-read.csv("test.csv",stringsAsFactors=FALSE)

library(dplyr)
full<-bind_rows(train,test)

library(mice)
#md.pairs(full)
md.pattern(full)
summary(full)

# Feature Engineering
full$Title<-gsub("(.*, )|(\\..*)", "", full$Name)  #Mr. Mrs, Master..
table(full$Sex,full$Title)

rare_title <-c("Dona", "Lady", "the Countess","Capt","Col","Don","Dr","Major","Rev","Sir","Jonkheer")
full$Title[full$Title=="Mlle"]<-"Miss"
full$Title[full$Title=="Ms"]<-"Miss"
full$Title[full$Title=="Mme"]<-"Miss"
full$Title[full$Title %in% rare_title]<-"Rare Title"
table(full$Sex,full$Title)

# , 또는.기준 분리 후 앞 첫단어 선택
full$Surname<-sapply(full$Name, function(x) strsplit(x, split="[,.]")[[1]][1]) 
full$Fsize<-full$SibSp + full$Parch + 1 #자기 자신 포함 
full$Family <-paste(full$Surname,full$Fsize, sep="_")

##Fsize에 따른 생존 여부 분ㅍ (train만 plot) 
ggplot(full[1:891,],aes(x=Fsize, fill=factor(Survived)))+geom_bar(stat="count",position="dodge")+
     scale_x_continuous(breaks=c(1:11)) +labs(x ="Family Size") +theme_few()

#plot 결과에 따른 Famil size 질적 변수 화 
full$FsizeD[full$Fsize==1]<-"singleton"
full$FsizeD[full$Fsize <5 & full$Fsize >1]<-"small"
full$FsizeD[full$Fsize >4]<-"large"
mosaicplot(table(full$FsizeD[1:891], full$Survived[1:891]), main="Survival", shade=TRUE)

#full$Cabin[1:28]
#strsplit(full$Cabin[2], NULL)[[1]]
full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x,NULL)[[1]][1])) ##Cabin번호의 앞 알파벳만 취함
summary(factor(full$Embarked))
which(factor(full$Embarked)=="")

# 출항지 별/티켓 등급 별 요금
embark_fare<-full %>% filter(PassengerId !=62 & PassengerId !=830) #Embark없는 2개 제외
ggplot(embark_fare, aes(x =Embarked, y =Fare, fill = factor(Pclass)))+ 
     geom_boxplot()+
     geom_hline(aes(yintercept =80), color ="red", linetype ="dashed", lwd =2) +
     scale_y_continuous(labels = dollar_format()) + theme_few()

full$Fare[c(62, 830)]  ## --> $80 이므로 C Embark일 것이다. 
full$Embarked[c(62,830)] <- "C"
which(is.na(full$Fare))
full[1044,]   #--> 3rd class and Southamptom ("S")에서 떠난 

## 3등석 이면서 Southampton에 출항한 승객 분포
ggplot(full[full$Pclass == "3" & full$Embarked =="S",], aes(x =Fare)) +
     geom_density(fill = "pink", alpha =0.5) +
     geom_vline(aes(xintercept =median(Fare, na.rm =T)), color ="red", linetype="dashed", lwd =1) +
     scale_x_continuous(labels =dollar_format()) + theme_few()

full$Fare[1044] <- median(full[full$Pclass =="3" & full$Embarked =="S",]$Fare, na.rm =TRUE) #$8.05

## Imputation
sum(is.na(full$Age))
md.pattern(full)
## 범주형 변수화 
factor_vars <- c("PassengerId", "Pclass", "Sex", "Embarked", "Title", "Surname", "Family", "FsizeD")
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))
md.pattern(full)

#Age와 새 변수 Deck을 채운다.
set.seed(129)
mice_mod <- mice(full[, !names(full) %in% c("PassengerId", "Name","Ticket","Cabin","Family",
                                            "Surname", "Survived")], m=5, method ="rf")
mice_output <- complete(mice_mod)  #List를 df화 

par(mfrow =c(1,2), mar =c(11, 6, 2, 2), las =2, cex =0.9)
hist(full$Age, freq =F, col = "grey", ylim=c(0,0.04), main ="Original Data")
hist(mice_output$Age, freq=F, main="Age: MICE Output", col ="grey", ylim=c(0,0.04))

full$Age <- mice_output$Age
sum(is.na(full$Age))

#성별과 나이에 따른 생존 여부
par(mfrow=c(1,1))
ggplot(full[1:891,], aes(Age, fill =factor(Survived))) + geom_histogram() +facet_grid(.~Sex) + theme_few()

full$Child[full$Age <18] <- "Child"
full$Child[full$Age >=18] <- "Adult"
table(full$Child, full$Survived)

#엄마가 있으면 생존이 높지 않을까?
full$Mother <- "Not Mother"
full$Mother[full$Sex =="female" & full$Parch >0 & full$Age > 18 & full$Title !="Miss"] <- "Mother"
table(full$Mother, full$Survived)
full$Child <- factor(full$Child)
full$Mother <- factor(full$Mother)

### Prediction
train <- full[1:891,]
test <- full[892:1309,]

library(randomForest)
set.seed(754)
rf_model <- randomForest(factor(Survived)~ Pclass + Sex + Age + SibSp + Parch +
                              Fare+ Embarked+Title + FsizeD +Child + Mother, data = train, mtry = 3)
#AGE 뺄 경우..
set.seed(754)
rf_model_age <- randomForest(factor(Survived)~ Pclass + Sex + SibSp + Parch +
                              Fare+ Embarked+Title + FsizeD +Child + Mother, data = train, mtry = 3)


par(mfrow =c(1,1), mar =c(11, 6, 2, 2), las =2, cex =0.9)
plot(rf_model, ylim =c(0,0.36))
legend("topright", colnames(rf_model$err.rate), col =1:3, fill =1:3)

importance <-  importance(rf_model)
varImpPlot(rf_model)

varImportance <- data.frame(Variables =row.names(importance), Importance =
                                 round(importance[,"MeanDecreaseGini"],2))
rankImportance <- varImportance %>% mutate(Rank =paste0("#", dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x =reorder(Variables, Importance), y = Importance, fill =Importance))+
     geom_bar(stat="identity") +
     geom_text(aes(x =Variables, y =0.5, label =Rank), hjust =0, vjust =0.55, size =4, color ="red")+
     labs(x ="Variables")+ coord_flip() +theme_few()

prediction <- predict(rf_model, test)
solution <- data.frame(PassengerID = test$PassengerId, Survived =prediction)


