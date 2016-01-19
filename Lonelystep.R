library(foreign)
library(sqldf)
library(ggplot2)
library(caret)
library(car)
TILDA <- read.dta("./TILDA/TILDA1.dta")
TILDA[TILDA=="-99"]<-NA
TILDA[TILDA=="-98"]<-NA
TILDA[TILDA=="Don't know"]<-NA
TILDA[TILDA=="Refused"]<-NA

TILDA$INCASSassets <- with(TILDA, (as101+as103+as108+as111+as112+hw108))

# Recoding the sleep problems variables
TILDA$sleep [TILDA$bh202 == "Most of the time"] <- "Yes"
TILDA$sleep [TILDA$bh202 != "Most of the time"] <- "No"
TILDA$sleep [TILDA$bh203 == "Most of the time"] <- "Yes"
TILDA$sleep [TILDA$bh203 != "Most of the time"] <- "No"
TILDA$sleep [TILDA$bh202 != "Most of the time" & TILDA$bh203 != "Most of the time"] <- "No"
TILDA$sleep <- as.factor(TILDA$sleep)

# Recoding the loneliness variable
TILDA$Lonely [TILDA$MHcapi_loneliness != "Rarely or never"] <- "Yes"
TILDA$Lonely [TILDA$MHcapi_loneliness == "Rarely or never"] <- "No"
TILDA$Lonely <- as.factor(TILDA$Lonely)
good <- complete.cases(TILDA$Lonely)
TILDA <- TILDA[good, ]

t.test(FRbmi~Lonely, data=TILDA)
boxplot(FRbmi~Lonely, data=TILDA)

TILDAvars <- sqldf("SELECT Lonely, BPhypertension, age, sex, mar4, local3, SOCliveswith5, Frbmi, dm001, dm004, dm005,
                dm006, dm011, dm015, dm017, ph001, ph002, ph003, ph004, ph005, ph009, ph101, ph102,
                ph112, ph113, ph114, ph401, ph405, ph408, ph501, ph601, ph701, ph702, hu001,
                hu002, hu005, hu007, hu008, hu009, hu010, hu011, hu012, hu015_01, hu015_08, mh023, we001, we004,
                we005, jh103, jh105, le101, bh001, bh002, bh101, bh107, bh105, bh201, bh202, bh203,
                tp036, tp037, dm020, dm022, si101, si201, hw105, ex101, ex104, ex105, dr002, dr013,
                dr014, dr015, height, weight, Frwaist, emp3, DISimpairments, DISadl, DISiadl,
                DISdisab3, DISspouse, DISchild, DISotherrel, DISvision, DIShearing,
                DISconverse1, DISconverse4, MDmeds, MHcesd_capi, MHhadsa_scq, MHcasp19_total, MHpennworry,
                COGimmediaterecall1, COGimmediaterecall2,
                COGdelayedrecall, IPAQmetminutes, IPAQexercise3, INCASSassets, SESsocial_class, SOChomeChild, SOCtotalChild, SOCProxChld, SOCpalive, tc018,
                tc021, tc008, tc016, SOCsocParticip, SOCSandwich, SOChelpfromNeighbour, SOChelptoNeighbour, SOCChurch,
                SOCClubs, SOCrelFriends, SOCyearlyactivity, SOCmonthlyactivity, SOCweeklyactivity,
                SOCsocnet, BEHsmoker, BEHcage FROM TILDA")

TILDAvars[sapply(TILDAvars, is.character)] <- lapply(TILDAvars[sapply(TILDAvars, is.character)], as.factor)
TILDAvars$dm015 <- factor(TILDAvars$dm015)
TILDAvars$ph101 <- factor(TILDAvars$ph101)
TILDAvars$bh202 <- factor(TILDAvars$bh202)
TILDAvars$emp3 <- factor(TILDAvars$emp3)

# Reduces sample size to 2362
TILDAvars <- na.omit(TILDAvars)

# Check for Zero variance
nsv <- nearZeroVar(TILDAvars)
TILDAvars <- TILDAvars[ , -nsv]


# Partition the dataset
set.seed(660)
inTrain <- createDataPartition(y=TILDAvars$Lonely, p=.70, list=FALSE)
training <- TILDAvars[inTrain,]
testing <- TILDAvars[-inTrain,]


#xgboost model
set.seed(666)

cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1,number = 3, 
                        #summaryFunction = twoClassSummary,
                        classProbs = TRUE,
                        allowParallel=T)

xgb.grid <- expand.grid(nrounds = 1000,
                        eta = c(0.01,0.05,0.1),
                        max_depth = c(2,4,6,8,10,14)
)
set.seed(45)
ModelXGB <-train(Lonely~.,
                 data=training,
                 method="xgbTree",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid,
                 verbose=T,
                 metric="Kappa",
                 nthread =3
)

predictXGB <- predict(ModelXGB, newdata=testing)
confusionMatrix(predictXGB, testing$Lonely)
ImportanceXGB <- varImp(ModelXGB)
rownames(ImportanceXGB$importance)

ggplot(ModelXGB$results, aes(x = as.factor(eta), y = max_depth, size = Accuracy, color = Accuracy)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")

#RF model
set.seed(52)
ModelRF <- train(Lonely~., data=training, method="rf")
predictRF <- predict(ModelRF, newdata=testing)
confusionMatrix(predictRF, testing$Lonely)
ImportanceRF <- varImp(ModelRF)
plot(ImportanceRF, 10)

df2 <- TILDA[, which(names(TILDA) %in% rownames(ImportanceXGB$importance))]

Finalvars <- sqldf("SELECT Lonely, MHcesd_capi, mar4, mh023, SOCrelFriends, DISimpairments, ph002, ph112,
                  ph408, hu005, MHpennworry, SOCsocnet, we005, dr015, dm022,
                   MHhadsa_scq, ph003, SOCliveswith5,
                   dm005, MHcasp19_total, local3, bh202, SOCProxChld, 
                   bh203, bh101, hu001, hu008, bh107, age, 
                  ph114, INCASSassets, COGdelayedrecall, ph102, ph005, 
                   hu015_01, ph702, ph009, ex101, dr014, FRwaist, ex105, ph001 FROM TILDA")

Finalvars[sapply(Finalvars, is.character)] <- lapply(Finalvars[sapply(Finalvars, is.character)], as.factor)
Finalvars <- na.omit(Finalvars)
nsv <- nearZeroVar(Finalvars)

set.seed(661)
inTrain <- createDataPartition(y=Finalvars$Lonely, p=.70, list=FALSE)
training <- Finalvars[inTrain,]
testing <- Finalvars[-inTrain,]

set.seed(660)
FinalXGB <- train(Lonely~., data=training, method="xgbTree")
confusionMatrix(predict(FinalXGB, newdata=testing), testing$Lonely)

ModelCF <- cforest(Lonely~ ., controls = cforest_unbiased(), data = training)
ImportanceCF <- as.data.frame(varimpAUC(ModelCF))
colnames(ImportanceCF) <- "Accuracy"
ImportanceCF <- ImportanceCF[order(-ImportanceCF$Accuracy), , drop = FALSE]

confusionMatrix(predict(ModelCF, newdata=testing), testing$Lonely)

ggplot(ImportanceCF, aes(x=Accuracy1, fill=Accuracy1, group=1)) + geom_bar(stat="identity")
barplot(ImportanceCF$Accuracy1)
