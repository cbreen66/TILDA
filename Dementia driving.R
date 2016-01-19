library(foreign)
library(ggplot2)
library(sqldf)
library(randomForest)
library(caret)


TILDA <- read.dta("./TILDA/TILDA1.dta")
TILDA$INCASSassets <- with(TILDA, as101+as103+as108+as111+as112+hw108)

TILDA$mmse [TILDA$COGmmse_ha <= 25] <- "Yes"
TILDA$mmse [TILDA$COGmmse_ha > 25] <- "No"

TILDA$impair [TILDA$ph301_11 == "Serious memory impairment"] <- "Yes"
TILDA$impair [TILDA$ph301_11 == "NOT Serious memory impairment"] <- "No"

Table1 <- with(TILDA, table(impair, mmse))

Flu[sapply(Flu, is.character)] <- lapply(Flu[sapply(Flu, is.character)], as.factor)
lapply(Flu, class)
Flu$ph701 <- factor(Flu$ph701)

na_count <-sapply(Flu, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

Flu <- na.omit(Flu)

nsv <- nearZeroVar(Flu, saveMetrics=TRUE)

mysample <- Flu[sample(1:nrow(Flu), 1000, replace=FALSE),] 
inTrain <- createDataPartition(y=mysample$ph701, p=.70, list=FALSE)
training <- mysample[inTrain,]
testing <- mysample[-inTrain,]

Frame <- cor(mysample[sapply(mysample, is.numeric)], method="pearson")

set.seed(660)
inTrain <- createDataPartition(y=Flu$ph701, p=.70, list=FALSE)
training <- Flu[inTrain,]
testing <- Flu[-inTrain,]

# Pre-process
set.seed(42)
ModelCF <- cforest(ph701~ ., controls = cforest_unbiased(ntree=5000, mtry=round(sqrt(ncol(Flu)))), data = training)
CFtab <- as.data.frame(varimpAUC(ModelCF))
colnames(CFtab) <- "Accuracy1"
CFtab <- CFtab[order(-CFtab$Accuracy1), , drop = FALSE]

ModelRF <- train(ph701~age, data=training, method="rf")
ModelXGB <- train(ph701~., data=training, method="xgbTree")

ModelVS <- varSelRF(training, training$ph701)

library(MASS)
Allin <- glm(aware~., family="binomial", data=training)
ModelGLM <- step(Allin)
summary(ModelGLM)

predictCF <- predict(ModelCF, newdata=testing)
confusionMatrix(predictCF, testing$aware)

predictRF <- predict(ModelRF, newdata=testing)
confusionMatrix(predictRF, testing$ph701)

predictXGB <- predict(ModelXGB, newdata=testing)
confusionMatrix(predictXGB, testing$ph701)

varImpPlot(ModelXGB$finalModel)

