library(foreign)
library(sqldf)
library(ggplot2)
library(caret)
library(randomForest)
library(party)
TILDA <- read.dta("./TILDA/TILDA1.dta")
na_count <-sapply(TILDA, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

# Removing high NA variables
High <- subset(na_count, na_count >= 3000)
List1 <- rownames(High)
TILDA <- TILDA[, -which(names(TILDA) %in% List1)]

TILDA <- na.omit(TILDA)

TILDA[TILDA=="-99"]<-NA; TILDA[TILDA=="99"]<-NA
TILDA[TILDA=="-98"]<-NA; TILDA[TILDA=="98"]<-NA
TILDA[TILDA=="Don't know"]<-NA
TILDA[TILDA=="Refused"]<-NA

TILDA$INCASSassets <- with(TILDA, (as101+as103+as108+as111+as112+hw108))

# Recoding the loneliness variable
TILDA$Lonely [TILDA$MHcapi_loneliness != "Rarely or never"] <- "Yes"
TILDA$Lonely [TILDA$MHcapi_loneliness == "Rarely or never"] <- "No"
TILDA$Lonely <- as.factor(TILDA$Lonely)
good <- complete.cases(TILDA$Lonely)
TILDA <- TILDA[good, ]

TILDA[sapply(TILDA, is.character)] <- lapply(TILDA[sapply(TILDA, is.character)], as.factor)

TILDA$mh0014 <- NULL
TILDA$SCQlonelns5 <- NULL
TILDA$MHcesd_capi <- NULL
TILDA$MHcapi_loneliness <- NULL
TILDA$MHucla_loneliness <- NULL
TILDA$SCQlonelns1 <- NULL


# Check for Zero variance
nsv <- nearZeroVar(TILDA)
TILDA <- TILDA[ , -nsv]

# Partition the dataset
set.seed(660)
inTrain <- createDataPartition(y=TILDA$Lonely, p=.70, list=FALSE)
training <- TILDA[inTrain,]
testing <- TILDA[-inTrain,]

ModelRF <- train(Lonely~., data=training, importance=TRUE, method="rf")

set.seed(660)
ModelXGB <- train(Lonely~., data=training, method="xgbTree")
confusionMatrix(predict(ModelXGB, newdata=testing), testing$Lonely)

Imp <- varImp(ModelXGB)
List2 <- rownames(Imp$importance)
TILDA <- TILDA[, which(names(TILDA) %in% List2)]


