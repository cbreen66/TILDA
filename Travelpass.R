library(foreign)
library(ggplot2)
library(sqldf)
library(randomForest)
library(caret)
library(party)
library(varSelRF)
TILDA <- read.dta("./TILDA/TILDA1.dta")
TILDA$INCASSassets <- with(TILDA, as101+as103+as108+as111+as112+hw108)

TILDA$travelp [TILDA$si327 > 0] <- "Yes"
TILDA$travelp [TILDA$si327 == 0] <- "No"

Travel <- sqldf("SELECT travelp, BPhypertension, age, sex, mar4, local3, SOCliveswith5, Frbmi, cm006, dm001, dm004, dm005,
                dm006, dm011, dm015, dm017, ph001, ph002, ph003, ph004, ph005, ph009, ph101, ph102,
                ph112, ph113, ph114, ph401, ph405, ph408, ph501, ph601, ph701, ph702, hu001,
                hu002, hu005, hu007, hu008, hu009, hu010, hu015_01, hu015_08, mh023, we001, we004,
                we005, jh103, jh105, le101, bh001, bh002, bh101, bh107, bh105, bh201, bh202, bh203,
                tp036, tp037, dm020, dm022, si101, si201, hw105, ex101, ex104, ex105, dr002, dr013,
                dr014, dr015, height, weight, Frwaist, emp3, DISimpairments, DISadl, DISiadl,
                DISdisab3, DISspouse, DISchild, DISotherrel, DISlongterm, DISvision, DIShearing,
                DISconverse1, DISconverse4, MDmeds, MHcesd_capi, MHhadsa_scq, MHcasp19_total, MHpennworry,
                MHucla_loneliness, MHcapi_loneliness, COGimmediaterecall1, COGimmediaterecall2,
                COGdelayedrecall, IPAQmetminutes, IPAQexercise3, INCASSassets, SESsocial_class, SOChomeChild, SOCtotalChild, SOCProxChld, SOCpalive, tc018,
                tc021, tc008, tc016, SOCsocParticip, SOCSandwich, SOChelpfromNeighbour, SOChelptoNeighbour, SOCChurch,
                SOCClubs, SOCrelFriends, SOCyearlyactivity, SOCmonthlyactivity, SOCweeklyactivity,
                SOCsocnet, BEHsmoker, BEHcage FROM TILDA")

Travel[sapply(Travel, is.character)] <- lapply(Travel[sapply(Travel, is.character)], as.factor)
lapply(Travel, class)

na_count <-sapply(Travel, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

Travel <- na.omit(Travel)

nsv <- nearZeroVar(Travel, saveMetrics=TRUE)

mysample <- Flu[sample(1:nrow(Flu), 1000, replace=FALSE),] 
inTrain <- createDataPartition(y=mysample$ph701, p=.70, list=FALSE)
training <- mysample[inTrain,]
testing <- mysample[-inTrain,]

Frame <- cor(mysample[sapply(mysample, is.numeric)], method="pearson")

set.seed(660)
inTrain <- createDataPartition(y=Travel$travelp, p=.70, list=FALSE)
training <- Travel[inTrain,]
testing <- Travel[-inTrain,]

# Pre-process
set.seed(42)
ModelCF <- cforest(ph701~ ., controls = cforest_unbiased(ntree=5000, mtry=round(sqrt(ncol(Flu)))), data = training)
CFtab <- as.data.frame(varimpAUC(ModelCF))
colnames(CFtab) <- "Accuracy1"
CFtab <- CFtab[order(-CFtab$Accuracy1), , drop = FALSE]

ModelRF <- train(ph701~age, data=training, method="rf")
ModelXGB <- train(travelp~., data=training, method="xgbTree")

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
confusionMatrix(predictXGB, testing$travelp)

Importance <- varImp(ModelXGB, scale=FALSE)
plot(Importance)
RocImp <- filterVarImp(x = training[, -ncol(training)], y = training$si326_1)

ModelVS <- varSelRF(training, training$travelp)
ModelVS$selected.vars
