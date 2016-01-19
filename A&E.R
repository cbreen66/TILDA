library(foreign)
library(ggplot2)
library(sqldf)
library(randomForest)
library(caret)
library(party)
library(varSelRF)
TILDA <- read.dta("./TILDA/TILDA1.dta")
TILDA$INCASSassets <- with(TILDA, as101+as103+as108+as111+as112+hw108)

TILDA$huse [(TILDA$hu007 + TILDA$hu005 + TILDA$hu008) > 0] <- "Yes"
TILDA$huse [(TILDA$hu007 + TILDA$hu005 + TILDA$hu008) == 0] <- "No"

Health <- sqldf("SELECT huse, BPhypertension, age, sex, mar4, local3, SOCliveswith5, Frbmi, cm006, dm001, dm004, dm005,
                dm006, dm011, dm015, dm017, ph001, ph002, ph003, ph004, ph005, ph009, ph101, ph102,
                ph112, ph113, ph114, ph401, ph405, ph408, ph501, ph601, ph701, ph702, hu001,
                hu002, hu015_01, hu015_08, mh023, we001, we004,
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

Health[sapply(Health, is.character)] <- lapply(Health[sapply(Health, is.character)], as.factor)
lapply(Health, class)

na_count <-sapply(Health, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

Health <- na.omit(Health)

nsv <- nearZeroVar(Travel, saveMetrics=TRUE)

set.seed(660)
inTrain <- createDataPartition(y=Health$huse, p=.70, list=FALSE)
training <- Health[inTrain,]
testing <- Health[-inTrain,]

ModelXGB <- train(huse~., data=training, method="xgbTree")

ModelVS <- varSelRF(training, training$huse)
ModelVS$selected.vars

predictXGB <- predict(ModelXGB, newdata=testing)
confusionMatrix(predictXGB, testing$huse)

Importance <- varImp(ModelXGB, scale=FALSE)
plot(Importance)
RocImp <- filterVarImp(x = training[, -ncol(training)], y = training$AE)


