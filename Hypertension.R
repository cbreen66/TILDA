library(foreign)
library(gplots)
library(sqldf)
library(caret)
library(caretEnsemble)
TILDA <- read.dta("./TILDA/TILDA1.dta")
TILDA$INCASSassets <- with(TILDA, as101+as103+as108+as111+as112+hw108)

#Subset Hypertensive population and split variable to aware / not aware
TILDA$ph202 <- as.factor(TILDA$ph202)
Hyper <- subset(TILDA, BPhypertension == "Hypertensive")
Hyper$aware [Hyper$ph202 == -1] <- "Notaware"
Hyper$aware [Hyper$ph202 != -1] <- "Aware"
Hyper$aware <- as.factor(Hyper$aware)
summary(Hyper$aware)

# Significant age difference
plotmeans(age~aware, data=Hyper)
t.test(age~aware, data=Hyper)

# Significant male-female difference
t1 <- table(Hyper$sex, Hyper$aware)
chisq.test(t1)
prop.table(t1, 1)

# No significance in marital status
t2 <- table(Hyper$mar4, Hyper$aware)
chisq.test(t2)

# No life satisfaction difference
t.test(mh023~aware, data=Hyper)

# No loneliness difference
t.test(MHucla_loneliness~aware, data=Hyper)

# People who are not aware have more friends
t.test(SOCrelFriends~aware,data=Hyper)

level(Hyper$emp3)
t3 <- table(Hyper$emp3, Hyper$aware)
prop.table(t3,1)


# No cognitive difference
t.test(COGmoca~aware, data=Hyper)
t.test(Hyper$COGmmse~aware, data=Hyper)

# Remove perfectly correlated variables
Hyper$ph202 <- NULL
Hyper$ph202a <- NULL
Hyper$ph201_01 <- NULL
Hyper$ph201_14 <- NULL
Hyper$in_scq <- NULL
Hyper$FRtugchairheight <- NULL
Hyper$in_ha <- NULL
Hyper$GRIPtestdominant <- NULL
Hyper$MHapqmiss <- NULL

BPlist <- grep("BPseated", names(Hyper), fixed=FALSE)
Hyper <- Hyper[,-BPlist]

MDlist <- grep("MD", names(Hyper), ignore.case=FALSE, fixed=FALSE)
Hyper <- Hyper[,-MDlist]

lapply(split(Hyper$sex, Hyper$BPhypertension), summary)
ggplot(Hyper, aes(x=aware, fill=aware, group=1)) + geom_bar(width=0.2, fill="pink")
ggplot(Hyper, aes(x=aware, fill=sex)) + geom_bar(width=0.2)

# Removing high NA variables
na_count <-sapply(Hyper, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
High <- subset(na_count, na_count >= 1000)
List1 <- rownames(High)
Hyper <- Hyper[, -which(names(Hyper) %in% List1)]

Hyper[sapply(Hyper, is.character)] <- lapply(Hyper[sapply(Hyper, is.character)], as.factor)

nsv <- nearZeroVar(Hyper)
Hyper <- Hyper[,-nsv]

Hyper <- na.omit(Hyper)

Hyper[sapply(Hyper, is.factor)] <- lapply(Hyper[sapply(Hyper, is.factor)], as.numeric)
Hyper$aware <- as.factor(Hyper$aware)


set.seed(5)
inTrain <- createDataPartition(y=Hyper$aware, p=.75, list=FALSE)
training <- Hyper[inTrain,]
testing <- Hyper[-inTrain,]


# Checking for correlation
M <- abs(cor(training[,-839]))
diag(M) <- 0
which(M>0.8,arr.ind=T)

# Caret pre-process for correlation
descrCor <-  cor(training[,-grep("aware", names(training))])
highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
training <- training[ ,-highlyCorDescr]

ModelXG <- train(aware~., data=training, method="xgbTree")
confusionMatrix(predict(ModelXG, testing), testing$aware)
varImp(ModelXG)


Allin <- glm(aware ~ ., data=training, family=binomial)
summary(Allin)

library(MASS)
ModelLM <- stepAIC(Allin, direction="both")
summary(ModelLM)

# Odds ratios
exp(coef(ModelLM))
exp(cbind(OR = coef(ModelLM), confint(ModelLM)))

# Add in mental health variables
ModelLM <- glm.fit(aware~dm001+age+sex+mar4+ph002+ph003+DISimpairments+local3+
                 emp3+SOCtotalChild+SOCProxChld+hu005+SOCrelFriends+COGmoca+
                 BEHalc_drinksperweek+BEHsmoker+MHucla_loneliness+MHcasp19_total+
                 MHcesd_capi+mh023+SOCliveswith5+as101+as103+as108+as111+as112+
                 hw108+IPAQexercise3+MHpennworry+COGimmediaterecall1+jh103+
                 as108+MHcapi_loneliness,data=Hyper, family=binomial)
summary(ModelLM)

ModelXG <- train(aware~dm001+age+sex+mar4+ph002+ph003+DISimpairments+local3+
                 emp3+SOCtotalChild+SOCProxChld+hu005+SOCrelFriends+COGmoca+
                 BEHalc_drinksperweek+BEHsmoker+MHucla_loneliness+MHcasp19_total+
                 MHcesd_capi+mh023+SOCliveswith5+as101+as103+as108+as111+as112+
                 hw108+IPAQexercise3+MHpennworry+COGimmediaterecall1+jh103+
                 as108+MHcapi_loneliness,data=training, method="xgbTree")
confusionMatrix(predict(ModelXG, testing), testing$aware)
summary(ModelXGB)
plot(varImp(ModelXGB, scale=F))

my_control <- trainControl(
  method='cv',
  number=1, repeats=5,
    classProbs=T,
  savePredictions = "final",
  index=createResample(training$aware, 25),
  summaryFunction=twoClassSummary
)

set.seed(7)
clist <- caretList(aware~dm001+age+sex+mar4+ph002+ph003+DISimpairments+local3+
                     emp3+SOCtotalChild+SOCProxChld+hu005+SOCrelFriends+COGmoca+
                     BEHalc_drinksperweek+BEHsmoker+MHucla_loneliness+MHcasp19_total+
                     MHcesd_capi+mh023+SOCliveswith5+as101+as103+as108+as111+as112+
                     hw108+IPAQexercise3+MHpennworry+COGimmediaterecall1+jh103+
                     as108+MHcapi_loneliness, data=training,
                   methodList=c("xgbTree", "parRF"),
                   trControl=my_control)
ModelE <- caretEnsemble(clist, iter=1000L)
confusionMatrix(predict(clist, newdata=testing), testing$aware)

myControl <- trainControl(method='cv', number=folds, repeats=repeats, 
                          returnResamp='none', classProbs=TRUE,
                          returnData=FALSE, savePredictions=TRUE, 
                          verboseIter=TRUE, allowParallel=TRUE,
                          summaryFunction=twoClassSummary,
                          index=createMultiFolds(training$aware, k=folds, times=repeats))

clist2 <- caretList(
  aware~dm001+age+sex+mar4+ph002+ph003+DISimpairments+local3+
    emp3+SOCtotalChild+SOCProxChld+hu005+SOCrelFriends+COGmoca+
    BEHalc_drinksperweek+BEHsmoker+MHucla_loneliness+MHcasp19_total+
    MHcesd_capi+mh023+SOCliveswith5+as101+as103+as108+as111+as112+
    hw108+IPAQexercise3+MHpennworry+COGimmediaterecall1+jh103+
    as108+MHcapi_loneliness, data=training,
  trControl=myControl,
  metric='ROC',
  methodList=c('xgbTree', 'lda2'),
  tuneList=list(
    rf1=caretModelSpec(method='rf', importance=TRUE),
    nn=caretModelSpec(method='nnet', tuneLength=2, trace=FALSE)
  )
)
ModelF <- caretEnsemble(clist2, iter=1000L)
plot(ModelF)
summary(ModelF)
varImp(ModelF)
Imp1 <- varImp(ModelF)

testPred <- predict(ModelF, newdata=testing)

sensitivity(testPred, testing$aware)
