library(foreign)
library(ggplot2)
library(sqldf)
library(caret)
library(cluster)
library(varSelRF)
TILDA <- read.dta("./TILDA/TILDA1.dta")
TILDA$INCASSassets <- with(TILDA, as101+as103+as108+as111+as112+hw108)

# DISlongterm includes ph003 & ph004

Health <- sqldf("SELECT BPhypertension, age, sex, mar4, local3, SOCliveswith5, Frbmi, cm006, dm001, dm004, dm005,
                dm006, dm011, dm015, dm017, ph001, ph002, ph003, ph004, ph005, ph009, ph101, ph102,
                ph112, ph113, ph114, ph401, ph405, ph408, ph501, ph601, ph701, ph702, hu001,
                hu002, hu005, hu007, hu008, hu009, hu010, hu011, hu012, hu015_01, hu015_08, mh023, we001, we004,
                we005, jh103, jh105, le101, bh001, bh002, bh101, bh107, bh105, bh201, bh202, bh203,
                tp036, tp037, dm020, dm022, si101, si201, hw105, ex101, ex104, ex105, dr002, dr013,
                dr014, dr015, height, weight, Frwaist, emp3, DISimpairments, DISadl, DISiadl,
                DISdisab3, DISspouse, DISchild, DISotherrel, DISvision, DIShearing,
                DISconverse1, DISconverse4, MDmeds, MHcesd_capi, MHhadsa_scq, MHcasp19_total, MHpennworry,
                MHucla_loneliness, MHcapi_loneliness, COGimmediaterecall1, COGimmediaterecall2,
                COGdelayedrecall, IPAQmetminutes, IPAQexercise3, INCASSassets, SESsocial_class, SOChomeChild, SOCtotalChild, SOCProxChld, SOCpalive, tc018,
                tc021, tc008, tc016, SOCsocParticip, SOCSandwich, SOChelpfromNeighbour, SOChelptoNeighbour, SOCChurch,
                SOCClubs, SOCrelFriends, SOCyearlyactivity, SOCmonthlyactivity, SOCweeklyactivity,
                SOCsocnet, BEHsmoker, BEHcage FROM TILDA")

Health$ph701 <- as.numeric(Health$ph701); Health$ph702 <- as.numeric(Health$ph702)
Health$hu015_01 <- as.numeric(Health$hu015_01)

Health$hu005 [Health$hu005 == -99 | Health$hu005 == -98] <- NA
Health$hu007 [Health$hu007 == -99 | Health$hu007 == -98] <- NA
Health$hu008 [Health$hu008 == -99 | Health$hu008 == -98] <- NA
Health$hu009 [Health$hu009 == -99 | Health$hu009 == -98] <- NA
Health$hu010 [Health$hu010 == -99 | Health$hu010 == -98] <- NA
Health$hu011 [Health$hu011 == -99 | Health$hu011 == -98] <- NA
Health$hu012 [Health$hu012 == -99 | Health$hu012 == -98] <- NA

Health$hu005 [Health$hu005 == -1] <- 0
Health$hu007 [Health$hu007 == -1] <- 0
Health$hu008 [Health$hu008 == -1] <- 0
Health$hu009 [Health$hu009 == -1] <- 0
Health$hu010 [Health$hu010 == -1] <- 0
Health$hu011 [Health$hu011 == -1] <- 0
Health$hu012 [Health$hu012 == -1] <- 0

Health <- na.omit(Health)

# Health <- scale(Health)

Subset1 <- subset(Health, select=c(MDmeds, hu005, hu015_01, ph701, ph702, hu007, hu008, hu009, hu010, hu011, hu012))
set.seed(42)
kmeans1 <- kmeans(Subset1, centers=2)
aggregate(Subset1, by=list(kmeans1$cluster), FUN=mean)


clusplot(Health, kmeans1$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)
library(fpc)
plotcluster(Health, kmeans1$cluster) 

Health$clusters <- kmeans1$cluster
Health$clusters [Health$clusters == 1] <- "High"
Health$clusters [Health$clusters == 2] <- "Low"
Health$clusters <- as.factor(Health$clusters)
summary(Health$clusters)

aggregate(Health$MDmeds, by=list(Health$clusters), FUN=mean)
sum(is.na(Health))

Health[sapply(Health, is.character)] <- lapply(Health[sapply(Health, is.character)], as.factor)

Health <- subset(Health, select= -c(MDmeds, hu005, hu015_01, hu007, ph701, ph702, hu008, hu009, hu010, hu011, hu012))

set.seed(660)
inTrain <- createDataPartition(y=Health$clusters, p=.70, list=FALSE)
training <- Health[inTrain,]
testing <- Health[-inTrain,]

xgb_grid_1 <- expand.grid(nrounds = 2, eta = 0.3, max_depth = 3)

ModelXGB <- train(clusters~., data=training, method="xgbTree")
predictXGB <- predict(ModelXGB, newdata=testing)
confusionMatrix(predictXGB, testing$clusters)
ImportanceXGB <- varImp(ModelXGB, scale=FALSE)
plot(ImportanceXGB, 20)

ModelRF <- train(clusters~., data=training, method="rf")
predictRF <- predict(ModelRF, newdata=testing)
confusionMatrix(predictRF, testing$clusters)
ImportanceRF <- varImp(ModelRF)
plot(ImportanceRF)

ModelCF <- train(clusters~., data=training, method="cforest")
predictCF <- predict(ModelCF, newdata=testing)
confusionMatrix(predictCF, testing$clusters)
ImportanceCF <- varImp(ModelCF)
plot(ImportanceCF)

# Accuracy 0.6434
ModelRad <- train(clusters~., data=training, method="lssvmRadial")
predictRad <- predict(ModelRad, newdata=testing)
confusionMatrix(predictRad, testing$clusters)

corrplot.mixed(cor(training), lower="circle", upper="color", 
               tl.pos="lt", diag="n", order="hclust", hclust.method="complete")
