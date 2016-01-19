library(foreign)
TILDA50 <- read.dta("./TILDA/TILDA1.dta")
library(sqldf)
TILDA <- sqldf("SELECT * FROM TILDA50 WHERE age >= 65")
library(ggplot2)
summary(TILDA$cs006)
TILDA$agecat [TILDA$age >= 50 & TILDA$age < 65] <- "50 to 64"
TILDA$agecat [TILDA$age >= 65 & TILDA$age < 75] <- "65 to 74"
TILDA$agecat [TILDA$age >= 75] <- "75+"
TILDA$agecat <- as.factor(TILDA$agecat)
summary(TILDA$agecat)

TILDA$MHcapi_loneliness <- as.factor(TILDA$MHcapi_loneliness)
TILDA$MHucla_loneliness <- as.factor(TILDA$MHucla_loneliness)
TILDA$Lonely [TILDA$MHcapi_loneliness == "All of the time" | TILDA$MHcapi_loneliness == "Moderate amount of the time"] <- "Yes"
TILDA$Lonely [TILDA$MHcapi_loneliness == "Rarely or never" | TILDA$MHcapi_loneliness == "Some of the time"] <- "No"
TILDA$Lonely <- as.factor(TILDA$Lonely)
good <- complete.cases(TILDA$Lonely)
TILDA <- TILDA[good, ]

Table1 <- table(TILDA$Lonely [TILDA$agecat == "50 to 64"])
Table2 <- round(prop.table(Table1), digits=2)
Merge <- cbind(Table1,Table2)
Merge <- as.data.frame(Merge)
colSums(Merge)
colnames(Merge) <- c("Count", "Percentage")
library(scales)
g <- ggplot(TILDA, aes(x=agecat, fill=marstatus)) + geom_bar(aes(y = (..count..)/sum(..count..)))
g + facet_grid(. ~ sex) + scale_fill_brewer(palette=3) + scale_y_continuous(labels=percent) + xlab("Age category") + ylab("Percent")

