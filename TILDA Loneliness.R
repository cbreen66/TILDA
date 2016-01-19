library(foreign)
TILDA50 <- read.dta("./TILDA/TILDA1.dta")
library(sqldf)
TILDA <- sqldf("SELECT * FROM TILDA50 WHERE age >= 50")
library(ggplot2)
TILDA$agecat [TILDA$age >= 50 & TILDA$age < 65] <- "50 to 64"
TILDA$agecat [TILDA$age >= 65 & TILDA$age < 75] <- "65 to 74"
TILDA$agecat [TILDA$age >= 75] <- "75+"
TILDA$agecat <- as.factor(TILDA$agecat)
summary(TILDA$agecat)
TILDA$marstatus [TILDA$cs006 == "Married" | TILDA$cs006 == "Living with a partner as if married"] <- "Married"
TILDA$marstatus [TILDA$cs006 == "Separated" | TILDA$cs006 == "Divorced"] <- "Separated / Divorced"
TILDA$marstatus [TILDA$cs006 == "Single (never married)"] <- "Single (never married)"
TILDA$marstatus [TILDA$cs006 == "Widowed"] <- "Widowed"
TILDA$marstatus <- as.factor(TILDA$marstatus)

# Recoding the loneliness variable
TILDA$MHcapi_loneliness <- as.factor(TILDA$MHcapi_loneliness)
TILDA$MHucla_loneliness <- as.factor(TILDA$MHucla_loneliness)
TILDA$Lonely [TILDA$MHcapi_loneliness == "All of the time" | TILDA$MHcapi_loneliness == "Moderate amount of the time"] <- "Yes"
TILDA$Lonely [TILDA$MHcapi_loneliness == "Rarely or never" | TILDA$MHcapi_loneliness == "Some of the time"] <- "No"
TILDA$Lonely <- as.factor(TILDA$Lonely)
good <- complete.cases(TILDA$Lonely)
TILDA <- TILDA[good, ]

# Recoding model variables as factors
TILDA$dm017 <- as.factor(TILDA$dm017)
TILDA$ph005 <- as.factor(TILDA$ph005)
TILDA$ph321 <- as.factor(TILDA$ph321)
TILDA$we004 <- as.factor(TILDA$we004)

#Building the model
Model1 <- glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011
              +dm015+ph001+ph002+ph003+ph005+ph321+ph401+ph408
              +ph501+hu005+hu015_01+mh023+jh105+bh202+bh203, family="binomial", data=TILDA)
summary(Model1)

glm(Lonely~age, family="binomial", data=TILDA) # AIC4256
glm(Lonely~age+sex, family="binomial", data=TILDA) # AIC4206
glm(Lonely~age+sex+marstatus, family="binomial", data=TILDA) # AIC3964
glm(Lonely~age+sex+marstatus+dm001, family="binomial", data=TILDA) #AIC3957
glm(Lonely~age+sex+marstatus+dm001+dm004, family="binomial", data=TILDA) # AIC3950
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005, family="binomial", data=TILDA) #3937
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011, family="binomial", data=TILDA) #3936
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011+dm015, family="binomial", data=TILDA) #3934
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011+dm015+dm017, family="binomial", data=TILDA) #3937
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011+dm015+ph001, family="binomial", data=TILDA) #3842
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011+dm015+ph001+ph002, family="binomial", data=TILDA) #3592
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011+dm015+ph001+ph002+ph003, family="binomial", data=TILDA) #3583
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011+dm015+ph001+ph002+ph003+ph004, family="binomial", data=TILDA) #3651
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011+dm015+ph001+ph002+ph003+ph005, family="binomial", data=TILDA) #3582
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011+dm015+ph001+ph002+ph003+ph005+ph111, family="binomial", data=TILDA) #3585
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011+dm015+ph001+ph002+ph003+ph005+ph114, family="binomial", data=TILDA) #3585
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011+dm015+ph001+ph002+ph003+ph005+ph321, family="binomial", data=TILDA) #3576
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011+dm015+ph001+ph002+ph003+ph005+ph321+ph401, family="binomial", data=TILDA) #3575
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011+dm015+ph001+ph002+ph003+ph005+ph321+ph401+ph408, family="binomial", data=TILDA) #3558
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011
    +dm015+ph001+ph002+ph003+ph005+ph321+ph401+ph408+ph501, family="binomial", data=TILDA) #3551
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011
    +dm015+ph001+ph002+ph003+ph005+ph321+ph401+ph408+ph501+hu002, family="binomial", data=TILDA) #3557
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011
    +dm015+ph001+ph002+ph003+ph005+ph321+ph401+ph408+ph501+hu005, family="binomial", data=TILDA) #3551
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011
    +dm015+ph001+ph002+ph003+ph005+ph321+ph401+ph408+ph501+hu005+hu007, family="binomial", data=TILDA) #3552
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011
    +dm015+ph001+ph002+ph003+ph005+ph321+ph401+ph408+ph501+hu005+hu008, family="binomial", data=TILDA) #3552
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011
              +dm015+ph001+ph002+ph003+ph005+ph321+ph401+ph408+ph501+hu005+hu010, family="binomial", data=TILDA) #3552
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011
    +dm015+ph001+ph002+ph003+ph005+ph321+ph401+ph408
    +ph501+hu005+hu015_01, family="binomial", data=TILDA) #3545
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011
    +dm015+ph001+ph002+ph003+ph005+ph321+ph401+ph408
    +ph501+hu005+hu015_01+hu015_08, family="binomial", data=TILDA) #3546
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011
    +dm015+ph001+ph002+ph003+ph005+ph321+ph401+ph408
    +ph501+hu005+hu015_01+hu015_10, family="binomial", data=TILDA) #3547
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011
    +dm015+ph001+ph002+ph003+ph005+ph321+ph401+ph408
    +ph501+hu005+hu015_01+mh023, family="binomial", data=TILDA) #3533
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011
    +dm015+ph001+ph002+ph003+ph005+ph321+ph401+ph408
    +ph501+hu005+hu015_01+mh023+we001, family="binomial", data=TILDA) #3540
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011
    +dm015+ph001+ph002+ph003+ph005+ph321+ph401+ph408
    +ph501+hu005+hu015_01+mh023+we004, family="binomial", data=TILDA) #3535
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011
    +dm015+ph001+ph002+ph003+ph005+ph321+ph401+ph408
    +ph501+hu005+hu015_01+mh023+we005, family="binomial", data=TILDA) #3533
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011
    +dm015+ph001+ph002+ph003+ph005+ph321+ph401+ph408
    +ph501+hu005+hu015_01+mh023+jh103, family="binomial", data=TILDA) #3535
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011
    +dm015+ph001+ph002+ph003+ph005+ph321+ph401+ph408
    +ph501+hu005+hu015_01+mh023+jh105, family="binomial", data=TILDA) #3532
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011
    +dm015+ph001+ph002+ph003+ph005+ph321+ph401+ph408
    +ph501+hu005+hu015_01+mh023+jh105+le101, family="binomial", data=TILDA) #3535
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011
    +dm015+ph001+ph002+ph003+ph005+ph321+ph401+ph408
    +ph501+hu005+hu015_01+mh023+jh105+bh107, family="binomial", data=TILDA) #3534
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011
    +dm015+ph001+ph002+ph003+ph005+ph321+ph401+ph408
    +ph501+hu005+hu015_01+mh023+jh105+bh201, family="binomial", data=TILDA) #3535
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011
    +dm015+ph001+ph002+ph003+ph005+ph321+ph401+ph408
    +ph501+hu005+hu015_01+mh023+jh105+bh202, family="binomial", data=TILDA) #3498
glm(Lonely~age+sex+marstatus+dm001+dm004+dm005+dm011
    +dm015+ph001+ph002+ph003+ph005+ph321+ph401+ph408
    +ph501+hu005+hu015_01+mh023+jh105+bh202+bh203, family="binomial", data=TILDA) #3483

