library(foreign)
library(sqldf)
library(ggplot2)
library(plyr)
TILDA <- read.dta("./TILDA/TILDA1.dta")

TILDA$agecat [TILDA$age >= 50 & TILDA$age < 65] <- "50 to 64"
TILDA$agecat [TILDA$age >= 65 & TILDA$age < 75] <- "65 to 74"
TILDA$agecat [TILDA$age >= 75] <- "75+"
TILDA$agecat <- as.factor(TILDA$agecat)

TILDA$nodrive [TILDA$dr005 == 1] <- "Yes" 
TILDA$nodrive [TILDA$dr005 != 1] <- "No" 

ddply(TILDA, c("agecat","sex"), function(TILDA)aov(TILDA$mh023~nodrive, na.rm=T))

TILDA$drive [TILDA$dr004 == 1] <- "Yes"
TILDA$drive [TILDA$dr004 == 5] <- "No"

t.test(mh023~nodrive, data=subset(TILDA, TILDA$sex=="Male"))
t.test(mh023~nodrive, data=subset(TILDA, TILDA$sex=="Female"))

t.test(mh023~nodrive, data=subset(TILDA, TILDA$agecat=="50 to 64"))
t.test(mh023~nodrive, data=subset(TILDA, TILDA$agecat=="65 to 74"))
t.test(mh023~nodrive, data=subset(TILDA, TILDA$agecat=="75+"))

t.test(mh023~nodrive, data=subset(TILDA, TILDA$local3 == "Dublin city or county"))
t.test(mh023~nodrive, data=subset(TILDA, TILDA$local3 == "Another town or city"))
t.test(mh023~nodrive, data=subset(TILDA, TILDA$local3 == "A rural area"))

t.test(MHcasp19_total~nodrive, data=subset(TILDA, TILDA$sex=="Male"))
t.test(MHcasp19_total~nodrive, data=subset(TILDA, TILDA$sex=="Female"))

t.test(MHcasp19_total~nodrive, data=subset(TILDA, TILDA$agecat=="50 to 64"))
t.test(MHcasp19_total~nodrive, data=subset(TILDA, TILDA$agecat=="65 to 74"))
t.test(MHcasp19_total~nodrive, data=subset(TILDA, TILDA$agecat=="75+"))

t.test(MHucla_loneliness~nodrive, data=subset(TILDA, TILDA$sex=="Male"))
t.test(MHucla_loneliness~nodrive, data=subset(TILDA, TILDA$sex=="Female"))

t.test(MHucla_loneliness~nodrive, data=subset(TILDA, TILDA$agecat=="50 to 64"))
t.test(MHucla_loneliness~nodrive, data=subset(TILDA, TILDA$agecat=="65 to 74"))
t.test(MHucla_loneliness~nodrive, data=subset(TILDA, TILDA$agecat=="75+"))

with(subset(TILDA, TILDA$agecat=="50 to 64"), mean(mh023, na.rm=T))
with(subset(TILDA, TILDA$agecat=="65 to 74"), mean(mh023, na.rm=T))
with(subset(TILDA, TILDA$agecat=="75+"), mean(mh023, na.rm=T))

dailypat <- aggregate(mh023 ~ age, TILDA, mean)
ggplot(dailypat, aes(x=age, y=mh023)) + geom_line()
ggplot(dailypat, aes(x=age, y=mh023)) + geom_line() + scale_y_reverse()

dailypat <- aggregate(MHcasp19_total ~ age, TILDA, mean)
ggplot(dailypat, aes(x=age, y=MHcasp19_total)) + geom_line()
ggplot(dailypat, aes(x=age, y=MHcasp19_total)) + geom_line() + scale_y_reverse()

TILDAvars <- sqldf("SELECT nodrive, MHucla_loneliness, age,sex,mar4,local3,SOCliveswith5,ph001,ph002,ph003,
                   mh023,dr002,dr003,dr004,dr005,dr006,dr007,dr009,dr010,dr011,dr012, dr013,dr014,
                   dr015,DISimpairments,MHcasp19_total, dr001_01, dr001_02,dr001_03, dr001_04,
                   dr001_05,dr001_06,dr001_07,dr001_08,dr001_09,dr001_10,dr001_11,dr001_12,
                   dr001_15 FROM TILDA")

n1 <- summary(TILDAvars$dr001_01); n2 <- summary(TILDAvars$dr001_02); n3 <- summary(TILDAvars$dr001_03) 
n4 <- summary(TILDAvars$dr001_04); n5 <- summary(TILDAvars$dr001_05); n6 <- summary(TILDAvars$dr001_06)
n7 <- summary(TILDAvars$dr001_07); n8 <- summary(TILDAvars$dr001_08); n9 <- summary(TILDAvars$dr001_09)
n10 <- summary(TILDAvars$dr001_10); n11 <- summary(TILDAvars$dr001_11); n12 <- summary(TILDAvars$dr001_12)
n13 <- summary(TILDAvars$dr001_15)
Freq <- as.data.frame(cbind(n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13))
rownames(Freq) <- c("No", "Yes")
colnames(Freq) <- c("Bikeormbike", "Driveself", "Drivefam", "Drivefriend", "Buscity", "Busintercity",
"Busrural", "Taxi", "DARTLuas", "Commutertrain", "Intercitytrain", "RTSbus", "None")
Freq <- round(Freq / colSums(Freq), digits=2)

ggplot(TILDAvars, aes(x=as.factor(dr002))) + geom_bar()


Drivefreq <- as.data.frame(summary(as.factor(TILDAvars$dr003)))
Drivefreq <- round(Drivefreq / colSums(Drivefreq), digits=2)
rownames(Drivefreq) <- c("NA", "Not at all", "1-2 per week", "3-4 per week", "5-6 per week", "Every day", "Don't know")

Driveless <- as.data.frame(summary(as.factor(TILDAvars$dr004)))
Driveless <- round(Driveless / colSums(Driveless), digits=2)
rownames(Driveless) <- c("NA", "Yes", "No", "Don't know", "Refused")

d1 <- summary(as.factor(TILDAvars$dr005))
d2 <- summary(as.factor(TILDAvars$dr006))
d3 <- summary(as.factor(TILDAvars$dr007))
d1 <- as.data.frame(rbind(d1,d2,d3))
d1[1,4] <- 0
d1 <- round(d1 / rowSums(d1), digits = 2)
colnames(d1) <- c("NA", "Yes", "No", "Don't know")

s1 <- summary(as.factor(TILDA$dr008_01)); s2 <- summary(as.factor(TILDA$dr008_02)); s3 <- summary(as.factor(TILDA$dr008_03)) 
s4 <- summary(as.factor(TILDA$dr008_04)); s5 <- summary(as.factor(TILDA$dr008_05)); s6 <- summary(as.factor(TILDA$dr008_06))
s7 <- summary(as.factor(TILDA$dr008_07)); s8 <- summary(as.factor(TILDA$dr008_08)); s9 <- summary(as.factor(TILDA$dr008_09))
s10 <- summary(as.factor(TILDA$dr008_10)); s11 <- summary(as.factor(TILDA$dr008_11))
Stop <- as.data.frame(cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11))
Stop <- round(Stop / colSums(Stop), digits=3)
colnames(Stop) <- c("Eyesight", "Hearing", "Physical incapacity", "Memory", "Do not want to", "Told by doctor", "Told by family", "Too expensive", "Other reason", "Don't know")
Stop

Rural <- subset(TILDA, local3 == "A rural area")
Urban <- subset(TILDA, local3 != "A rural area")

s1 <- summary(as.factor(Rural$dr008_01)); s2 <- summary(as.factor(Rural$dr008_02)); s3 <- summary(as.factor(Rural$dr008_03)) 
s4 <- summary(as.factor(Rural$dr008_04)); s5 <- summary(as.factor(Rural$dr008_05)); s6 <- summary(as.factor(Rural$dr008_06))
s7 <- summary(as.factor(Rural$dr008_07)); s8 <- summary(as.factor(Rural$dr008_08)); s9 <- summary(as.factor(Rural$dr008_09))
s10 <- summary(as.factor(Rural$dr008_10)); s11 <- summary(as.factor(Rural$dr008_11))
Stop <- as.data.frame(cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11))
Stop <- round(Stop / colSums(Stop), digits=3)
colnames(Stop) <- c("Eyesight", "Hearing", "Physical incapacity", "Memory", "Do not want to", "Told by doctor", "Told by family", "Too expensive", "Other reason", "Don't know")
Stop

s1 <- summary(as.factor(Urban$dr008_01)); s2 <- summary(as.factor(Urban$dr008_02)); s3 <- summary(as.factor(Urban$dr008_03)) 
s4 <- summary(as.factor(Urban$dr008_04)); s5 <- summary(as.factor(Urban$dr008_05)); s6 <- summary(as.factor(Urban$dr008_06))
s7 <- summary(as.factor(Urban$dr008_07)); s8 <- summary(as.factor(Urban$dr008_08)); s9 <- summary(as.factor(Urban$dr008_09))
s10 <- summary(as.factor(Urban$dr008_10)); s11 <- summary(as.factor(Urban$dr008_11))
Stop <- as.data.frame(cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11))
Stop <- round(Stop / colSums(Stop), digits=3)
colnames(Stop) <- c("Eyesight", "Hearing", "Physical incapacity", "Memory", "Do not want to", "Told by doctor", "Told by family", "Too expensive", "Other reason", "Don't know")
Stop

r1 <- summary(as.factor(TILDAvars$dr010))
r2 <- summary(as.factor(TILDAvars$dr011))
r3 <- summary(as.factor(TILDAvars$dr012))
r1 <- as.data.frame(rbind(r1,r2,r3))
r1 <- r1[ , 2:6]
r1 <- round(r1 / rowSums(r1), digits = 2)
colnames(r1) <- c("Never", "Rarely", "Some of the time", "Most of the time", "All of the time")
rownames(r1) <- c("Affect social", "Affect business", "Affect health appointments")

Private <- as.data.frame(summary(TILDAvars$dr013))
Private <- round(Private / colSums(Private), digits=2)
Public <- as.data.frame(summary(TILDAvars$dr014))
Public <- round(Public / colSums(Public), digits=2)

barplot(summary(TILDAvars$dr015))
lapply(split(TILDAvars$dr015, TILDAvars$local3), summary)

TILDAvars$mh023 <- as.factor(TILDAvars$mh023)
TILDAvars$mh023[TILDAvars$mh023 ==99] <- NA
TILDAvars$mh023[TILDAvars$mh023 ==98] <- NA 
g <- ggplot(TILDAvars, aes(x=dr014, fill=mh023))
g + geom_bar() + scale_fill_brewer(palette=1)

TILDAvars$MHucla_loneliness <- as.numeric(TILDAvars$MHucla_loneliness)
Lonely <- ggplot(na.omit(TILDAvars), aes(x=dr014, fill=as.factor(MHucla_loneliness)))
Lonely + geom_bar() + scale_fill_brewer(palette=1)


