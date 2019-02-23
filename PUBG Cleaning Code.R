getwd()
setwd("E:/USF - BAIS/Data Mining/Final Project/PUBG/New Data/all")
library(car)
library(dplyr)
library(corrplot)


install.packages("corrplot")


d <- read.csv("Solo_train.csv")
view(d)
head(d)
summary(d)

d1 <- d[d$Flag!= "0",]
d1 <- data.frame(d1)
d1 <- d1[,-2:-4]


names(d)
#m <- cor(cbind(d[, 3:10], d[, 13:16]))
d1<- cor(cbind(d[,2:26]))
corrplot(d1, method = "circle")

corrpl(d1)

hist(trans(d1$winPlacePerc))
View(d1)
write.csv(d1, "Solo.csv")

###############################
d <- read.csv("Solo_train.csv")
d_test <- read.csv("Solo_test.csv")
names(d_test)
attach(d_test)

d_test <- d_test[-c(1,6,18)] #Removing DBNOs, revives and Sr. No
d <- d[-c(5,14)] #Removing DBNo's and Revive because they are zero
pg_cor<- cor(cbind(d[,2:18]))
corrplot(pg_cor, method = "circle")

#Feature Reduction
d$distance <- (walkDistance + 0.48*rideDistance)
d$Eff_kill <- ((headshotKills+roadKills)/kills)
d$mapsize <- ifelse(matchDuration <= 1600,"small","large")

#Feature engg test data
d_test$distance <- (walkDistance + 0.48*rideDistance)
d_test$Eff_kill <- ((headshotKills+roadKills)/kills)
d_test$mapsize <- ifelse(matchDuration <= 1600,"small","large")


#names(d)[15] <- "Swim_Distance"
names(d)[12] <- "Num_Players"
names(d)

#dropping columns not necessary for analysis
d <- d[-c(2,5,7,10,13,14,15,17)]
View(d)
write.csv(d,"Solo_TrainCleaned.csv")

#renaming test data and Dropping columns not required for analysis 
names(d_test)[14] <- "Num_Players"
names(d_test)
d_test <- d_test[-c(2,4,5,7,8,11,13,15,16,17,18,19,21,23)]
names(d_small)
write.csv(d_test,"Solo_TestCleaned.csv")

#Splitting the data by map size

d1 <- read.csv("Solo_TrainCleaned.csv") 
d2 <- read.csv("Solo_TestCleaned.csv")

d_small <- subset(d1,d1$mapsize == "small");d_small
d_large <- subset(d1,d1$mapsize == "large");d_large
d_small_new <- subset(d_small,d_small$Num_Players > 90 & d_small$heals < 30 & d_small$boosts < 20 & d_small$weaponsAcquired < 20)
d_large_new <- subset(d_large,d_large$Num_Players > 90 & d_large$heals < 30 & d_large$boosts < 20 & d_large$weaponsAcquired < 20)

write.csv(d_small_new,"d_small_new.csv")
write.csv(d_large_new,"d_large_new.csv")

#FOR TEST DATA
d_small_test  <- subset(d2,d2$mapsize == "small" & d2$Num_Players > 90 & d2$heals < 30 & d2$boosts < 20 & d2$weaponsAcquired < 20)
d_large_test <- subset(d2,d2$mapsize == "large" & d2$Num_Players > 90 & d2$heals < 30 & d2$boosts < 20 & d2$weaponsAcquired < 20)
write.csv(d_small_test,"d_small_test.csv")
write.csv(d_large_test,"d_large_test.csv")

######################################
#Calling New Files
######################################

d_small_new <- read.csv("d_small_new.csv")
d_large_new <- read.csv("d_large_new.csv")

#d_small_new <- d_small_new[-c(3)]
#d_large_new <- d_large_new[-c(3)]

#write.csv(d_small_new,"d_small_new.csv")
#write.csv(d_large_new,"d_large_new.csv")
###############################
#All plots
###############################

par(mfrow=c(1,2)) 
#Corrplot small
names(d_small_new)
small_corr <- cor(cbind(d_small_new[,3:12]))
corrplot(small_corr, method = "circle", main = "Small Map Correlation")

#Corrplot large
names(d_large_new)
large_corr <- cor(cbind(d_large_new[,3:12]))
corrplot(large_corr, method = "circle", main = "Large Map Correlation")



#write.csv(d_small,"d_small.csv")
#write.csv(d_large,"d_large.csv")

##################################
#All Plots
##################################
#Old Data Graphs

hist(d$swimDistance, main = "Histogram of Swim_Distance", xlab = "Swim Distance")
hist(d$longestKill, main = "Histogram of LongestKill", xlab = "Longest Kill", ylim = c(0,800000))
hist(d$maxPlace, main = "Histogram of MaxPlace", xlab = "Max Place", ylim = c(0,500000))
#distance vs WinPer
plot(d1$winPlacePerc,d1$distance, xlab = "Distance Covered", ylab = "Win_Place_Prec", main = "Distance and Win Place Perc")

hist(d1$matchDuration, xlab = "Match Duration", main = "Histogram of matchDuration")
#Weapons Vs WinPer
plot(d1$winPlacePerc, d1$weaponsAcquired, xlab = "Win_Place_Prec", ylim = c(0,50), ylab = "Weapons Acquired", main = "Weapons Acquired and Win Place Perc")

###########################
#New Graphs
###########################

#Win% vs weapons acquired
par(mfrow=c(1,1)) 
plot(d_large_new$winPlacePerc, d_large_new$weaponsAcquired, ylim =c(0,30))
plot(d_small_new$winPlacePerc, d_small_new$weaponsAcquired)



par(mfrow=c(1,1)) 
plot(d$winPlacePerc, d$swimDistance)
plot(d_small$winPlacePerc, d_small$weaponsAcquired)


#Distance Vs WinPer
par(mfrow=c(1,2)) 
plot(d_large_new$winPlacePerc,d_large_new$distance, xlab = "Win place Perc", ylab = "Distance Covered")
plot(d_small_new$winPlacePerc, d_small_new$distance, xlab = "Win place Perc", ylab = "Distance Covered")



#Distance vs Weapons Collected
par(mfrow=c(1,2)) 
plot(d_large_new$winPlacePerc, d_large_new$weaponsAcquired, xlab = "Win Place Perc", ylab = "Weapons Acquired", ylim =c(0,30))
plot(d_small_new$winPlacePerc, d_small_new$weaponsAcquired, xlab = "Win Place Perc", ylab = "Weapons Acquired", ylim =c(0,30))

#distance vs damage dealt
par(mfrow=c(1,2)) 
plot(d_large_new$distance, d_large_new$damageDealt)
plot(d_small_new$distance, d_small_new$damageDealt)

#mapsize vs boosts
par(mfrow=c(1,2)) 
plot(d_large_new$matchDuration, d_large_new$boosts)
plot(d_small_new$matchDuration, d_small_new$boosts)

#Boosts Vs Kills
par(mfrow=c(1,2)) 
plot(d_large_new$kills, d_large_new$boosts)
plot(d_small_new$kills, d_small_new$boosts)

#Heals vs Damage Dealt
par(mfrow=c(1,2)) 
plot(d_large_new$heals, d_large_new$damageDealt)
plot(d_small_new$heals, d_small_new$damageDealt)



plot(d_large_new$winPlacePerc, d_large_new$weaponsAcquired, xlim=range(ROLL, en_m1$fitted.values), ylim=range(YEAR, YEAR)) 
points(en_m1$fitted.values, YEAR, col='red')

plot(winPlacePerc,d$distance)
plot(walkDistance)
hist(matchDuration)


plot(winPlacePerc,boosts)
plot(winPlacePerc, heals)

summary(d$distance)

pg_cor1<- cor(cbind(d[,2:24]))
corrplot(pg_cor1, method = "circle")

plot(winPlacePerc, kills)
plot(winPlacePerc, killStreaks)


################################
#R Model
###############################
par(mfrow=c(1,2))
hist(d_small_new$winPlacePerc, main = "Small Map", xlab = "Win_place_%", ylim = c(0,25000))
hist(d_large_new$winPlacePerc, main = "Large Map", xlab = "Win_place_%", ylim = c(0,20000))

mm <- lm(d_small_new$winPlacePerc ~ d_small_new$boosts 
         + d_small_new$damageDealt + d_small_new$heals + d_small_new$kills + d_small_new$weaponsAcquired 
         + d_small_new$distance +d_small_new$Eff_kill)
summary(mm)
