##################set up##################
setwd("~/Downloads/2019Fall/628/Module2")

BodyFatData <- read.csv('BodyFat.csv')

##################Data Clean##################
head(BodyFatData)

#--------------summary table to see extreme value--------------
# summary of the data, there are extreme values of bodyfat percentage which is abnormal
summary(BodyFatData[, -1])

# Body fat percentage extreme values
check.index <- which(BodyFatData$BODYFAT<2 | BodyFatData$BODYFAT >39)
BodyFatData[check.index, ]
# *172* is kind of the lowest possible value for Essential fat
# *216* is sever obesity, which may also occur
# *182* - delete it, since it's impossible to be 0 bodyfat percentage
# applied the siri's equation, the bodyfat becomes negative, we filter it out of our analysis
495/BodyFatData$DENSITY[182] - 450
BodyFatData <- BodyFatData[-182, ]

# *39* The highest weight also makes sense, other largest values also make sense
BodyFatData[which(BodyFatData$WEIGHT==max(BodyFatData$WEIGHT)),]

# *42* - fix his height, The lowest height does not make sense
check.index <- which(BodyFatData$HEIGHT==min(BodyFatData$HEIGHT))
BodyFatData[check.index,]
BodyFatData$HEIGHT[check.index] <- sqrt(703*BodyFatData$WEIGHT[check.index]/BodyFatData$ADIPOSITY[check.index])

#--------------Check siri's formula--------------
# calculate bodyfat using siri's equation
bodyfat_siri <- 495/BodyFatData$DENSITY - 450 
summary(bodyfat_siri-BodyFatData$BODYFAT)
plot(BodyFatData$BODYFAT~bodyfat_siri)
fit <- lm(BodyFatData$BODYFAT~bodyfat_siri)
par(mfrow = c(1,2))
plot(fit, which = c(1,2)) # find three abnormal records: 48, 76, 96
dev.off()
check.index <- c(48, 76, 96)
bodyfat_siri[check.index]
BodyFatData[check.index,]
# *96*, his other variables are all normal, which indicates his desity might be wrongly recorded
# *76 and 48 - fix their bodyfat 
# their variables are similar to each other > body fat should also be similar, use siri's result
BodyFatData$BODYFAT[76] <- bodyfat_siri[76]
BodyFatData$BODYFAT[48] <- bodyfat_siri[48]

#--------------Check BMI--------------
# BMI's formula
BMI <- (703*BodyFatData$WEIGHT)/(BodyFatData$HEIGHT)^2
summary(BMI-BodyFatData$ADIPOSITY)
plot(BodyFatData$ADIPOSITY~BMI)
fit <- lm(BodyFatData$ADIPOSITY~BMI)
par(mfrow = c(1,2))
plot(fit, which = c(1,2)) # find three abnormal records: 163, 220, 234
dev.off()
check.index <- c(163, 220, 234)
BMI[check.index]
BodyFatData[check.index,]
# *163, 220, 234* - fix their BMI, their BMI should not be similar to each other
BodyFatData[163, "ADIPOSITY"] <- BMI[163]
BodyFatData[220, "ADIPOSITY"] <- BMI[220]
BodyFatData[234, "ADIPOSITY"] <- BMI[234]

# fit again, we found new possible outliers
BMI <- (703*BodyFatData$WEIGHT)/(BodyFatData$HEIGHT)^2
fit <- lm(BodyFatData$ADIPOSITY~BMI)
par(mfrow = c(1,2))
plot(fit, which = c(1,2)) # 46, 116, 156
dev.off()
check.index <- c(46, 116, 156)
BMI[check.index]
BodyFatData[check.index,]
# there is no evidence to tell what's wrong with these records, keep them

#--------------Check linear model to find outliers--------------
fit <- lm(BODYFAT ~ ., data = BodyFatData[, c(2, 4:6, 8:17)])
# check cook's distance
par(mfrow = c(1,1))
plot(fit, which = 4) #  39, 54, 86 need to be checked 
check.index <- c(39, 86, 221)
BodyFatData[check.index,]
# all the three records make sense



