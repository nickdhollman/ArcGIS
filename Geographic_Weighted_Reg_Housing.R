library(tidyverse)
library(readr)
library(psych)
library(summarytools)

ames <- read.csv("#### path deleted to maintain anonymity###/Ames2.csv", header=TRUE)

#descriptive stats
describe(ames$SalePrice)
describe(ames$TotalBsmtSF)
describe(ames$OverallQual)
describe(ames$LotArea)
#remove person missing TotalBsmtSF
ames_b <- ames %>% drop_na(TotalBsmtSF)
describe(ames_b$SalePrice)
describe(ames_b$TotalBsmtSF)
describe(ames_b$OverallQual)
describe(ames_b$OverallCond)
describe(ames_b$GrLivArea)
describe(ames_b$LotArea)
#Histogram creation

############### SALE PRICE ################
options(scipen=999)
ggplot(ames_b, aes(SalePrice)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()
ggplot(ames_b, aes(sample=SalePrice)) +
  stat_qq() +
  stat_qq_line() +
  theme_classic() +
  labs(
    title = "QQ Plot",
    x = "Theoretical",
    y = "SalePrice"
  )

ames_b <- ames_b %>% mutate(logSale = log(SalePrice))
describe(ames_b$logSale)

ggplot(ames_b, aes(logSale)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()
ggplot(ames_b, aes(sample=logSale)) +
  stat_qq() +
  stat_qq_line() +
  theme_classic() +
  labs(
    title = "QQ Plot",
    x = "Theoretical",
    y = "ln(SalePrice)"
  )

############ TOTAL BASEMENT SQUARE FEET ##############

ggplot(ames_b, aes(TotalBsmtSF)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()
ggplot(ames_b, aes(sample=TotalBsmtSF)) +
  stat_qq() +
  stat_qq_line() +
  theme_classic() +
  labs(
    title = "QQ Plot",
    x = "Theoretical",
    y = "total Basement Square Feet"
  )

ames_b <- ames_b %>% mutate(logBase = log(TotalBsmtSF))
ames_b <- ames_b %>% mutate(sqrtBase = sqrt(TotalBsmtSF))
ggplot(ames_b, aes(logBase)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()
ggplot(ames_b, aes(sample=logBase)) +
  stat_qq() +
  stat_qq_line() +
  theme_classic() +
  labs(
    title = "QQ Plot",
    x = "Theoretical",
    y = "ln(Total Basement Square Feet)"
  )
ggplot(ames_b, aes(sample=sqrtBase)) +
  stat_qq() +
  stat_qq_line() +
  theme_classic() +
  labs(
    title = "QQ Plot",
    x = "Theoretical",
    y = "sqrt(Total Basement Square Feet)"
  )

################# OVERALL QUALITY OF THE HOUSE ##############
ggplot(ames_b, aes(OverallQual)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()
ggplot(ames_b, aes(sample=OverallQual)) +
  stat_qq() +
  stat_qq_line() +
  theme_classic() +
  labs(
    title = "QQ Plot",
    x = "Theoretical",
    y = "Overall Quality"
  )

################ OVERALL CONDITION OF THE HOUSE #############
ames_b <- ames_b %>% mutate(logCond = log(OverallCond))
ggplot(ames_b, aes(OverallCond)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()
ggplot(ames_b, aes(sample=OverallCond)) +
  stat_qq() +
  stat_qq_line() +
  theme_classic() +
  labs(
    title = "QQ Plot",
    x = "Theoretical",
    y = "Overall Condition"
  )

ggplot(ames_b, aes(logCond)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()
ggplot(ames_b, aes(sample=logCond)) +
  stat_qq() +
  stat_qq_line() +
  theme_classic() +
  labs(
    title = "QQ Plot",
    x = "Theoretical",
    y = "ln(Overall Condition)"
  )

############# Above Ground Living Area ###########
ggplot(ames_b, aes(GrLivArea)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()
ggplot(ames_b, aes(sample=GrLivArea)) +
  stat_qq() +
  stat_qq_line() +
  theme_classic() +
  labs(
    title = "QQ Plot",
    x = "Theoretical",
    y = "Above Ground Living Area"
  )

########## TOTAL LOT AREA ###########
ggplot(ames_b, aes(LotArea)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()
ggplot(ames_b, aes(sample=LotArea)) +
  stat_qq() +
  stat_qq_line() +
  theme_classic() +
  labs(
    title = "QQ Plot",
    x = "Theoretical",
    y = "Total Lot Area"
  )

ames_b <- ames_b %>% mutate(logLotArea = log(LotArea))
ggplot(ames_b, aes(logLotArea)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()
ggplot(ames_b, aes(sample=logLotArea)) +
  stat_qq() +
  stat_qq_line() +
  theme_classic() +
  labs(
    title = "QQ Plot",
    x = "Theoretical",
    y = "ln(Total Lot Area)"
  )

write.csv(ames_b, "C:/#### path deleted to maintain anonymity###/Ames_logprice_B.csv", row.names = TRUE)