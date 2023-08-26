setwd("/Users/aliciang/Downloads")
install.packages("ggplot2")
install.packages("ggcorrplot")
install.packages("scatterplot3d")
install.packages("rpart")
install.packages("dplyr")
install.packages("tibble") # for column_to_rownames function in 3 (a)
install.packages("tidyverse") # for summarise in 3 (b)
library(ggplot2)
library(ggcorrplot)
library(scatterplot3d)
library(rpart)
library(dplyr)
library(tibble)
library(tidyverse)
rm(list = ls())
set.seed(31861148)
cvbase = read.csv("PsyCoronaBaselineExtract.csv")
cvbase <- cvbase[sample(nrow(cvbase), 40000), ] # 40000 rows
# Question 1 (a) - Describing data --------------------------------------------
dim(cvbase) # To get dimension
str(cvbase) # To show data types of all attributes
# NUMERICAL DISTRIBUTION #
numerical_data <- cvbase[,-50] # removed coded-country in col 50
summary(numerical_data)
par(mar=c(5,4,2,2)) # Adjusting margin size
# Distribution for Numeric Data within range 1 to 8
cols_to_exclude <- c(14:41) # we can exclude [14 to 41]
cols <- setdiff(1:49, cols_to_exclude) # to get col [1 to 13] and [42 to 29]
# set of colors where each number are the number of attributes in the concept
my_colors <- c(rep("violet",11), rep("orange", 2), rep("yellow", 3), rep("green", 2)
               , rep("pink", 3))
boxplot(numerical_data[, cols], col = my_colors, las=2,
        main = "Distribution for Numerical Data within ranges 1 to 8",
        cex.axis = 0.65)
legend("topleft", legend = c("Affect","Likelihood","Corona Community Injunctive norms",
                             "Trust in Government","General") ,
       col = c("violet", "orange", "yellow", "green", "pink") , pch=20 ,
       pt.cex = 2, cex = 0.7)
# Distribution for Numeric Data within range -2 to 10
cols_to_exclude <- c(21:30)
cols <- setdiff(14:41, cols_to_exclude)
my_colors <- c(rep("red", 3), rep("orange", 4), rep("yellow", 3), rep("green", 3),
               rep("pink", 3), rep("purple", 2))
boxplot(numerical_data[, cols], col = my_colors, las=2,
        main = "Distribution for Numerical Data within ranges -2 to 10", cex.axis = 0.65)
legend("topleft", legend = c("Societal Discontent","Job Insecurity",
                             "Perceived Financial Strain","Disempowerment",
                             "Life Satisfaction","Corona Community Injunctive norms") ,
       col = c("red", "orange", "yellow", "green", "pink", "purple") , pch=20 , pt.cex = 2,
       cex = 0.7)
# Distribution for Employment Status
# counts the number of 1s in each employment status
EmployStatus1 = table(cvbase$employstatus_1)
EmployStatus2 = table(cvbase$employstatus_2)
EmployStatus3 = table(cvbase$employstatus_3)
EmployStatus4 = table(cvbase$employstatus_4)
EmployStatus5 = table(cvbase$employstatus_5)
EmployStatus6 = table(cvbase$employstatus_6)
EmployStatus7 = table(cvbase$employstatus_7)
EmployStatus8 = table(cvbase$employstatus_8)
EmployStatus9 = table(cvbase$employstatus_9)
EmployStatus10 = table(cvbase$employstatus_10)
# create a frequency table by binding all previous values
EmployStatusFreqTable =cbind(EmployStatus1, EmployStatus2, EmployStatus3, EmployStatus4,
                             EmployStatus5, EmployStatus6, EmployStatus7, EmployStatus8,
                             EmployStatus9, EmployStatus10)
rownames(EmployStatusFreqTable) = c("Frequency")
par(mar=c(8,4,2,2))
barplot(EmployStatusFreqTable, main = "Distribution of Employment status",
        col="#CBC3E3", ylab = "Frequency", las = 2)
# Distribution for ProSocial Behaviours
# Generating frequency table
ProSocial01Freq = table(cvbase$c19ProSo01)
ProSocial02Freq = table(cvbase$c19ProSo02)
ProSocial03Freq = table(cvbase$c19ProSo03)
ProSocial04Freq = table(cvbase$c19ProSo04)
ProSoFreqTable = rbind(ProSocial01Freq, ProSocial02Freq, ProSocial03Freq, ProSocial04Freq)
par(mar=c(5,4,2,2))
colnames(ProSoFreqTable) <- c("Strongly disagree","Disagree","Somewhat disagree",
                              "Neither","Somewhat agree","Agree","Strongly agree")
barplot(ProSoFreqTable, main = "Distribution of ProSocial Behaviours", ylab = "Frequency",
        xlab = "Response", col = c("red","orange","yellow","green"),beside = T)
legend("topleft",c("ProSocial Behaviour 1", "ProSocial Behaviour 2", "ProSocial Behaviour 3",
                   "ProSocial Behaviour 4"), fill = c("red","orange","yellow","green"))
# NA ANALYSIS #
NumofNa = colSums(is.na(cvbase)) # counts num of NA in each col
NumofNa/400 # show in percentage
# Number of rows where every field is NA except coded_country
nrow(cvbase[rowSums(is.na(cvbase)) == ncol(cvbase)-1, ])
# TEXT ATTRIBUTES #
# Outputs the number of unique values in coded_country
dim(table(unique(unlist(cvbase$coded_country))))
# Outputs the number of empty responses for coded_country
nrow(cvbase[cvbase$coded_country == "", ])
# Question 1 (b) - Data Manipulation --------------------------------------------
# extract all rows except the ones where all attributes are NA except coded_country
cvbase = cvbase[rowSums(is.na(cvbase)) != ncol(cvbase)-1, ]
nrow(cvbase)
# eliminating rows where ALL pro-social behaviours are NA
cvbase <- cvbase[!apply(is.na(cvbase[, c(51:54)]), 1, all), ]
nrow(cvbase)
# Change all NA in employment status to O
cvbase[is.na(cvbase$employstatus_1),21]= 0
cvbase[is.na(cvbase$employstatus_2),22]= 0
cvbase[is.na(cvbase$employstatus_3),23]= 0
cvbase[is.na(cvbase$employstatus_4),24]= 0
cvbase[is.na(cvbase$employstatus_5),25]= 0
cvbase[is.na(cvbase$employstatus_6),26]= 0
cvbase[is.na(cvbase$employstatus_7),27]= 0
cvbase[is.na(cvbase$employstatus_8),28]= 0
cvbase[is.na(cvbase$employstatus_9),29]= 0
cvbase[is.na(cvbase$employstatus_10),30]= 0
# Change all NA in job insecurity to O
cvbase[is.na(cvbase$jbInsec01),17]= -3
cvbase[is.na(cvbase$jbInsec02),18]= -3
cvbase[is.na(cvbase$jbInsec03),19]= -3
cvbase[is.na(cvbase$jbInsec04),20]= -3
# Question 2 (a) - Difference in responses for Malaysia and other countries -----
# Splitting into two datas et, Malaysia and not in Malaysia
MsiaCvbase = cvbase[cvbase$coded_country == 'Malaysia', ]
OthersCvbase = cvbase[cvbase$coded_country != 'Malaysia', ]
# Removing coded_country since it's redundant now
MsiaCvbase$coded_country <- NULL
OthersCvbase$coded_country <- NULL
# Get Dimensions
dim(MsiaCvbase) 
dim(OthersCvbase)

# DIFFERENCE IN RESPONSES #
# Get number of NA in each column, divide by number of rows to get percentage of NA # comparison between Malaysia and Other countries
NApercentForMalaysia = as.data.frame(as.table(colSums(is.na(MsiaCvbase))/570)) NApercentForOthers = as.data.frame(as.table(colSums(is.na(OthersCvbase))/3943))
# Rounding
NApercentForMalaysia$Freq <- round(NApercentForMalaysia$Freq ,digit=3) NApercentForOthers$Freq <- round(NApercentForOthers$Freq ,digit=3)
# Bind to create a table
NAcomparison = cbind(NApercentForMalaysia, NApercentForOthers[,2])
# Removing Job Insecurity and Employment Status as NA are replaced previously 
NAcomparison <- subset(NAcomparison, !(row.names(NAcomparison) %in% c(17,18,19,20,21,22,23,24,25,26,27,28,29,30)))
# Renaming columns
colnames(NAcomparison) = c("Attributes", "NAinMalaysia","NAinOtherCountries" )
# Sum of columns where % of NA in Malaysia is less than Other Countries sum(NAcomparison$NAinMalaysia < NAcomparison$NAinOtherCountries)
# Scatter plot for % of NA values in Malaysia Vs Other Countries colors <- c("Malaysia" = "red", "Other countries" = "blue") ggplot(NAcomparison, aes(x = Attributes)) +
# Output : 39
geom_point(aes(y = NAinMalaysia, color = "Malaysia"), size = 1.5) + geom_point(aes(y = NAinOtherCountries, color = "Other countries"), size = 1.5) + labs(x = "Attributes", y = "Percentage of NA values", color = "Legend") + scale_color_manual(values = colors) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = c(.95, .95),legend.justification = c("right", "top"),
        legend.box.just = "right",legend.margin = margin(6, 6, 6, 6) ) + ggtitle("Percentage of NA values between Malaysia and Other Countries") + coord_cartesian( ylim = c(0, 0.175))
# Question 2 (b) - Predictors for Malaysia -------------------------------------- MsiaCvbase = na.omit(MsiaCvbase) # Remove rows with NA for correlation
# Correlation list for all Pro-socials
MsiaCorr01 = round(cor(MsiaCvbase[,1:49],MsiaCvbase[,50]), digits= 3) MsiaCorr02 = round(cor(MsiaCvbase[,1:49],MsiaCvbase[,51]), digits= 3) MsiaCorr03 = round(cor(MsiaCvbase[,1:49],MsiaCvbase[,52]), digits= 3) MsiaCorr04 = round(cor(MsiaCvbase[,1:49],MsiaCvbase[,53]), digits= 3)
# CORRELATION MATRIX #
corr_matrix = cor(MsiaCvbase[,c(42,44,46,50:53)]) ggcorrplot(corr_matrix, hc.order =FALSE, type = "lower", lab = TRUE,
                                                             title = "Strong Correlation Predictors Summary (Malaysia)")
# Strongest correlation with trustGovState for c19ProSo03
Msiacorr = round(cor(MsiaCvbase[,1:49],MsiaCvbase[,52]), digits= 3) colnames(Msiacorr) = c("Correlation")
fit_MsiaCvbase = lm(c19ProSo03 ~ trustGovState, data = MsiaCvbase) ggplot(MsiaCvbase, aes(x=trustGovState, y=c19ProSo03) ) + geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", se = FALSE) # LINEAR REGRESSION #
# Linear regression with all predictors for c19ProSo01
fit_MsiaCvbase = lm(c19ProSo01 ~ affAnx + affCalm + affContent + affBor + affEnerg + affDepr +
                      affExc + affNerv + affExh + affInsp + affRel + PLRAC19 + PLRAEco + disc01 + disc02 + disc03 + jbInsec01 + jbInsec02 + jbInsec03 + jbInsec04 + employstatus_1 + employstatus_2 + employstatus_3 + employstatus_4 + employstatus_5 + employstatus_6 + employstatus_7 + employstatus_8 + employstatus_9 + employstatus_10 + PFS01 + PFS02 + PFS03 + fail01 + fail02 + fail03 + happy + lifeSat + MLQ + c19NormShould + c19NormDo + c19IsStrict + c19IsPunish + c19IsOrg + trustGovCtry + trustGovState +
                      gender + age + edu , data = MsiaCvbase) summary(fit_MsiaCvbase)
# Strong predictors : employstatus_10, PLRAC19, MLQ and employstatus_7 # orders p-values from lowest to highest
modcoef <- summary(fit_MsiaCvbase)[["coefficients"]] modcoef[order(modcoef[ , 4]), ]
# Attempts to reduce R^2 value

# Attempt 1 - removing predictors with highest correlation ()
fit_MsiaCvbase = lm(c19ProSo01 ~ affAnx + affCalm + affContent + affBor + affEnerg + affDepr +
                      affExc + affNerv + affExh + affInsp + affRel + PLRAC19 + PLRAEco + disc01 + disc02 + disc03 + jbInsec01 + jbInsec02 + jbInsec03 + jbInsec04 + employstatus_1 + employstatus_2 + employstatus_3 + employstatus_4 + employstatus_5 + employstatus_6 + employstatus_7 + employstatus_8 + employstatus_9 + employstatus_10 + PFS01 + PFS02 + PFS03 + fail01 + fail02 + fail03 + happy + lifeSat + MLQ + c19NormShould + c19NormDo + c19IsStrict + c19IsPunish + c19IsOrg + trustGovCtry + gender + age + edu , data = MsiaCvbase)
summary(fit_MsiaCvbase) # R^2 value : 0.1375
# Attempt 2 - removing predictors with high p-value
# if up until employstatus_9 was removed (the 6th predictor with largest p-value)
fit_MsiaCvbase = lm(c19ProSo01 ~ affBor + affEnerg + affDepr + affNerv + affExh + affInsp +
                      affRel + PLRAC19 + PLRAEco + disc01 + disc02 + disc03 + jbInsec01 +
                      jbInsec02 + jbInsec03 + jbInsec04 + employstatus_1 + employstatus_3 +
                      employstatus_4 + employstatus_5 + employstatus_6 + employstatus_7 +
                      employstatus_8 + employstatus_10 + PFS01 + PFS02 +
                      PFS03 + fail01 + fail02 + fail03 + happy + lifeSat + MLQ +
                      c19NormShould + c19NormDo + c19IsStrict + c19IsPunish + c19IsOrg +
                      trustGovCtry + trustGovState + gender + age + edu , data = MsiaCvbase)
summary(fit_MsiaCvbase) # R^2 value : 0.1438 , which is lesser than original
# the best set of predictors to use is to eliminate the top 5 largest p-values
# (affAnx, affExc, affCalm, affContent and employstatus_2)
fit_MsiaCvbase = lm(c19ProSo01 ~ affBor + affEnerg + affDepr + affNerv + affExh + affInsp +
                      affRel + PLRAC19 + PLRAEco + disc01 + disc02 + disc03 + jbInsec01 +
                      jbInsec02 + jbInsec03 + jbInsec04 + employstatus_1 + employstatus_3 +
                      employstatus_4 + employstatus_5 + employstatus_6 + employstatus_7 +
                      employstatus_8 + employstatus_9 + employstatus_10 + PFS01 + PFS02 +
                      PFS03 + fail01 + fail02 + fail03 + happy + lifeSat + MLQ +
                      c19NormShould + c19NormDo + c19IsStrict + c19IsPunish + c19IsOrg +
                      trustGovCtry + trustGovState + gender + age + edu , data = MsiaCvbase)
summary(fit_MsiaCvbase)
# R^2 value : 0.1439 but with reduced RSE and overall p-value compared to original
# Strong predictors : employstatus_10, PLRAC19, MLQ and employstatus_7 which is still same
# Relationship between the 2 strong predictors
qplot(PLRAC19,c19ProSo01, data = MsiaCvbase, size = employstatus_10)
# Linear regression with all predictors for c19ProSo02
fit_MsiaCvbase = lm(c19ProSo02 ~ affAnx + affCalm + affContent + affBor + affEnerg + affDepr +
                      affExc + affNerv + affExh + affInsp + affRel + PLRAC19 + PLRAEco +
                      disc01 + disc02 + disc03 + jbInsec01 + jbInsec02 + jbInsec03 +
                      jbInsec04 + employstatus_1 + employstatus_2 + employstatus_3 +
                      employstatus_4 + employstatus_5 + employstatus_6 + employstatus_7 +
                      employstatus_8 + employstatus_9 + employstatus_10 + PFS01 + PFS02 +
                      PFS03 + fail01 + fail02 + fail03 + happy + lifeSat + MLQ +
                      c19NormShould + c19NormDo + c19IsStrict + c19IsPunish + c19IsOrg +
                      trustGovCtry + trustGovState +
                      gender + age + edu , data = MsiaCvbase)
summary(fit_MsiaCvbase) # R^2 value : 0.2185
# the best set of predictors to use is to eliminate the top 4 largest p-values
# (employstatus_7, disc_01, fail03 and fail02)
fit_MsiaCvbase = lm(c19ProSo02 ~ affAnx + affCalm + affContent + affBor + affEnerg + affDepr +
                      affExc + affNerv + affExh + affInsp + affRel + PLRAC19 + PLRAEco +
                      disc02 + disc03 + jbInsec01 + jbInsec02 + jbInsec03 +
                      jbInsec04 + employstatus_1 + employstatus_2 + employstatus_3 +
                      employstatus_4 + employstatus_5 + employstatus_6 +
                      employstatus_8 + employstatus_9 + employstatus_10 + PFS01 + PFS02 +
                      PFS03 + fail01 + happy + lifeSat + MLQ +
                      c19NormShould + c19NormDo + c19IsStrict + c19IsPunish + c19IsOrg +
                      trustGovCtry + trustGovState +
                      gender + age + edu , data = MsiaCvbase)
summary(fit_MsiaCvbase) # R^2 value : 0.2185
# 3D scatterplot of the 2 strongest predictors
sur <- scatterplot3d(MsiaCvbase$c19ProSo02,MsiaCvbase$c19IsOrg,MsiaCvbase$c19NormShould,
                     pch=16)
fit_MsiaCvbase = lm(c19ProSo02 ~ c19IsOrg + c19NormShould, data = MsiaCvbase)
sur$plane3d(fit_MsiaCvbase)
# Linear regression with all predictors for c19ProSo03
fit_MsiaCvbase = lm(c19ProSo03 ~ affAnx + affCalm + affContent + affBor + affEnerg + affDepr +
                      affExc + affNerv + affExh + affInsp + affRel + PLRAC19 + PLRAEco +
                      disc01 + disc02 + disc03 + jbInsec01 + jbInsec02 + jbInsec03 +
                      jbInsec04 + employstatus_1 + employstatus_2 + employstatus_3 +
                      employstatus_4 + employstatus_5 + employstatus_6 + employstatus_7 +
                      employstatus_8 + employstatus_9 + employstatus_10 + PFS01 + PFS02 +
                      PFS03 + fail01 + fail02 + fail03 + happy + lifeSat + MLQ +
                      c19NormShould + c19NormDo + c19IsStrict + c19IsPunish + c19IsOrg +
                      trustGovCtry + trustGovState +
                      gender + age + edu , data = MsiaCvbase)
summary(fit_MsiaCvbase) # R^2 : 0.1445
modcoef <- summary(fit_MsiaCvbase)[["coefficients"]]
modcoef[order(modcoef[ , 4]), ]
# the best set of predictors to use is to eliminate the top 4 largest p-values
# (affExh, affContent, disc02 and affBor)
fit_MsiaCvbase = lm(c19ProSo03 ~ affAnx + affCalm + affEnerg + affDepr +
                      affExc + affNerv + affInsp + affRel + PLRAC19 + PLRAEco +
                      disc01 + disc03 + jbInsec01 + jbInsec02 + jbInsec03 +
                      jbInsec04 + employstatus_1 + employstatus_2 + employstatus_3 +
                      employstatus_4 + employstatus_5 + employstatus_6 + employstatus_7 +
                      employstatus_8 + employstatus_9 + employstatus_10 + PFS01 + PFS02 +
                      PFS03 + fail01 + fail02 + fail03 + happy + lifeSat + MLQ +
                      c19NormShould + c19NormDo + c19IsStrict + c19IsPunish + c19IsOrg +
                      trustGovCtry + trustGovState +
                      gender + age + edu , data = MsiaCvbase)
summary(fit_MsiaCvbase) # R^2 : 0.1445
# Linear regression with all predictors for c19ProSo04
fit_MsiaCvbase = lm(c19ProSo04 ~ affAnx + affCalm + affContent + affBor + affEnerg + affDepr +
                      affExc + affNerv + affExh + affInsp + affRel + PLRAC19 + PLRAEco +
                      disc01 + disc02 + disc03 + jbInsec01 + jbInsec02 + jbInsec03 +
                      jbInsec04 + employstatus_1 + employstatus_2 + employstatus_3 +
                      employstatus_4 + employstatus_5 + employstatus_6 + employstatus_7 +
                      employstatus_8 + employstatus_9 + employstatus_10 + PFS01 + PFS02 +
                      PFS03 + fail01 + fail02 + fail03 + happy + lifeSat + MLQ +
                      c19NormShould + c19NormDo + c19IsStrict + c19IsPunish + c19IsOrg +
                      trustGovCtry + trustGovState +
                      gender + age + edu , data = MsiaCvbase)
summary(fit_MsiaCvbase) # R^2 : 0.1417
# the best set of predictors to use is to eliminate the top 4 largest p-values
# (affRel, fail01, affEnerg, employstatus_9 and c19IsPunish)
fit_MsiaCvbase = lm(c19ProSo04 ~ affAnx + affCalm + affContent + affBor + affDepr +
                      affExc + affNerv + affExh + affInsp + PLRAC19 + PLRAEco +
                      disc01 + disc02 + disc03 + jbInsec01 + jbInsec02 + jbInsec03 +
                      jbInsec04 + employstatus_1 + employstatus_2 + employstatus_3 +
                      employstatus_4 + employstatus_5 + employstatus_6 + employstatus_7 +
                      employstatus_8 + employstatus_10 + PFS01 + PFS02 +
                      PFS03 + fail02 + fail03 + happy + lifeSat + MLQ +
                      c19NormShould + c19NormDo + c19IsStrict + c19IsOrg +
                      trustGovCtry + trustGovState +
                      gender + age + edu , data = MsiaCvbase)
summary(fit_MsiaCvbase) # R^2 : 0.1417
# Question 2 (c) - Predictors for Other countries ------------------------------
# CORRELATION MATRIX #
# Remove rows with NA for correlation
OthersCvbase = na.omit(OthersCvbase)
# Correlation list for all Pro-socials
OthersCorr01 = round(cor(OthersCvbase[,c(1:27,29:49)],OthersCvbase[,50]), digits= 3)
# Strongest Correlation : MLQ & trustGovState
OthersCorr02 = round(cor(OthersCvbase[,c(1:27,29:49)],OthersCvbase[,51]), digits= 3)
# Strongest Correlation : MLQ
OthersCorr03 = round(cor(OthersCvbase[,c(1:27,29:49)],OthersCvbase[,52]), digits= 3)
# Strongest Correlation : trustGovState
OthersCorr04 = round(cor(OthersCvbase[,c(1:27,29:49)],OthersCvbase[,53]), digits= 3)
# Strongest Correlation : c19NormShould
# create subset for strong correlation predictors and ProSocial Behaviors
corr_matrix = round(cor(OthersCvbase[,c(39,40,46,50:53)]),2)
ggcorrplot(corr_matrix, hc.order =FALSE, type = "lower", lab = TRUE,
           title = "Strong Correlation Predictors Summary (Other Countries)")
# Linear regression with all predictors for c19ProSo01
fit_OthersCvbase = lm(c19ProSo01 ~ affAnx + affCalm + affContent + affBor + affEnerg +
                        affDepr + affExc + affNerv + affExh + affInsp + affRel + PLRAC19 + PLRAEco +
                        disc01 + disc02 + disc03 + jbInsec01 + jbInsec02 + jbInsec03 + jbInsec04 + employstatus_1 + employstatus_2 + employstatus_3 + employstatus_4 + employstatus_5 + employstatus_6 + employstatus_7 + employstatus_8 + employstatus_9 + employstatus_10 + PFS01 + PFS02 + PFS03 + fail01 + fail02 + fail03 + happy + lifeSat + MLQ + c19NormShould + c19NormDo + c19IsStrict + c19IsPunish + c19IsOrg + trustGovCtry + trustGovState +
                        gender + age + edu , data = OthersCvbase) summary(fit_OthersCvbase)
# orders p-values from lowest to highest
modcoef <- summary(fit_OthersCvbase)[["coefficients"]] modcoef[order(modcoef[ , 4]), ]
# Linear regression with all predictors for c19ProSo02
fit_OthersCvbase = lm(c19ProSo02 ~ affAnx + affCalm + affContent + affBor + affEnerg +
                        affExc + affNerv + affExh + affInsp + affRel + PLRAC19 + PLRAEco + disc01 + disc02 + disc03 + jbInsec01 + jbInsec02 + jbInsec03 + jbInsec04 + employstatus_1 + employstatus_2 + employstatus_3 + employstatus_4 + employstatus_5 + employstatus_6 + employstatus_7 + employstatus_8 + employstatus_9 + employstatus_10 + PFS01 + PFS02 + PFS03 + fail01 + fail02 + fail03 + happy + lifeSat + MLQ + c19NormShould + c19NormDo + c19IsStrict + c19IsPunish + c19IsOrg + trustGovCtry + trustGovState + affDepr +
                        gender + age + edu , data = OthersCvbase) summary(fit_OthersCvbase)
# Linear regression with all predictors for c19ProSo03
fit_OthersCvbase = lm(c19ProSo03 ~ affAnx + affCalm + affContent + affBor + affEnerg +
                        affExc + affNerv + affExh + affInsp + affRel + PLRAC19 + PLRAEco + disc01 + disc02 + disc03 + jbInsec01 + jbInsec02 + jbInsec03 + jbInsec04 + employstatus_1 + employstatus_2 + employstatus_3 + employstatus_4 + employstatus_5 + employstatus_6 + employstatus_7 + employstatus_8 + employstatus_9 + employstatus_10 + PFS01 + PFS02 + PFS03 + fail01 + fail02 + fail03 + happy + lifeSat + MLQ + c19NormShould + c19NormDo + c19IsStrict + c19IsPunish + c19IsOrg + trustGovCtry + trustGovState + affDepr +
                        gender + age + edu , data = OthersCvbase) summary(fit_OthersCvbase)
# Linear regression with all predictors for c19ProSo04
fit_OthersCvbase = lm(c19ProSo04 ~ affAnx + affCalm + affContent + affBor + affEnerg +affDepr+
                        affExc + affNerv + affExh + affInsp + affRel + PLRAC19 + PLRAEco + disc01 + disc02 + disc03 + jbInsec01 + jbInsec02 + jbInsec03 + jbInsec04 + employstatus_1 + employstatus_2 + employstatus_3 + employstatus_4 + employstatus_5 + employstatus_6 + employstatus_7 + employstatus_8 + employstatus_9 + employstatus_10 + PFS01 + PFS02 + PFS03 + fail01 + fail02 + fail03 + happy + lifeSat + MLQ + c19NormShould + c19NormDo + c19IsStrict + c19IsPunish + c19IsOrg + trustGovCtry + trustGovState +
                        gender + age + edu , data = OthersCvbase) summary(fit_OthersCvbase)
# CONFUSION MATRIX #
set.seed(31861148)
# Changing Malaysia's pro-socials to factors for better prediction
MsiaCvbase$c19ProSo01 = as.factor(MsiaCvbase$c19ProSo01)
MsiaCvbase$c19ProSo02 = as.factor(MsiaCvbase$c19ProSo02)
MsiaCvbase$c19ProSo03 = as.factor(MsiaCvbase$c19ProSo03)
MsiaCvbase$c19ProSo04 = as.factor(MsiaCvbase$c19ProSo04)
# Recursive partitioning model for other countries predicting c19ProSo01 in Malaysia
fit_OthersCvbaseRpart = rpart(c19ProSo01 ~ affAnx + affCalm + affContent + affBor +
                                affEnerg + affDepr + affExc + affNerv + affExh + affInsp +
                                affRel + PLRAC19 + PLRAEco + disc01 + disc02 + disc03 +
                                jbInsec01 + jbInsec02 + jbInsec03 + jbInsec04 +
                                employstatus_1 + employstatus_2 + employstatus_3 +
                                employstatus_4 + employstatus_5 + employstatus_6 +
                                employstatus_7 + employstatus_8 + employstatus_9 +
                                employstatus_10 + PFS01 + PFS02 + PFS03 + fail01 +
                                fail02 + fail03 + happy + lifeSat + MLQ + c19NormShould +
                                c19NormDo + c19IsStrict + c19IsPunish + c19IsOrg + 
                                trustGovCtry + trustGovState + gender + age + edu ,
                              data = OthersCvbase
                              , method="class")
                                
predicted_values = as.data.frame(predict(fit_OthersCvbaseRpart, MsiaCvbase[1:49], type
                                         ="class"))
colnames(predicted_values) = c("c19ProSo01")
CM_Others1 = table(predicted_values$c19ProSo01, MsiaCvbase$c19ProSo01)
# Recursive partitioning model for other countries predicting c19ProSo02 in Malaysia
fit_OthersCvbaseRpart = rpart(c19ProSo02 ~ affAnx + affCalm + affContent + affBor +
                                affEnerg + affDepr + affExc + affNerv + affExh + affInsp +
                                affRel + PLRAC19 + PLRAEco + disc01 + disc02 + disc03 +
                                jbInsec01 + jbInsec02 + jbInsec03 + jbInsec04 +
                                employstatus_1 + employstatus_2 + employstatus_3 +
                                employstatus_4 + employstatus_5 + employstatus_6 +
                                employstatus_7 + employstatus_8 + employstatus_9 +
                                employstatus_10 + PFS01 + PFS02 + PFS03 + fail01 +
                                fail02 + fail03 + happy + lifeSat + MLQ + c19NormShould +
                                c19NormDo + c19IsStrict + c19IsPunish + c19IsOrg +
                                trustGovCtry + trustGovState + gender + age + edu ,
                              data = OthersCvbase
                              , method="class")
predicted_values = as.data.frame(predict(fit_OthersCvbaseRpart, MsiaCvbase[1:49], type
                                         ="class"))
colnames(predicted_values) = c("c19ProSo02")
CM_Others2 = table(predicted_values$c19ProSo02, MsiaCvbase$c19ProSo02)
# Recursive partitioning model for other countries predicting c19ProSo03 in Malaysia
fit_OthersCvbaseRpart = rpart(c19ProSo03 ~ affAnx + affCalm + affContent + affBor +
                                affEnerg + affDepr + affExc + affNerv + affExh + affInsp +
                                affRel + PLRAC19 + PLRAEco + disc01 + disc02 + disc03 +
                                jbInsec01 + jbInsec02 + jbInsec03 + jbInsec04 +
                                employstatus_1 + employstatus_2 + employstatus_3 +
                                employstatus_4 + employstatus_5 + employstatus_6 +
                                employstatus_7 + employstatus_8 + employstatus_9 +
                                employstatus_10 + PFS01 + PFS02 + PFS03 + fail01 +
                                fail02 + fail03 + happy + lifeSat + MLQ + c19NormShould +
                                c19NormDo + c19IsStrict + c19IsPunish + c19IsOrg +
                                trustGovCtry + trustGovState + gender + age + edu ,
                              data = OthersCvbase
                              , method="class")
predicted_values = as.data.frame(predict(fit_OthersCvbaseRpart, MsiaCvbase[1:49], type
                                         ="class"))
colnames(predicted_values) = c("c19ProSo03")
CM_Others3 = table(predicted_values$c19ProSo03, MsiaCvbase$c19ProSo03)
# Recursive partitioning model for other countries predicting c19ProSo04 in Malaysia
fit_OthersCvbaseRpart = rpart(c19ProSo04 ~ affAnx + affCalm + affContent + affBor +
                                affEnerg + affDepr + affExc + affNerv + affExh + affInsp +
                                affRel + PLRAC19 + PLRAEco + disc01 + disc02 + disc03 +
                                jbInsec01 + jbInsec02 + jbInsec03 + jbInsec04 +
                                employstatus_1 + employstatus_2 + employstatus_3 +
                                employstatus_4 + employstatus_5 + employstatus_6 +
                                employstatus_7 + employstatus_8 + employstatus_9 +
                                employstatus_10 + PFS01 + PFS02 + PFS03 + fail01 +
                                fail02 + fail03 + happy + lifeSat + MLQ + c19NormShould +
                                c19NormDo + c19IsStrict + c19IsPunish + c19IsOrg +
                                trustGovCtry + trustGovState + gender + age + edu ,
                              data = OthersCvbase
                              , method="class")
predicted_values = as.data.frame(predict(fit_OthersCvbaseRpart, MsiaCvbase[1:49], type
                                         ="class"))
colnames(predicted_values) = c("c19ProSo04")
CM_Others4 = table(predicted_values$c19ProSo04, MsiaCvbase$c19ProSo04)

# Question 3 (a) - Cluster similar countries -----------------------------------
cvbase = na.omit(cvbase)
# create subset of cvbase with only indicator attributes
cvbaseIndicators = cvbase[,c(12,14:16,34:36,40:46,50)]
# This function finds the most common value (the mode) for the attribute
mode_fun <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
# Group the indicator dataset by coded_country and calculate the mode of each
# variable using the function above
# column_to_rownames turns the coded_country into the index for each row (row name)
country_indicator <- cvbaseIndicators %>%
  group_by(coded_country) %>%
  summarise(across(PLRAC19:trustGovState, mode_fun)) %>%
  column_to_rownames("coded_country")
write_csv(country_indicator,
          "/Users/aliciang/Downloads/FIT3152_Assignment_1/Indicator_dataset")
# perform hierarchical clustering on country_indicator by first computing the distance
# matrix between responses using the Euclidean distance metric
# Complete linkage clustering is then used on the result
Indicator_cluster <- hclust(dist(country_indicator, method = 'euclidean'),
                            method = 'complete')
plot(Indicator_cluster, main = 'Country Clusters', xlab = "Country", sub = "", cex = 0.8)
# Make a dendrogram object
Indicator_dendogram = as.dendrogram(Indicator_cluster)
# Plot the branch for clearer view of cluster
plot(cut(Indicator_dendogram, h =4)$lower[[10]],
     main="Tenth branch of lower tree with cut at h=4")
# Question 3 (b) - Predictors for Cluster --------------------------------------
ClusteredCvbase <- cvbase[cvbase$coded_country %in% c("Indonesia","Vietnam", "Singapore",
                                                      "United Arab Emirates", "Austria",
                                                      "Croatia"), ]
ClusteredCvbase$coded_country <- NULL
MsiaCvbase = na.omit(MsiaCvbase) # Remove rows with NA for correlation
# Correlation list for all Pro-socials
ClusCorr01 = sort(round(cor(ClusteredCvbase[,1:49],ClusteredCvbase[,50]), digits= 3))
ClusCorr02 = sort(round(cor(ClusteredCvbase[,1:49],ClusteredCvbase[,51]), digits= 3))
ClusCorr03 = sort(round(cor(ClusteredCvbase[,1:49],ClusteredCvbase[,52]), digits= 3))
ClusCorr04 = sort(round(cor(ClusteredCvbase[,1:49],ClusteredCvbase[,53]), digits= 3))
# CORRELATION MATRIX #
corr_matrix = cor(ClusteredCvbase[,c(40,44,46,50:53)])
ggcorrplot(corr_matrix, hc.order =FALSE, type = "lower", lab = TRUE,
           title = "Strong Correlation Predictors Summary (Cluster)")
# LINEAR REGRESSION #
# Linear regression model for cluster predicting c19ProSo01
fit_ClusteredCvbaseReg = lm(c19ProSo01 ~ affAnx + affCalm + affContent + affBor + affEnerg +
                              affDepr + affExc + affNerv + affExh + affInsp + affRel +
                              PLRAC19 + PLRAEco + disc01 + disc02 + disc03 + jbInsec01 +
                              jbInsec02 + jbInsec03 + jbInsec04 + employstatus_1 +
                              employstatus_2 + employstatus_3 + employstatus_4 +
                              employstatus_5 + employstatus_6 + employstatus_7 +
                              employstatus_8 + employstatus_9 + employstatus_10 +
                              PFS01 + PFS02 + PFS03 + fail01 + fail02 + fail03 +
                              happy + lifeSat + MLQ + c19NormShould + c19NormDo +
                              c19IsStrict + c19IsPunish + c19IsOrg + trustGovCtry +
                              trustGovState + gender + age + edu , data = ClusteredCvbase)
summary(fit_ClusteredCvbaseReg)
modcoef <- summary(fit_ClusteredCvbaseReg)[["coefficients"]]
modcoef[order(modcoef[ , 4]), ]
# Linear regression model for cluster predicting c19ProSo02
fit_ClusteredCvbaseReg = lm(c19ProSo02 ~ affAnx + affCalm + affContent + affBor + affEnerg +
                              affDepr + affExc + affNerv + affExh + affInsp + affRel +
                              PLRAC19 + PLRAEco + disc01 + disc02 + disc03 + jbInsec01 +
                              jbInsec02 + jbInsec03 + jbInsec04 + employstatus_1 + 
                              employstatus_2 + employstatus_3 + employstatus_4 +
                              employstatus_5 + employstatus_6 + employstatus_7 +
                              employstatus_8 + employstatus_9 + employstatus_10 +
                              PFS01 + PFS02 + PFS03 + fail01 + fail02 + fail03 +
                              happy + lifeSat + MLQ + c19NormShould + c19NormDo +
                              c19IsStrict + c19IsPunish + c19IsOrg + trustGovCtry +
                              trustGovState + gender + age + edu , data = ClusteredCvbase)
summary(fit_ClusteredCvbaseReg)
# Linear regression model for cluster predicting c19ProSo03
fit_ClusteredCvbaseReg = lm(c19ProSo03 ~ affAnx + affCalm + affContent + affBor + affEnerg +
                              affDepr + affExc + affNerv + affExh + affInsp + affRel +
                              PLRAC19 + PLRAEco + disc01 + disc02 + disc03 + jbInsec01 +
                              jbInsec02 + jbInsec03 + jbInsec04 + employstatus_1 +
                              employstatus_2 + employstatus_3 + employstatus_4 +
                              employstatus_5 + employstatus_6 + employstatus_7 +
                              employstatus_8 + employstatus_9 + employstatus_10 +
                              PFS01 + PFS02 + PFS03 + fail01 + fail02 + fail03 +
                              happy + lifeSat + MLQ + c19NormShould + c19NormDo +
                              c19IsStrict + c19IsPunish + c19IsOrg + trustGovCtry +
                              trustGovState + gender + age + edu , data = ClusteredCvbase)
summary(fit_ClusteredCvbaseReg)
# Linear regression model for cluster predicting c19ProSo04
fit_ClusteredCvbaseReg = lm(c19ProSo04 ~ affAnx + affCalm + affContent + affBor + affEnerg +
                              affDepr + affExc + affNerv + affExh + affInsp + affRel +
                              PLRAC19 + PLRAEco + disc01 + disc02 + disc03 + jbInsec01 +
                              jbInsec02 + jbInsec03 + jbInsec04 + employstatus_1 +
                              employstatus_2 + employstatus_3 + employstatus_4 +
                              employstatus_5 + employstatus_6 + employstatus_7 +
                              employstatus_8 + employstatus_9 + employstatus_10 +
                              PFS01 + PFS02 + PFS03 + fail01 + fail02 + fail03 +
                              happy + lifeSat + MLQ + c19NormShould + c19NormDo +
                              c19IsStrict + c19IsPunish + c19IsOrg + trustGovCtry +
                              trustGovState + gender + age + edu , data = ClusteredCvbase)
summary(fit_ClusteredCvbaseReg)
# CONFUSION MATRIX #
# Repetitive partitioning model for cluster predicting c19ProSo01
fit_ClusteredCvbaseRpart = rpart(c19ProSo01 ~ affAnx + affCalm + affContent + affBor +
                                   affEnerg + affDepr + affExc + affNerv + affExh + affInsp +
                                   affRel + PLRAC19 + PLRAEco + disc01 + disc02 + disc03 +
                                   jbInsec01 + jbInsec02 + jbInsec03 + jbInsec04 +
                                   employstatus_1 + employstatus_2 + employstatus_3 +
                                   employstatus_4 + employstatus_5 + employstatus_6 +
                                   employstatus_7 + employstatus_8 + employstatus_9 +
                                   employstatus_10 + PFS01 + PFS02 + PFS03 + fail01 +
                                   fail02 + fail03 + happy + lifeSat + MLQ + c19NormShould +
                                   c19NormDo + c19IsStrict + c19IsPunish + c19IsOrg +
                                   trustGovCtry + trustGovState + gender + age + edu ,
                                 data = ClusteredCvbase
                                 , method="class")
predicted_values = as.data.frame(predict(fit_ClusteredCvbaseRpart, MsiaCvbase[1:49], type
                                         ="class"))
colnames(predicted_values) = c("c19ProSo01")
CM_Cluster1 = table(predicted_values$c19ProSo01, MsiaCvbase$c19ProSo01)
# Repetitive partitioning model for cluster predicting c19ProSo02
fit_ClusteredCvbaseRpart = rpart(c19ProSo02 ~ affAnx + affCalm + affContent + affBor +
                                   affEnerg + affDepr + affExc + affNerv + affExh + affInsp +
                                   affRel + PLRAC19 + PLRAEco + disc01 + disc02 + disc03 +
                                   jbInsec01 + jbInsec02 + jbInsec03 + jbInsec04 +
                                   employstatus_1 + employstatus_2 + employstatus_3 +
                                   employstatus_4 + employstatus_5 + employstatus_6 +
                                   employstatus_7 + employstatus_8 + employstatus_9 +
                                   employstatus_10 + PFS01 + PFS02 + PFS03 + fail01 +
                                   fail02 + fail03 + happy + lifeSat + MLQ + c19NormShould +
                                   c19NormDo + c19IsStrict + c19IsPunish + c19IsOrg +
                                   trustGovCtry + trustGovState + gender + age + edu ,
                                 data = ClusteredCvbase
                                 , method="class")
predicted_values = as.data.frame(predict(fit_ClusteredCvbaseRpart, MsiaCvbase[1:49], type
                                         ="class"))
colnames(predicted_values) = c("c19ProSo02")
CM_Cluster2 = table(predicted_values$c19ProSo02, MsiaCvbase$c19ProSo02)
# Repetitive partitioning model for cluster predicting c19ProSo03
fit_ClusteredCvbaseRpart = rpart(c19ProSo03 ~ affAnx + affCalm + affContent + affBor +
                                   affEnerg + affDepr + affExc + affNerv + affExh + affInsp +
                                   affRel + PLRAC19 + PLRAEco + disc01 + disc02 + disc03 +
                                   jbInsec01 + jbInsec02 + jbInsec03 + jbInsec04 +
                                   employstatus_1 + employstatus_2 + employstatus_3 +
                                   employstatus_4 + employstatus_5 + employstatus_6 +
                                   employstatus_7 + employstatus_8 + employstatus_9 +
                                   employstatus_10 + PFS01 + PFS02 + PFS03 + fail01 +
                                   fail02 + fail03 + happy + lifeSat + MLQ + c19NormShould +
                                   c19NormDo + c19IsStrict + c19IsPunish + c19IsOrg +
                                   trustGovCtry + trustGovState + gender + age + edu ,
                                 data = ClusteredCvbase
                                 , method="class")
predicted_values = as.data.frame(predict(fit_ClusteredCvbaseRpart, MsiaCvbase[1:49], type
                                         ="class"))
colnames(predicted_values) = c("c19ProSo03")
CM_Cluster3 = table(predicted_values$c19ProSo03, MsiaCvbase$c19ProSo03)
# Repetitive partitioning model for cluster predicting c19ProSo04
fit_ClusteredCvbaseRpart = rpart(c19ProSo04 ~ affAnx + affCalm + affContent + affBor +
                                   affEnerg + affDepr + affExc + affNerv + affExh + affInsp +
                                   affRel + PLRAC19 + PLRAEco + disc01 + disc02 + disc03 +
                                   jbInsec01 + jbInsec02 + jbInsec03 + jbInsec04 +
                                   employstatus_1 + employstatus_2 + employstatus_3 +
                                   employstatus_4 + employstatus_5 + employstatus_6 +
                                   employstatus_7 + employstatus_8 + employstatus_9 +
                                   employstatus_10 + PFS01 + PFS02 + PFS03 + fail01 +
                                   fail02 + fail03 + happy + lifeSat + MLQ + c19NormShould +
                                   c19NormDo + c19IsStrict + c19IsPunish + c19IsOrg +
                                   trustGovCtry + trustGovState + gender + age + edu ,
                                 data = ClusteredCvbase
                                 , method="class")
predicted_values = as.data.frame(predict(fit_ClusteredCvbaseRpart, MsiaCvbase[1:49], type
                                         ="class"))
colnames(predicted_values) = c("c19ProSo04")
CM_Cluster4 = table(predicted_values$c19ProSo04, MsiaCvbase$c19ProSo04)
                                
                                
                                

# Summary Key Findings
CorrectClusterPred = cbind(CM_Cluster1[1,1],CM_Cluster1[2,2],CM_Cluster1[3,3], CM_Cluster1[4,4],CM_Cluster1[5,5],CM_Cluster1[6,6],
                           CM_Cluster1[7,7])
CorrectOthersPred = cbind(CM_Others1[1,1],CM_Others1[2,2],CM_Others1[3,3],
                          CM_Others1[4,4],CM_Others1[5,5],CM_Others1[6,6],
                          CM_Others1[7,7])
CorrectPred1 = rbind(CorrectClusterPred,CorrectOthersPred)
par(mar=c(7,4,2,2))
colnames(CorrectPred1) <- c("Strongly Disagree","Disagree","Somewhat Disagree",
                            "Neither","Somewhat Agree","Agree","Strongly Agree")
# Fits 4 plots together
par(mfrow=c(2,2))
barplot(CorrectPred1, main = "Pro-social behaviour 1", ylab = "Frequency",
        col = c("pink","yellow"),beside = T, las =2, cex.names=0.75) legend("topleft", legend = c("Cluster countries","Other countries") ,
                                                                            col = c("pink","yellow") , pch=20 , pt.cex = 2, cex = 0.7)
CorrectClusterPred = cbind(CM_Cluster2[1,1],CM_Cluster2[2,2],CM_Cluster2[3,3], CM_Cluster2[4,4],CM_Cluster2[5,5],CM_Cluster2[6,6],
                           CM_Cluster2[7,7])
CorrectOthersPred = cbind(CM_Others2[1,1],CM_Others2[2,2],CM_Others2[3,3],
                          CM_Others2[4,4],CM_Others2[5,5],CM_Others2[6,6],
                          CM_Others2[7,7])
CorrectPred2 = rbind(CorrectClusterPred,CorrectOthersPred)
colnames(CorrectPred2) <- c("Strongly Disagree","Disagree","Somewhat Disagree", "Neither","Somewhat Agree","Agree","Strongly Agree")
barplot(CorrectPred2, main = "Pro-social behaviour 2", ylab = "Frequency", col = c("pink","yellow"),beside = T, las =2, cex.names=0.75)

CorrectClusterPred = cbind(CM_Cluster3[1,1],CM_Cluster3[2,2],CM_Cluster3[3,3], CM_Cluster3[4,4],CM_Cluster3[5,5],CM_Cluster3[6,6],
                           CM_Cluster3[7,7])
CorrectOthersPred = cbind(CM_Others3[1,1],CM_Others3[2,2],CM_Others3[3,3],
                          CM_Others3[4,4],CM_Others3[5,5],CM_Others3[6,6],
                          CM_Others3[7,7])
CorrectPred3 = rbind(CorrectClusterPred,CorrectOthersPred)
colnames(CorrectPred3) <- c("Strongly Disagree","Disagree","Somewhat Disagree", "Neither","Somewhat Agree","Agree","Strongly Agree")
barplot(CorrectPred3, main = "Pro-social behaviour 3", ylab = "Frequency", col = c("pink","yellow"),beside = T, las =2, cex.names=0.75)
CorrectClusterPred = cbind(CM_Cluster4[1,1],CM_Cluster4[2,2],CM_Cluster4[3,3], CM_Cluster4[4,4],CM_Cluster4[5,5],CM_Cluster4[6,6],
                           CM_Cluster4[7,7])
CorrectOthersPred = cbind(CM_Others4[1,1],CM_Others4[2,2],CM_Others4[3,3],
                          CM_Others4[4,4],CM_Others4[5,5],CM_Others4[6,6],
                          CM_Others4[7,7])
CorrectPred4 = rbind(CorrectClusterPred,CorrectOthersPred)
colnames(CorrectPred4) <- c("Strongly Disagree","Disagree","Somewhat Disagree", "Neither","Somewhat Agree","Agree","Strongly Agree")
barplot(CorrectPred4, main = "Pro-social behaviour 4", ylab = "Frequency", col = c("pink","yellow"),beside = T, las =2, cex.names=0.75)