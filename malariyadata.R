# necessary library
install.packages("DescTools")
library(DescTools)
library(readxl) # read excel data 

###################################################

# Step 0: Load data and read data from excel


MalariaData2 <- read_excel("C:/All data/computational statistics/project/MalariaData2.xlsx",

                                                      na = "**")
View(MalariaData2)
names(MalariaData2) # To see the list of variables with the data set. 
dim(MalariaData2)
MalariaData2 <- MalariaData2[-c(801),] # removing the last row due to inappropriate input value
dim(MalariaData2)
View(MalariaData2)
###################################################



# Visualization of variable "Age_mos" and "Hgbfinal" from our data set using histogram.

var_agemos <- MalariaData2$Age_mos

hist(var_agemos, xlim = c(0,50), ylim = c(0,.1), las=1,
     breaks = seq(from = min(var_agemos),
     to = max(var_agemos), length = 50),
     xlab = "Age in months", ylab = "Perchantage",
     main = "Age frequency",
     prob = TRUE,col = "lightgray"
     )
# the histogram of variable "Age_mos" is skewed right. It is more like to the age of the patients are between 5 to 18 months. The maximum age of patient is 38 months, and minimum age is 2 months



var_hgbfinal <- MalariaData2$Hgbfinal

hist(var_hgbfinal, xlim = c(0,20), ylim = c(0,.2), las=1, 
     breaks = seq(min(var_hgbfinal),
                  max(var_hgbfinal), length = 50),
     xlab = "Haemoglobin level", ylab = "Perchantage",
     main = "Haemoglobin frequency",
     prob = TRUE,col = "lightgray"
     )
#the histogram of hgbfinal variable shows that the haemoglobin level mostly fall between 4 to 12.


###################################################

# Visualization of variable "Age_mos" and "Hgbfinal" from our data set using boxplot.

boxplot(var_agemos,
        main = "age in months with Boxplot",
        #xlab = "",
        ylab = "age in months",
        col = "orange",
        broder = "brown",
        horizontal = F,
        notch = TRUE
        )
boxplot(var_hgbfinal,
        main = "Hemoglobin level of Malaria Data with Boxplot",
        xlab = "",
        ylab = "hgbfinal",
        col = "lightgreen",
        broder = "brown",
        horizontal = FALSE,
        notch = TRUE)

summary(var_agemos)
summary(var_hgbfinal)

###################################################

# Visualization of variable "Age_mos" and "Hgbfinal" from our data set using qqplot.


qqnorm(var_agemos)
qqline(var_agemos)


qqnorm(var_hgbfinal)
qqline(var_hgbfinal)

PlotQQ(var_agemos)
PlotQQ(var_hgbfinal)


###################################################
shapiro.test(var_agemos) 
shapiro.test(var_hgbfinal)  

# The variable "Age_mos" and "Hgbfinal" is not normally distributed..


###################################################

# Summary Statistics i.e. mean, median, quartiles, standard deviation of the variable "Age_mos".

mean(var_agemos) #mean is 14.20316
median(var_agemos) #median is 12.3
sd(var_agemos) # standard deviation is 8.033098
quantile(var_agemos,0.25) # 1st quartile 7.97
quantile(var_agemos,0.75) # 3rd quartile 18.43

# we can execute an overall summary by the following command
summary(var_agemos)


# Summary Statistics i.e. mean, median, quartiles, standard deviation of the variable "Age_mos".

mean(var_hgbfinal)
median(var_hgbfinal)
sd(var_hgbfinal)
quantile(var_hgbfinal,0.25)
quantile(var_hgbfinal,0.75)

# we can execute an overall summary by the following command
summary(var_hgbfinal)


###################################################

# Plotting of scatter plot of "Age_mos" and "Hgbfinal"


plot(var_agemos,var_hgbfinal,
     main = "scatter plot of hemoglobin level with age",
     xlab = "age in months",
     ylab = "Hemoglobin level",
     col = "purple",
     cex = 0.6, # to resize the size of the circle default value is 1
     
     )
lines(lowess(var_agemos, var_hgbfinal))



###################################################


# pearson Correlation between "Age_mos" and "Hgbfinal"

cor(var_agemos,var_hgbfinal, method = "pearson")



# spearman Correlation between "Age_mos" and "Hgbfinal"

cor(var_agemos,var_hgbfinal, method = "spearman")



###################################################

# choosing the one
### which one to pick between two?
 
###################################################

#test pearson
cor.test(var_agemos,var_hgbfinal, method = "pearson")


#test spearson
cor.test(var_agemos,var_hgbfinal, method = "spearman", exact = F) # exact = False to avoid the ties.


###################################################
###################################################
###################################################                   

# Summarization of the variables "G6PD" and "Sex" with a frequency table 

var_G6PD <- MalariaData2$G6PD
var_sex <- MalariaData2$sex


table_G6PD <- table(Var_G6PD)
table_G6PD
prop.table(table_G6PD)
prop.table(table_G6PD)*100


table_sex <- table(var_sex)
table_sex
prop.table(table_sex)
prop.table(table_sex)*100



###################################################

# Visualization of the variables "G6PD" and "Sex"  using a bar chart

barplot(table_G6PD, 
        ylim = c(0,700),
        main = "G6PD deficiency count",
        xlab = "Level of deficiency",
        ylab = "counts",
        col = "dodgerblue"
        )
abline(h=0)





barplot(table_sex,
        ylim = c(0,500),
        main = "Distribution of gender",
        xlab = "Gender",
        ylab = "counts",
        col = "lightgray"
        )
abline(h=0)

# contingency table of "G6PD" vs. "sex"  variables

table_G6PD_sex <- table(var_sex,var_G6PD)
table_G6PD_sex


prop.table(table_G6PD_sex)

prop.table(table_G6PD_sex)*100



barplot(table_G6PD_sex,
        main = "G6PD counts between Genders",
        
        beside = T,
        ylim = c(0,500),
        xlab = "Level of deficiencey",
        ylab = "counts",
        #legend = T,
        col = c("purple","black"),
        #legend("top", legend = c("Female", "Male"))
        )
legend("topleft", legend= c("F", "M"), col= c("purple", "black"),lty = 1,lwd = 3, bty= "n")
abline(h=0)




###################################################
# Fisher's exact test of independence 

fisher.test(table_G6PD_sex)

fisher.test(table_G6PD_sex)$p.value
# weather independent or not? 
#dependent while p-value is small and we cancel the null hypothesis and consider alternative.


###################################################

# comparison between variables  "Hgbfinal" with different level of "G6PD"




tapply(var_hgbfinal, var_G6PD, mean) # mean value of hgbfibnal according to G6PD level
tapply(var_hgbfinal, var_G6PD, sd)  # standard deviation of hgbfinal according to G6PD level    
tapply(var_hgbfinal, var_G6PD,median) # median of hgbfibnal according to G6PD level
tapply(var_hgbfinal, var_G6PD,quantile) # quantile of hgbfibnal according to G6PD level


boxplot(var_hgbfinal~var_G6PD)


oneway.test(var_hgbfinal~var_G6PD) #ANOVA test with no assumption of equal variances





###################################################

SMA <- ifelse(MalariaData2$PfMalaria ==1 & MalariaData2$Hgbfinal < 5.0, 1, 0)
my_data <- cbind(MalariaData2,SMA)



names(my_data)
View(my_data)
str(my_data)
dim(my_data)

###################################################

table(my_data$SMA)# 84 cases with SMA is 1 and 714 are 0s.


my_data[my_data==""] <- NA

colSums(is.na(my_data)) #number of NAs in each column

sapply(my_data, function(x) length(unique(x)))#how many unique values are there

######################################
glm(SMA~my_data$Age.first, data = my_data,family = "binomial") # AIC 531.5
summary(glm(SMA~my_data$Age.first, data = my_data,family = "binomial"))#important **
######################################
HIV <- as.factor(my_data$HIV1)# its a categorical data but in our data its type is number so we factor them.
glm(SMA~my_data$HIV, data = my_data,family = "binomial") # AIC 536.3
summary(glm(SMA~my_data$HIV, data = my_data,family = "binomial"))#important *
######################################
glcs <- as.numeric(my_data$Glucose)# it is a numeric data but in our data its a character
glm(SMA~glcs, data = my_data,family = "binomial") # AIC 473
summary(glm(SMA~glcs, data = my_data,family = "binomial"))
######################################
glm(SMA~my_data$Hct, data = my_data,family = "binomial") # AIC 194.8
summary(glm(SMA~my_data$Hct, data = my_data,family = "binomial")) #important ***
######################################
glm(SMA~my_data$Plt, data = my_data,family = "binomial") # AIC 532.4
summary(glm(SMA~my_data$Plt, data = my_data,family = "binomial")) #important **
######################################
glm(SMA~my_data$PDW, data = my_data,family = "binomial") # AIC 529.4
summary(glm(SMA~my_data$PDW, data = my_data,family = "binomial")) #important **
######################################
throm <- as.factor(my_data$Thrombocytopenia)
glm(SMA~throm, data = my_data,family = "binomial") # AIC 532.3
summary(glm(SMA~throm, data = my_data,family = "binomial"))
######################################
glm(SMA~my_data$WBCx103uL, data = my_data,family = "binomial") # AIC 524.9
summary(glm(SMA~my_data$WBCx103uL, data = my_data,family = "binomial")) #important ***
######################################
glm(SMA~my_data$temperature, data = my_data,family = "binomial") # AIC 540
summary(glm(SMA~my_data$temperature, data = my_data,family = "binomial"))
######################################
absretic <- as.numeric(my_data$AbsReticIndex)
glm(SMA~absretic, data = my_data,family = "binomial") # AIC 539.4
summary(glm(SMA~absretic, data = my_data,family = "binomial"))
######################################
glm(SMA~my_data$nutritional_status, data = my_data,family = "binomial") # AIC 509.2
summary(glm(SMA~my_data$nutritional_status, data = my_data,family = "binomial")) # nutritional status good is significant *
######################################
glm(SMA~my_data$G6PD, data = my_data,family = "binomial") # AIC 535.9
summary(glm(SMA~my_data$G6PD, data = my_data,family = "binomial"))
######################################
glm(SMA~my_data$HbAS, data = my_data,family = "binomial") # AIC 533.5
summary(glm(SMA~my_data$HbAS, data = my_data,family = "binomial")) #my_data$HbASAS is important *
######################################
glm(SMA~my_data$ALPHATHAL, data = my_data,family = "binomial") # AIC 507.7
summary(glm(SMA~my_data$ALPHATHAL, data = my_data,family = "binomial"))
######################################
glm(SMA~my_data$weightkg, data = my_data,family = "binomial") # AIC 633.5
summary(glm(SMA~my_data$weightkg, data = my_data,family = "binomial")) # weight 5.5 and 5.9 *
######################################
glm(SMA~my_data$dehydration, data = my_data,family = "binomial") # AIC 528.7
summary(glm(SMA~my_data$dehydration, data = my_data,family = "binomial")) #my_data$dehydrationSome  is significant
######################################
glm(SMA~my_data$MPV, data = my_data,family = "binomial") # AIC 534.9
summary(glm(SMA~my_data$MPV, data = my_data,family = "binomial")) #is significant *
######################################
######################################
glm(SMA~my_data$Hct, data = my_data,family = "binomial") # AIC 194.8 is the best model based on the AIC 

p.adjust(my_data$Hct, method = "holm")

