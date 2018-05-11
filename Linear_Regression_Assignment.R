# Assignment- Linear Regression - Geely Auto 

# All coding are done in MacOS - Sierra v10.12.6
# Author - Anindya Chatterjee

install.packages("MASS")
install.packages("car")
install.packages("stringr")
install.packages("DescTools")
library(MASS)
library(car)
library(stringr)
library(DescTools)


#Loading the data set
auto_data_set<- read.csv("CarPrice_Assignment.csv")

#Structure of the data set
str(auto_data_set)

#Check for data quality issues in the provided data set
sum(duplicated(auto_data_set$car_ID))
sum(is.na(auto_data_set$car_ID))

# Splitting carName into car company and car model
# Only car company to be considered as per the model building
cars<-str_split_fixed(auto_data_set$CarName, " ", 2)
cars_df<- as.data.frame(cars)

auto_data_set$CarCompany<-cars_df$V1
auto_data_set$CarName<-cars_df$V2

# Checking for car companies
auto_data_set$CarCompany

# Fixing the upper case - lower case issue
auto_data_set$CarCompany = tolower(auto_data_set$CarCompany)

# Data Preparation - Name issues in car company 
auto_data_set$CarCompany[which(auto_data_set$CarCompany=="toyouta")] = "toyota"
auto_data_set$CarCompany[which(auto_data_set$CarCompany=="porcshce")] = "porsche"
auto_data_set$CarCompany[which(auto_data_set$CarCompany=="vokswagen")] = "volkswagen"
auto_data_set$CarCompany[which(auto_data_set$CarCompany=="vw")] = "volkswagen"
auto_data_set$CarCompany[which(auto_data_set$CarCompany=="maxda")] = "mazda"

# Data preparation - Convert the door number to integer
auto_data_set$doornumber<- as.character(auto_data_set$doornumber)
auto_data_set$doornumber[which(auto_data_set$doornumber=="four")]=4
auto_data_set$doornumber[which(auto_data_set$doornumber=="two")]=2
auto_data_set$doornumber<-as.integer(auto_data_set$doornumber)

# Data preparation - Engine Type
# Changing dohcv to dohc
auto_data_set$enginetype[which(auto_data_set$enginetype=="dohcv")] = "dohc"

#Data preparation - number of cylinders - Convert to integer
auto_data_set$cylindernumber=as.character(auto_data_set$cylindernumber)
auto_data_set$cylindernumber[which(auto_data_set$cylindernumber=="two")] = 2
auto_data_set$cylindernumber[which(auto_data_set$cylindernumber=="three")] = 3
auto_data_set$cylindernumber[which(auto_data_set$cylindernumber=="four")] = 4
auto_data_set$cylindernumber[which(auto_data_set$cylindernumber=="five")] = 5
auto_data_set$cylindernumber[which(auto_data_set$cylindernumber=="six")] = 6
auto_data_set$cylindernumber[which(auto_data_set$cylindernumber=="eight")] = 8
auto_data_set$cylindernumber[which(auto_data_set$cylindernumber=="twelve")] = 12
auto_data_set$cylindernumber=as.integer(auto_data_set$cylindernumber)

# Setting up data set for analysis - Dummy creation 
# Remove carName as it is not needed for analysis
auto_data_set<-auto_data_set[,-3]

#Fuel Type contains 2 levels. Lets convert it to numeric
str(auto_data_set$fueltype)
levels(auto_data_set$fueltype)<-c(0,1)

#Aspiration has 2 levels. Lets convert it to numeric
str(auto_data_set$aspiration)
levels(auto_data_set$aspiration)<-c(0,1)

#Carbody has more than 2 levels. So we need dummy variable
str(auto_data_set$carbody)
dummy_1 <- data.frame(model.matrix( ~carbody, data = auto_data_set))
dummy_1 <- dummy_1[,-1]
auto_data_set_1<-cbind(auto_data_set[,-6], dummy_1)


#Drivewheel also has more than 2 levels. So we need dummy variable
str(auto_data_set_1$drivewheel)
dummy_2 <- data.frame(model.matrix( ~drivewheel, data = auto_data_set_1))
dummy_2 <- dummy_2[,-1]
auto_data_set_2 <- cbind(auto_data_set_1[,-6], dummy_2)

#Engine location has 2 levels
str(auto_data_set_2$enginelocation)
levels(auto_data_set_2$enginelocation)<-c(0,1)

#Engine Type has more than 2 levels
str(auto_data_set_2$enginetype)
dummy_3 <- data.frame(model.matrix( ~enginetype, data = auto_data_set_2))
dummy_3 <- dummy_3[,-1]
auto_data_set_3 <- cbind(auto_data_set_2[,-12], dummy_3)

#CarCompany has more than 2 levels
str(factor(auto_data_set_3$CarCompany))
dummy_4 <- data.frame(model.matrix( ~CarCompany, data = auto_data_set_3))
dummy_4 <- dummy_4[,-1]
auto_data_set_4 <-cbind(auto_data_set_3[,-23],dummy_4)


# Model Building - Splitting the data into training and testing data
set.seed(1000)
sample_data = sample(2,nrow(auto_data_set_4),replace = T,prob = c(0.7,0.3))

#Prepare training and testing data
auto_data_training = auto_data_set_4[sample_data==1,]
auto_data_testing = auto_data_set_4[sample_data==2,]

lm_model1 = lm(price~.,data=auto_data_training)
summary(lm_model1)
# Multiple R-squared:  0.9731,	Adjusted R-squared:  0.9559 
# Using STEP AIC - Variable Selection method
step_lm_model1<- stepAIC(lm_model1, direction="both")
step_lm_model1

# Based on the last result of the Step AIC , lets build the 2nd model

lm_model2 = lm(formula = price ~ car_ID + aspiration + enginelocation + carwidth + 
     curbweight + cylindernumber + enginesize + boreratio + stroke + 
     peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
     carbodywagon + enginetypel + enginetypeohcf + enginetypeohcv + 
     CarCompanyaudi + CarCompanybmw + CarCompanydodge + CarCompanyhonda + 
     CarCompanyisuzu + CarCompanymazda + CarCompanymercury + CarCompanymitsubishi + 
     CarCompanynissan + CarCompanypeugeot + CarCompanyplymouth + 
     CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen + 
     CarCompanyvolvo + enginetyperotor, data = auto_data_training)

# Let us look at the summary of the model
summary(lm_model2)
# Multiple R-squared:  0.9701,	Adjusted R-squared:  0.9605 

# Let us check for multicollinearity using VIF
vif(lm_model2)

# We see that enginetypel as VIF value 11.024763 and p value 0.205709, so we remove that from the next model

lm_model3 = lm(formula = price ~ car_ID + aspiration + enginelocation + carwidth + 
                 curbweight + cylindernumber + enginesize + boreratio + stroke + 
                 peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon  + enginetypeohcf + enginetypeohcv + 
                 CarCompanyaudi + CarCompanybmw + CarCompanydodge + CarCompanyhonda + 
                 CarCompanyisuzu + CarCompanymazda + CarCompanymercury + CarCompanymitsubishi + 
                 CarCompanynissan + CarCompanypeugeot + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen + 
                 CarCompanyvolvo + enginetyperotor, data = auto_data_training)

# Let us look at the summary of the model
summary(lm_model3)
# Multiple R-squared:  0.9696,	Adjusted R-squared:  0.9603 
# Let us check for multicollinearity using VIF
vif(lm_model3)

# The below variables have high multicolinearity but they have high significance
# CarCompanyvolvo, CarCompanyvolkswagen, CarCompanytoyota, cylindernumber, enginesize, enginesize
# car_ID has high multicolinearity and cannot have significance in price. 
# So we remove that from our next model

lm_model4 = lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 curbweight + cylindernumber + enginesize + boreratio + stroke + 
                 peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon  + enginetypeohcf + enginetypeohcv + 
                 CarCompanyaudi + CarCompanybmw + CarCompanydodge + CarCompanyhonda + 
                 CarCompanyisuzu + CarCompanymazda + CarCompanymercury + CarCompanymitsubishi + 
                 CarCompanynissan + CarCompanypeugeot + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen + 
                 CarCompanyvolvo + enginetyperotor, data = auto_data_training)


summary(lm_model4)
# Multiple R-squared:  0.959,	Adjusted R-squared:  0.9469 
# We dont see a much difference in the r-squared and adjusted r-squared value

vif(lm_model4)

# cylinder number has low significance p- value - 0.170258 and high VIF - 16.331650
# We remove cylinder number from our next model

lm_model5 = lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 curbweight  + enginesize + boreratio + stroke + 
                 peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon  + enginetypeohcf + enginetypeohcv + 
                 CarCompanyaudi + CarCompanybmw + CarCompanydodge + CarCompanyhonda + 
                 CarCompanyisuzu + CarCompanymazda + CarCompanymercury + CarCompanymitsubishi + 
                 CarCompanynissan + CarCompanypeugeot + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen + 
                 CarCompanyvolvo + enginetyperotor, data = auto_data_training)

summary(lm_model5)
# Multiple R-squared:  0.9582,	Adjusted R-squared:  0.9465 
# We dont see a much difference in the r-squared and adjusted r-squared value, so we are good

vif(lm_model5)

# Below variables donot have significance
# stroke, peakrpm, enginetypeohcf, CarCompanyisuzu, CarCompanymercury, CarCompanysaab, CarCompanyvolvo
# Lets remove the below variables and check for the r-sqared and adjusted r-squared values in the necxt model

lm_model6 = lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 curbweight  + enginesize + boreratio
                  + carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon   + enginetypeohcv + 
                 CarCompanyaudi + CarCompanybmw + CarCompanydodge + CarCompanyhonda + 
                 CarCompanymazda + CarCompanymitsubishi + 
                 CarCompanynissan + CarCompanypeugeot + CarCompanyplymouth + 
                 CarCompanyrenault  + CarCompanytoyota + CarCompanyvolkswagen + 
                 enginetyperotor, data = auto_data_training)

# Lets check the summary for the model 
summary(lm_model6)

# Multiple R-squared:  0.9551,	Adjusted R-squared:  0.9458 
# Since we do not see much difference in the r-squared and adjusted r-squared value, 
# so we are good to remove the variables

# Lets check for multicolinearity
vif(lm_model6)

# Below are the variables with low significance. Lets remove them and check for the r values
# aspiration, carbodyhardtop, CarCompanyaudi, carbodysedan, carbodyhatchback with significant VIF values 
# for some. Lets remove the above variables in the next model

lm_model7 = lm(formula = price ~   enginelocation + carwidth + 
                 curbweight  + enginesize + boreratio
                 + carbodywagon   + enginetypeohcv 
                  + CarCompanybmw + CarCompanydodge + CarCompanyhonda + 
                 CarCompanymazda + CarCompanymitsubishi + 
                 CarCompanynissan + CarCompanypeugeot + CarCompanyplymouth + 
                 CarCompanyrenault  + CarCompanytoyota + CarCompanyvolkswagen + 
                 enginetyperotor, data = auto_data_training)

#Lets check the summary for the model
summary(lm_model7)
# Multiple R-squared:  0.9488,	Adjusted R-squared:  0.9409 
# There is not much difference in the r-squared and adjusted r-squared values

# Checking for multicolinearity
vif(lm_model7)

# Lets remove some more less significant variables based on the p-values
# carbodywagon, CarCompanydodge, CarCompanyhonda, CarCompanymazda, CarCompanyplymouth
# Lets remove the variables in the next model

lm_model8 = lm(formula = price ~   enginelocation + carwidth + 
                 curbweight  + enginesize + boreratio
                  + enginetypeohcv 
               + CarCompanybmw  + CarCompanymitsubishi + 
                 CarCompanynissan + CarCompanypeugeot +
                 CarCompanyrenault  + CarCompanytoyota + CarCompanyvolkswagen + 
                 enginetyperotor, data = auto_data_training)

summary(lm_model8)
# Multiple R-squared:  0.9392,	Adjusted R-squared:  0.9325 
# We dont see much change in the Adjusted R-squared

#Check for multicolinearity
vif(lm_model8)

# Predicting with test data
auto_data_testing$prediction = predict(lm_model8,auto_data_testing)
# Checking for the difference in the test data set b/w the actual value and predicted output
auto_data_testing$difference = auto_data_testing$price - auto_data_testing$prediction

#Comparison in the prices - actual vs predicted
plot(auto_data_testing$price,type = "l",col="red")
lines(auto_data_testing$prediction ,type = "l",col="blue")

#Seems like the predicted values are nearly good

# Lets test on the complete data set
auto_data_set_4$prediction = predict(lm_model8,auto_data_set_4)
auto_data_set_4$difference = auto_data_set_4$price - auto_data_set_4$prediction
#Comparison in the prices - actual vs predicted
plot(auto_data_set_4$price,type = "l",col="red")
lines(auto_data_set_4$prediction ,type = "l",col="blue")

# Checking for the average error
average_error = abs(auto_data_set_4$price-auto_data_set_4$prediction)/auto_data_set_4$price
mean(average_error)

# Variables that we use to predict the variables
# enginelocation , carwidth , curbweight  , enginesize , boreratio
# enginetypeohcv , CarCompanybmw  , CarCompanymitsubishi ,CarCompanynissan , CarCompanypeugeot 
# CarCompanyrenault  , CarCompanytoyota , CarCompanyvolkswagen , enginetyperotor

# Final model stats
# Multiple R-squared:  0.9392,	Adjusted R-squared:  0.9325 
