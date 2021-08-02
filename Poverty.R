#Installing libraries
library(dplyr)
library(ggplot2)
#Importing the data file
data_3 <- read.csv("C:/Users/Amogh/Desktop/Data sets/Population characteristics/Population_and_household_characteristics-Rural_DLHS3.csv")
# Getting the data types and column names
attach(data_3)
str(data_3)
summary(data_3)
#Creating the binary depedent variable
print(data_3$Have.a.BPL.card....)
dummy <- as.numeric(data_3$Have.a.BPL.card.... >= 50.0)
print(dummy)

# Assigning the Data
Poverty.status <-dummy
State.UT <-data_3$State.UT
District <-data_3$District
literate.Population <- data_3$Percent.total.literate.Population..Age.7...
Have.Electricity.connection <- data_3$Have.Electricity.connection....
Have.Access.to.toilet.facility <- data_3$Have.Access.to.toilet.facility....
Use.piped.drinking.water <- data_3$Use.piped.drinking.water....
Use.LPG.for.cooking <- data_3$Use.LPG.for.cooking....
Live.in.a.pucca.house <- data_3$Live.in.a.pucca.house....
Own.a.house <- data_3$Own.a.house....
Own.Agriculture.Land <- data_3$Own.Agriculture.Land....
Have.a.mobile.phone <- data_3$Have.a.mobile.phone....
Have.a.Motorized.Vehicle <- data_3$Have.a.Motorized.Vehicle....
Have.a.television <- data_3$Have.a.television....

#Combining the Data
H <- cbind(literate.Population,Have.Electricity.connection,Have.Access.to.toilet.facility,Use.piped.drinking.water,Use.LPG.for.cooking,Live.in.a.pucca.house,Own.Agriculture.Land,Own.a.house,Have.a.mobile.phone,Have.a.Motorized.Vehicle)
#Running the logisitic regression 
model_1 <- glm(Poverty.status ~ literate.Population + Have.Electricity.connection + Have.a.television+ Have.Access.to.toilet.facility + Use.piped.drinking.water+ Use.LPG.for.cooking +Live.in.a.pucca.house+ Own.Agriculture.Land + Own.a.house+ Have.a.mobile.phone+ Have.a.Motorized.Vehicle, data = data_3, family = binomial('logit'))
summary(model_1)
# Check for multicollinearity
car::vif(model_1)
#Due to the high VIF value of Have.a.television we have excluded the variable and re-run the regression
model_1 <- glm(Poverty.status ~ literate.Population + Have.Electricity.connection + Have.Access.to.toilet.facility + Use.piped.drinking.water+ Use.LPG.for.cooking +Live.in.a.pucca.house+ Own.Agriculture.Land + Own.a.house+ Have.a.mobile.phone+ Have.a.Motorized.Vehicle, data = data_3, family = binomial('logit'))
summary(model_1)
car::vif(model_1)
#computing the odds ratio
library(oddsratio) 
fit_glm <- glm(Poverty.status ~ literate.Population + Have.Electricity.connection + Have.Access.to.toilet.facility + Use.piped.drinking.water+ Use.LPG.for.cooking +Live.in.a.pucca.house+ Own.Agriculture.Land + Own.a.house+ Have.a.mobile.phone+ Have.a.Motorized.Vehicle, data = data_3, family = binomial('logit')) 
or_glm(data = data_3, model = fit_glm)
  
library(ggplot2)
#Below we plot the distribution of significant predictors on the dependent variable to show a bivariate distribution 
ggplot(data= data_3, aes(x =Own.a.house, y =Poverty.status , color = Poverty.status)) + geom_jitter()
ggplot(data= data_3, aes(x =Have.a.Motorized.Vehicle , y =Poverty.status , color = Poverty.status)) + geom_jitter()
ggplot(data= data_3, aes(x =Have.a.mobile.phone , y =Poverty.status, color = Poverty.status)) + geom_jitter()
ggplot(data= data_3, aes(x =Have.Access.to.toilet.facility , y =Poverty.status, color = Poverty.status)) + geom_jitter()
ggplot(data= data_3, aes(x = Live.in.a.pucca.house , y = Poverty.status , color = Poverty.status)) + geom_jitter()

