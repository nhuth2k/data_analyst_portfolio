install.packages("car")
install.packages("effects")

library(dplyr)
library(forcats)
library(car)
library(effects)

Eggs <- read.csv("E:\data analytics\R\R Studio\egg_data.csv")

glimpse(Eggs)
#view snapshot of data

variable.summary(Eggs)
#View variable summary

#A scatterplot with a Best-fit line, Boxplots, and Smooth Line
scatterplot(Cases~Egg.Pr, #R Formula Notation: Y~X
            regLine = TRUE, #plot the fitted regression line
            smooth = TRUE, #Show smooth line
            boxplots = "XY" , #Show marginal boxplots for both axis
            data = Eggs) #Specify dataset

##Line graph
# Plot a line graph
plot(Cases~Week,
     type = 'o', #Plot points and lines
     data = Eggs)

##
# Plot a Boxplot
Boxplot(Cases ~ Easter, data = Eggs)

View(Eggs)
boxplot(Cases~ First.Week, data = Eggs)



# Median and quartile values: 
# Median : No, just below 100000, 99000
#          Yes, just above 100000, 101000
#quartile: No: 25th: 95000, 75th: 102000
#          Yes: 25th: 99000, 75th: 110000

###
#Scatterplot Matrix
#plot a scatterplot matrix, the function is in the cars package
# R formula Notation for the scatterplot variables: ~ X1 + X2 + ...
#
# Note: drag plot window to be large enough to show plots clearly
scatterplotMatrix(~Beef.Pr + Cases + Cereal.Pr + Chicken.Pr + Egg.Pr
                  + Pork.Pr + Week,
                  diagonal=list(method="boxplot"), #boxpplot on diagonal
                  col = "light blue",
                  data=Eggs)


# There is somewhat a gentle positive correlation between Beef Price and Egg Cases, 
# illustrating by the upward regression line. 

#Create a linear regression model 
# R Formula Notation: Y~ X1 + X2 + ...

LinearEggs <- lm(formula = Cases ~ Cereal.Pr + Chicken.Pr + Easter
                 + First.Week + Egg.Pr + Month + Pork.Pr + 
                   Beef.Pr, data=Eggs)
summary(LinearEggs)


# Which of those are likely yo have some imact generalizable to the population:
# Cereal Price: 98% confidence
# First week: more than 99% confidence
# Egg Price: mroe than 99% confidence
# On December, compared to April 
# On January
# On November, 
# Beef Price

###
# Interpreting Categorical Predictors
#

levels(Eggs$Month) #Examine the current ordering

#relevel Month 
# the relevel function is in the forcats package
Eggs$Month.New <- fct_relevel(Eggs$Month, "January", "February",
                              "March", "April", "May", "June",
                              "July", "August", "September", "October"
                              , "November", "December")
levels(Eggs$Month.New)
#relevel Easter
Eggs$Easter.New <- fct_relevel(Eggs$Easter, 
                               "Non Easter", "Pre Easter",
                               "Easter", "Post Easter")
levels(Eggs$Easter.New)

LinearEggs <- lm(formula = Cases ~ Cereal.Pr + Chicken.Pr + 
                   Easter.New + First.Week + Egg.Pr + Month.New
                 + Pork.Pr + Beef.Pr, data=Eggs)
summary(LinearEggs)

#Describe the seasonal pattern of egg sales>

#Effect size plots for Egg Prices from the LinearEggs model
plot(predictorEffects(LinearEggs,"Egg.Pr"))

plot(predictorEffects(LinearEggs,c("Egg.Pr", "Easter.New", "Beef.Pr")))

plot(allEffects(LinearEggs))

Anova(LinearEggs)

#
# Natural-Log transform "Cases" and all prices variables into new variables
Eggs$Log.Cases <- log(Eggs$Cases)
Eggs$Log.Egg.Pr <- log(Eggs$Egg.Pr)
Eggs$Log.Beef.Pr <- log(Eggs$Beef.Pr)
Eggs$Log.Pork.Pr <- log(Eggs$Pork.Pr)
Eggs$Log.Chicken.Pr <- log(Eggs$Chicken.Pr)
Eggs$Log.Cereal.Pr <- log(Eggs$Cereal.Pr)

#Veryfy transformation
glimpse(Eggs)

LinearEggs2 <- lm(formula = Log.Cases ~ Log.Cereal.Pr + 
                    Log.Chicken.Pr + Easter.New + First.Week +
                    Log.Egg.Pr + Month.New 
                 + Log.Pork.Pr + Log.Beef.Pr, data=Eggs)
summary(LinearEggs2)
summary(LinearEggs)
#
# 
Eggs$Month.New <- recode_factor(Eggs$Month.New,
                       `Month.NewMay` = "Month.NewFebruary",
                       `Month.NewMay` = "Month.Newmarch",
                       `Month.NewMay` = "Month.NewNovember",
                       `Month.NewMay` = "Month.NewDecember",
                       `Month.NewMay` = "Month.NewMay"
                   )
View(Eggs)

na.omit(Eggs$Month.New2)



LinearEggs15 <- lm(formula = Cases ~ Cereal.Pr + 
                   Easter.New + First.Week +
                    Egg.Pr + Month.New
                   + Beef.Pr, data=Eggs) 
summary(LinearEggs15)

LinearEggs15 <- lm(formula = Cases ~ Cereal.Pr +
                     Easter.New + First.Week + 
                     Egg.Pr + Month.New23 + Beef.Price)
