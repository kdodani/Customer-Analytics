### LECTURE 4 MARGINS AND LINEAR REGRESSION, MISSING VALUES, MEAN CENTERED APPROACH





#set working directory
rm(list=ls())

#import the data "flight_purchase.csv"
data = read.csv("flight_purchase.csv")
head(data)

#Visualize the data in a scatterplot where frequent flyers are shown with an X and non-frequent flyers as O
plot(data$minutes, data$spend, 
     pch = ifelse(data$loyalty_program,"x", "o"),
     col = ifelse(data$loyalty_program,2,1))


library(visreg)

# conduct a linear regression using spend as the dv, and minutes and loyalty program as the two independent variables
maineffects <- lm(spend ~ loyalty_program + minutes, data=data)
summary(maineffects)
visreg(maineffects, xvar="minutes", by="loyalty_program")

visreg(maineffects, xvar="minutes", by="loyalty_program", overlay="TRUE")



# conduct a linear regression based on the previous model (maineffects) but now build in the interaction between loaylty member and minutes
interaction <- lm(spend ~ loyalty_program + minutes + loyalty_program:minutes, data=data)
summary(interaction)

visreg(interaction, xvar="minutes", by="loyalty_program")

visreg(interaction, xvar="minutes", by="loyalty_program", overlay ="TRUE")



# Interaction model with frequent flyer coded the other way
# create a new variable non_loyalty ==1 or 0
data$non_loyalty = 1-data$loyalty_program

interaction_nonloyal <- lm(spend ~ non_loyalty + minutes + non_loyalty:minutes, data=data)
summary(interaction_nonloyal)

visreg(interaction_nonloyal, xvar="minutes", by="non_loyalty")

visreg(interaction_nonloyal, xvar="minutes", by="non_loyalty", overlay ="TRUE")



# Interaction model with mean-centered time spent
# try to think about why we would prefer mean-centered X first before running a linear regression? trying to interpret the inteercept when minutes is = 0 is awkward and doesnt really provide any meaningful info. using a mean centered approach would highlight how much one spends when they spend an average number of minutes online
## interpretation when all the other variables are zero...for the average non ff flyer who spends an average of 14 minutes online, they will spend the intercept amount

data$minutes_meancenter = data$minutes - mean(data$minutes)

interaction.centered <- lm(spend ~ loyalty_program + minutes_meancenter + loyalty_program:minutes_meancenter, data=data)
summary(interaction.centered)

visreg(interaction.centered, "minutes_meancenter", by="loyalty_program")
visreg(interaction.centered, "minutes_meancenter", by="loyalty_program", overlay ="TRUE")

mean(data$minutes)


  