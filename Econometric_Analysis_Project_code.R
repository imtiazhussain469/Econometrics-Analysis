#setting the working directory
setwd("C:/Users/A S Computer/Downloads")

#loading the dataset
golf = read.csv("golfdata.csv")

str(golf)

## Pricing strategirs

golf$MGS = as.factor(golf$MGS)

golf$WINTER = as.factor(golf$WINTER)

# Load the ggplot2 package
library(ggplot2)


# coloured barplot
ggplot(golf, aes(x = MGS, y = FEE, fill = WINTER, colour = WINTER)) + 
  geom_bar(stat = "identity", position = "dodge")

# coloured barplot
ggplot(golf, aes(x = MGS, y = FEESUB, fill = WINTER, colour = WINTER)) + 
  geom_bar(stat = "identity", position = "dodge")

# coloured barplot
ggplot(golf, aes(x = MGS, y = AVEFEE, fill = WINTER, colour = WINTER)) + 
  geom_bar(stat = "identity", position = "dodge")

t.test(golf$FEE~golf$MGS)

golf = na.omit(golf)

model = lm (ROUNDS ~ CART + RANGE + AVEFEE + FEESUB + RAIN + TEMP + WINTER + MGS   , data = golf )

summary(model)

summary.aov(model)



# Regression for paved cart paths
cart_model <- lm(ROUNDS ~ CART + WINTER + TEMP + RAIN + YARD + RATING + SLOPE + DISTANCE + AVEFEE + FEESUB, data = golf)

summary(cart_model)

summary.aov(cart_model)


par(mfrow=c(2,2))

plot(cart_model)
