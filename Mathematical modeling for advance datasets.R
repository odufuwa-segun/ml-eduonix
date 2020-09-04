
#
# 1. Open the Longley data set from the datasets package
# 2. Plot Employed against each variable and select the 3 variables most correlated to Employed.
# 3. For each variable selected in number 2, create a regression model for Employed. Select the best model.
# 4. For the champion model from number 3, create the model matrices.
# 5. Recalculate the regression parameters and predicted values from the model matrices.

# 1. Open longley dataset
data.00 = longley

# 2. Plot Employed against each variable
plot(x=data.00$Employed, y=data.00$GNP.deflator, col='red', pch = 16, xlim=c(50,80), ylim=c(80,370))
points(x=data.00$Employed, y=data.00$GNP, col='blue', pch = 16)
points(x=data.00$Employed, y=data.00$Unemployed, col='green', pch = 16)
points(x=data.00$Employed, y=data.00$Armed.Forces, col='coral', pch = 16)


plot(x=data.00$Employed, y=data.00$Year, col='violet', pch = 16)
# variables with correlation to Employed: GNP.deflator, GNP, Year

# 3. Create regression Model, select best model
model.GNP.deflator <- lm(Employed ~ GNP.deflator, data=data.00)
summary(model.GNP.deflator)

model.GNP <- lm(Employed ~ GNP, data=data.00) # intercept: 51.843590, GNP: 0.034752
summary(model.GNP)

model.Year <- lm(Employed ~ Year, data=data.00)
summary(model.Year)

# best model was selected to be with GNP since its has lower residual error, meaning
# it provides a much closer prediction to the output relative to the others
plot(data.00$Employed, model.GNP$fitted.values, col='red', pch=16)


len <- length(data.00$Employed)
len

# 4. Create the model matrices.
x.0 <- rep(1, len)  # unit vector for intercept term
x.1 <- data.00$GNP          # GNP vector
X <- cbind(x.0,x.1)

y <- data.00$Employed

beta <- solve(t(X) %*% X) %*% t(X) %*% y
beta # 51.84358978, 0.03475229  -> same as our answer above

library(tidyverse)

pred <- data.00 %>%
  mutate(Intercept = beta[1], 
         Employed.Prediction = Intercept + beta[2]*x.1)

pred

plot(pred$Employed, pred$Employed.Prediction)



