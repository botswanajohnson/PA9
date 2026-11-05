# fuel_efficiency.R
# Botswana Johnson
# 11/05/2026
# analyze fuel efficiency using linear regression

# plot MPG against vehicle weight
plot(mtcars$wt, mtcars$mpg,
     main = "Fuel Efficiency vs Vehicle Weight",
     xlab = "Vehicle Weight (1000 lbs)",
     ylab = "Miles Per Gallon (MPG)",
     pch = 19)

# fit a linear regression model
model <- lm(mpg ~ wt, data = mtcars)

# add the regression line to the plot
abline(model)

# summarize the regression model
model_summary <- summary(model)

model_summary

# find the 95% confidence interval
model_ci <- confint(model)

model_ci

# print the slope and p-value
slope <- coef(model)[2]
pval <- model_summary$coefficients[2, 4]

cat("Slope Estimate:", slope, "\n")
cat("p-value:", pval, "\n")
cat("95% Confidence Interval for Slope:", model_ci[2, ], "\n")

# explain what the slope means
cat("For every 1000 pounds, MPG decreases by about 5.34.\n")

# check if the slope is significant
if (pval < 0.05) {
  cat("The slope is statistically significant at the 0.05 level.\n")
} else {
  cat("The slope is not statistically significant at the 0.05 level.\n")
}

# create data for a 6000-pound vehicle
new_car <- data.frame(wt = 6)

# predict MPG with a 95% prediction interval
prediction <- predict(model, newdata = new_car,
                      interval = "prediction", level = 0.95)

# print the predicted MPG and prediction interval
cat("Predicted MPG for a 6,000-pound vehicle:\n")
print(prediction)

# explain the reliability of the prediction
cat("The weight is outside the range of the dataset.\n")
cat("The prediction may not be accurate, so use caution.\n")