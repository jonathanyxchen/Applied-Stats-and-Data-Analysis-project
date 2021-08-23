data <- read.csv('TRAIN_ver04.csv')
full_model <- lm(NMONTHS ~ . - LID - REMMNTHS - TIMGAP2 - FORCLOSED - ADJRMTHS, data = data)

#backward elimination with AIC
AIC_model <- step(full_model, direction = "both")

#backward elimination with BIC
number_rows <- nrow(data)
BIC_model <- step(full_model, direction = "both", k = log(number_rows))

#backward elimination with adjusted R-squared
variables <- names(data)
variables <- variables[-1]
variables <- variables[-7]
variables <- variables[1:22]
variables <- variables[-7]

listOfModels <- vector("list", length(variables))
# loop over features
updated_r_squared <- summary(full_model)$adj.r.squared
updated_variables <- variables
repeat {
  original_r_squared <- updated_r_squared
  variables <- updated_variables
  for (i in seq_along(variables)) {
  # exclude feature i
    currentFeatures <- variables[-i]
  # programmatically assemble regression formula
    regressionFormula <- as.formula(
      paste("NMONTHS ~ ", paste(currentFeatures, collapse="+")))
  # fit model
    currentModel <- lm(formula = regressionFormula, data = data)
  # store model in container
    if (summary(currentModel)$adj.r.squared > updated_r_squared) {
      updated_variables <- currentFeatures
      updated_r_squared <- summary(currentModel)$adj.r.squared
    }
  } 
  if (updated_r_squared == original_r_squared) {
    break
  }
}