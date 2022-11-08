library(tidyverse)
library(splines)
library(boot)

data.train = read.csv("data-train.csv")
data.test = read.csv("data-test.csv") 

predicted.test = data.test  %>%
  mutate(Fr = as.factor(Fr),
         Re = as.factor(Re))

clean = data.train %>%
  mutate(Fr = as.factor(Fr),
         Re = as.factor(Re))

expMSE = function(y1, y2) mean((exp(y1)-exp(y2))^2)

lm1 = glm(log(R_moment_1) ~ log(St)*Fr*Re + St*Fr*Re, data = clean)
cv.glm(data = clean, glmfit = lm1,  cost = expMSE)$delta[1]

predictions = clean
predictions$moment_1 = predict(lm1, predictions)

lm2 = glm(log(R_moment_2) ~ log(St)*Fr + log(St)*Fr, data = clean)
cv.glm(data = clean, glmfit = lm2,  cost = expMSE)$delta[1]

predictions$pred_m2 = predict(lm2, predictions)

lm3 = glm(log(R_moment_3) ~ moment_1*Fr*Re, data = predictions)
cv.glm(data = predictions, glmfit = lm3, cost = expMSE)$delta[1]

predictions$pred_m3 = predict(lm3, predictions)

lm4 = glm(log(R_moment_4) ~ log(St)*Fr*Re, data = predictions)
cv.glm(data = predictions, glmfit = lm4, cost = expMSE)$delta[1]

predictions$pred_m4 = predict(lm4, predictions)

predicted.test$moment_1 = exp(predict(lm1, predicted.test))
predicted.test$moment_2 = exp(predict(lm2, predicted.test))
predicted.test$moment_3 = exp(predict(lm3, predicted.test))
predicted.test$moment_4 = exp(predict(lm4, predicted.test))
