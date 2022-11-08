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

lm1 = glm(R_moment_1 ~ log(St)*Fr*Re + St*Fr*Re, data = clean)
cv.glm(data = clean, glmfit = lm1)$delta[1]

predictions = clean
predictions$moment_1 = predict(lm1, predictions)

lm2 = glm(R_moment_2 ~ log(St)*Fr*Re + St*Fr*Re, data = clean)
cv.glm(data = clean, glmfit = lm2)$delta[1]

predictions$pred_m2 = predict(lm2, predictions)

lm3 = glm(R_moment_3 ~ moment_1*Fr*Re, data = predictions)
cv.glm(data = predictions, glmfit = lm3)$delta[1]

predictions$pred_m3 = predict(lm3, predictions)

lm4 = glm(R_moment_4 ~ log(St)*Fr*Re, data = predictions)
cv.glm(data = predictions, glmfit = lm4)$delta[1]

predictions$pred_m4 = predict(lm4, predictions)

predicted.test$moment_1 = predict(lm1, predicted.test)
predicted.test$moment_2 = predict(lm2, predicted.test)
predicted.test$moment_3 = predict(lm3, predicted.test)
predicted.test$moment_4 = predict(lm4, predicted.test)


predicted.test %>%
  mutate(moment_1 = if_else(moment_1 < 0, 0, moment_1),
         moment_2 = if_else(moment_2 < 0, 0, moment_2),
         moment_3 = if_else(moment_3 < 0, 0, moment_3),
         moment_4 = if_else(moment_4 < 0, 0, moment_4))
