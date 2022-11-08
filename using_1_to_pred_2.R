library(tidyverse)
library(splines)
library(boot)

data.train = read.csv("data-train.csv")
data.test = read.csv("data-test.csv") 

clean = data.train %>%
  mutate(Fr = as.factor(Fr),
         Re = as.factor(Re))

lm1 = glm(R_moment_1 ~ log(St)*Fr*Re + St*Fr*Re, data = clean)
cv.glm(data = clean, glmfit = lm1)$delta[1]

predictions = clean
predictions$pred_m1 = predict(lm1, predictions)

lm2 = glm(R_moment_2 ~ log(St)*Fr*Re + St*Fr*Re, data = clean)
cv.glm(data = clean, glmfit = lm2)$delta[1]

predictions$pred_m2 = predict(lm2, predictions)

