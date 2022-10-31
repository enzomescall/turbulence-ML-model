library(tidyverse)
library(splines)
library(boot)

data.train = read.csv("data-train.csv")
data.test = read.csv("data-test.csv") 

clean = data.train %>%
  mutate(Fr = as.factor(Fr),
         Re = as.factor(Re))

# final models
model_m1 = glm(log(R_moment_1) ~ Re + log(St) + Fr*St, data = clean)
model_m2 = lm(log(R_moment_2) ~ Re + Fr + log(St) + St*Re*Fr, data = clean)
model_m3 = lm(log(R_moment_3) ~ Re + Fr + log(St) + St*Re*Fr, data = clean)
model_m4 = lm(log(R_moment_4) ~ Re + Fr + log(St) + St*Re*Fr, data = clean)

cv_mse = function(input_model, k = 5, iter = 25, data = clean) {
  sum_mse = 0
  for(i in 1:iter) {
    sum_mse = sum_mse + cv.glm(input_model, data = clean, K = k)$delta[1]
  }
  avg_mse = sum_mse/iter
  return(avg_mse)
}

predictive_model = function(test_St, test_Re, test_Fr) {
  newdata = data.frame(list(St = test_St, Re = as.factor(test_Re), Fr = as.factor(test_Fr)))
  
  m1 = predict(model_m1, newdata =  newdata) 
  m2 = predict(model_m2, newdata =  newdata)
  m3 = predict(model_m3, newdata =  newdata) 
  m4 = predict(model_m4, newdata =  newdata) 
  
  return(m1)
}
