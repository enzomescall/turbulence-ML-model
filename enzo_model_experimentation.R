library(tidyverse)
library(splines)
library(boot)

data.train = read.csv("data-train.csv")
data.test = read.csv("data-test.csv") 

clean = data.train %>%
  mutate(Fr = as.factor(Fr),
         Re = as.factor(Re))

# experimenting for model 1
poly_mse_cv = c()
for (degree in 1:10) {
  degree_i = glm(R_moment_1 ~ Re + Fr + poly(St, degree), data = clean)
  poly_mse_cv[degree] = cv.glm(degree_i, data = clean)$delta[1]
}
min(poly_mse_cv)
which.min(poly_mse_cv)

spline_mse = c(99,99,99)
for (df in 1:10) {
  degree_i = glm(R_moment_1 ~ Re + Fr + St*Re + bs(St, df = df), data = clean)
  spline_mse[df] = cv.glm(degree_i, data = clean)$delta[1]
}
min(spline_mse) # 0.000261363 current best, simple lm
which.min(spline_mse)

predictive_model = function(test_St, test_Re, test_Fr) {
  newdata = data.frame(list(St = test_St, Re = as.factor(test_Re), Fr = as.factor(test_Fr)))
  
  m1 = predict(model_m1, newdata =  newdata) 
  m2 = predict(model_m2, newdata =  newdata)
  m3 = predict(model_m3, newdata =  newdata) 
  m4 = predict(model_m4, newdata =  newdata) 
  
  return(m1)
}
