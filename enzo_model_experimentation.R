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
  degree_i = glm(R_moment_1 ~ Re + Fr + Re*poly(St, degree), data = clean)
  poly_mse_cv[degree] = cv.glm(degree_i, data = clean)$delta[1]
}
min(poly_mse_cv)
which.min(poly_mse_cv)
# 0.0001079192 current best, R_moment_1 ~ Fr + Re*poly(St, 2)

# experimenting for model 2
errors = matrix(NA, nrow=10, ncol=10)
for (j in 1:10) {
  for (i in j:10) {
    lm = glm(R_moment_2 ~ bs(St, df = i, degree = j)*Re + Fr, data = clean)
    errors[[i,j]] = suppressWarnings(cv.glm(data = clean, glmfit = lm)$delta[1])
  }
}
errors

under_100 = clean %>%
  filter(R_moment_2 < 100)

over_100 = clean %>%
  filter(R_moment_2 >= 100)

errors = matrix(NA, nrow=10, ncol=10)
for (j in 1:10) {
  for (i in j:10) {
    lm = glm(R_moment_2 ~ log(St)*Re*Fr, data =  clean)
    errors[[i,j]] = suppressWarnings(cv.glm(data = clean, glmfit = lm)$delta[1])
  }
}
errors

rm2_over = data.train %>%
  filter(Re == 90) %>%
  mutate(Fr = as.factor(Fr),
         Re = as.factor(Re))

rm2_under = data.train %>%
  filter(Re != 90) %>%
  mutate(Fr = as.factor(Fr),
         Re = as.factor(Re))

rm2_over %>%
  ggplot(aes(x = St, y = R_moment_2, color = Re, shape = Fr)) +
  geom_smooth(method = "lm", formula = "y ~ log(x)") +
  geom_point()

rm2_under %>%
  ggplot(aes(x = St, y = R_moment_2, color = Re, shape = Fr)) +
    geom_smooth(method = "lm", formula = "y ~ log(x)") +
    geom_point()

lm = glm(R_moment_2 ~ log(St)*Fr, data =  rm2_over)
cv.glm(data = rm2_over, glmfit = lm)$delta[1]
summary(lm)

lm = glm(R_moment_2 ~ log(St)*Re + Fr, data = rm2_under)
cv.glm(data = rm2_under, glmfit = lm)$delta[1]
summary(lm)

predictive_model = function(test_St, test_Re, test_Fr) {
  newdata = data.frame(list(St = test_St, Re = as.factor(test_Re), Fr = as.factor(test_Fr)))
  
  m1 = predict(model_m1, newdata =  newdata) 
  m2 = predict(model_m2, newdata =  newdata)
  m3 = predict(model_m3, newdata =  newdata) 
  m4 = predict(model_m4, newdata =  newdata) 
  
  return(m1)
}
