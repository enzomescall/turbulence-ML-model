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

expMSE = function(y1, y2) mean((exp(y1)-exp(y2))^2)
logMSE = function(y1, y2) mean((log(y1)-log(y2))^2)
RMSLE = function(y_true, y_pred) sqrt(mean((log(y_true + 1) - log(y_pred + 1))^2))
MAE = function(y_true, y_pred) mean(abs(y_true - y_pred))

lm = glm(R_moment_1 ~ log(St)*Fr*Re + St*Fr*Re, data = clean)
cv.glm(data = clean, glmfit = lm)$delta[1]

only_predictors = clean
only_predictors$m1 = predict(lm, only_predictors)

lm = glm(R_moment_1 ~ log(St)*Fr*Re, data = clean)
cv.glm(data = clean, glmfit = lm)$delta[1]

only_predictors$logm1 = predict(lm, only_predictors)

mse_df = only_predictors %>%
  mutate(explogm1 = exp(logm1),
         SEm1 = (R_moment_1 - m1)^2,
         SElogm1 = (R_moment_1 - exp(logm1))^2)

mean(mse_df$SEm1)
mean(mse_df$SElogm1)

# 6.255864e-05 current best, R_moment_1 ~ log(St)*Fr*Re + St*Re*Fr, data = clean

#
# experimenting for model 2
#

errors = matrix(NA, nrow=10, ncol=10)
for (j in 1:10) {
  for (i in j:10) {
    lm = glm(R_moment_2 ~ bs(St, df = i, degree = j)*Re + Fr, data = clean)
    errors[[i,j]] = suppressWarnings(cv.glm(data = clean, glmfit = lm)$delta[1])
  }
}
errors

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

lm_over = glm(R_moment_2 ~ log(St)*Fr + St*Fr, data =  rm2_over)
cv.glm(data = rm2_over, glmfit = lm_over)$delta[1]

lm_under = glm(R_moment_2 ~ log(St)*Re*Fr, data = rm2_under)
cv.glm(data = rm2_under, glmfit = lm_under)$delta[1]

SE = cv.glm(data = rm2_over, glmfit = lm_over)$delta[1]*nrow(rm2_over) + cv.glm(data = rm2_under, glmfit = lm_under)$delta[1]*nrow(rm2_under)
SE/(nrow(clean))

lm = glm(R_moment_2 ~ log(St)*Fr*Re + St*Fr*Re, data = clean)
cv.glm(data = clean, glmfit = lm)$delta[1]

###
### Trying for moment 3
###

lm = glm(R_moment_3 ~ log(St)*Fr*Re, data = clean)
cv.glm(data = clean, glmfit = lm)$delta[1]

errors = matrix(NA, nrow=10, ncol=10)
for (j in 1:10) {
  for (i in j:10) {
    lm = glm(R_moment_3 ~ bs(R_moment_1, df = i, degree = j)*Re*Fr*log(St), data = clean)
    errors[[i,j]] = suppressWarnings(cv.glm(data = clean, glmfit = lm)$delta[1])
  }
}
errors

which.min(errors)
min(errors[5])
