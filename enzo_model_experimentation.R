library(tidyverse)
library(splines)
library(boot)

data.train = read.csv("data-train.csv")
data.test = read.csv("data-test.csv") 

clean = data.train %>%
  mutate(Fr = as.factor(Fr),
         Re = as.factor(Re))

summary(lm(R_moment_2 ~ Fr + St + Re, data = clean))

set.seed(1)
k = 5
range = 1:10

poly.RSS = c()

for(i in range){
  poly.fit = glm(R_moment_1 ~ poly(St, degree = i) + poly(Re, degree = i) + Fr, data = clean)
  RSS = cv.glm(poly.fit, data = clean, K = k)$delta[1]
  poly.RSS[i] = RSS
}

plot_df = data.frame(y = poly.RSS) %>%
  mutate(x = row_number())

min_degree = which.min(poly.RSS)
min_RSS = poly.RSS[min_degree]

ggplot(data = plot_df, aes(x = x, y = y)) +
  geom_line() +
  geom_point(x = min_degree, y = min_RSS, color = "red") +
  labs(title = paste0(k, "-fold CV RSS"),
       xlab = "Polynomial Degree", ylab = "CV RSS")

spline.RSS = c()

for(i in range){
  spline.fit = glm(R_moment_1 ~ bs(St, df = i, degree = 1) + bs(Re, df = i, degree = 1) + Fr, data = clean)
  
  RSS = cv.glm(spline.fit, data = clean, K = k)$delta[1]
  
  spline.RSS[i] = RSS
}

plot_df = data.frame(y = spline.RSS) %>%
  mutate(x = row_number())

min_degree = which.min(spline.RSS)
min_RSS = spline.RSS[min_degree]

ggplot(data = plot_df, aes(x = x, y = y)) +
  geom_line() +
  geom_point(x = min_degree, y = min_RSS, color = "red") +
  labs(title = paste0(k, "-fold CV RSS"), x = "DF for St's spline", y = "CV RSS")

# final models
model_m1 = lm(R_moment_1 ~ Re + Fr + St, data = clean)
model_m2 = lm(R_moment_2 ~ Re + Fr + St, data = clean)
model_m3 = lm(R_moment_3 ~ Re + Fr + St, data = clean)
model_m4 = lm(R_moment_4 ~ Re + Fr + St, data = clean)

predictive_model = function(test_St, test_Re, test_Fr) {
  newdata = data.frame(list(St = test_St, Re = as.factor(test_Re), Fr = as.factor(test_Fr)))
  
  m1 = predict(model_m1, newdata =  newdata)
  m2 = predict(model_m2, newdata =  newdata)
  m3 = predict(model_m3, newdata =  newdata)
  m4 = predict(model_m4, newdata =  newdata)
  
  return(c(m1, m2, m3, m4))
}
