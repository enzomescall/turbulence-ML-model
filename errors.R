library(kableExtra)
all_errors_df <- data.frame(
  model = 1:4,
  error = c(6.255864e-05, 359.2733, 37922043707, 3.528563e+18),
  log_error = c(7.842919e-05, 76171.24, 8.249564e+13, 3.728082e+23)
)

all_errors_df %>%
  kable()

lm1_log_error
lm2_log_error
lm3_log_error
lm4_log_error