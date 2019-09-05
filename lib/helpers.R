con <- shar_linkage_con()


delta_test <- function(data, level){
  delta <- data$Est[level] - data$Est[1]
  std <- sqrt(data$Se[level]^2 + data$Se[1]^2)
  ll <- delta - qnorm(0.975)*std
  ul <- delta + qnorm(0.975)*std
  return(round(c(delta = delta, ll = ll, ul = ul), 2))
}
