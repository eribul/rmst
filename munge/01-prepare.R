df <-
  df_orig %>%
  filter(is.na(DateOfDeath) | DateOfDeath >= P_SurgDate) %>%
  mutate(
    Age = as.numeric(P_Age),
    Sex = factor(P_Gender, c("Man", "Kvinna"), c("Male", "Female")),
    t   = pmin(DateOfDeath, as.Date("2018-02-01"), na.rm = TRUE) - P_SurgDate,
    t   = as.numeric(t) %>% pmax(.01),
    d   = !is.na(DateOfDeath)
  )

cache("df")
