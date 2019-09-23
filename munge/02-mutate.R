df <-
  df %>%
  transmute(
    Age = as.numeric(P_Age),
    Sex = factor(P_Gender, c("Man", "Kvinna"), c("Male", "Female")),
    t   = pmin(DateOfDeath, as.Date("2018-02-01"), na.rm = TRUE) - P_SurgDate,
    t   = as.numeric(t) %>% pmax(.01),
    d   = !is.na(DateOfDeath),
    ECI = case_when(
      is.na(elix_icd10_index_sum_all) ~ "0",
      elix_icd10_index_sum_all >= 4     ~ "4+",
      TRUE                             ~ as.character(as.integer(elix_icd10_index_sum_all))
    )
  )

cache("df")
