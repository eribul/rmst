library(ProjectTemplate)
load.project()

table_rmst_rmtl <-
  rmst_rmtl %>%
  unnest(data) %>%
  filter(days %in% c(90, 365 * c(1, 5, 10))) %>%
  transmute(
    ECI = gsub("ECI=", "", strata),
    time = case_when(
      years < 1  ~ sprintf("%.0f days", days),
      years == 1 ~ "1 year",
      years > 1  ~ sprintf("%.0f years", years)
    ),
    RMST = rmst_text,
    RMTL = rmtl_text
  ) %>%
  gather("Measure", "est with 95 % CI", RMST, RMTL) %>%
  spread(time, `est with 95 % CI`) %>%
  mutate(
    ECI = if_else(duplicated(ECI), "", ECI)
  ) %>%
  select(ECI, Measure, `90 days`, everything())

cache("table_rmst_rmtl")
