library(ProjectTemplate)
load.project()

table_rmst_rmtl <-
  rmst_rmtl %>%
  unnest(data) %>%
  filter(days %in% c(90, 365 * c(1, 5, 10))) %>%
  transmute(
    Elixhauser = gsub("ECI=", "", strata),
    time = case_when(
      years < 1  ~ "1 / 4 years",
      years == 1 ~ "1 year",
      years > 1  ~ sprintf("%.0f years", years)
    ),
    RMST = rmst_text,
    RMTL = rmtl_text
  ) %>%
  gather("Measure", "est with 95 % CI", RMST, RMTL) %>%
  spread(time, `est with 95 % CI`) %>%
  mutate(
    Elixhauser = if_else(duplicated(Elixhauser), "", Elixhauser)
  ) %>%
  select(Elixhauser, Measure, `1 year`, `5 years`, `10 years`)

cache("table_rmst_rmtl")
