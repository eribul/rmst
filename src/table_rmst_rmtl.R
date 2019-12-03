library(ProjectTemplate)
load.project()

table_rmst_rmtl <-
  rmst_rmtl %>%
  unnest(data) %>%
  filter(days %in% c(90, 365 * c(1, 5, 10))) %>%
  transmute(
    Elixhauser = gsub("ECI=", "", strata),
    time = case_when(
      years < 1  ~ "90 days",
      years == 1 ~ "365 days",
      years > 1  ~ sprintf("%.0f years", years)
    ),
    RMST = if_else(time %in% c("90 days", "365 days"), rmst_text_days, rmst_text_years),
    RMTL = if_else(time %in% c("90 days", "365 days"), rmtl_text_days, rmtl_text_years)
  ) %>%
  gather("Measure", "est with 95 % CI", RMST, RMTL) %>%
  spread(time, `est with 95 % CI`) %>%
  mutate(
    Elixhauser = if_else(duplicated(Elixhauser), "", Elixhauser)
  ) %>%
  select(Elixhauser, Measure, `90 days`, `365 days`, `5 years`, `10 years`)

cache("table_rmst_rmtl")
