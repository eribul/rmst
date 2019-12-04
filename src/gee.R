
geedata <-
  pseudo %>%
  nest(data = c(-days))

geefit <- function(d, f = mu_pseudo ~ Age70 + Sex + ECI) {
  geeglm(
    f,
    data      = d,
    id        = id,
    #jack      = TRUE,
    scale.fix = FALSE
  )
}

regr <-
  geedata %>%
  mutate(
    gee_Crude     = future_map(data, geefit, f = mu_pseudo ~ ECI),
    gee_Adjusted  = future_map(data, geefit)
  )

regr_tidy <-
  regr %>%
  pivot_longer(
    starts_with("gee"),
    names_to     = "model",
    values_to    = "gee",
    names_prefix = "gee_"
  ) %>%
  mutate(
    tidy = map(gee, broom::tidy, conf.int = TRUE)
  ) %>%
  unnest(tidy) %>%
  mutate(
    ci_days  = sprintf("%.2f (%.2f; %.2f)", estimate, conf.low, conf.high),
    ci_years = sprintf("%.2f (%.2f; %.2f)", estimate / 365.241, conf.low / 365.241, conf.high / 365.241),
    ci       = ifelse(as.numeric(days) <= 365, ci_days, ci_years)
  ) %>%
  select(days, model, term, starts_with("ci"))

cache("regr_tidy")

regr_table <-
  regr_tidy %>%
  select(-ci_days, -ci_years) %>%
  pivot_wider(names_from = days, values_from = ci) %>%
  mutate(
    term = gsub("70", " (- 70)", term),
    term = gsub("SexMale", "Male sex", term),
    term = gsub("ECI", "Elixhauser = ", term)
  ) %>%
  separate(
    term,
    c("term", "level"),
    sep = " = ",
    fill = "right"
  ) %>%
  group_by(model) %>%
  mutate(term = ifelse(duplicated(term), "", term)) %>%
  ungroup() %>%
  mutate(
    model = ifelse(duplicated(model), "", model),
    level = coalesce(level, "")
  ) %>%
  rename(
    `90 days`  = `90`,
    `365 days`   = `365`,
    `5 years`  = `1825`,
    `10 years` = `3650`
  )

cache("regr_table")
