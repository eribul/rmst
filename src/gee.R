
geedata <-
  pseudo %>%
  nest(data = c(-days))

geefit <- function(d) {
  geese(
    mu_pseudo ~ Age70 + Sex + ECI,
    data      = d,
    id        = id,
    jack      = TRUE,
    family    = gaussian,
    corstr    = "independence",
    scale.fix = FALSE
  )
}

tid <- function(fit) {
  mu  <- fit$beta
  sd  <- sqrt(diag(fit$vbeta.ajs))
  tibble(
    coef = names(mu),
    mean = mu,
    sd   = sd,
    Z    = mu / sd,
    p    = 2 - 2 * pnorm(abs(Z))
  )
}

regr <-
  geedata %>%
  mutate(
    gee = future_map(data, geefit),
    tidy = map(gee, tid)
  )


regr_table <-
  regr %>%
  unnest(tidy) %>%
   mutate(
    ci = sprintf("%.0f (%.0f - %.0f)", mean, mean - 1.96 * sd / nrow(df), mean + 1.96 * sd / nrow(df))
  ) %>%
  select(days, coef, mean) %>%
  pivot_wider(names_from = days, values_from = mean) %>%
  mutate(
    coef = gsub("70", " (- 70)", coef),
    coef = gsub("SexMale", "Male sex", coef),
    coef = gsub("ECI", "ECI = ", coef)
  ) %>%
  separate(
    coef,
    c("coef", "level"),
    sep = " = ",
    fill = "right"
  ) %>%
  mutate(
    coef = ifelse(duplicated(coef), "", coef),
    level = coalesce(level, "")
  ) %>%
  rename(
    `90 days` = `90`,
    `1 year` = `365`,
    `5 years` = `1825`,
    `10 years` = `3650`
  )

cache("regr_table")
