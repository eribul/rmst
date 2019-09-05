
elix_fit <- survival::survfit(Surv(t, d) ~ ECI, df)
cache("elix_fit")


# Calculate RSTM and RMTL based on KM-fit

rm_ci <- function(days, fit, alpha = 0.05 ){

  sm   <- survival:::survmean(fit, rmean = days)[[1]]
  rmst <- sm[,"*rmean"] / 365
  z    <- qnorm(1 - alpha / 2)

  tibble(
    strata    = names(rmst),
    rmst      = rmst,
    rmtl      = days / 365 - rmst,
    se        = sm[,"*se(rmean)"] / 365,
    rmst_ll   = rmst - z * se,
    rmst_ul   = rmst + z * se,
    rmtl_ll   = rmtl - z * se,
    rmtl_ul   = rmtl + z * se,
    rmst_text = sprintf("%.2f (%.2f-%.2f)", rmst, rmst_ll, rmst_ul),
    rmtl_text = sprintf("%.2f (%.2f-%.2f)", rmtl, rmtl_ll, rmtl_ul)
  )
}

# Prepare all dta --------------------------------------------------------------

days <- sort(unique(c(
  c(30, 90, 365 * c(1, 5, 10)), # Days of special interest
  seq(1, 10 * 365, len = 100))) # other days to fill in for plot
)

rmst_rmtl <-
  tibble(
    days = days
  ) %>%
  mutate(
    days_lost = map(days, rm_ci, elix_fit)
  ) %>%
  unnest(days_lost) %>%
  mutate(years = days / 365) %>%
  nest(-strata)

cache("rmst_rmtl")
