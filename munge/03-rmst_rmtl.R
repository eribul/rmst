
elix_fit <- survival::survfit(Surv(t, d) ~ ECI, df)
cache("elix_fit")


# Calculate RSTM and RMTL based on KM-fit

rm_days <- function(days, fit, alpha = 0.05 ){

  sm   <- survival:::survmean(fit, rmean = days)[[1]]
  rmst <- sm[,"*rmean"]
  z    <- qnorm(1 - alpha / 2)

  tibble(
    strata    = names(rmst),
    rmst      = rmst,
    rmtl      = days - rmst,
    se        = sm[,"*se(rmean)"],
    rmst_ll   = pmax(0L, rmst - z * se),
    rmst_ul   = rmst + z * se,
    rmtl_ll   = pmax(0L, rmtl - z * se),
    rmtl_ul   = rmtl + z * se
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
    days_lost = map(days, rm_days, elix_fit)
  ) %>%
  unnest(days_lost) %>%
  mutate(
    rmst_text_days = sprintf("%.1f (%.1f-%.1f)", rmst, rmst_ll, rmst_ul),
    rmtl_text_days = sprintf("%.1f (%.1f-%.1f)", rmtl, rmtl_ll, rmtl_ul),

    years = days / 365, # Integer for later identification/filtering
    rmst_text_years =
      sprintf("%.2f (%.2f-%.2f)",
        rmst / 365.241, rmst_ll / 365.241, rmst_ul / 365.241),
    rmtl_text_years =
      sprintf("%.2f (%.2f-%.2f)",
        rmtl / 365.241, rmtl_ll / 365.241, rmtl_ul / 365.241)
  ) %>%
  nest(data = -strata)

cache("rmst_rmtl")
