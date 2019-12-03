df <-
  df %>%
  mutate(
    ECI = as.factor(ECI),
    Sex = factor(Sex, c("Female", "Male"), c("Female", "Male")),
    Age70 = Age - 70
  ) %>%
  rownames_to_column("id")


f <- prodlim(Hist(t, d) ~ 1, df)

ds <- seq_len(3650)
ps <- jackknife(f, times = ds); gc()


ps <- apply(ps, 1, cumsum); gc()
ps <- t(ps)

ds_show <- c(90, 365 * c(1, 5, 10))
ps_ds <- ps[, ds_show]
colnames(ps_ds) <- paste0("mu.", ds_show)

pseudo <-
  df %>%
  bind_cols(as_tibble(ps_ds)) %>%
  pivot_longer(
    starts_with("mu."),
    names_to     = "days",
    names_prefix = "mu.",
    values_to    = "mu_pseudo"
  )

cache("pseudo")
