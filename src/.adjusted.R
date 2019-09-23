# Too time consuming to run in practice

library(ProjectTemplate)
load.project()

# Help function for crude or adjusted RMST
rmst <- function(x, adj = FALSE, tau = 30) {
  covs <- if (!adj) NULL else model.matrix(~ -1 + Age + Sex, x)

  with(
    x,
    survRM2::rmst2(t, as.numeric(d), ECI != "0", tau = tau, covariates = covs)
  )
}


df0 <- filter(df, ECI == "0")

dfs <-
  tibble(
    ECI = setdiff(unique(df$ECI), "0")
  ) %>%
  mutate(
    data        = map(ECI, ~bind_rows(filter(df, ECI == .), df0)),
    # rmst2_crude = map(data, rmst),
    rmst2_crude = map(data, rmst, adj = TRUE),
  )
