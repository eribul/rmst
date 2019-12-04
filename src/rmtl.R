library(ProjectTemplate)
load.project()

rmst_rmtl <- mutate(rmst_rmtl, strata = as.factor(strata))

# Reference values from ECI = 0
rmtl_ref <-
  rmst_rmtl %>%
  filter(strata == "ECI=0") %>%
  select(data) %>%
  pluck(1, 1) %>%
  select(years, rmtl, se)

# Relative RMTL compared to ECI = 0
rmtl_rr <-
  rmst_rmtl %>%
  filter(strata != "ECI=0") %>%
  mutate(
    data = map(data, left_join, rmtl_ref, "years")
  ) %>%
  unnest(data) %>%
  transmute(
    strata,
    years,
    rr  = rmtl.x / rmtl.y,
    std = sqrt((se.x ^ 2 + rr ^ 2 * se.y ^ 2) / rmtl.y ^ 2),
    ll  = rr - 1.96 * std,
    ul  = rr + 1.96 * std,
    Elixhauser = factor(strata, levels(strata), gsub("ECI=", "", levels(strata))),
  ) %>%
  filter(years >= 1)

cache("rmtl_rr")

# RMTL Figure -----------------------------------------------------------------

rmtl_rr %>%
  ggplot(aes(years, rr)) +
  geom_line(aes(col = Elixhauser)) +
  geom_hline(yintercept = 1, color = scales::hue_pal()(1)) +
  geom_ribbon(
    aes(ymin = ll, ymax = ul, fill = Elixhauser, alpha = 0.1),
    show.legend = FALSE
  ) +
  ylab("Restricted Mean Time Lost Ratio") +
  xlab("Years since THA") +
  theme_minimal() +
  theme(
    legend.position      = c(1, 1),
    legend.justification = c(1, 1)
  ) +
  scale_color_discrete(drop = FALSE) +
  scale_fill_discrete(drop = FALSE) +
  scale_x_continuous(breaks = 0:10, minor_breaks = NULL) +
  scale_y_continuous(breaks = 0:8, minor_breaks = NULL)

ggsave("graphs/rmtl.png", height = 10, width = 10, units = "cm")
ggsave("graphs/rmtl.tiff", height = 10, width = 10, units = "cm", dpi = 1200, compression = "lzw")

