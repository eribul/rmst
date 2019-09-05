library(ProjectTemplate)
load.project()


rmst <-
  rmst_rmtl %>%
  unnest(data) %>%
  select(strata, years, contains("rmst"))

rmst %>%
  ggplot(aes(years, rmst, col = strata)) +
  geom_line() +
  geom_ribbon(
    aes(ymin = rmst_ll, ymax = rmst_ul, fill = strata, alpha = 0.1, col = NULL),
    show.legend = FALSE
  ) +
  theme_minimal() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_x_continuous(
    limits = c(0, 10), expand = c(0, 0), breaks = 0:10, minor_breaks = NULL) +
  scale_y_continuous(
    limits = c(0, 10), expand = c(0, 0), breaks = 0:10, minor_breaks = NULL) +
  ylab("Life yeras left") +
  xlab("Years since THA") +
  theme(
    legend.position      = c(0, 1),
    legend.justification = c(0, 1),
    legend.title         = element_blank()
  )

ggsave("graphs/rmst.png", height = 10, width = 10, units = "cm")
