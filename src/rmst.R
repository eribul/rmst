library(ProjectTemplate)
load.project()


rmst <-
  rmst_rmtl %>%
  unnest(data) %>%
  select(strata, years, contains("rmst")) %>%
  mutate(Elixhauser = gsub("ECI=", "", strata))

rmst %>%
  ggplot(aes(years, rmst / 365.241, col = Elixhauser)) +
  geom_line() +
  geom_ribbon(
    aes(ymin = rmst_ll / 365.241, ymax = rmst_ul / 365.241, fill = Elixhauser, alpha = 0.1, col = NULL),
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
    legend.justification = c(0, 1)
  )

ggsave("graphs/rmst.png",  height = 10, width = 10, units = "cm")
ggsave("graphs/rmst.tiff", height = 10, width = 10, units = "cm", dpi = 1200, compression = "lzw")
