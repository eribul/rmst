library(ProjectTemplate)
load.project()

rmst %>%
  mutate(
    Elixhauser = factor(Elixhauser, paste("Elixhauser", c(0:3, "4+")))
  ) %>%
  ggplot(aes(Time, rmst, col = Elixhauser)) +
  geom_line() +
  geom_ribbon(aes(ymin = ll, ymax = ul, fill = Elixhauser, alpha = 0.1, col = NULL), show.legend = FALSE)+
  theme_minimal() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_x_continuous(limits = c(0, 10), expand = c(0, 0), breaks = 0:10, minor_breaks = NULL) +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0), breaks = 0:10, minor_breaks = NULL) +
  ylab("Life yeras left") +
  xlab("Years since THA") +
  theme(
    legend.position      = c(0, 1),
    legend.justification = c(0, 1),
    legend.title         = element_blank()
  )

ggsave("graphs/rmst.png", height = 10, width = 10, units = "cm")



#### RMLT
rmtl %>%
  mutate(
    Elixhauser = factor(Elixhauser, paste("Elixhauser", c(0:3, "4+")))
  ) %>%
ggplot(aes(Time, rr)) +
  geom_line(aes(col = Elixhauser)) +
  geom_hline(yintercept = 1, color = scales::hue_pal()(1)) +
  geom_ribbon(aes(ymin = ll, ymax = ul, fill = Elixhauser, alpha = 0.1), show.legend = FALSE) +
  ylab("Restricted Mean Time Lost Ratio") +
  xlab("Years since THA") +
  theme_minimal() +
  theme(
    legend.position      = c(1, 1),
    legend.justification = c(1, 1),
    legend.title = element_blank()
  ) +
  scale_color_discrete(drop = FALSE) +
  scale_fill_discrete(drop = FALSE) +
  scale_x_continuous(breaks = 0:10, minor_breaks = NULL) +
  scale_y_continuous(breaks = 0:8, minor_breaks = NULL)


ggsave("graphs/rmtl.png", height = 10, width = 10, units = "cm")
