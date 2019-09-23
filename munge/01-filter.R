df <-
  df_orig %>%
  filter(
    between(P_SurgDate, as.Date("1999-01-01"), as.Date("2015-12-31")),
    P_ProstType == "Totalprotes",
    P_DiaGrp == "Primär artros"
  )

n <- list()
n$all_tha <- nrow(df)
n$all_pat <- n_distinct(df$LopNr)

df      <- filter(df, as.logical(op_last))
n$incl1 <- nrow(df)
n$excl1 <- n$all_tha - n$incl1

df      <- filter(df, is.na(DateOfDeath) | DateOfDeath >= P_SurgDate)
n$goal  <- nrow(df)
n$excl2 <- n$incl1 - n$goal

ns <- map(n, format, big.mark = ",")

cache("df")



fl <-
  grViz("graphs/flowchart.gv") %>%
  export_svg %>%
  charToRaw

rsvg_png(fl, "graphs/flowchart.png")
rsvg_ps(fl, "graphs/flowchart.ps")
