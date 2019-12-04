df_tmp <-
  df_orig %>%
  filter(
    between(P_SurgDate, as.Date("1999-01-01"), as.Date("2015-12-31")),
    P_ProstType == "Totalprotes",
    P_DiaGrp == "Primär artros"
  )

n <- list()
n$all_tha <- nrow(df_tmp)
n$all_pat <- n_distinct(df_tmp$LopNr)

df_tmp      <- filter(df_tmp, as.logical(op_last))
n$incl1 <- nrow(df_tmp)
n$excl1 <- n$all_tha - n$incl1

df_tmp      <- filter(df_tmp, is.na(DateOfDeath) | DateOfDeath >= P_SurgDate)
n$goal  <- nrow(df_tmp)
n$excl2 <- n$incl1 - n$goal

ns <- map(n, format, big.mark = ",")


fl <-
  grViz("graphs/flowchart.gv") %>%
  export_svg %>%
  charToRaw

rsvg_png(fl, "graphs/flowchart.png")
rsvg_ps(fl, "graphs/flowchart.ps")
