
sql_prim <-
  tbl(con, "primary") %>%
  left_join(tbl(con, "operations_factors_opnr"), c("LopNr", "P_Side")) %>%
  filter(
    between(P_SurgDate, "1999-01-01", "2015-12-31"),
    P_ProstType == "Totalprotes",
    P_DiaGrp == "Primär artros",
    op_last
  ) %>%
  left_join(tbl(con, "elix_1yr_before"),  c("LopNr", "P_Side")) %>%
  mutate(
    ECI = case_when(
      is.na(elix_icd10_index_sum_all) ~ "0",
      elix_icd10_index_sum_all >= 4     ~ "4+",
      TRUE                             ~ as.character(as.integer(elix_icd10_index_sum_all))
    )
  ) %>%
  select(LopNr, P_SurgDate, DateOfDeath, ECI, P_Gender, P_Age)

df_orig <-
  collect(sql_prim) %>%
  mutate_if(is.character, na_if, "NA") %>%
  mutate_at(vars(contains("Date")), as.Date)

cache("df_orig")
