
sql_prim <-
  tbl(con, "primary") %>%
  left_join(tbl(con, "operations_factors_opnr"), c("LopNr", "P_Side")) %>%
  left_join(tbl(con, "elix_1yr_before"),  c("LopNr", "P_Side")) %>%
  select(
    LopNr, P_SurgDate, DateOfDeath, P_Gender, P_Age,
    P_ProstType, P_DiaGrp, op_last, elix_icd10_index_sum_all
  )

df_orig <-
  collect(sql_prim) %>%
  mutate_if(is.character, na_if, "NA") %>%
  mutate_at(vars(contains("Date")), as.Date)

cache("df_orig")
