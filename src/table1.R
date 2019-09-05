library(ProjectTemplate)
load.project()

df_table1 <-
  df %>%
  transmute(
    Event = if_else(d, "Dead", "Censored"),
    Sex,
    Age,
    ECI
  )

table1 <-
  tableone::CreateTableOne(
    setdiff(names(df_table1), "Event"),
    strata = "Event",
    data = df_table1,
    test = FALSE
  )

cache("table1")
