library(ProjectTemplate)
load.project()

clean_names <- function(x) {
  gsub("elix_icd10_", "", x) %>%
  {gsub("_", " ", .)} %>%
  {paste0(toupper(substr(., 1, 1)), substring(., 2))} %>%
  {gsub("Aids hiv", "AIDS/HIV", .)}
}

df_table1 <-
  df %>%
  mutate(
    Event = if_else(d, "Dead", "Censored"),
    Sex,
    Age,
    Elixhauser = ECI
  ) %>%
  rename_at(vars(starts_with("elix_icd10_")), clean_names) %>%
  select(Age, Sex, Elixhauser, everything(), -t, -d, -ECI)



table1 <-
  tableone::CreateTableOne(
    setdiff(names(df_table1), "Event"),
    strata = "Event",
    data = df_table1
  )

cache("table1")
