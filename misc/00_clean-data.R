library(tidyverse)
library(haven)

# Access from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/FVCGHC
tkr_raw <- read_dta("~/Downloads/dataverse_files/Ties that Double Bind Replication Data/data/conjoint_data.dta")

tkr <- tkr_raw |>
  zap_labels() |>
  filter(sample == "usa voter") |>
  mutate(
    cand_gender = factor(
      if_else(orig_cand_female == 1, "Female", "Male"),
      levels = c("Male", "Female")
    ),
    dv = factor(
      dv,
      levels = 1:3,
      labels = c("City council", "Congress", "Governor")
    ),
    # Making these more text-like so they don't conflict with things like 0 kids
    years_politics = case_when(
      orig_0ys == 1 ~ "None",
      orig_1ys == 1 ~ "1 year",
      orig_3ys == 1 ~ "3 years",
      orig_8ys == 1 ~ "8 years"
    ) |>
      factor(levels = c("None", "1 year", "3 years", "8 years")),
    spouse_occupation = case_when(
      orig_UN_sp == 1 ~ "Unmarried",
      orig_FM_sp == 1 ~ "Farmer",
      orig_MD_sp == 1 ~ "Doctor"
    ) |>
      factor(levels = c("Unmarried", "Farmer", "Doctor")),
    occupation = case_when(
      orig_teach == 1 ~ "Teacher",
      orig_law == 1 ~ "Corporate lawyer",
      orig_may == 1 ~ "Mayor",
      orig_leg == 1 ~ "State legislator"
    ) |>
      factor(
        levels = c("Teacher", "Corporate lawyer", "Mayor", "State legislator")
      ),
    n_children = case_when(
      orig_0ch == 1 ~ "0",
      orig_1ch == 1 ~ "1",
      orig_3ch == 1 ~ "3"
    ) |>
      factor(levels = c("0", "1", "3")),
    age = case_when(
      orig_29 == 1 ~ "29",
      orig_45 == 1 ~ "45",
      orig_65 == 1 ~ "65"
    ) |>
      factor(levels = c("29", "45", "65")),
    resp_gender = factor(
      if_else(female_respondent == 1, "Female", "Male"),
      levels = c("Male", "Female")
    ),
    resp_party = case_when(
      democrat_respondent == 1 ~ "Democrat",
      republican_respondent == 1 ~ "Republican",
      .default = "Independent"
    ) |>
      factor(levels = c("Democrat", "Republican", "Independent")),
    # Globally unique task ID: respondent × contest pair
    task_id = paste0(responseid, "_", contest)
  ) |>
  select(
    resp_id = responseid,
    resp_gender,
    resp_party,
    task_id,
    task = contest,
    choice = winner,
    gender = cand_gender,
    years_politics,
    spouse_occupation,
    occupation,
    n_children,
    age,
    office = dv
  )

saveRDS(tkr, "data/candidate.rds")
