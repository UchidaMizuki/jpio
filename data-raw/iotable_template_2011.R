library(tidyverse)
library(jpstat)

pkgload::load_all(".")

# iotable -----------------------------------------------------------------

appId <- keyring::key_get("estat-api")

iotable_template_2011 <- estat(appId, "https://www.e-stat.go.jp/dbview?sid=0003119272")
iotable_template_2011 <- iotable_template_2011 |>
  activate(tab) |>
  # unit: 1 million yen

  select() |>

  activate(cat01) |>
  rekey("output") |>
  select(name) |>

  activate(cat02) |>
  rekey("input") |>
  select(name) |>

  collect("value_M")

iotable_template_2011 <- iotable_template_2011 |>
  filter(!input_name %in% c("70_内生部門計",
                            "96_粗付加価値部門計",
                            "97_国内生産額"),
         !output_name %in% c("70_内生部門計",
                             "78_国内最終需要計",
                             "79_国内需要合計",
                             "82_最終需要計",
                             "83_需要合計",
                             "87_（控除）輸入計",
                             "88_最終需要部門計",
                             "97_国内生産額")) |>
  mutate(input_type = case_when(str_starts(input_name, "[01]") ~ "industry",
                                str_starts(input_name, "[79]") ~ "valueadded"),
         output_type = case_when(str_starts(output_name, "[01]") ~ "industry",
                                 str_starts(output_name, "7") ~ "finaldemand",
                                 str_starts(output_name, "81") ~ "export",
                                 str_starts(output_name, "8[4-6]") ~ "import"),
         value = parse_number(value_M) * 1e6) |>
  select(!value_M) |>
  relocate(input_type, input_name, output_type, output_name) |>
  as_iotable()

usethis::use_data(iotable_template_2011,
                  overwrite = TRUE)
