library(tidyverse)
library(jpstat)

pkgload::load_all()

# iotable_sector13_2011 ---------------------------------------------------

appId <- keyring::key_get("estat-api")

iotable_sector13_2011 <- estat(appId, "https://www.e-stat.go.jp/dbview?sid=0003119272")

iotable_sector13_2011 <- iotable_sector13_2011 |>
  activate(tab) |>
  select() |>

  activate(cat01) |>
  rekey("output") |>
  select(name) |>

  activate(cat02) |>
  rekey("input") |>
  select(name) |>

  collect("value_M_JPY") |>
  mutate(value_M_JPY = parse_number(value_M_JPY))

pattern_1to13 <- "^(0[1-9]|1[0-3])_"
pattern_subtotal <- "(内生部門計|粗付加価値部門計|国内最終需要計|最終需要計|需要合計|（控除）輸入計|最終需要部門計)$"
pattern_total <- "国内生産額$"
pattern_export <- "輸出計$"
pattern_import <- "(（控除）輸入|（控除）関税|（控除）輸入品商品税)$"

iotable_sector13_2011 <- iotable_sector13_2011 |>
  relocate(input_name, output_name) |>
  mutate(input_type = case_when(str_detect(input_name, pattern_1to13) ~ "industry",
                                str_detect(input_name, pattern_subtotal) ~ "subtotal",
                                str_detect(input_name, pattern_total) ~ "total",
                                TRUE ~ "valueadded"),
         output_type = case_when(str_detect(output_name, pattern_1to13) ~ "industry",
                                 str_detect(output_name, pattern_export) ~ "export",
                                 str_detect(output_name, pattern_import) ~ "import",
                                 str_detect(output_name, pattern_subtotal) ~ "subtotal",
                                 str_detect(output_name, pattern_total) ~ "total",
                                 TRUE ~ "finaldemand")) |>
  filter(input_type != "subtotal",
         output_type != "subtotal") |>
  relocate(input_type, input_name, output_type, output_name)

# total
total_output_sector13_2011 <- iotable_sector13_2011 |>
  filter(output_type == "total") |>
  select(!starts_with("output"))

total_input_sector13_2011 <- iotable_sector13_2011 |>
  filter(input_type == "total") |>
  select(!starts_with("input"))

iotable_sector13_2011 <- iotable_sector13_2011 |>
  filter(input_type != "total",
         output_type != "total")

stopifnot(
  iotable_sector13_2011 |>
    filter(input_type == "industry") |>
    group_by(input_type, input_name) |>
    summarise(value_M_JPY = sum(value_M_JPY),
              .groups = "drop") |>
    left_join(total_output_sector13_2011 |>
                rename(value_M_JPY_total = value_M_JPY),
              by = c("input_type", "input_name")) |>
    filter(!near(value_M_JPY, value_M_JPY_total)) |>
    vec_is_empty(),
  iotable_sector13_2011 |>
    filter(output_type == "industry") |>
    group_by(output_type, output_name) |>
    summarise(value_M_JPY = sum(value_M_JPY),
              .groups = "drop") |>
    left_join(total_input_sector13_2011 |>
                rename(value_M_JPY_total = value_M_JPY),
              by = c("output_type", "output_name")) |>
    filter(!near(value_M_JPY, value_M_JPY_total)) |>
    vec_is_empty()
)

iotable_sector13_2011 <- iotable_sector13_2011 |>
  as_iotable()

usethis::use_data(iotable_sector13_2011,
                  overwrite = TRUE)
