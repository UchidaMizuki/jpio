test_that("skyline", {
  # library(dibble)

  x <- iotable_sector13_2011

  as_tibble_unnest <- function(x, n) {
    x |>
      tibble::as_tibble(n = n) |>
      tidyr::unnest(output,
                    names_sep = "_")
  }

  # total input
  totalinput <- total_input(x) |>
    as_tibble_unnest(n = "totalinput") |>
    dplyr::mutate(totalinput = totalinput / sum(totalinput)) |>
    dplyr::mutate(xmax = cumsum(totalinput),
                  xmin = dplyr::lag(xmax,
                                    default = 0),
                  .keep = "unused")

  # self sufficiency
  leontiefinv <- leontief_inv(x,
                              mat_type = "closed")

  finaldemand <- x |>
    dplyr::filter(input$type == "industry",
                  output$type == "finaldemand") |>
    dibble::apply("input", sum)

  export <- x |>
    dplyr::filter(input$type == "industry",
                  output$type == "export") |>
    dibble::apply("input", sum)

  import <- x |>
    dplyr::filter(input$type == "industry",
                  output$type == "import") |>
    dibble::apply("input", sum)

  # induced production value
  induced_by_finaldemand <- leontiefinv %*% finaldemand
  induced_by_export <- leontiefinv %*% export
  induced_by_import <- leontiefinv %*% import

  self_sufficiency_export <- induced_by_export / induced_by_finaldemand
  self_sufficiency_import <- induced_by_import / induced_by_finaldemand

  domestic_production <- as_tibble_unnest(1 + self_sufficiency_export + self_sufficiency_import,
                                          n = "domestic_production") |>
    tibble::add_column(ymin = 0) |>
    dplyr::rename(ymax = domestic_production)
  import_substitution <- as_tibble_unnest(1 + self_sufficiency_export,
                                          n = "import_substitution") |>
    dplyr::rename(ymax = import_substitution) |>
    dplyr::left_join(domestic_production |>
                       dplyr::select(!ymin) |>
                       dplyr::rename(ymin = ymax),
                     by = c("output_type", "output_name"))

  self_sufficiency <- list(domestic_production = domestic_production,
                           import_substitution = import_substitution) |>
    dplyr::bind_rows(.id = "fill") |>
    dplyr::left_join(totalinput,
                     by = c("output_type", "output_name"))

  self_sufficiency |>
    ggplot2::ggplot() +
    ggplot2::geom_rect(ggplot2::aes(xmin = xmin,
                                    xmax = xmax,
                                    ymin = ymin,
                                    ymax = ymax,
                                    fill = fill)) +
    ggplot2::geom_hline(yintercept = 1)
})
