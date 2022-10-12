#' Draw a skyline chart
#'
#' @param x An input-output table.
#'
#' @return A ggplot object.
#'
#' @export
skyline <- function(x) {
  as_tibble_unnest <- function(x, n) {
    x |>
      tibble::as_tibble(n = n) |>
      tidyr::unnest("output",
                    names_sep = "_")
  }

  # total input
  totalinput <- total_input(x) |>
    as_tibble_unnest(n = "totalinput") |>
    dplyr::mutate(totalinput = .data$totalinput / sum(.data$totalinput)) |>
    dplyr::mutate(xmax = cumsum(.data$totalinput),
                  xmin = dplyr::lag(.data$xmax,
                                    default = 0),
                  .keep = "unused")

  # self sufficiency
  leontiefinv <- leontief_inv(x,
                              mat_type = "closed")

  finaldemand <- x |>
    dplyr::filter(.data$input$type == "industry",
                  .data$output$type == "finaldemand") |>
    dibble::apply("input", sum)

  export <- x |>
    dplyr::filter(.data$input$type == "industry",
                  .data$output$type == "export") |>
    dibble::apply("input", sum)

  import <- x |>
    dplyr::filter(.data$input$type == "industry",
                  .data$output$type == "import") |>
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
    dplyr::rename(ymax = "domestic_production")
  import_substitution <- as_tibble_unnest(1 + self_sufficiency_export,
                                          n = "import_substitution") |>
    dplyr::rename(ymax = import_substitution) |>
    dplyr::left_join(domestic_production |>
                       dplyr::select(!"ymin") |>
                       dplyr::rename(ymin = "ymax"),
                     by = c("output_type", "output_name"))

  self_sufficiency <- list(domestic_production = domestic_production,
                           import_substitution = import_substitution) |>
    dplyr::bind_rows(.id = "fill") |>
    dplyr::left_join(totalinput,
                     by = c("output_type", "output_name"))

  # line
  line_self_sufficiency <- self_sufficiency |>
    dplyr::filter(.data$fill == "domestic_production") |>
    dplyr::select(!c("fill", "ymin")) |>
    dplyr::rename(x = "xmin",
                  y = "ymax",
                  xend = "xmax") |>
    dplyr::mutate(yend = .data$y)

  line_self_sufficiency <- dplyr::bind_rows(line_self_sufficiency,
                                            line_self_sufficiency |>
                                              dplyr::select(!"xend") |>
                                              dplyr::mutate(xend = .data$x,
                                                            yend = dplyr::lag(.data$y)) |>
                                              dplyr::slice(-1))

  self_sufficiency |>
    ggplot2::ggplot() +
    ggplot2::geom_rect(ggplot2::aes(xmin = .data$xmin,
                                    xmax = .data$xmax,
                                    ymin = .data$ymin,
                                    ymax = .data$ymax,
                                    fill = .data$fill),
                       color = "black") +
    ggplot2::geom_hline(yintercept = 1) +
    ggplot2::geom_segment(data = line_self_sufficiency,
                          ggplot2::aes(.data$x, .data$y,
                                       xend = .data$xend,
                                       yend = .data$yend),
                          color = "red") +
    ggplot2::scale_fill_manual(NULL,
                               values = c(import_substitution = "darkgray",
                                          domestic_production = "whitesmoke"),
                               labels = c(import_substitution = "Import Substitution",
                                          domestic_production = "Domestic Production")) +
    ggplot2::scale_x_continuous(NULL,
                                breaks = self_sufficiency |>
                                  dplyr::mutate(breaks = (.data$xmin + .data$xmax) / 2) |>
                                  dplyr::pull("breaks"),
                                labels = self_sufficiency$output_name) +
    ggplot2::scale_y_continuous(NULL,
                                labels = scales::label_percent()) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90,
                                                       hjust = 0,
                                                       vjust = 0.5))
}
