#' Production inducement
#'
#' @param x An input-output table.
#' @param mat_type A type of Leontief inverse matrix.
#'
#' @return A dibble of production inducement.
#'
#' @export
production_inducement <- function(x,
                                  mat_type = c("open", "closed")) {
  mat_type <- arg_match(mat_type, c("open", "closed"))

  leontiefinv <- leontief_inv(x,
                              mat_type = mat_type)
  finaldemand <- total_output(x,
                              output_type = c("industry", "finaldemand"))
  export <- total_export(x)

  if (mat_type == "open") {
    M <- import_coef(x)
    dibble::dibble(finaldemand = leontiefinv %*% ((1 - M) * finaldemand),
                   export = leontiefinv %*% export)
  } else if (mat_type == "closed") {
    import <- total_import(x)
    dibble::dibble(finaldemand = leontiefinv %*% finaldemand,
                   export = leontiefinv %*% export,
                   import = -leontiefinv %*% import)
  }
}

#' Self sufficiency
#'
#' @param x An input-output table.
#'
#' @return A dibble of self sufficiency.
#'
#' @export
self_sufficiency <- function(x,
                             summary = FALSE) {
  productioninducement <- production_inducement(x,
                                                mat_type = "closed")
  finaldemand <- productioninducement$finaldemand
  export <- productioninducement$export
  import <- productioninducement$import

  if (summary) {
    (sum(finaldemand) + sum(export) + sum(import)) / sum(finaldemand)
  } else {
    (finaldemand + export + import) / finaldemand
  }
}

#' Draw a skyline chart
#'
#' @param x An input-output table.
#'
#' @return A ggplot object.
#'
#' @export
skyline_chart <- function(x) {
  # total input
  totalinput <- total_input(x) |>
    as_tibble_iotable(n = "totalinput") |>
    dplyr::mutate(totalinput = .data$totalinput / sum(.data$totalinput)) |>
    dplyr::mutate(xmax = cumsum(.data$totalinput),
                  xmin = dplyr::lag(.data$xmax,
                                    default = 0),
                  .keep = "unused")

  # self sufficiency
  productioninducement <- production_inducement(x,
                                                mat_type = "closed")
  finaldemand <- productioninducement$finaldemand
  export <- productioninducement$export
  import <- productioninducement$import

  domestic_production <- as_tibble_iotable((finaldemand + export + import) / finaldemand,
                                           n = "domestic_production") |>
    tibble::add_column(ymin = 0) |>
    dplyr::rename(ymax = "domestic_production")
  import_substitution <- as_tibble_iotable((finaldemand + export) / finaldemand,
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
    ggrepel::geom_text_repel(data = self_sufficiency |>
                               dplyr::distinct(output_type, output_name, xmin, xmax) |>
                               dplyr::mutate(x = (.data$xmin + .data$xmax) / 2),
                             ggplot2::aes(x = .data$x,
                                          y = 0,
                                          label = .data$output_name),
                             nudge_y = -0.1,
                             hjust = 0,
                             angle = -90,
                             direction = "x") +
    ggplot2::scale_x_continuous(NULL,
                                breaks = seq(0, 1, 0.2),
                                labels = scales::label_percent(),
                                position = "top") +
    ggplot2::scale_y_continuous(NULL,
                                limits = c(-0.5, NA_real_),
                                labels = \(x) {
                                  dplyr::if_else(x >= 0,
                                                 scales::label_percent()(x),
                                                 "")
                                }) +
    ggplot2::scale_fill_manual(NULL,
                               values = c(import_substitution = "darkgray",
                                          domestic_production = "whitesmoke"),
                               labels = c(import_substitution = "Import Substitution",
                                          domestic_production = "Domestic Production"))
}
