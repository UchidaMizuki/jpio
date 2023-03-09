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
                              output_type = "finaldemand")
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
#' @param domestic_production_color Domestic production color.
#' @param domestic_production_label Domestic production label.
#' @param import_substitution_color Import substitution color.
#' @param import_substitution_label Import substitution label.
#' @param self_sufficiency_label Self sufficiency label.
#' @param ylim Limits of y axis.
#' @param args_geom_rect Arguments for [ggplot2::geom_rect()].
#' @param args_geom_hline Arguments for [ggplot2::geom_hline()].
#' @param args_geom_segment Arguments for [ggplot2::geom_segment()].
#' @param args_geom_text_repel Arguments for [ggrepel::geom_text_repel()].
#' @param args_scale_x_continuous Arguments for [ggplot2::scale_x_continuous()].
#' @param args_scale_y_continuous Arguments for [ggplot2::scale_y_continuous()].
#' @param args_scale_fill_manual Arguments for [ggplot2::scale_fill_manual()].
#' @param args_annotate Arguments for [ggpp::annotate()].
#'
#' @return A ggplot object.
#'
#' @export
skyline_chart <- function(x,
                          domestic_production_color = "whitesmoke",
                          domestic_production_label = "Domestic Production",
                          import_substitution_color = "darkgray",
                          import_substitution_label = "Import Substitution",
                          self_sufficiency_label = "Self sufficiency: {scales::label_percent(accuracy = 0.1)(value)}",
                          ylim = c(-1, NA_real_),
                          args_geom_rect = NULL,
                          args_geom_hline = NULL,
                          args_geom_segment = NULL,
                          args_geom_text_repel = NULL,
                          args_scale_x_continuous = NULL,
                          args_scale_y_continuous = NULL,
                          args_scale_fill_manual = NULL,
                          args_annotate = NULL) {
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

  # Arguments
  dots_list_named_first <- purrr::partial(dots_list,
                                          .named = TRUE,
                                          .homonyms = "first")
  args_geom_rect <- dots_list_named_first(!!!args_geom_rect,
                                          color = "black")
  args_geom_hline <- dots_list_named_first(!!!args_geom_hline,
                                           color = "black")
  args_geom_segment <- dots_list_named_first(!!!args_geom_segment,
                                             color = "red")
  args_geom_text_repel <- dots_list_named_first(!!!args_geom_text_repel,
                                                nudge_y = -0.1,
                                                hjust = 0,
                                                angle = -90,
                                                direction = "x")
  args_scale_x_continuous <- dots_list_named_first(!!!args_scale_x_continuous,
                                                   name = NULL,
                                                   breaks = seq(0, 1, 0.2),
                                                   labels = scales::label_percent(),
                                                   position = "top")
  args_scale_y_continuous <- dots_list_named_first(!!!args_scale_y_continuous,
                                                   name = NULL,
                                                   labels = \(x) {
                                                     dplyr::if_else(x >= 0,
                                                                    scales::label_percent()(x),
                                                                    "")
                                                   })
  args_scale_fill_manual <- dots_list_named_first(values = c(domestic_production = domestic_production_color,
                                                             import_substitution = import_substitution_color),
                                                  labels = c(domestic_production = domestic_production_label,
                                                             import_substitution = import_substitution_label),
                                                  !!!args_scale_fill_manual,
                                                  name = NULL)
  if (!is.null(self_sufficiency_label)) {
    value <- (sum(finaldemand) + sum(export) + sum(import)) / sum(finaldemand)
    args_annotate <- dots_list_named_first(label = stringr::str_glue(self_sufficiency_label),
                                           !!!args_annotate,
                                           geom = "text_npc",
                                           npcx = "right",
                                           npcy = "top",
                                           color = "red")
  }

  out <- self_sufficiency |>
    ggplot2::ggplot() +
    exec(ggplot2::geom_rect,
         ggplot2::aes(xmin = .data$xmin,
                      xmax = .data$xmax,
                      ymin = .data$ymin,
                      ymax = .data$ymax,
                      fill = .data$fill),
         !!!args_geom_rect) +
    exec(ggplot2::geom_hline,
         yintercept = 1,
         !!!args_geom_hline) +
    exec(ggplot2::geom_segment,
         data = line_self_sufficiency,
         ggplot2::aes(.data$x, .data$y,
                      xend = .data$xend,
                      yend = .data$yend),
         !!!args_geom_segment) +
    exec(ggrepel::geom_text_repel,
         data = self_sufficiency |>
           dplyr::distinct(output_type, output_name, xmin, xmax) |>
           dplyr::mutate(x = (.data$xmin + .data$xmax) / 2),
         ggplot2::aes(x = .data$x,
                      y = 0,
                      label = .data$output_name),
         !!!args_geom_text_repel) +
    exec(ggplot2::scale_x_continuous,
         !!!args_scale_x_continuous) +
    exec(ggplot2::scale_y_continuous,
         !!!args_scale_y_continuous) +
    exec(ggplot2::scale_fill_manual,
         !!!args_scale_fill_manual) +
    ggplot2::coord_cartesian(ylim = ylim)

  if (!is.null(self_sufficiency_label)) {
    out +
      exec(ggpp::annotate,
           !!!args_annotate)
  }
}
