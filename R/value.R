#' Total input
#'
#' @param x A input-output table.
#' @param output_type Output type.
#'
#' @return A dibble of total input.
#'
#' @export
total_input <- function(x,
                        output_type = "industry") {
  output_type <- arg_match(output_type, c("industry", "finaldemand", "export", "import"),
                           multiple = TRUE)

  x |>
    dibble::apply("output", sum) |>
    dplyr::filter(.data$output$type %in% output_type)
}

#' Total output
#'
#' @param x A input-output table.
#' @param input_type Input type.
#'
#' @return A dibble of total output.
#'
#' @export
total_output <- function(x,
                         input_type = "industry") {
  input_type <- arg_match(input_type, c("industry", "valueadded"),
                          multiple = TRUE)

  x |>
    dibble::apply("input", sum) |>
    dplyr::filter(.data$input$type %in% input_type)
}

#' Local demand
#'
#' @param x A input-output table.
#' @param input_type Input type.
#'
#' @return A dibble of local demand.
#'
#' @export
local_demand <- function(x,
                         input_type = "industry") {
  input_type <- arg_match(input_type, c("industry", "valueadded"),
                          multiple = TRUE)

  x |>
    dplyr::filter(!.data$output$type %in% c("export", "import")) |>
    dibble::apply("input", sum) |>
    dplyr::filter(.data$input$type %in% input_type)
}

#' Export value
#'
#' @param x A input-output table.
#'
#' @return A dibble of export value.
#'
#' @export
export_value <- function(x) {
  x |>
    dplyr::filter(.data$input$type == "industry",
                  .data$output$type == "export") |>
    dibble::apply("input", sum)
}

#' Import value
#'
#' @param x A input-output table.
#'
#' @return A dibble of import value.
#'
#' @export
import_value <- function(x) {
  out <- x |>
    dplyr::filter(.data$input$type == "industry",
                  .data$output$type == "import") |>
    dibble::apply("input", sum)
  -out
}
