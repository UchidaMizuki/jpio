#' Total input
#'
#' @param x A input-output table.
#' @param output_type Output type.
#'
#' @return A dibble of total input.
#'
#' @export
total_input <- function(x,
                        input_type = c("industry", "valueadded"),
                        output_type = "industry") {
  input_type <- arg_match(input_type, c("industry", "valueadded"),
                          multiple = TRUE)
  output_type <- arg_match(output_type, c("industry", "finaldemand", "export", "import"),
                           multiple = TRUE)

  x |>
    dplyr::filter(.data$input$type %in% input_type,
                  .data$output$type %in% output_type) |>
    dibble::apply("output", sum)
}

#' Total output
#'
#' @param x A input-output table.
#' @param input_type Input type.
#' @param output_type Output type.
#'
#' @return A dibble of total output.
#'
#' @export
total_output <- function(x,
                         input_type = "industry",
                         output_type = c("industry", "finaldemand", "export", "import")) {
  input_type <- arg_match(input_type, c("industry", "valueadded"),
                          multiple = TRUE)
  output_type <- arg_match(output_type, c("industry", "finaldemand", "export", "import"),
                           multiple = TRUE)

  x |>
    dplyr::filter(.data$input$type %in% input_type,
                  .data$output$type %in% output_type) |>
    dibble::apply("input", sum)
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
    dplyr::filter(.data$input$type %in% input_type,
                  .data$output$type %in% c("industry", "finaldemand")) |>
    dibble::apply("input", sum)
}

#' Total export
#'
#' @param x A input-output table.
#'
#' @return A dibble of export value.
#'
#' @export
total_export <- function(x) {
  total_output(x,
               input_type = "industry",
               output_type = "export")
}

#' Total import
#'
#' @param x A input-output table.
#'
#' @return A dibble of import value.
#'
#' @export
total_import <- function(x) {
  -total_output(x,
                input_type = "industry",
                output_type = "import")
}
