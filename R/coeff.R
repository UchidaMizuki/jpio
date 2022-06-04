#' Input coefficient matrix
#'
#' @param x An input-output table.
#' @param input_type Input type.
#' @param output_type Output type.
#'
#' @return A dibble of input coefficient matrix.
#'
#' @export
input_coeff <- function(x,
                        input_type = "industry",
                        output_type = "industry") {
  input_type <- arg_match(input_type, c("industry", "valueadded"),
                          multiple = TRUE)
  output_type <- arg_match(output_type, c("industry", "finaldemand", "export", "import"),
                           multiple = TRUE)

  totalinput <- x |>
    total_input(output_type)

  x <- x |>
    dplyr::filter(input$type %in% input_type,
                  output$type %in% output_type)

  dibble::broadcast(dibble::ifelse(x == 0,
                                   0,
                                   x / totalinput),
                    c("input", "output"))
}

#' Allocation coefficient matrix
#'
#' @param x An input-output table.
#' @param input_type Input type.
#' @param output_type Output type.
#'
#' @return A dibble of allocation coefficient matrix.
#'
#' @export
alloc_coeff <- function(x,
                        input_type = "industry",
                        output_type = "industry") {
  input_type <- arg_match(input_type, c("industry", "valueadded"),
                          multiple = TRUE)
  output_type <- arg_match(output_type, c("industry", "finaldemand", "export", "import"),
                           multiple = TRUE)

  totaloutput <- x |>
    total_output(input_type)

  x <- x |>
    dplyr::filter(input$type %in% input_type,
                  output$type %in% output_type)

  dibble::broadcast(dibble::ifelse(x == 0,
                                   0,
                                   x / totaloutput),
                    c("input", "output"))
}

#' Import coefficient vector
#'
#' @param x An input-output table.
#'
#' @return A dibble of import coefficient vector.
#'
#' @export
import_coeff <- function(x) {
  import <- import_value(x)
  localdemand <- local_demand(x)

  dibble::ifelse(import == 0,
                 0,
                 import / localoutput)
}

#' Export coefficient vector
#'
#' @param x An input-output table.
#'
#' @return A dibble of export coefficient vector.
#'
#' @export
export_coeff <- function(x) {
  export <- export_value(x)
  totaloutput <- total_output(x)

  dibble::ifelse(export == 0,
                 0,
                 export / totaloutput)
}
