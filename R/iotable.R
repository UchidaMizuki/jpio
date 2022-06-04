#' Input-output table
#'
#' @param x A data frame.
#' @param input A named list of column names of inputs.
#' @param output A named list of column names of outputs.
#' @param type A named list of input or output names.
#'
#' @return A dibble of input-output table.
#'
#' @export
as_iotable <- function(x,
                       input = list(),
                       output = list(),
                       type = list()) {
  input <- as.list(input)
  input$type <- input$type %||% "input_type"
  input$name <- input$name %||% "input_name"
  input <- vec_c(!!!input)

  output <- as.list(output)
  output$type <- output$type %||% "output_type"
  output$name <- output$name %||% "output_name"
  output <- vec_c(!!!output)

  type <- as.list(type)
  type$industry <- type$industry %||% "industry"
  type$valueadded <- type$valueadded %||% "valueadded"
  type$finaldemand <- type$finaldemand %||% "finaldemand"
  type$export <- type$export %||% "export"
  type$import <- type$import %||% "import"
  type <- vec_c(!!!type)

  out <- x |>
    dplyr::rename_with(~ c("input_type",
                           "input_name",
                           "output_type",
                           "output_name"),
                       c(unname(input[c("type", "name")]),
                         unname(output[c("type", "name")]))) |>
    dplyr::mutate(dplyr::across(c("input_type", "output_type"),
                                ~ vec_slice(names(type),
                                            vec_match(.x, unname(type)))),
                  input_type = input_type |>
                    factor(c("industry", "valueadded")),
                  output_type = output_type |>
                    factor(c("industry", "finaldemand", "export", "import"))) |>
    dplyr::arrange(input_type, input_name, output_type, output_name) |>
    dibble::dibble_by(input = c(input_type, input_name),
                      output = c(output_type, output_name),
                      .names_sep = "_")

  out[[1L]] |>
    tidyr::replace_na(0)
}
