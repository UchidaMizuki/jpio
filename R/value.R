#' @export
total_input <- function(x,
                        output_type = "industry") {
  output_type <- arg_match(output_type, c("industry", "finaldemand", "export", "import"),
                           multiple = TRUE)

  x |>
    dibble::apply("output", sum) |>
    dplyr::filter(output$type %in% output_type)
}

#' @export
total_output <- function(x,
                         input_type = "industry") {
  input_type <- arg_match(input_type, c("industry", "valueadded"),
                          multiple = TRUE)

  x |>
    dibble::apply("input", sum) |>
    dplyr::filter(input$type %in% input_type)
}

#' @export
local_output <- function(x,
                         input_type = "industry") {
  input_type <- arg_match(input_type, c("industry", "valueadded"),
                          multiple = TRUE)

  x |>
    dplyr::filter(!output$type %in% c("export", "import")) |>
    dibble::apply("input", sum) |>
    dplyr::filter(input$type %in% input_type)
}

#' @export
export_value <- function(x) {
  x |>
    dplyr::filter(input$type == "industry",
                  output$type == "export") |>
    dibble::apply("input", sum)
}

#' @export
import_value <- function(x) {
  out <- x |>
    dplyr::filter(input$type == "industry",
                  output$type == "import") |>
    dibble::apply("input", sum)
  -out
}
