#' @export
spillover_effect <- function(x,
                             finaldemand_change = list(),
                             mat_type = c("I-(I-M)A", "I-A")) {
  mat_type <- arg_match(mat_type, c("I-(I-M)A", "I-A"))

  finaldemand_change <- vec_c(!!!finaldemand_change) |>
    tibble::enframe("input_name") |>
    tibble::add_column(input_type = factor("industry"),
                       .before = 1L) |>
    dibble::dibble_by(input = c(input_type, input_name),
                      .names_sep = "_")

  FD <- x |>
    filter(input$type == "industry") |>
    dibble::apply("input", \(x) 0) |>
    dplyr::rows_update(finaldemand_change)

  L <- leontief_inv(x, mat_type)

  if (mat_type == "I-(I-M)A") {
    M <- import_coeff(x)

    L %*% ((1 - M) * FD)
  } else if (mat_type == "I-A") {
    L %*% FD
  }
}