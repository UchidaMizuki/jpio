#' Spillover effect
#'
#' @param x An input-output table.
#' @param finaldemand_change A named numeric vector of changes in final demand.
#' @param mat_type A type of Leontief inverse matrix.
#'
#' @return A dibble of spillover effect.
#'
#' @export
spillover_effect <- function(x, finaldemand_change,
                             mat_type = c("open", "closed")) {
  mat_type <- arg_match(mat_type, c("open", "closed"))

  finaldemand_change <- vec_c(!!!finaldemand_change) |>
    tibble::enframe("input_name") |>
    tibble::add_column(input_type = factor("industry"),
                       .before = 1L) |>
    dibble::dibble_by(input = c("input_type", "input_name"),
                      .names_sep = "_")

  FD <- x |>
    dplyr::filter(.data$input$type == "industry") |>
    dibble::apply("input", \(x) 0) |>
    dplyr::rows_update(finaldemand_change)

  L <- leontief_inv(x, mat_type)

  if (mat_type == "open") {
    M <- import_coef(x)

    L %*% ((1 - M) * FD)
  } else if (mat_type == "closed") {
    L %*% FD
  }
}
