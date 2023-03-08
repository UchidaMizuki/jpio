as_tibble_iotable <- function(x, n) {
  x |>
    tibble::as_tibble(n = n) |>
    tidyr::unnest(names2(dimnames(x)),
                  names_sep = "_")
}
