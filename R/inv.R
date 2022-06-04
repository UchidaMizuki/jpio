#' @export
leontief_inv <- function(x,
                         mat_type = c("I-(I-M)A", "I-A")) {
  mat_type <- arg_match(mat_type, c("I-(I-M)A", "I-A"))

  A <- input_coeff(x)
  I <- dibble::eye(A)

  if (mat_type == "I-(I-M)A") {
    M <- import_coeff(x)

    dibble::broadcast(solve(I - (1 - M) * A),
                      c("output", "input"))
  } else if (mat_type == "I-A") {
    solve(I - A)
  }
}

#' @export
ghosh_inv <- function(x) {
  B <- alloc_coeff(x)
  I <- dibble::eye(B)

  solve(I - B)
}
