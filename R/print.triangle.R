
#' Print a triangle object
#'
#' @param x The triangle
#'
#' @export
#'
#' @examples
#'
#' t <- triangle(c(3,4,5))
#' print(t)
#'
print.triangle <- function(x) {

  if (is.triangle(x, quiet=TRUE))
  {
    cat("A triangle:\n")
    print(c(x))
  } else {
    cat("Not a triangle\n")
    if (class(x)[1] == "triangle")
      class(x) <- class(x)[2:length(class(x))]
    print(x)
  }

}
