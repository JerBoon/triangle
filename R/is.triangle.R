

#' is.triangle
#'
#' Test whether the supplied object is a valid trangle.
#'
#' Returns TRUE if:
#'
#' - x is a numeric vector
#' - Has length 3
#' - The values are all positive
#' - The values define a genometrically valid triangle of volume > 0 (i.e. it's not a line)
#'
#' Returns FALSE otherwise
#' @param x The object top be tested.
#' @param quiet Logical, default is FALSE. I TRUE will print a messages if it's not a valid triangle.
#'
#' @return Logical
#'
#' @export
#'
#' @examples
#'
#' is.triangle(c(3,4,5))
#'
is.triangle <- function(x, quiet=FALSE) {

  if(!is(x,"numeric")) {
    if (!quiet) message("This is not a numeric vector")
    return(FALSE)
  }

  if(length(x) != 3) {
    if (!quiet) message("This is not a numeric vector of length 3")
    return(FALSE)
  }

  if(min(x) <= 0) {
    if (!quiet) message("Sides must all be positive in length")
    return(FALSE)
  }

  if(max(x) >= (sum(x)/2)) {
    if (!quiet) message("Length of longest side must be less that the sum of the lengths of the shorter two sides")
    return(FALSE)
  }

  return(TRUE)

}
