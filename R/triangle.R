
#' triangle
#'
#' return a triangle object from the supplied vector
#'
#' @param x Vector of triangle side lengths
#'
#' @return Triangle object
#'
#' @export
#'
#' @examples
#'
#' t <- triangle(c(3,4,5))
#'
triangle <- function(x) {

  if (is.triangle(x)) {
    class(x) <- c("triangle",class(x))
    return(x)
  } else {
    stop("Object is not a valid triangle")
  }

}
