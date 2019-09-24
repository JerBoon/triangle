
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
    ang <- function(a,b,c) {
      return(acos((a^2+b^2-c^2)/(2*a*b))*180/pi)
    }
    cat("A triangle with sides length:\n")
    print(c(x))
    cat("Internal angles:\n")
    print(c(ang(x[1],x[2],x[3]),ang(x[3],x[2],x[1]),ang(x[1],x[3],x[2])))
  } else {
    print("Object is not a triangle")
    if (class(x)[1] == "triangle")
      class(x) <- class(x)[2:length(class(x))]
    print(x)
  }

}
