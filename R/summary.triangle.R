
#' Print summary details of a triangle object
#'
#' @param x The triangle
#'
#' @export
#'
#' @examples
#'
#' t <- triangle(c(3,4,5))
#' summary(t)
#'
summary.triangle <- function(x) {

  if (!is.triangle(x, quiet=TRUE))
  {
    cat("Object is not a triangle\n")
    if (class(x)[1] == "triangle")
      class(x) <- class(x)[2:length(class(x))]
    print(x)
    return()
  }

  ang <- function(a,b,c)
    return(acos((a^2+b^2-c^2)/(2*a*b))*180/pi)

  cat("A triangle with sides length:\n")
  print(c(x))

  cat("Internal angles:\n")
  print(c(ang(x[1],x[2],x[3]),ang(x[3],x[2],x[1]),ang(x[1],x[3],x[2])))

  p <- sum(x)

  cat("Perimeter:\n")
  print(p)

  # Area using heron's formula
  cat("Area:\n")
  p <- p/2
  print(sqrt(p * (p-x[1]) * (p-x[2]) * (p-x[3])))

}
