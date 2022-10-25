#' Plot a real elliptic curve
#'
#' Creates a plot of the elliptic curve y^2 = x^3 + bx + c. Uses the \code{contour} plot
#' function.
#'
#' @param b A real number representing the coefficient b in the equation of the curve.
#' @param c A real number representing the coefficient c in the equation of the curve.
#' @param xmin Lower limit for x
#' @param xmax Upper limit for x
#' @param ymin Lower limit for y
#' @param ymax Upper limit for y
#' @param asp Aspect ratio
#'
#' @return A plot of the elliptic curve.
#' @export
#'
#' @examples
#' ecPlotReal(-5,1)
ecPlotReal <- function(b, c, xmin=-5, xmax=10, ymin=-30, ymax=30, asp=1) {
  x<-seq(xmin, xmax, length=1000)
  y<-seq(ymin, ymax, length=1000)
  z<-outer(x,y,function(x,y) -y^2 + x^3+b*x+c )
  contour(x,y,z,levels=0, labels="", labcex=0.1, asp=asp)
}

#' Addition in Real Elliptic Curves
#'
#' Adds two points on the real elliptic curve y^2 = x^3 + bx + c. Uses the 
#' Addition Law given on page 352 of [Trappe]. All points on elliptic curves 
#' are represented as length two numeric vectors: the point at infinity is 
#' represented as \code{c(Inf,Inf)}. Assume that \code{p1} and \code{p2} are 
#' valid points on the elliptic curve.
#'
#' @param b A real number representing the coefficient b in the equation of the curve.
#' @param c A real number representing the coefficient c in the equation of the curve.
#' @param p1 A length 2 numeric vector, representing a point on the curve.
#' @param p2 A length 2 numeric vector, representing a point on the curve.
#'
#' @return A length 2 numeric vector, representing the sum p1 + p2.
#' @export
#'
#' @examples
#' ecAddReal(0, 73, c(2,9), c(3,10)) # see page 350
#' ecAddReal(0, 73, c(2,9), c(Inf,Inf)) # c(Inf,Inf) is the additive identity
#' ecAddReal(0, 73, c(2,9), c(2,-9)) # additive inverses
#' ecAddReal(0, 73, c(-4,-3), c(-4,-3)) # See page 351
ecAddReal <- function(b, c, p1, p2) {
  if(all(is.infinite(p1)))
    return(p2)
  if(all(is.infinite(p2)))
    return(p1)
  m <- ifelse(all(p1==p2), 
              (3*p1[1]^2 + b)/(2 * p1[2]),
              (p2[2] - p1[2])/(p2[1] - p1[1]))
  p3 <- c(m^2 - p1[1] - p2[1])
  p3[2] <- m * (p1[1] - p3[1]) - p1[2]
  return(p3)
}


#' Elliptic curve additive inverse
#'
#' Computes the additive inverse (negative) of a point on an elliptic curve. Note that
#' the output of this function does not depend on the choice of elliptic curve, or even
#' on the field.
#'
#' @param p A length 2 vector representing a point on an elliptic curve.
#'
#' @return A length 2 vector representing the additive inverse of \code{p}.
#' @export
#'
#' @examples
#' ecNeg(c(3,4)) # see page 351
#' ecNeg(c(Inf,Inf)) # this is the additive identity
ecNeg <- function(p){
  p[2] <- ifelse(is.infinite(p[2]), p[2], -p[2])
  return(p)
}

#' Real elliptic curve "exponentiation"
#'
#' Give a point \code{p} on a real elliptic curve y^2 = x^3 + bx + c and an 
#' integer \code{n}, returns the point np, that is, p added to itself n times. 
#' If n is negative, returns -p added to itself n times. If n is zero, returns 
#' the point at infinity. If p is the point at infinity, also returns the 
#' point at infinity. We call this operation "exponentiation", but since we 
#' represent elliptic curves with additive notation, "exponentiation" is 
#' repeated addition.
#'
#' @param b A real number representing the coefficient b in the equation of the curve.
#' @param c A real number representing the coefficient c in the equation of the curve.
#' @param p A length 2 numeric vector, representing a point on the curve.
#' @param n An integer
#'
#' @return A length 2 vector representing \code{n} times \code{p} (on the elliptic curve).
#' @export
#'
#' @examples
#' ecPowReal(0, 73, c(-4, -3), 2) # See page 351
ecPowReal <- function(b, c, p, n) {
  if (n == 0 || all(is.infinite(p)))
    return(c(Inf, Inf))
  if (n < 0) {
    p <- -p
    n <- -n
  }
  total <- p
  for (i in 1:(n-1))
    total <- ecAddReal(b, c, p, total)
  return(total)
}

#' Addition in mod p Elliptic Curves
#'
#' Adds two points on the mod p elliptic curve y^2 = x^3 + bx + c. Uses the 
#' Addition Law given on page 352 of [Trappe]. All points on elliptic curves 
#' are represented as length two bigz vectors: the point at infinity is 
#' represented as \code{c(Inf,Inf)}. While \code{p1} and \code{p2} are assumed 
#' to be valid points on the elliptic curve (not checked), the program should 
#' return a warning (using the \code{warning} command) if any of the required 
#' inverses in the addition rule fail to exist, returning the value that failed 
#' to be invertible.
#'
#' @param b A bigz or integer representing the coefficient b in the equation of the curve.
#' @param c A bigz or integer representing the coefficient c in the equation of the curve.
#' @param modulus The modulus of the curve (integer or bigz).
#' @param p1 A length 2 integer or bigz vector, representing a point on the curve.
#' @param p2 A length 2 integer or bigz vector, representing a point on the curve.
#'
#' @return A length 2 bigz vector, representing the sum p1 + p2.
#' @export
#' @import gmp
#'
#' @examples
#' ecAddModp(4, 4, 5, c(1,2), c(4,3)) # see page 353
#' ecAddModp(4, 4, 5, c(1,2), c(1,3)) # these points are additive inverses
#' ecAddModp(4, 4, 2773, c(1,3), c(1,3)) # see pp. 353-4
#' ecAddModp(4, 4, 2773, c(1771,705), c(1,3)) # see pp. 356-7
#' # Should return a warning message (or two messages):
#' # Warning messages:
#' # 1: In ecAddModp(4, 4, 2773, c(1771, 705), c(1, 3)) :
#' #   -1770 is not invertible mod 2773
#' # 2: In inv.bigz((x2 - x1), modulus) :
#' #   inv(x,m) returning NA as x has no inverse modulo m
ecAddModp <- function(b, c, modulus, p1=c(Inf,Inf), p2=c(Inf,Inf))
{
  # TODO
}

#' Negation in mod p Elliptic Curves
#'
#' Computes the additive inverse (negative) of a point on an elliptic curve mod p. 
#' Note that the output of this function does not depend on the choice of elliptic curve.
#'
#' @param p A length 2 vector representing a point on an elliptic curve mod p.
#' @param modulus The modulus for the elliptic curve
#'
#' @return A length 2 vector representing the additive inverse of p with the given modulus.
#' @export
#'
#' @examples
#' ecNegModp(c(34, 12), 253) #should return c(34, 241)
#' ecNegModp(c(Inf, Inf), 253)
ecNegModp <- function(p, modulus)
{
  # TODO
}

#' Efficient "exponentiation" in mod p elliptic curves
#'
#' Given a point \code{P} on an elliptic curve mod \code{modulus} and an 
#' integer/bigz \code{n}, computes the "power" nP. (Since elliptic curves 
#' form an additive group, we express "powers" as integer multiples.) Does 
#' so using repeated "squaring" (doubling). (See the QPower function in the notes.)
#'
#' @param b A bigz or integer representing the coefficient b in the equation of the curve.
#' @param c A bigz or integer representing the coefficient c in the equation of the curve.
#' @param modulus The modulus of the curve (bigz or integer).
#' @param p A length 2 integer or bigz vector, representing a point on the curve.
#' @param n A nonnegative bigz or integer representing the "exponent".
#'
#' @return A length 2 bigz vector, representing the "power" nP.
#' @export
#' @import gmp
#'
#' @examples
#' ecPowModp(3, 45, 8831, c(4,11), 8) # see p. 364 (this is kG)
#' library(gmp)
#' ecPowModp(3, 45, 8831, c(4,11), as.bigz("2349089023472938409283490823")) # 6863 449
ecPowModp <- function(b, c, modulus, p, n)
{
  # TODO
}

#' Elliptic curve discrete logarithm (BSGS)
#'
#' Given a point \code{nG} on an elliptic curve mod \code{modulus} and a point 
#' \code{G} on the curve, returns the integer/bigz \code{n}, the discrete 
#' logarithm of \code{nG}. Uses the Baby-step Giant-step algorithm.
#'
#' @param b A bigz or integer representing the coefficient b in the equation of the curve.
#' @param c A bigz or integer representing the coefficient c in the equation of the curve.
#' @param modulus The modulus of the curve (bigz or integer).
#' @param G A length 2 integer or bigz vector, representing the "base".
#' @param nG A length 2 integer or bigz vector, representing a "power" of the base.
#' @param N The number of baby/giant steps to take.
#'
#' @return The "exponent" n as a bigz.
#' @export
#' @import gmp
#'
#' @examples
#' ecDiscreteLog(-3, 3, 761, c(1,1), c(533,687), N=30)
#' ecDiscreteLog(4, -12063, 34543427, c(23,14), c(10735908, 411234))
#' \dontrun{
#' library(gmp)
#' # Try if you want a challenge:
#' ecDiscreteLog(4, 1, as.bigz("426904703359"), c(4,9), as.bigz(c("26917083261", "24329765219")), N=500000)
#' }
ecDiscreteLog <- function(b, c, modulus, G, nG, N = 5000)
{
  # TODO
}
