#' Convert a byte to a polynomial
#' 
#' Given a length 8 vector of 0s and 1s, this function returns
#' the associated polynomial, as a string.
#' 
#' @param v A length 8 vector of 0s and 1s
#' 
#' @return A string showing the associated polynomial
#' @export
#' 
#' @examples
#' byteAsPoly(c(1,0,1,0,1,1,1,1))
#' byteAsPoly(c(0,0,0,1,0,0,0,1))
#' byteAsPoly(c(0,0,0,1,0,1,0,0))
byteAsPoly <- function(v) {
  if(all(v == 0))
    return("0")
  termlist <- lapply(8 - which(v == 1), function(i) {paste0("x^", i)} )
  if(termlist[[length(termlist)]] == "x^0")
    termlist[[length(termlist)]] <- "1"
  return(do.call(paste, c(termlist, sep = " + ")))
}

#' Add two elements of GF(256)
#'
#' This function adds two elements of the Galois field of order 256. 
#' The elements are encoded as length 8 vectors of 0s and 1s. 
#'
#' @param a A length 8 vector of 0s and 1s. 
#' @param b A length 8 vector of 0s and 1s. 
#'
#' @return A length 8 vector of 0s and 1s, representing the sum of a and b in GF(256)
#' @export
#'
#' @examples
#' addGF256(c(1,0,1,1,0,1,0,0), c(0,0,0,1,0,0,0,1))
#' byteAsPoly(c(1,0,1,1,0,1,0,0))
#' byteAsPoly(c(0,0,0,1,0,0,0,1))
#' byteAsPoly(addGF256(c(1,0,1,1,0,1,0,0), c(0,0,0,1,0,0,0,1)))
addGF256  <- function(a, b)
{
  # TODO
}

#' Multiply by x in GF(256)
#'
#' This function multiplies a given element of the Galois field of order 256 
#' by the element x. We represent GF(256) by modding by the irreducible 
#' polynomial (x^8+x^4+x^3+x+1). The elements are encoded as length 8 vectors 
#' of 0s and 1s. 
#'
#' @param a A length 8 vector of 0s and 1s. 
#'
#' @return A length 8 vector of 0s and 1s, representing the product (a)(x) in GF(256)
#' @export
#'
#' @examples
#' multByXinGF256(c(0,0,0,1,0,1,0,0))
#' byteAsPoly(c(0,0,0,1,0,1,0,0))
#' byteAsPoly(multByXinGF256(c(0,0,0,1,0,1,0,0)))
#' multByXinGF256(c(1,1,0,1,0,1,0,0))
#' byteAsPoly(c(1,1,0,1,0,1,0,0))
#' byteAsPoly(multByXinGF256(c(1,1,0,1,0,1,0,0)))
multByXinGF256  <- function(a)
{
  # TODO
}

#' Multiply two elements of GF(256)
#'
#' This function multiplies two elements of the Galois field of order 256. 
#' We represent GF(256) by modding by the irreducible polynomial 
#' (x^8+x^4+x^3+x+1). The elements are encoded as length 8 vectors 
#' of 0s and 1s. 
#'
#' @param a A length 8 vector of 0s and 1s. 
#' @param b A length 8 vector of 0s and 1s. 
#'
#' @return A length 8 vector of 0s and 1s, representing the product of a and b in GF(256)
#' @export
#'
#' @examples
#' multGF256(c(1,0,1,1,0,1,0,0), c(0,0,0,1,0,0,0,1))
#' byteAsPoly(c(1,0,1,1,0,1,0,0))
#' byteAsPoly(c(0,0,0,1,0,0,0,1))
#' byteAsPoly(multGF256(c(1,0,1,1,0,1,0,0), c(0,0,0,1,0,0,0,1)))
multGF256  <- function(a, b)
{
  # TODO
}
