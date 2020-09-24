#' Add two elements of GF(256)
#'
#' This function adds two elements of the Galois field of order 256. The elements are encoded as raw bytes. For example, the elements x^2+x+1 and x^7+x^6+x correspond to the bytes 00000111 and 11000010 respectively, so they would be represented by the raw bytes 07 and c2. Adding these elements in GF(256) yields the sum x^7+x^6+x^2+1, which is 11000101 in binary, or c5 in hexadecimal.
#'
#' @param a A single raw byte
#' @param b A single raw byte
#'
#' @return A single raw byte, representing the sum of a and b in GF(256)
#' @export
#'
#' @examples
#' addGF256(charToRaw("s"), charToRaw("t"))
#' addGF256(charToRaw("\x07"), charToRaw("\xC2"))
#' addGF256(as.raw(7), as.raw(194))
addGF256  <- function(a, b)
{
  return(xor(a,b))
}

#' Multiply by x in GF(256)
#'
#' This function multiplies a given element of the Galois field of order 256 by the element x. We represent GF(256) by modding by the irreducible polynomial (x^8+x^4+x^3+x+1). The elements are encoded as raw bytes. For example, the element x^7+x^6+x corresponds to the byte 11000010, so it would be represented by the raw byte c2. the product (x^7+x^6+x)(x) in GF(256) simplifies to x^7+x^4+x^3+x^2+x+1, which is 10011111 in binary, or 9f in hexadecimal.
#'
#' @param a A single raw byte
#'
#' @return A single raw byte, representing the product (a)(x) in GF(256)
#' @export
#'
#' @examples
#' multByXinGF256(charToRaw("s"))
#' multByXinGF256(charToRaw("\xC2"))
#' multByXinGF256(as.raw(194))
multByXinGF256  <- function(a)
{
  # TODO
}

#' Multiply two elements of GF(256)
#'
#' This function multiplies two elements of the Galois field of order 256. We represent GF(256) by modding by the irreducible polynomial (x^8+x^4+x^3+x+1). The elements are encoded as raw bytes. For example, the elements x^2+x+1 and x^7+x^6+x correspond to the bytes 00000111 and 11000010 respectively, so they would be represented by the raw bytes 07 and c2. Multiplying these elements in GF(256) yields the product x^6+x^5+x^4+x^3, which is 01111000 in binary, or 78 in hexadecimal.
#'
#' @param a A single raw byte
#' @param b A single raw byte
#'
#' @return A single raw byte, representing the product of a and b in GF(256)
#' @export
#'
#' @examples
#' multGF256(charToRaw("s"), charToRaw("t"))
#' multGF256(charToRaw("\x07"), charToRaw("\xC2"))
#' multGF256(as.raw(7), as.raw(194))
multGF256  <- function(a, b)
{
  # TODO
}
