#' Hamming distance
#'
#' Compute the Hamming distance between two n-dimensional vectors in Z_2.
#'
#' @param c1 A vector of 0's and 1's of length n.
#' @param c2 A vector of 0's and 1's of length n.
#'
#' @return The number of positions where \code{c1} and \code{c2} differ.
#' @export
#'
#' @examples
#' hammingDistance(c(1,0,0,1,1), c(0,1,0,1,0))
hammingDistance <- function(c1, c2)
{
  # TODO
}

#' Minimum distance of a code
#'
#' Compute the smallest Hamming distance between two words of a code.
#'
#' @param C A code, represented as a matrix of 0's and 1's. The rows of this 
#' matrix are the words of the code.
#'
#' @return The minimum distance d(C) of the code.
#' @export
#'
#' @examples
#' codeDistance(matrix(c(0,1,0,0,1,
#'                       0,0,1,1,1,
#'                       0,1,0,0,0,
#'                       1,1,1,1,1), nrow=4, byrow=TRUE))
codeDistance <- function(C)
{
  # TODO
}

#' Generate a code from a basis
#'
#' @param B The basis code words, represented as a matrix of 0's and 1's. The 
#' rows of this matrix are the basis code words.  (The function does not require 
#' that the rows of B be linearly independent.)
#'
#' @return A matrix consisting of all possible, distinct, linear combinations of 
#' the basis code words over Z_2.
#' @export
#'
#' @examples
#' generateCode(matrix(c(1,0,0,0,1,1,0,
#'                       0,1,0,0,1,0,1,
#'                       0,0,1,0,0,1,1,
#'                       0,0,0,1,1,1,1), nrow=4, byrow=TRUE))
generateCode <- function(B)
{
  # TODO
}

