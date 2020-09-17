#' Create a shift matrix 
#'
#' Given a numeric vector of length at least 2n-1, create an n by n shift matrix. The first row of this matrix contains the first n terms of \code{x}, the second row contains terms 2 through n+1, and so on.
#'
#' @param x A numeric vector (usually of 0's and 1's) of length at least 2n-1
#' @param n The number of rows and columns in the shift matrix.
#'
#' @return An n by n matrix whose rows are shifts of the sequence \code{x}. 
#' @export
#'
#' @examples
#' shiftMatrix(c(1,1,0,1,1,0,1,1,0,1,1), 4)
shiftMatrix  <- function(x, n) 
{
  # TODO
}

#' Guess the recurrence length
#'
#' Given a vector of 0's and 1's, examine the shift matrices to determine the dimension of the largest invertible shift matrix. 
#'
#' @param x A numeric vector of 0's and 1's
#'
#' @return The number of rows in the largest invertible shift matrix
#' @export
#'
#' @examples
#' recurrenceLength(c(1,0,0,1,1,0,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,1,1,0,0,0,1,1,1,1,1))
recurrenceLength <- function(x) 
{
  # TODO
}
