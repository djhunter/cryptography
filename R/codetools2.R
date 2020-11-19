#' Create generator matrix from parity check matrix
#'
#' Given a parity check matrix for a systematic linear binary code,
#' return the generator matrix.
#'
#' @param H A parity check matrix of the form [P^T I_{n-k}].
#'
#' @return A generator matrix of the form [I_k P].
#' @export
#'
#' @examples
#' generatorMatrix(matrix(c(1,1,0,1,1,0,0,
#'                          1,0,1,1,0,1,0,
#'                          0,1,1,1,0,0,1), nrow=3, byrow=TRUE))
generatorMatrix <- function(H)
{
  # TODO
}

#' Create a parity check matrix from a generator matrix
#'
#' Given a generator matrix for a systematic linear binary code,
#' return the parity check matrix.
#'
#' @param G A generator matrix of the form [I_k P]
#'
#' @return A parity check matrix of the form [P^T I_{n-k}].
#' @export
#'
#' @examples
#' parityCheckMatrix(matrix(c(1,0,0,0,1,1,0,
#'                            0,1,0,0,1,0,1,
#'                            0,0,1,0,0,1,1,
#'                            0,0,0,1,1,1,1), nrow=4, byrow=TRUE))
parityCheckMatrix <- function(G)
{
  # TODO
}

#' Syndrome of a vector
#'
#' Given an n-dimensional binary vector and a parity check matrix
#' for a binary linear [n,k] code, returns the syndrome of the vector,
#' as a character string of "1"'s and "0"'s.
#'
#' @param r An n-dimensional numeric vector of 0's and 1's.
#' @param H A parity check matrix for an [n,k] code.
#'
#' @return A character string giving the syndrome of \code{r}.
#' @export
#'
#' @examples
#' syndrome(c(1,1,0,1,0,1,1), matrix(c(1,1,0,1,1,0,0,
#'                                     1,0,1,1,0,1,0,
#'                                     0,1,1,1,0,0,1),
#'                                     nrow=3, byrow=TRUE)) # "111"
syndrome <- function(r, H)
{
  # TODO
}

#' Coset of a code given a representative
#'
#' @param r An n-dimensional numeric vector of 0's and 1's.
#' @param C An [n,k] binary linear code, represented as a 2^k by n matrix. The rows of
#' the matrix are the code words.
#'
#' @return A 2^k by n matrix, whose rows are all the elements of the coset r + C
#' in the vector space of all binary vectors of dimension n.
#' @export
#'
#' @examples
#' coset(c(1,1,1,1), generateCode(matrix(c(1,0,1,1,
#'                                         0,1,1,0),
#'                                         nrow=2, byrow=TRUE))) # see page 412
coset <- function(r, C)
{
  # TODO
}

#' Coset leader
#'
#' @param r An n-dimensional numeric vector of 0's and 1's.
#' @param C An [n,k] binary linear code, represented as a 2^k by n matrix. The rows of
#' the matrix are the code words.
#'
#' @return An n-dimensional vector, which is a vector of minimal weight in the
#' coset r + C.
#' @export
#'
#' @examples
#' cosetLeader(c(1,1,1,1), generateCode(matrix(c(1,0,1,1,
#'                                         0,1,1,0),
#'                                         nrow=2, byrow=TRUE))) # see page 412
cosetLeader <- function(r, C)
{
  # TODO
}

#' Coset leader syndrome table
#'
#' @param G A generator matrix for a linear binary code.
#'
#' @return A of all the coset leaders indexed by their corresponding syndromes.
#' @export
#'
#' @examples
#' cosetLeaderSyndromeTable(matrix(c(1,0,1,1,
#'                                   0,1,1,0),
#'                                   nrow=2, byrow=TRUE)) # see page 412
cosetLeaderSyndromeTable <- function(G) {
  n <- ncol(G)
  k <- nrow(G)
  C <- generateCode(G)
  H <- parityCheckMatrix(G)
  Z2n <- t(sapply(0:(2^n-1), function(x){as.numeric(intToBits(x)[1:n])}))
  allsynd <- sapply(1:2^n, function(x) {syndrome(Z2n[x,],H)})
  uniqueReps <- which(!duplicated(allsynd))
  syndromes <- allsynd[uniqueReps]
  cosetLeaders <- lapply(uniqueReps, function(x) {cosetLeader(Z2n[x,],C)} )
  names(cosetLeaders) <- syndromes
  return(cosetLeaders)
}
