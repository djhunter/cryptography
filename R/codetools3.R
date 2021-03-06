#' Create generating matrix for cyclic code
#'
#' Given a generating polynomial \code{g} and a code length \code{n}, returns a generating matrix for the binary cyclic code generated by \code{g}. The code will be the ideal of ZZ_2[x]/(x^n+1) generated by g(x). The generating matrix need not be systematic. When converting a polynomial to a bitstring, the lower degree terms go first (e.g., see p. 427).
#'
#' @param g A vector containing the powers of the nonzero elements of the polynomial g(x).
#' @param n The length of the code.
#'
#' @return An n-r by n matrix, where r is the degree of g(x).
#' @export
#'
#' @examples
#' cyclicCodeGenMat(c(10,8,5,4,2,1,0), 15) # BCH [15,5] code
cyclicCodeGenMat <- function(g, n) {
 # TODO
}

#' Generate a random binary vector
#'
#' Given a desired length and Hamming weight, return a binary vector (using
#' numeric variables) of the specified length and weight. Uses \code{sample} to
#' randomize.
#'
#' @param length The number of components of the vector.
#' @param weight The Hamming weight of the vector. Assumed to be less than or equal to \code{length}.
#'
#' @return A numeric vector of 0's and 1's with specified length and weight.
#' @export
#'
#' @examples
#' randBinVector(8,4)
randBinVector <- function(length, weight) {
  v <- numeric(length)
  oneLocs <- sample(1:length, weight)
  v[oneLocs] <- 1
  return(v)
}

#' Generate random binary matrix and inverse
#'
#' Given a dimension n, this function uses a random sequence of elementary
#' row operations to produce an invertible n by n matrix, along with its
#' inverse. It does so by performing a sequence of \code{numops} randomly
#' chosen row operations on the identity matrix. It computes the inverse
#' at the same time by performing the inverse of these row operations in
#' reverse order.
#'
#' @param n The dimension of the square matrix.
#' @param numops The number of random row operations to perform. Defaults to 100.
#'
#' @return A list, consisting of a random binary matrix M and its inverse.
#' @export
#'
#' @examples
#' matrixList <- randInvMatrix(5)
#' matrixList$M
#' matrixList$Minv
randInvMatrix <- function(k, numops = 100) {
  M <- diag(k) # the k by k identity matrix
  Minv <- diag(k)
  rowOps <- replicate(numops, sample(1:k, 2))
  for(i in 1:numops) {
    M[rowOps[1,i],] <- (M[rowOps[1,i],] + M[rowOps[2,i],]) %% 2
    Minv[rowOps[1,numops-i+1],] <- (Minv[rowOps[1,numops-i+1],] + Minv[rowOps[2,numops-i+1],]) %% 2
  }
  return(list(M = M, Minv = Minv))
}

#' Generate random permutation matrix
#'
#' Given a dimension n, this function generates a random n by n permutation matrix. It does so by performing a sequence of \code{numswaps} randomly chosen row swaps on the identity matrix.
#'
#' @param n The dimension of the matrix.
#' @param numswaps The number of random swaps to perform. Defaults to 100.
#'
#' @return A randomly generated n by n permutation matrix.
#' @export
#'
#' @examples
#' randPermMatrix(5)
randPermMatrix <- function(n, numswaps = 100) {
  # TODO
}

