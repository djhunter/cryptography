#' Next Primitive Root
#'
#' Finds the next primitive root after \code{after} modulo \code{p}.
#'
#' @param p A prime number. Could be \code{bigz}.
#' @param after Number after which to start testing. Defaults to 1.
#'
#' @return The smallest primitive root modulo \code{p} greater than \code{after}. Applies the shortcut procedure described in Exercise 21 on p. 107 of [Trappe].
#' @export
#' @import gmp
#'
#' @examples
#' library(gmp)
#' nextPrimRoot(601) # See page 107, Problem 21
#' nextPrimRoot(as.bigz("811177151133889")) # see last assignment
#' nextPrimRoot(nextprime(as.bigz("1203481092840918409408098")), 200) # should be 203
nextPrimRoot <- function(p, after = 1)
{
  # TODO
}

#' Discrete Logarithm (brute force)
#'
#' Computes a discrete logarithm by brute force.
#'
#' @param alpha The base, assumed to be a primitive root modulo p.
#' @param beta An element of Z_p
#' @param p A prime modulus.
#'
#' @return Computes L_alpha(beta), the smallest nonnegative integer x such that beta = alpha^x, modulo p.
#' @export
#' @import gmp
#'
#' @examples
#' discreteLogBrute(2,9,11) # should be 6
discreteLogBrute <- function(alpha, beta, p)
{
  #TODO
}

#' Discrete Logarithm (baby step, giant step)
#'
#' Computes a discrete logarithm using the Baby Step, Giant Step attack described on page 206 of [Trappe].
#'
#' @param alpha The base, assumed to be a primitive root modulo p.
#' @param beta An element of Z_p
#' @param p A prime modulus.
#' @param N The number of steps to try. Default is 1000.
#'
#' @return Computes L_alpha(beta), the smallest nonnegative integer x such that beta = alpha^x, modulo p.
#' @export
#' @import gmp
#'
#' @examples
#' discreteLogBSGS(2,9,11) # should be 6
#' discreteLogBSGS(103, 41425148, 240922393, 16000) # check your answer!
discreteLogBSGS <- function(alpha, beta, p, N = 1000)
{
  # TODO
}

#' Miniature version of the Secure Hash Algorithm
#'
#' Returns a substring of the SHA1 hash of specified length.
#'
#' @param x A character vector, raw vector, or connection object.
#' @param hashLength A number between 1 and 20 giving the number of bytes in the hash.
#'
#' @return The first `hashLength` bytes of the SHA1 hash, in hexadecimal as a character string
#' @export
#' @importFrom openssl sha1
#'
#' @examples
#' miniSHA("toenails")
miniSHA <- function(x, hashLength = 4) {
  return(substr(sha1(x), 1, 2*hashLength))
}

#' Birthday Attack on a hash function
#'
#' Attempts to find a collision given a hash function
#'
#' @param h A function that inputs a character string and outputs a hash
#' @param giveUp Number of different objects to try before giving up
#'
#' @return If a collision is found, returns a vector of strings that have the same hash.
#' @export
#'
#' @examples
#' collisions <- birthdayAttack(miniSHA, giveUp = 100000) # probably should find one
#' miniSHA(collisions) # should be the same hash
#' birthdayAttack(miniSHA, giveUp = 1000000) # almost certainly will find one
#' \dontrun{
#' ms2 <- function(x){miniSHA(x, 5)}
#' birthdayAttack(ms2, giveUp = 10000000) # try if you're feeling lucky
#' }
birthdayAttack <- function(h, giveUp) {
  # TODO
}
