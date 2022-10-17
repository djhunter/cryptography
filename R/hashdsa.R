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

#' Generate DSA parameters
#'
#' Randomly generates DSA parameters q and p and returns them in a list. Here, q is an N-bit prime and p is an L-bit prime such that q divides p-1.
#'
#' @param N The number of bits in q
#' @param L The number of bits in p
#'
#' @return A list, containing q and p.
#' @export
#' @import gmp
#'
#' @examples
#' library(gmp)
#' key <- setUpDSA()
#' log2(key$q) # should be between N and N-1
#' log2(key$p) # should be between L and L-1
#' isprime(key$q)
#' isprime(key$p)
#' gcd(key$p-1, key$q) # should be key$q
setUpDSA <- function(B = 256, N = 256, L = 2048) {
  p <- NA # TODO
  q <- NA # TODO

  return(list(q=q, p=p))
}
