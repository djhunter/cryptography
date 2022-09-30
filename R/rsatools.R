#' Tests to see if a number is composite or probably prime
#'
#' Tests to see if a 'bigz' integer n is composite or probably prime using the Fermat Primality Test. [T] p. 177
#'
#' @param n A 'bigz' integer to test
#' @param a A 'bigz' integer for the base to use
#'
#' @return A Boolean expression: FALSE if n is composite or TRUE if n is probably prime
#' @export
#' @import gmp
#'
#' @examples
#' FermatTest(31, 14) #should return TRUE
#' FermatTest(205, 42) #should return TRUE (weak pseudoprime)
#' FermatTest(8, 6) #should return FALSE
FermatTest <- function(n, a)
{
  # TODO
}

#' Generate a decent RSA modulus
#'
#' Randomly generates a 2048-bit RSA modulus n = pq. Checks to make sure p-q is 
#' at least a googol (10^100), and checks to make sure that p-1 and q-1 don't have 
#' only small prime factors (i.e., up to 100). We'll use e = 65537. 
#' Returns a list containing n, p, q, e, and d, the decryption exponent.
#'
#' @return A list, containing n, p, q, e, and d.
#' @export
#' @import gmp
#'
#' @examples
#' key <- makeRSAkey()
#' log2(key$n) # should be between 2047 and 2048
#' isprime(key$p) # q should be prime too
#' key$p - key$q # should be at least 10^100 in absolute value
#' key$p %% 17 # better not be zero
#' log2(key$q) # better be at least 1000
#' (key$d * key$e) %% ((key$p-1) * (key$q-1)) # d and e should be inverses mod (p-1)(q-1)
makeRSAkey <- function() {
  p <- NA # TODO
  q <- NA # TODO
  e <- as.bigz("65537")
  d <- NA # TODO
  return(list(n=p*q, p=p, q=q, e=e, d=d))
}

