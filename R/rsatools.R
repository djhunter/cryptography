#' Convert string to big integer
#'
#' Converts a string to a bigz integer. First the characters of the string are converted to raw, then the raw (hexadecimal) vector is converted to an integer, where the place values of this vector are assigned from right to left.
#'
#' @param txt A character string
#'
#' @return A `bigz` integer
#' @export
#' @import gmp
#'
#' @examples
#' stringToBigz("A string, which may contain punctuation.")
stringToBigz <- function(txt) {
  nraw <- charToRaw(txt)
  l <- length(nraw)
  return(sum(as.bigz(256)^(l-(1:l))*as.numeric(nraw)))
}

#' Convert (some) big integers to strings
#'
#' This function is intended to serve as an inverse to the \code{\link{stringToBigz}} function. If the raw representation of `n` contains 00's, this function will produce an `embedded nul in string` error. Thus it is not suitable for all integers.
#'
#' @param n A `bigz` integer, whose raw representation must not contain 00's
#'
#' @return A character string
#' @export
#' @import gmp
#'
#' @examples
#' stringAsInt <- stringToBigz("Any string.")
#' bigzToString(stringAsInt)
#' # bigzToString(256^3+256) # embedded nul error!
bigzToString <- function(n) {
  numbytes <- ceiling(log2.bigz(n)/8)
  nnumeric <- numeric(numbytes)
  for(i in 0:(numbytes-1)) {
    b <- as.numeric(mod.bigz(n, 256))
    n <- divq.bigz(n, 256)
    nnumeric[numbytes-i] <- b
  }
  return(rawToChar(as.raw(nnumeric)))
}

#' Brute force attack RSA on small plaintext
#'
#' Given only a public RSA key and public exponent, this function will decrypt a ciphertext and return the plaintext, assuming the the plaintext is smaller than \code{ptBound}. Returns \code{NA} if decryption fails.
#'
#' @param n The RSA modulus as a `bigz` integer
#' @param e The RSA encryption exponent as a `bigz` integer
#' @param ct The ciphertext as a `bigz` integer
#' @param ptBound A `bigz` integer giving us the largest plaintext to try
#'
#' @return The plaintext as a `bigz` integer
#' @export
#' @import gmp
#'
#' @examples
#' rsaModulus <- as.bigz("22707858128516078577522361738665981657809809994800879647099566900106275857083781927535157151969024025146392877047548296429131296960565225703131554564424303895839201970309317909253878238372772038385743017743501324244450407606625837901798398885199423574643610744324562528276931827066473866615454887905670524424503707688926818794119993936431086630160065072554898339473100008792325122685429509518165954131128398922958816869795758667406044391829713985703446445013454864090920545268599414852198609275476082969800254336576218717151624678407111982345920999659967265380242402996336414819723456853027238543378738028207982298417")
#' rsaExponent <- as.bigz("65537")
#' cipherText <- powm(345, rsaExponent, rsaModulus)
#' bfRSAattack(rsaModulus,
#'             rsaExponent,
#'             cipherText)
#' bfRSAattack(rsaModulus,
#'             rsaExponent,
#'             cipherText,
#'             as.bigz(150)) # bound not big enough
bfRSAattack <- function(n, e, ct, ptBound = as.bigz(1000)) {
  # TODO
}

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
#' Randomly generates an RSA modulus n = pq. Checks to make sure p-q is at least a googol, and checks to make sure that p-1 and q-1 have no small prime factors. We'll use e = 65537. Returns a list containing n, p, q, e, and d, the decryption exponent.
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

