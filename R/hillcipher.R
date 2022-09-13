#' Encrypt a string with the Hill cipher
#'
#' This function implements the Hill cipher. The plaintext is made into an
#' appropriately-dimensioned matrix, then multiplied by the key matrix.
#'
#' @param txt String to be encrypted (assumed to contain only lowercase letters)
#' @param keyMatrix A square matrix of integers (mod 26) to multiply by
#'
#' @return An encrypted string
#' @export
#'
#' @examples
#' plaintext <- "fifteenletterpt"
#' keyAsMatrix <- matrix(c(1,2,3, 4,5,6, 11,9,8), nrow = 3, byrow = TRUE)
#' hillCipher(plaintext, keyAsMatrix)
hillCipher <- function(txt, keyMatrix) {
  pt <- stringToMod26(txt)
  n <- attributes(keyMatrix)$dim[2]
  suppressWarnings(
    mPtxt <- matrix(pt,nrow=n) # repeats text so length is a multiple of n
  )
  mCtxt <- (keyMatrix %*% mPtxt) %% 26
  return(mod26ToString(as.vector(mCtxt)))
}