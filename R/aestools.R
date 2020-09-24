#' Brute force known plaintext AES attack
#'
#' Given a list of possible 128-bit AES keys, a 128-bit plaintext block and a 128-bit ciphertext block, determine the key that was used. We are assuming that AES was used in ECB mode as implemented by the \code{digest} package. If no key was found, returns \code{NA}.
#'
#' @param pt A plaintext block, given as a 16-character string
#' @param ct A ciphertext block, given as a 16-character string
#' @param keys A vector of 16-character strings, one of which was used to encrypt
#'
#' @return A 16-character string giving the key that was used
#' @export
#' @import digest
#'
#' @examples
#' knownPT <- "myplaintextblock"
#' keystotry <- c("collaborationist",
#'                "eigenfrequencies",
#'                "greatgrandmother",
#'                "lighthousekeeper",
#'                "multibillionaire")
#' cipherText <- "U\xa4\x90\020pt\037\xb2\xf8\xfaB`5\xb7$]"
#' bfkpAESattack(knownPT, cipherText, keystotry)
bfkpAESattack  <- function(pt, ct, keys)
{
  # TODO
}

