#' Encrypt using an affine cipher.
#'
#' This function performs the affine encryption function \eqn{y = \alpha x + \beta mod 26} where \eqn{(\alpha,26) = 1}.
#'
#' @param plainText String to be encrypted (assumed to contain only lowercase letters)
#' @param alpha Scale factor.
#' @param beta Affine shift.
#'
#' @return A encrypted string, where each letter has been replaced by the result of the encryption function.
#' @export
#'
#' @examples
#' affineCipher("thisisasecretmessage", 9, 2)
affineCipher <- function(plainText, alpha, beta) {
  # TODO: complete this function
}
