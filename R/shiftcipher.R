#' Convert a string to a vector of integers mod 26
#'
#' @param x A string (assumed to contain only lowercase letters)
#'
#' @return A vector of integers, where 0 corresponds to 'a', 1 to 'b', etc.
#' @export
#' @examples
#' stringToMod26("cubswin")
stringToMod26 <- function(x) {utf8ToInt(x)-utf8ToInt("a")}

#' Convert a vector of integers mod 26 to a string
#'
#' @param x A vector of integers (assumed to contain only 0...25)
#'
#' @return A string, where 'a' corresponds to 0, 'b' to 1, etc.
#' @export
#' @examples
#' mod26ToString(c(18,14,23,11,14,18,4))
mod26ToString <- function(x) {intToUtf8(x+utf8ToInt("a"))}

#' Encrypt a string using a shift cipher
#'
#' @param plainText A string of lowercase letters
#' @param shift An integer (mod 26) to shift by
#'
#' @return An encrypted (or decrypted) string, where each letter has been shifted by \code{shift} places.
#' @export
#'
#' @examples
#' plainText <- "thisisasupersecretmessagethatwewanttoencrypt"
#' cipherText <- shiftCipher(plainText, 5)
#' print(cipherText)
#' print(shiftCipher(cipherText, 21)) # should be original plaintext
shiftCipher <- function(plainText, shift){
  pt <- stringToMod26(plainText)
  ct <- (pt + shift) %% 26  # encrypt by shifting
  return(mod26ToString(ct))
}
