#' Modified One-Time Pad encryption
#'
#' Encrypt a string using a modified one-time pad. Both the string and the key are converted to raw and then xor-ed.
#'
#' @param plaintxt A plaintext string of ASCII characters.
#' @param key A string of ASCII characters representing the key. Must be at least as long as the plaintext.
#'
#' @return An encrypted string of characters, all at least \code{"\x80"}. 
#' @export
#'
#' @examples
#' motpEncrypt("hello", "goodbye")
#' motpEncrypt("hello", "mellow")
#' motpEncrypt("hello", "harry")
motpEncrypt  <- function(plaintext, key) 
{
  # TODO
}

#' Modified One-Time Pad decryption
#'
#' Decrypt a ciphertext string that was encrypted using \code{motpEncrypt}
#'
#' @param ciphertext A ciphertext string of characters, all at least \code{"\x80"}
#' @param key A string of ASCII characters representing the key. Must be at least as long as the plaintext.
#'
#' @return The original ASCII plaintext
#' @export
#'
#' @examples
#' ct <- motpEncrypt("hello", "harry")
#' motpDecrypt(ct, "harry")
motpDecrypt  <- function(ciphertext, key) 
{
  # TODO
}
