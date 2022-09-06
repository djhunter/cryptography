#' Count the occurence of each letter in a string
#'
#' This function counts how many times each letter occurs in a string and presents in a table.
#'
#' @param txt String (assumed to contain only lowercase letters)
#'
#' @return A table of letter counts, sorted by decreasing count
#' @export
#'
#' @examples
#' letterCounts("westleyisanolddog")
#'
letterCounts <- function(txt)
{
  return(sort(table(unlist(strsplit(txt,""))),decreasing=TRUE))
}

#' Count digrams in a string
#'
#' This function counts how many times each digram (two successive letters) occurs in a string.
#'
#' @param txt String (assumed to contain only lowercase letters)
#'
#' @return A table with the numbers of digrams of each possible type
#' @export
#'
#' @examples
#' digramTable("thedogsnameiswestleythedog")
#'
digramTable <- function(txt)
{
  l <- unlist(strsplit(txt,""))
  dgs <- data.frame(l,c(l[2:length(l)],NA))
  names(dgs) <- c("first","second")
  table(dgs)
}

#' Encrypt a string using a Vigenere cipher
#'
#' This function implements the Vigenere cipher. Each letter of the  
#' plaintext is shifted by the corresponding value of the repeated key vector.
#'
#' @param txt String to be encrypted (assumed to contain only lowercase letters)
#' @param keyVector Vector of integers (mod 26) to shift by
#'
#' @return An encrypted string
#' @export
#'
#' @examples
#' plaintext <- "thisistheplaintextthatwewouldliketoencryptusingthevigenerecipher"
#' keyAsVector <- stringToMod26("mypassphrase")
#' vigenere(plaintext, keyAsVector)
#'
vigenere <- function(txt, keyVector)
{
  pt <- stringToMod26(txt)
  suppressWarnings(
    ct <- (pt + keyVector) %% 26
  )
  return(mod26ToString(ct))
}
