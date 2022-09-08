#' Count the occurrence of each letter in a string
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

#' Shift a vector by n positions
#'
#' This function shifts the elements of a vector n positions to
#' the right, with the end wrapping around to the beginning.
#'
#' @param v Vector to be shifted
#' @param n Number of positions to shift the vector
#'
#' @return The shifted vector
#' @export
#'
#' @examples
#' shiftVec(c("a","b","c","d","e"), 1)
#' shiftVec(c(2, 3, 5, 7, 9, 11, 13, 17), 3)
#'
shiftVec <- function(v, n) {
  v[(seq_along(v) - (n+1)) %% length(v) + 1]
}

#' Find characters in positions n, n+r, n+2r, n+3r, etc
#'
#' Given a string, this function returns a new string comprising the 
#' characters in positions n, n+r, n+2r, n+3r, etc.
#'
#' @param txt A string
#' @param n Position of the first character
#' @param r Relative position of each subsequent character
#'
#' @return A string containing the selected characters
#' @export
#'
#' @examples
#' skipString("whatshouldwedotodaybrain",3,4)
#'
skipString <- function(txt, n, r) {
  l <- unlist(strsplit(txt,""))
  ss <- l[seq(n,length(l),r)]
  return(paste0(ss,collapse=""))
}

#' Compute the relative frequency of letters in a string
#'
#' This function computes the relative frequency each letter occurs 
#' in a string of lower case letters.
#'
#' @param txt String (assumed to contain only lowercase letters)
#'
#' @return A vector of letter frequency, in order of lower case English letters.
#' @export
#'
#' @examples
#' letterFreq("albuquerqueisacityinnewmexico")
#'
letterFreq <- function(txt) {
  l <- unlist(c(strsplit(txt, ""), letters))
  t <- as.vector(table(l)) - 1
  return(t/sum(t))
}

#' Find the Vigenere key
#'
#' This function consumes a ciphertext string `ciphertext` and a key 
#' length `keyLength`, and returns the key that produced the ciphertext, 
#' assuming that the ciphertext was produced using a Vigenère cipher with 
#' key length `keyLength`.
#'
#' @param ciphertext String (assumed to contain only lowercase letters)
#' @param keylength The length of the key
#'
#' @return The key, as a vector of elements of Z mod 26.
#' @export
#'
#' @examples
#' ptext <- "howeverinthisextremedistresshewasnotdestituteofhisusualsagacitybuttrustinghimselftotheprovidenceofgodheputhislifeintohazardinthemannerfollowingandnowsaidhesinceitisresolvedamongyouthatyouwilldie"
#' keyAsVector <- stringToMod26("skyler")
#' ctext <- vigenere(ptext, keyAsVector)
#' findVigKey(ctext, 6)  # These...
#' keyAsVector           #    should match.
#'
findVigKey <- function(ciphertext, keyLength) {
  vKey <- numeric(keyLength) # preallocate a vector of the desired length
  englishFreqs <- c(0.082,0.015,0.028,0.043,0.127,0.022,0.020,0.061,0.070, 0.002,0.008,0.040,0.024,
                  0.067,0.075,0.019,0.001,0.060,0.063,0.091,0.028,0.010,0.023,0.001,0.020,0.001)

    # TODO: Insert code here to find the key, and assign the pth element of the key (as an element
    # of Z_26) the variable vKey[p], for values of p from 1 to keyLength

  return(vKey)
}