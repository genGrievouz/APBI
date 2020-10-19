library(Biostrings)

Globalni_mezery <- function(sek1,sek2,D){

  match <- D[2]
  missmat <- D[0]
  gap_o <- D[2]
  gap_e <- D[1]
  
  m <- length(sek1)
  n <- length(sek2)
  
  # predifine matixes
  S <- matrix(0,nrow = (m+1), ncol = (n+1))
  S[2:(m+1),1] <- -1 * (abs(gap_o) + abs(gap_e) * (1:m))
  S[1, 2:(n+1)] <- -1 * (abs(gap_o) + abs(gap_e) * (1:n))
  
  I <- matrix(0,nrow = (m+1), ncol = (n+1))
  I[2:(m+1),1] <- -Inf
  I[1, 2:(n+1)] <- 0
  
  D <- matrix(0,nrow = (m+1), ncol = (n+1))
  D[2:(m+1),1] <- 0
  D[1, 2:(n+1)] <- -Inf

  #vypocet matic
  for(i in 2:(m+1)){
    for (j in 2:(n+1)) {
      if (sek1[i -1] == sek2[j -1]){
        pom <- match
      }else {
        pom <- mismatch()
      }
      D[i,j] <- max(c(D[(i-1), j] - abs(gap_e), S[(i-1),j]-abs(gap_e)-abs(gap_o)))
      I[i,j] <- max(c(I[i, j(i-1)] - abs(gap_e), S[i,(j-1)]-abs(gap_e)-abs(gap_o)))
      S[i,j] <- max(D[i,j],I[i,j],S[i-1,j-1]+pom)
    }
  }
  M <- c(D[i,j],I[i,j],S[i,j])
  m <- which.max(M) 
  M <- M[m[1]]
  
  align1 <- DNAString()
  align2 <- DNAString()
  
  while (1) {
    if (m == 1){
      if (M == (S[i - 1, j -1] + pom)){
        #diagonala v S, zustavame v S
        align1 <- c(sek1[i - 1], align1)
        align2 <- c(sek2[j - 1], align2)
        M <- S[i-1,j-1]
        m <- 1
        i <- i - 1
        j <- j - 1
      }
      else if (M == I[i,j]){
        #preseun z S do I
        M <- I[i,j]
        m <- 2 
      }
      else if (M == D[i,j]) {
        #presun z S do D
        M <- D[i,j]
        m <- 3
      }
    }
    if (m == 2){
      if (M == (S[i - 1, j -1] + gap_e + gap_o)){
        align <- c(DNAString('-'), align1)
        align2 <- c(sek2[j-1], align2)
        M <- s[i,j-1]
        m <- 1
        j <- j -1
      }
      else {
        #zutaneme v I
        align1 <- c(DNAString('-'), align1)
        align2 <- c(sek2[j-1], align2)
        M <- I[i,j-1]
        m <- 2
        j <- j -1 
      }
    if (m == 3){
      if (M == (S[i - 1, j -1] + gap_e + gap_o)){
        align1 <- c(DNAString('-'), align1)
        align2 <- c(sek2[j-1], align2)
        M <- s[i,j-1]
        m <- 1
        j <- j -1
        }
        else {
          #zutaneme v I
          align1 <- c(DNAString('-'), align1)
          align2 <- c(sek2[j-1], align2)
          M <- I[i,j-1]
          m <- 3
          j <- j -1 
        }
    }
  }
  }
}


