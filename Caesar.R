###HLAVNI FUNKCE CEASAR
## autor: Pawel Santarius 203202 xsanta08@vutbr.cz
# dne 7.2 2020

Caesar <- function(retezec,posun){
  retezec = tolower(unlist(strsplit(retezec,''))) #rozdeleni retezce 
  delka = length(retezec)
  a = letters[seq(from=1,to=26)] #generace abecedy
  k <- 1 
  novy_vektor = c()
  
  while (k <= delka) {
    novy_vektor = append(novy_vektor,match(retezec[k],a)) #do noveho vektoru pripoji novy element, match najde podle pismena v a(vygenerovana abeceda)
    k = k+1
  }
  novy_vektor = novy_vektor + posun #spositani indexu s posunem, puvodni + posun
  #return(novy_vektor)
  novy_vektor = Kalkulace(novy_vektor, delka) #vyvolani funkce Kalkulace
  return(Back2alphabet(novy_vektor,a)) #
}

##KALKULACE
#funkce pro vypocet indexu, kdyz index(novy_vektor > 26 nebo < 0)
Kalkulace <- function(x,delka){
  k <- 1
  y = c()
  while (k <= delka) {
    if(x[k] > 26 & !is.na(x[k])){
      z = x[k] - 26
      y = append(y,z,after = length(y))
      k = k+1
    }else if(x[k] < 1 & !is.na(x[k])){
      z = x[k] + 26
      y = append(y,z,after = length(y))
      k = k+1
    }else {
      z = x[k]
      y = append(y,z,after = length(y))
      k = k+1
    }
  }
  return(y)
}

## BACK2ALPHABET
# Funkce ktera prevede ciselne indexy zpatky na adecedu
Back2alphabet <- function(novy_vektor,a){
  delka_nov = length(novy_vektor)
  nov_vektor =c()
  k <- 1
  
  while (k <= delka_nov){
    j = novy_vektor[k]
    nov_vektor = append(nov_vektor,a[j])
    k = k+1
  }
  #gsub -> substituce 'NA' za prazdne pole priklad: goNAgoNAtimmy -> go go timmy
  nov_vektor = gsub('NA'," ",paste(nov_vektor,collapse = '')) #nov_vektor = paste(nov_vektor,collapse = '') slucuje vsechny string uvnitr vektoru do jednoho slova
  return(nov_vektor)
}