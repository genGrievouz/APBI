##HLAVNI funkce Vinegere
## autor: Pawel Santarius 203202 xsanta08@vutbr.cz
# dne 20.2 2020
Vinegere <- function(x, key){
  if(is.character(x)==TRUE && is.character(key)==TRUE){
    
    index_mezer <- IndexyMezer(x) - 1# -1 jedna protoze pri vkladani do vektoru budeme vklada after ... napr pauza ma index 5 .. ale budeme vlkada za 4
    x <- gsub(" ","",x) #odstraneni mezer aby nedoslo ke spatnemu indexovani v key
    x <- tolower(unlist(strsplit(x,'')))
      
    key <- tolower(unlist(strsplit(key,'')))
    key <- Opakuj(x,key)
    
    vektor_x <- NaCisla(x) #prevod na cisla
    vektor_k <- NaCisla(key)
    
    final <- Sifruj(vektor_x,vektor_k)
    final <- PasteMezera(final,index_mezer)
    final <- paste(final,collapse = '') #list precede do jednoho stringu 
    return(final)
  }
  else if(is.character(x)!=TRUE && is.character(key)==TRUE){
    print("x neni typu character")
  }
  else if(is.character(x)!=TRUE && is.character(key)!=TRUE){
    print("x a key nejsou typu character")
  }
  else{
    print("key neni typu character")
  }
}

##Sifruj, z vektorů vememe cisla, ktere budou slouzit jako indexy v matici
#pomocna funkce
Sifruj <- function(x,y){
  result <- c()
  i<-1
  matrix <- Matice() #vygeneruj matici
  while(i<=length(x)){
    l <- matrix[x[i],y[i]]
    result <- append(result,l)
    i = i + 1
  }
  return(result)
}

##NaCisla, preved pismena na cisla podle abecedy
#pomocna funkce
NaCisla <- function(x){
  k<-1
  vektor<-c()
  abeceda<-letters[seq(from=1,to=26)] #gerneruj abecedu podle ktere vytvorime indexy
  while (k <= length(x)){
    vektor = append(vektor,match(x[k],abeceda)) #do noveho vektoru pripoji novy element, match najde podle pismena v a(vygenerovana abeceda)
    k = k+1
  }
  return(vektor)
}

##Opakuj, opakuje klic do delky x, vysledek delka key = delka x
#pomocna funkce
Opakuj <- function(x,y){
  l_x <- length(x)
  l_y <- length(y)
  r <- l_x - l_y
  i <- 1
  while(i <= r){
    uloz<-y[i]
    y[i+l_y]<-uloz
    i = i + 1 
  }
  return(y)
}

##Matice, generuje Vinegere matici
#pomocna funkce
Matice <- function(){
  matrix <- matrix(0,nrow = 26,ncol = 26)
  i <- 0
  while(i<26){
    matrix[i+1,1:ncol(matrix)] <- letters[seq(from=i+1,to=26+i)] #generace od a do z, na dalsim radku zacni abecedu s indexem i+1
    i = i + 1
  }
  for(k in 2:26){
    matrix[28-k,k:ncol(matrix)] <- letters[seq(from=1,to=27-k)] #doplneni pismen od 
    k = k + 1
  }
  return(matrix)
}

#pomocna funkce
#uklada indexy mezer do vektoru 
IndexyMezer <- function(x){
  x <- tolower(unlist(strsplit(x,'')))
  y <- gregexpr(' ',x)
  k <- c()
  i <- 1
  while (i<length(y)) {
    if(y[i]==1){
      k <- append(k,i)
      i <- i + 1
    }
    else{
      i = i + 1
    }
  }
  return(k)
}

#pomocna funkce
#vklada mezery podle indexu na zacatku
PasteMezera <- function(final,index_mezer){
  i <- 1 
  while(i <= length(index_mezer)){
    index <- index_mezer[i]
    final <- append(final," ",after = index)
    index_mezer + 1
    i = i + 1
  }
  return(final)
}
