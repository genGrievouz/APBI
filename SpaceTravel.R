##HLAVNI FUNKCE SpaceTravel()  
#autor: Pawel Santarius
#dne: 07.03.2019
SpaceTravel <- function(cesta,start,cil){
  
  cesta = read.csv(file = cesta,header=FALSE,sep=',');
  cesta = unlist(lapply(cesta, as.character)) ## prevod na charakter, unlist
  pocet_k = 0
  i = 1
  
  for (i in cesta) {
    start1 = Hledej(cesta,start,cil)
    pocet_k = pocet_k + 1
  }
 
  return(pocet_k)
  
}

##POMOCNA FUNKCE HLEDEJ 
Hledej <- function(cesta,start,cil){
  if(start == cil){
  return(cil)
    break
  }
  else{
  start = paste(start,")",sep="")
  i = match(start,cesta)
  slovo = unlist(strsplit(cesta[i],""))
  l_slova =length(slovo)
  slovo_konec = paste(slovo[5:l_slova],collapse='')
  return(slovo_konec)
  }
}