##HLAVNI FUNKCE PipeIntersection
#autor: Pawel Santarius
#dne: 07.03.2019
PipeIntersection <- function(cesta){
  vstup = read.csv(cesta,head = FALSE)
  pipe1 <- vstup[1,] #vem prvni rourun
  pipe2 <- vstup[2,] #vem druhou rouru
  d <- PipeMapCreation(vstup,pipe2) #vyvoreni matice s cestou pipe1, cesta vyznacena cisly 1, 
  c <- PipeMapCreation(vstup,pipe1) #vytvoreni matice s cestou pipe2, cesta vyznacena cisly 1,
  result_matrix = c + d #secteni matic, v mistech spojeni se objevi 2ky,
  start = which(result_matrix == 10,arr.ind = T) #start v matici pozname podle cisla 10
  spoje = which(result_matrix == 2,arr.ind = T) #spoje pozname podle 2
  result = Vzdalenost(start,spoje) #vypocet vzdalenosti
  return(result)
}
#POMOCNA FUNKCE vypocet vzdalenosti
Vzdalenost <- function(start,spoje){
  vzdalenosti = c()
  i = 1
  while (i <= length(spoje)/2) {
    x = abs(start[1]-spoje[i,1])
    y = abs(start[2]-spoje[i,2])
    vzdalenosti = append(vzdalenosti,x+y)
    i = i + 1 
  }
  vzdalenost = min(vzdalenosti)
  return(vzdalenost)
}
#POMOCNA FUNKCE vytvarejici matici s cestou roury
PipeMapCreation <- function(vstup,pipe1){
  c <-matrix(data=0,nrow=10000,ncol=10000)
  x = 5000;
  y = 5000;
  c[x,y] <- 5 #zacatek reprezentuje cislo 5, sumace dvou matic start = 5 + 5 = 10
  i = 1
  while(i<=length(pipe1)){
    if(PipeDirection(pipe1,i)=='r'){
      if(i == 1){
        c[x,(y+1):(y+PipeLength(pipe1,i))]=1
        x = x
        y = y + PipeLength(pipe1,i)
        i = i + 1
      }
      else if(i != 1){
        c[x,y:(y+PipeLength(pipe1,i))]=1
        x = x
        y = y + PipeLength(pipe1,i)
        i = i + 1
      }
    }
    else if(PipeDirection(pipe1,i)=='l'){
      if(i != 1){
        c[x,abs(y-PipeLength(pipe1,i)):y]=1
        x = x
        y = abs(y-PipeLength(pipe1,i))
        i = i + 1 
      }
      else if(i == 1){
        c[x,abs(y-PipeLength(pipe1,i)):y-1]=1
        x = x
        y = abs(y-PipeLength(pipe1,i))
        i = i + 1
      }
    }
    else if(PipeDirection(pipe1,i)=='u'){
      if(i != 1){
        c[abs(x-PipeLength(pipe1,i)):x,y]=1
        x = abs(x-PipeLength(pipe1,i))
        y = y
        i = i + 1 
      }
      else if(i == 1){
        c[abs(x-PipeLength(pipe1,i)+1):x-1,y]=1
        x = abs(x-PipeLength(pipe1,i))
        y = y
        i = i + 1  
      }
    }
    else if(PipeDirection(pipe1,i)=='d'){
      if(i != 1){
        c[x:(x+PipeLength(pipe1,i)),y]=1 
        x = abs(x+PipeLength(pipe1,i))
        y = y
        i = i + 1  
      }
      else if(i == 1){
        c[(x+1):(x+PipeLength(pipe1,i)),y]=1 
        x = abs(x+PipeLength(pipe1,i))
        y = y
        i = i + 1
      }
    }
  }
  return(c)
}
#POMOCNA FUNKCE, odhad smeru
PipeDirection <- function(pipe,i){
  direction = unlist(strsplit(unlist(lapply(pipe[i],as.character)),""))
  direction = direction[1]
  if(direction=="R"){
    direction = 'r'  
  }
  else if(direction=="L"){
    direction = 'l' 
  }
  else if(direction=="U"){
    direction = 'u'
  }
  else if(direction=="D"){
    direction = 'd'
  }
  return(direction)
}
#POMOCNA FUNKCE PipeLength
PipeLength <- function(pipe,i){
  lengthOfPipe = unlist(strsplit(unlist(lapply(pipe[i],as.character)),""))
  lengthOfPipe = lengthOfPipe[2:length(lengthOfPipe)] #cti za pismenem cisla
  lengthOfPipe = as.integer(paste(lengthOfPipe,collapse = '')) #udelej z nej integer
  return(lengthOfPipe)
}