vstup = read.csv(cesta,head = FALSE)
pipe1 <- vstup[1,] #vem prvni rourun
pipe2 <- vstup[2,] #vem druhou rouru

TvorbaMatice<-function(pipe){
i = 1
vyska <- c()
while(i < length(pipe)
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