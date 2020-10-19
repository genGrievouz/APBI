x <- 5 #fkc u mate
class(x)
y <- "5"
class(y)
y <- as.integer(y)
slova <- "halo mate"

delka_znaky <- nchar(slova)
bacha <- length(slova)

vektor <- c(1,2,3,4,5) #vektor
delka_vektoru <- length(vektor) #indexovani jako v matlabu od 1 
# spravna idexace vektor[1]
#pojmenovani 
names(vektor) <- c("prvni","druhy","treti","ctyry","pet") #prirazeni vlastnich indexu

M <- matrix(1:9, nrow = 3) #pocet radku
N <- matrix(1:9, nrow = 3, byrow = TRUE) #pleni po radcich

rownames(M) <- c("a","b","c") #prizazeni vlastnich indexu 
M[3,] #indexace
M[,3] #indexace




#faktorialni promene 
biftek <- c('rare','medium','rare','medium')
biftek <- as.factor(biftek) #prevedeni na faktoriovou promenou




#Dataframery 
nazev <- c("zeme","mercury","venus")
typ <- c("pevna","pevna","plynna")
polomer <- c(0.382,0.546,0.56)
prstenec <- c(F,F,T)

planety <- data.frame(nazev, typ, polomer, prstenec)
planety$nazev[3] #vyber, ouput do vektoru
planety[!planety$prstenec] # ! = negace, inverze hodnot




#Listy
#nema problem s uloyenim jinych struktur 
mujlist <- list(nazev, typ, prstenec, polomer)
mujlist[3]
mujlist[[3]] #vrati i datovou strukturu v list !!!


#cykly a podminky
podminka <- TRUE
if (podminka){
  print("splneno")
}else{
  peint("nesplneno")
}


for(k in nazev){
  print(k)
}

k <- 1
while (k < 10){
  print("kurwa")
  k = k+1
}


#Funcke
mojeFunkce <- function(x) {
  print(x)
  return(x+5)
}
#volani funkce .... -> mojefunkce(cosikgdosiukrodl)
apply(M,2, max) #co, dimenze, funkce
?diag #priklady pouziti
