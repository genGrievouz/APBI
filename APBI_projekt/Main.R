library(seqinr)
library(msa)
library(ape)
library(Biostrings)

#geny COX1, COX2, COX3, ND1, ND2, ND3, ND4, ND5, ND6, ATP6, ATP8, CYTB, ND4L
#organismus A,B,C
# A <- ursus thibetanus
# B <- papio hamadryas
# C <- Phyllodactylus

#Nepsal jsem funkci, protoze mne zajimaji dva vystupy -> mat, a plot(Tree)
# Pro nacteni distancni matice pouzijte volani -> mat
gen <- "ND6" # tady si vyberte gen 
org <- "A" #tady neznamy organismus

cesta <- "C:/Users/Pawel/Desktop/trzeciok/semester6/APBI/APBI/APBI_projekt/fylogen/soubory_mt_fasta" #pamatujte na zmenu cesty !!!!
ces <- "C:/Users/Pawel/Desktop/trzeciok/semester6/APBI/APBI/APBI_projekt/fylogen" #zmena cesty !!! 

files <- list.files(path = cesta,pattern = "*.fa")
f1 <- list.files(path = ces,pattern = "*.fa")
gen_all_b <- c()
gen_names_b <- c()
j = 1
while (j < length(f1)) {
  b <- readDNAStringSet(paste(ces, f1[j], sep = "", collapse = NULL))
  if(isEmpty(b[grep(gen,names(b))])){
    j = j + 1
  }
  else{
    b_gen <- b[grep(gen,names(b))]
    gen_all_b <- append(gen_all_b,b_gen)
    gen_names_b <- append(gen_names_b,substring(f1[j],1,length(unlist(strsplit(f1[j],"")))-3))
    names(gen_all_b) <- gen_names_b
    j = j + 1
  }
}
gen_all_b_protein <- translate(gen_all_b)


i <- 1
gen_all <- c()
gen_names <- c()
while (i <= length(files)) {
  a <- readDNAStringSet(paste(cesta, files[i], sep = "", collapse = NULL))
  if(isEmpty(a[grep(gen,names(a))])){
    i = i + 1
  }
  else{
    a_gen <- a[grep(gen,names(a))]
    gen_all <- append(gen_all,a_gen)
    gen_names <- append(gen_names,substring(files[i],11,length(unlist(strsplit(files[i],"")))-3))
    names(gen_all) <- gen_names
    i = i + 1
  }
}

if(gen == "COX1"){
  gen_all <- gen_all[-161]
}
if(gen == "ND1"){
  gen_all <- gen_all[-18]
}
if(gen == "COX2"){
  gen_all <- gen_all[-174]
}
if(gen == "ND4"){
  gen_all <- gen_all[-156]
  gen_all <- gen_all[-114]
}
if(gen == "ATP8"){
  gen_all <- gen_all[-167]  
}
if(gen == "ATP6"){
  gen_all <- gen_all[-254]  
}
if(gen == "COX3"){
  gen_all <- gen_all[-163]
}
if(gen == "ND4"){
  gen_all <- gen_all[-156]
  gen_all <- gen_all[-397]
}
if(gen == "ND6"){
  gen_all <- gen_all[-248]
  gen_all <- gen_all[-222]
  gen_all <- gen_all[-211]
  gen_all <- gen_all[-204]
}
if(gen == "CYTB"){
  gen_all <- gen_all[-198]
}
if(gen == "ND4L"){
  gen_all <- gen_all[-79]
}

gen_all_protein <- translate(gen_all[1:length(gen_all)])
gen_all_protein <- append(gen_all_protein,gen_all_b_protein[1])
gen_alignment <- msa(gen_all_protein)
gen_alignment <- msaConvert(gen_alignment, type="seqinr::alignment")
d <- dist.alignment(gen_alignment, "identity")
matice <- as.matrix(d)[1:length(gen_all), org, drop=FALSE]
n <- order(matice)[1:10] #vytahni 10 nejmensich hodnot z matice
mat <- as.matrix(d)[n, org, drop=FALSE]
nam <- row.names(mat)
v <- c()
p <- 1
while(p < 11){
  v <- append(v,gen_all_protein[nam[p]])
  p = p + 1
}
gen2al <- msa(v)
gen2al <- msaConvert(gen2al, type="seqinr::alignment")
d2 <- dist.alignment(gen2al, "identity")
matice <- as.matrix(d2)[1:2, "A", drop=FALSE]
Tree <- nj(d2) #Strom je vytvoreny pro 10 nejblizsich/pribuznych organismu
plot(Tree, main= paste(gen,"fylogenetick� strom"))





