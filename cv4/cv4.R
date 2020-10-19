library(Biostrings)

sek <- readDNAStringSet(filepath = "D.fa", format = "fasta")
sek <- sek[[1]] #prevedeni do DNAString

genkod <- "SGC1" #mitochondrialni sgc1
useky <- ORFFinder(sek,genkod) #jenom sekvence, ne cely stringset proto [[1]]
writeXStringSet(useky, "useky.fasta")

ORFFinder <- function(sek,genkod){
  kod <- getGeneticCode(genkod) #ku kterymu kodonu patri AK
  start_kodony <- names(kod[which(kod == "M")])
  stop_kodony <- names(kod[which(kod=="*")])
  
  start_ind <- c()
  for (start in start_kodony){
    inds <- matchPattern(start,sek)
    start_ind <- c(start_ind, start(inds))
  }
  
  STOP_IND <- c()
  for (stop in stop_kodony){
    inds <- matchPattern(stop,sek)
    STOP_IND <- c(STOP_IND, start(inds))
  }
  
  #serazeni
  start_ind <- sort(start_ind)
  STOP_IND <- sort(STOP_IND)
  
  #pomic operace modulo, urcim cteci ramec
  cc <- which((start_ind %% 3) == 1)
  start_ind_ORF1 <- start_ind[cc]
  
  cc <- which((STOP_IND %% 3) == 1)
  stop_ind_ORF1 <- STOP_IND[cc]
  
  kodujici_sek <- DNAStringSet()
  for (i in start_ind_ORF1) {
    cc <- stop_ind_ORF1[which(stop_ind_ORF1>i)]
    if (!isEmpty(cc)){
      cc <- cc[1]
      kodujici_sek <- c(kodujici_sek, DNAStringSet(sek[i:(cc+2)]))
    }
  }
  return(kodujici_sek)
}



