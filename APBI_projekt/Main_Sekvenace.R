library(BiocManager)
library(Biostrings)
library(ShortRead)
library(fastqcr)
library(ggplot2) 
library(dplyr)
library(hrbrthemes)
library(sangerseqR)
library(ape)
library(msa)
library(tidyverse)
library(Biostrings)
library(Rsamtools)
library(Rbowtie2)
library(GenomicAlignments)

#definujte vlastni cestu
cesta <- "C:/Users/Pawel/Desktop/trzeciok/semester6/APBI/APBI/APBI_projekt/Sekvenace/runIllumina/"
  
files <- list.files(cesta,pattern = ".fq")
b <- c()
j = 1

while (j<= length(files)){
  a <- readFastq(paste(cesta, files[j], sep = "", collapse = NULL))
  b <- append(b,a)
  j = j + 1
}

seq <- sread(b[[1]]) #primary read
seq2 <- sread(b[[2]]) #secondary read
ref <- 'mus.fasta'
q1 <- quality(b[[7]]) #kvalita primarniho fastq souboru
q2 <- quality(b[[8]]) #kvalita sekundarniho fastq souboru 

#vykresli pocet quality G1_1 a G1_2, jednoho souboru
graf_kvalita <- function(q1,q2){
  q <- phredToQ(q1)
  q <- append(q,phredToQ(q2))
  data = data.frame(Phred_Quality_Score=q)
  e <- ggplot(data, aes(x=Phred_Quality_Score)) + geom_histogram( binwidth=1, fill="#69b3a2", color="#69b3a2", alpha=0.9) + theme_ipsum()+ theme(plot.title = element_text(size=15))
  return(e)
}

data = data.frame(value=q)
e <- ggplot(data, aes(x=value)) + geom_histogram( binwidth=1, fill="#69b3a2", color="#69b3a2", alpha=0.9) + theme_ipsum()+ theme(plot.title = element_text(size=15))

graf_pocet <- function(x){
  data2 = data.frame(Phred_Quality_Score=phredToQ(rdy4Chroma(x),1)) # 1 jako prvni file fastaq
  d <- ggplot(data2, aes(x=Phred_Quality_Score)) + geom_histogram( binwidth=1, fill="#69b3a2", color="#69b3a2", alpha=0.9) + theme_ipsum()+ theme(plot.title = element_text(size=15) )
  return(d)
}

#hist(phredToQ(quality,))
phredToQ <- function(x){
  s <- NaString(x)
  i <- 1
  while (i<=length(s)) {
    s[i] <- utf8ToInt(s[i])-33
    i <- i + 1
  }
  return(as.numeric(s))
}

#FUNKCE kvalita do stringove podoby
NaString <- function(x){
  j<-1
  y <- c()
  while (j<=length(x)) {
    y <- append(y,unlist(strsplit(as.character(x[[j]]),"")))
    j <- j + 1
  }
  return(y)
}

#priprava dat na chromatogram
rdy4Chroma <- function(seq){
  v <- unlist(strsplit(toString(seq),""))
  v <- v[v!=","] #odstraneni z retezce ","
  v <- v[v!=" "] #ostraneni z retezce " "
  v <- paste(v,collapse = "")
  return(v)  
}

o <- oligonucleotideFrequency(seq[1],width = 1) # pocet A,G,T,C width=1, width2 -> AA,AC,AT ... pocet

#pa <- pairwiseAlignment(rdy4Chroma(seq), rdy4Chroma(seq2),type = "global-local")
#writePairwiseAlignments(pa)

# namapování
Mapovani <- function(seq1,seq2){
ref <- paste(cesta,"mus.fasta",sep = "",collapse = NULL)
reads1 <- paste(cesta,seq1,sep = "",collapse = NULL)
reads2 <- paste(cesta,seq2,sep = "",collapse = NULL)
index <- 'G3_index'

bowtie2_build(references = ref, bt2Index = index,overwrite=TRUE)
bowtie2(bt2Index = index, samOutput = 'G3_mapped', seq1 = reads1, seq2 = reads2,overwrite=TRUE)

# convert sam to bam
asBam('G3_mapped',overwrite=TRUE)

# pileup
pileup <- pileup('G3_mapped.bam',overwrite=TRUE)
PileupParam(max_depth=250, min_base_quality=13, min_mapq=0, min_nucleotide_depth=1, min_minor_allele_depth=0, distinguish_strands=TRUE, distinguish_nucleotides=TRUE, ignore_query_Ns=TRUE, include_deletions=TRUE, include_insertions=TRUE, left_bins=NULL, query_bins=NULL, cycle_bins=NULL)

# coverage visualization
plot(pileup$pos, pileup$count, 'l')
ggplot(pileup,aes(x=pos,y=count)) + geom_smooth(method = 'loess', span = 0.1) + geom_point(size = 0.1)
table(pileup$strand, pileup$nucleotide)

# better visualization
ggplot(pileup,aes(x=pos,y=count)) +
  geom_smooth(method = 'loess', span = 0.1) +
  geom_point(size = 0.1)

#return(table(pileup$strand, pileup$nucleotide))
return(ggplot(pileup,aes(x=pos,y=count)) + geom_smooth(method = 'loess', span = 0.1) + geom_point(size = 0.1))
}




