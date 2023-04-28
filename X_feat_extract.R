#install.packages("seqinr")
#install.packages("protr")
##install.packages(Biostrings)
library("seqinr")
library(protr)


#if (!requireNamespace("BiocManager", quietly = TRUE))
 #install.packages("BiocManager")
#BiocManager::install()

#BiocManager::install(c("Biostrings", "AnnotationDbi"))


library(Biostrings)
library(seqinr)

showMethods("readFasta")





#ncrna<-read.fasta(file = "D:\Research Work\\Disertation Project 1\\Raw Data\\featureExtraction\BlastP\\Test\\positve_sub.fasta", as.string = TRUE, seqtype = "AA")

ncrna<-read.fasta(file = "D:\\Research Work\\Disertation Project 2\\Raw Data\\CDHIT\\negative\\negativeBac.faa",as.string = TRUE, seqtype = "AA")



#print(ncrna)
l<-length(ncrna)
min(getLength(ncrna))


#l<-nrow(ncrna)
#l
#ncrna<-ncrna_na$Sequence
#ncrna[[12]]
## amino acid composition (20D)
extractAAC_BAC <- function(x) {
  
  AADict <- c(
    "A", "R", "N", "D", "C", "E", "Q", "G", "H", "I",
    "L", "K", "M", "F", "P", "S", "T", "W", "Y", "V" 
  )
  
  if (all(strsplit(x, split = "")[[1]] %in% AADict) == FALSE) {
    stop("x has unrecognized amino acid type")
  }
  
  # 20 Amino Acid Abbrevation Dictionary from
  # https://en.wikipedia.org/wiki/Amino_acid#Table_of_standard_amino_acid_abbreviations_and_properties
  
  
  
  AAC <- summary(
    factor(strsplit(x, split = "")[[1]], levels = AADict),
    maxsum = 21
  ) / nchar(x)
  
  return (AAC)
}

col_n<-"aac_1"

for(k in 2:20){
  col_n<-paste0(col_n," ","aac_",k)
  
}

ls<-c(col_n)
ls
#x<-ncrna[[114]]
#x
#x<-toString(ncrna[[175]])
#x

#strsplit(x)
#extractAAC(x)
#d<-extractAAC(54)
#d
for(i in 1: l){
  
  x<-ncrna[[i]]
  #x<-ncrna[[12]]
  d<-extractAAC_BAC(x)
  temp<-d[[1]]
  for(j in 2:20){
    temp<-paste0(temp," ",d[[j]])
    
  }
  
  ls<-c(ls,temp)
  
  print(i)
}

write.csv(ls, "D:\\Research Work\\Disertation Project 2\\Raw Data\\featureExtraction\\projectDataNegative/aac.txt", row.names = FALSE)


ncrna<-read.fasta(file = "D:\\Research Work\\Disertation Project 2\\Raw Data\\CDHIT\\negative\\negativeBac.faa",as.string = TRUE, seqtype = "AA")



#print(ncrna)
l<-length(ncrna)
min(getLength(ncrna))

#Normalized Moreau-Broto Autocorrelation Descriptor

col_n<-"moreau_1"

for(k in 2:80){
  col_n<-paste0(col_n," ","moreau_",k)
  
}

ls<-c(col_n)


for(i in 1: l){
  
  x<-ncrna[[i]]
  #x<-ncrna[[12]]
  d<-extractMoreauBroto(x, props = c("CIDH920105", "BHAR880101", "CHAM820101",
                                     "CHAM820102", "CHOC760101", "BIGC670101", "CHAM810101", "DAYM780201"),
                        nlag = 10L, customprops = NULL)
  
  
  temp<-d[[1]]
  for(j in 2:80){
    temp<-paste0(temp," ",d[[j]])
    
  }
  
  ls<-c(ls,temp)
  
  print(i)
}


write.csv(ls, "D:\\Research Work\\Disertation Project 2\\Raw Data\\featureExtraction\\projectDataNegative/moreau.txt", row.names = FALSE)

#QSO


ncrna<-read.fasta(file = "D:\\Research Work\\Disertation Project 2\\Raw Data\\CDHIT\\negative\\negativeBac.faa",as.string = TRUE, seqtype = "AA")



#print(ncrna)
l<-length(ncrna)
min(getLength(ncrna))

col_n<-"qso_1"

for(k in 2:40){
  col_n<-paste0(col_n," ","qso_",k)
  
}

ls<-c(col_n)
ls

for(i in 1: l){
  
  x<-ncrna[[i]]
  #x<-ncrna[[12]]
  d<-extractQSO(x, nlag = 10, w = 0.1)
  
  
  temp<-d[[1]]
  for(j in 2:40){
    temp<-paste0(temp," ",d[[j]])
    
  }
  
  ls<-c(ls,temp)
  
  print(i)
}

write.csv(ls, "D:\\Research Work\\Disertation Project 2\\Raw Data\\featureExtraction\\projectDataNegative/qso.txt", row.names = FALSE)

#extractSOCN -Sequence-Order-Coupling Numbers


ncrna<-read.fasta(file = "D:\\Research Work\\Disertation Project 2\\Raw Data\\CDHIT\\negative\\negativeBac.faa",as.string = TRUE, seqtype = "AA")



#print(ncrna)
l<-length(ncrna)
min(getLength(ncrna))

col_n<-"socn_1"

for(k in 2:20){
  col_n<-paste0(col_n," ","socn_",k)
  
}

ls<-c(col_n)
ls

for(i in 1: l){
  
  x<-ncrna[[i]]
  #x<-ncrna[[12]]
  d<-extractSOCN(x, nlag = 10)
  
  
  temp<-d[[1]]
  for(j in 2:20){
    temp<-paste0(temp," ",d[[j]])
    
  }
  
  ls<-c(ls,temp)
  
  print(i)
}

write.csv(ls, "D:\\Research Work\\Disertation Project 2\\Raw Data\\featureExtraction\\projectDataNegative/socn.txt", row.names = FALSE)


#cojoint Triad


ncrna<-read.fasta(file = "D:\\Research Work\\Disertation Project 2\\Raw Data\\CDHIT\\negative\\negativeBac.faa",as.string = TRUE, seqtype = "AA")



#print(ncrna)
l<-length(ncrna)
min(getLength(ncrna))

col_n<-"ctriad_1"

for(k in 2:343){
  col_n<-paste0(col_n," ","ctriad_",k)
  
}

ls<-c(col_n)
ls

for(i in 1: l){
  
  x<-ncrna[[i]]
  #x<-ncrna[[12]]
  d<-extractCTriad(x)
  
  
  temp<-d[[1]]
  for(j in 2:343){
    temp<-paste0(temp," ",d[[j]])
    
  }
  
  ls<-c(ls,temp)
  
  print(i)
}

write.csv(ls, "D:\\Research Work\\Disertation Project 2\\Raw Data\\featureExtraction\\projectDataNegative/ctriad.txt", row.names = FALSE)

#Dipeptide Composition features


ncrna<-read.fasta(file = "D:\\Research Work\\Disertation Project 2\\Raw Data\\CDHIT\\negative\\negativeBac.faa",as.string = TRUE, seqtype = "AA")



#print(ncrna)
l<-length(ncrna)
min(getLength(ncrna))

col_n<-"dipep_1"

for(k in 2:400){
  col_n<-paste0(col_n," ","dipep_",k)
  
}

ls<-c(col_n)



for(i in 1: l){
  x<-ncrna[[i]]
  d<-extractDC(x)
  temp<-d[[1]]
  for(j in 2:400){
    temp<-paste0(temp," ",d[[j]])
    
  }
  
  ls<-c(ls,temp)
  print(i)
  
}


write.csv(ls, "D:\\Research Work\\Disertation Project 2\\Raw Data\\featureExtraction\\projectDataNegative/dipeptide.txt", row.names = FALSE)

#Pseudo-amino acid Composition features

ncrna<-read.fasta(file = "D:\\Research Work\\Disertation Project 2\\Raw Data\\CDHIT\\negative\\negativeBac.faa",as.string = TRUE, seqtype = "AA")



#print(ncrna)
l<-length(ncrna)
min(getLength(ncrna))


extractPAAC_revised <- function(
  x, props = c("Hydrophobicity", "Hydrophilicity", "SideChainMass"),
  lambda = 10, w = 0.05, customprops = NULL) {
  if (protcheck(x) == FALSE) {
    stop("x has unrecognized amino acid type")
  }
  
  if (nchar(x) <= lambda) {
    stop('Length of the protein sequence must be greater than "lambda"')
  }
  
  AAidx <- read.csv(system.file("sysdata/AAidx.csv", package = "protr"), header = TRUE)
  
  tmp <- data.frame(
    AccNo = c("Hydrophobicity", "Hydrophilicity", "SideChainMass"),
    A = c(0.62, -0.5, 15), R = c(-2.53, 3, 101),
    N = c(-0.78, 0.2, 58), D = c(-0.9, 3, 59),
    C = c(0.29, -1, 47), E = c(-0.74, 3, 73),
    Q = c(-0.85, 0.2, 72), G = c(0.48, 0, 1),
    H = c(-0.4, -0.5, 82), I = c(1.38, -1.8, 57),
    L = c(1.06, -1.8, 57), K = c(-1.5, 3, 73),
    M = c(0.64, -1.3, 75), F = c(1.19, -2.5, 91),
    P = c(0.12, 0, 42), S = c(-0.18, 0.3, 31),
    T = c(-0.05, -0.4, 45), W = c(0.81, -3.4, 130),
    Y = c(0.26, -2.3, 107), V = c(1.08, -1.5, 43)
  )
  
  AAidx <- rbind(AAidx, tmp)
  
  if (!is.null(customprops)) AAidx <- rbind(AAidx, customprops)
  
  aaidx <- AAidx[, -1]
  row.names(aaidx) <- AAidx[, 1]
  
  n <- length(props)
  
  # Standardize H0 to H
  
  H0 <- as.matrix(aaidx[props, ])
  
  H <- matrix(ncol = 20, nrow = n)
  for (i in 1:n) {
    H[i, ] <-
      (H0[i, ] - mean(H0[i, ])) / (sqrt(sum((H0[i, ] - mean(H0[i, ]))^2) / 20))
  }
  AADict <- c(
    "A", "R", "N", "D", "C", "E", "Q", "G", "H", "I",
    "L", "K", "M", "F", "P", "S", "T", "W", "Y", "V"
  )
  dimnames(H) <- list(props, AADict)
  
  # Compute (big) Theta
  
  Theta <- vector("list", lambda)
  
  xSplitted <- strsplit(x, split = "")[[1]]
  
  N <- length(xSplitted)
  
  for (i in 1:lambda) {
    for (j in 1:(N - i)) {
      Theta[[i]][j] <- mean((H[, xSplitted[j]] - H[, xSplitted[j + i]])^2)
    }
  }
  
  # Compute (small) theta
  
  theta <- sapply(Theta, mean)
  
  # Compute first 20 features
  
  fc <- summary(factor(xSplitted, levels = AADict), maxsum = 21)
  Xc1 <- fc / (1 + (w * sum(theta)))
  names(Xc1) <- paste("Xc1.", names(Xc1), sep = "")
  
  # Compute last lambda features
  
  Xc2 <- (w * theta) / (1 + (w * sum(theta)))
  names(Xc2) <- paste("Xc2.lambda.", 1:lambda, sep = "")
  
  # Combine (20 + lambda) features
  
  Xc <- c(Xc1, Xc2)
  
  Xc
}



col_n<-"pseudo_1"

for(k in 2:30){
  col_n<-paste0(col_n," ","pseudo_",k)
  
}

ls<-c(col_n)

for(i in 1: l){
  x<-toString(ncrna[i])
  
  d<-extractPAAC_revised(x)
  length(d)
  temp<-d[[1]]
  for(j in 2:30){
    temp<-paste0(temp," ",d[[j]])
    
  }
  
  ls<-c(ls,temp)
  print(i)
  
}


write.csv(ls, "D:\\Research Work\\Disertation Project 2\\Raw Data\\featureExtraction\\projectDataNegative/pseudo.txt", row.names = FALSE)



#Amphiphilic Pseudo-amino acid Composition features


ncrna<-read.fasta(file = "D:\\Research Work\\Disertation Project 2\\Raw Data\\CDHIT\\negative\\negativeBac.faa",as.string = TRUE, seqtype = "AA")



#print(ncrna)
l<-length(ncrna)
min(getLength(ncrna))

extractAPAAC_revised <- function(
  x, props = c("Hydrophobicity", "Hydrophilicity"),
  lambda = 10, w = 0.05, customprops = NULL) {
  if (protcheck(x) == FALSE) {
    stop("x has unrecognized amino acid type")
  }
  
  if (nchar(x) <= lambda) {
    stop('Length of the protein sequence must be greater than "lambda"')
  }
  
  AAidx <- read.csv(system.file(
    "sysdata/AAidx.csv",
    package = "protr"
  ), header = TRUE)
  
  tmp <- data.frame(
    AccNo = c("Hydrophobicity", "Hydrophilicity", "SideChainMass"),
    A = c(0.62, -0.5, 15), R = c(-2.53, 3, 101),
    N = c(-0.78, 0.2, 58), D = c(-0.9, 3, 59),
    C = c(0.29, -1, 47), E = c(-0.74, 3, 73),
    Q = c(-0.85, 0.2, 72), G = c(0.48, 0, 1),
    H = c(-0.4, -0.5, 82), I = c(1.38, -1.8, 57),
    L = c(1.06, -1.8, 57), K = c(-1.5, 3, 73),
    M = c(0.64, -1.3, 75), F = c(1.19, -2.5, 91),
    P = c(0.12, 0, 42), S = c(-0.18, 0.3, 31),
    T = c(-0.05, -0.4, 45), W = c(0.81, -3.4, 130),
    Y = c(0.26, -2.3, 107), V = c(1.08, -1.5, 43)
  )
  
  AAidx <- rbind(AAidx, tmp)
  
  if (!is.null(customprops)) AAidx <- rbind(AAidx, customprops)
  
  aaidx <- AAidx[, -1]
  row.names(aaidx) <- AAidx[, 1]
  
  n <- length(props)
  
  # Standardize H0 to H
  
  H0 <- as.matrix(aaidx[props, ])
  
  H <- matrix(ncol = 20, nrow = n)
  for (i in 1:n) {
    H[i, ] <-
      (H0[i, ] - mean(H0[i, ])) / (sqrt(sum((H0[i, ] - mean(H0[i, ]))^2) / 20))
  }
  AADict <- c(
    "A", "R", "N", "D", "C", "E", "Q", "G", "H", "I",
    "L", "K", "M", "F", "P", "S", "T", "W", "Y", "V"
  )
  dimnames(H) <- list(props, AADict)
  
  # Compute H^{1,2, ..., n}_{i, j}
  
  Theta <- vector("list", lambda)
  for (i in 1:lambda) Theta[[i]] <- vector("list", n)
  
  xSplitted <- strsplit(x, split = "")[[1]]
  
  N <- length(xSplitted)
  
  for (i in 1:lambda) {
    for (j in 1:n) {
      for (k in 1:(N - i)) {
        Theta[[i]][[j]][k] <-
          H[props[j], xSplitted[k]] * H[props[j], xSplitted[k + i]]
      }
    }
  }
  
  # Compute tau
  
  tau <- sapply(unlist(Theta, recursive = FALSE), mean)
  
  # Compute first 20 features
  
  fc <- summary(factor(xSplitted, levels = AADict), maxsum = 21)
  Pc1 <- fc / (1 + (w * sum(tau)))
  names(Pc1) <- paste("Pc1.", names(Pc1), sep = "")
  
  # Compute last n * lambda features
  
  Pc2 <- (w * tau) / (1 + (w * sum(tau)))
  names(Pc2) <- paste(
    "Pc2", as.vector(outer(props, 1:lambda, paste, sep = ".")),
    sep = "."
  )
  
  # Combine 20 + (n * lambda) features
  
  Pc <- c(Pc1, Pc2)
  
  Pc
}

col_n<-"amphipseudo_1"

for(k in 2:40){
  col_n<-paste0(col_n," ","amphipseudo_",k)
  
}

ls<-c(col_n)

for(i in 1: l){
  x<-toString(ncrna[i])
  
  d<-extractAPAAC_revised(x)
  d
  length(d)
  temp<-d[[1]]
  for(j in 2:40){
    temp<-paste0(temp," ",d[[j]])
    
  }
  
  ls<-c(ls,temp)
  print(i)
  
}



write.csv(ls, "D:\\Research Work\\Disertation Project 2\\Raw Data\\featureExtraction\\projectDataNegative/amphipseudo.txt", row.names = FALSE)


#Composition features


ncrna<-read.fasta(file = "D:\\Research Work\\Disertation Project 2\\Raw Data\\CDHIT\\negative\\negativeBac.faa",as.string = TRUE, seqtype = "AA")



#print(ncrna)
l<-length(ncrna)
min(getLength(ncrna))

extractCTDC_revised <- function(x) {
  
  AADict <- c(
    "A", "C", "D", "E", "F", "G", "H", "I",
    "L", "K", "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y" 
  )
  
  if (all(strsplit(x, split = "")[[1]] %in% AADict) == FALSE) {
    stop("x has unrecognized amino acid type")
  }
  
  group1 <- list(
    "hydrophobicity" = c("R", "K", "E", "D", "Q", "N"),
    "normwaalsvolume" = c("G", "A", "S", "T", "P", "D", "C"),
    "polarity" = c("L", "I", "F", "W", "C", "M", "V", "Y"),
    "polarizability" = c("G", "A", "S", "D", "T"),
    "charge" = c("K", "R"),
    "secondarystruct" = c("E", "A", "L", "M", "Q", "K", "R", "H"),
    "solventaccess" = c("A", "L", "F", "C", "G", "I", "V", "W"),
    "surfacetension" = c("G", "Q", "D", "N", "A", "H", "R")
  )
  
  group2 <- list(
    "hydrophobicity" = c("G", "A", "S", "T", "P", "H", "Y"),
    "normwaalsvolume" = c("N", "V", "E", "Q", "I", "L"),
    "polarity" = c("P", "A", "T", "G", "S"),
    "polarizability" = c("C", "P", "N", "V", "E", "Q", "I", "L"),
    "charge" = c(
      "A", "N", "C", "Q", "G", "H", "I", "L",
      "M", "F", "P", "S", "T", "W", "Y", "V"
    ),
    "secondarystruct" = c("V", "I", "Y", "C", "W", "F", "T"),
    "solventaccess" = c("R", "K", "Q", "E", "N", "D"),
    "surfacetension" = c("K", "T", "S", "E", "C")
  )
  
  group3 <- list(
    "hydrophobicity" = c("C", "L", "V", "I", "M", "F", "W"),
    "normwaalsvolume" = c("M", "H", "K", "F", "R", "Y", "W"),
    "polarity" = c("H", "Q", "R", "K", "N", "E", "D"),
    "polarizability" = c("K", "M", "H", "F", "R", "Y", "W"),
    "charge" = c("D", "E"),
    "secondarystruct" = c("G", "N", "P", "S", "D"),
    "solventaccess" = c("M", "S", "P", "T", "H", "Y"),
    "surfacetension" = c("I", "L", "M", "F", "P", "W", "Y", "V")
  )
  
  
  xSplitted <- strsplit(x, split = "")[[1]]
  n <- nchar(x)
  
  # Get groups for each property & each amino acid
  
  g1 <- lapply(group1, function(g) length(which(xSplitted %in% g)))
  names(g1) <- paste(names(g1), "Group1", sep = ".")
  g2 <- lapply(group2, function(g) length(which(xSplitted %in% g)))
  names(g2) <- paste(names(g2), "Group2", sep = ".")
  g3 <- lapply(group3, function(g) length(which(xSplitted %in% g)))
  names(g3) <- paste(names(g3), "Group3", sep = ".")
  CTDC <- unlist(c(g1, g2, g3)) *100 / n
  # This reorders the groups from
  # p1.g1, p2.g1, p3.g1 ...
  # to
  # p1.g1, p1.g2, p1.g3 ...
  # This reordering is not really important
  # just to make the results look pretty
  ids <- unlist(lapply(1:8, function(x) x + c(0, 8, 16)))
  
  #  CTDC[ids]
  
  val<-c()
  for(i in 1:24){
    val<-c(val, CTDC[[i]])
    
  }
  # return (val)
  
  return (CTDC[ids])
}

#extractCTDC1(x)




col_n<-"comp_1"

for(k in 2:21){
  col_n<-paste0(col_n," ","comp_",k)
  
}

ls<-c(col_n)

for(i in 1: l){
  x<-toString(ncrna[i])
  d<-extractCTDC_revised(x)
  temp<-d[[1]]
  for(j in 2:21){
    temp<-paste0(temp," ",d[[j]])
    
  }
  
  ls<-c(ls,temp)
  
  print(i)
}



write.csv(ls, "D:\\Research Work\\Disertation Project 2\\Raw Data\\featureExtraction\\projectDataNegative/composition.txt", row.names = FALSE)


#Transition features


ncrna<-read.fasta(file = "D:\\Research Work\\Disertation Project 2\\Raw Data\\CDHIT\\negative\\negativeBac.faa",as.string = TRUE, seqtype = "AA")



#print(ncrna)
l<-length(ncrna)
min(getLength(ncrna))

extractCTDT_revised <- function(x) {
  
  AADict <- c(
    "A", "C", "D", "E", "F", "G", "H", "I",
    "L", "K", "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y" 
  )
  
  if (all(strsplit(x, split = "")[[1]] %in% AADict) == FALSE) {
    stop("x has unrecognized amino acid type")
  }
  
  group1 <- list(
    "hydrophobicity" = c("R", "K", "E", "D", "Q", "N"),
    "normwaalsvolume" = c("G", "A", "S", "T", "P", "D", "C"),
    "polarity" = c("L", "I", "F", "W", "C", "M", "V", "Y"),
    "polarizability" = c("G", "A", "S", "D", "T"),
    "charge" = c("K", "R"),
    "secondarystruct" = c("E", "A", "L", "M", "Q", "K", "R", "H"),
    "solventaccess" = c("A", "L", "F", "C", "G", "I", "V", "W"),
    "surfacetension" = c("G", "Q", "D", "N", "A", "H", "R")
  )
  
  group2 <- list(
    "hydrophobicity" = c("G", "A", "S", "T", "P", "H", "Y"),
    "normwaalsvolume" = c("N", "V", "E", "Q", "I", "L"),
    "polarity" = c("P", "A", "T", "G", "S"),
    "polarizability" = c("C", "P", "N", "V", "E", "Q", "I", "L"),
    "charge" = c(
      "A", "N", "C", "Q", "G", "H", "I", "L",
      "M", "F", "P", "S", "T", "W", "Y", "V"
    ),
    "secondarystruct" = c("V", "I", "Y", "C", "W", "F", "T"),
    "solventaccess" = c("R", "K", "Q", "E", "N", "D"),
    "surfacetension" = c("K", "T", "S", "E", "C")
  )
  
  group3 <- list(
    "hydrophobicity" = c("C", "L", "V", "I", "M", "F", "W"),
    "normwaalsvolume" = c("M", "H", "K", "F", "R", "Y", "W"),
    "polarity" = c("H", "Q", "R", "K", "N", "E", "D"),
    "polarizability" = c("K", "M", "H", "F", "R", "Y", "W"),
    "charge" = c("D", "E"),
    "secondarystruct" = c("G", "N", "P", "S", "D"),
    "solventaccess" = c("M", "S", "P", "T", "H", "Y"),
    "surfacetension" = c("I", "L", "M", "F", "P", "W", "Y", "V")
  )
  
  
  xSplitted <- strsplit(x, split = "")[[1]]
  n <- nchar(x)
  
  G <- vector("list", 8)
  for (i in 1:7) G[[i]] <- rep(NA, n)
  
  # Get groups for each property & each amino acid
  
  for (i in 1:8) {
    try(G[[i]][which(xSplitted %in% group1[[i]])] <- "G1")
    try(G[[i]][which(xSplitted %in% group2[[i]])] <- "G2")
    try(G[[i]][which(xSplitted %in% group3[[i]])] <- "G3")
  }
  
  # Combine single amino acids by a 2-length step
  
  for (i in 1:8) G[[i]] <- paste(G[[i]][-n], G[[i]][-1], sep = "")
  G <- lapply(G, function(x)
    factor(x, levels = c(
      "G1G2", "G2G1", "G1G3",
      "G3G1", "G2G3", "G3G2",
      "G1G1", "G2G2", "G3G3"
    )))
  
  GSummary <- lapply(G, summary)
  
  # Compute (n_rs + n_sr) / (N - 1)
  
  CTDT <- vector("list", 8)
  
  for (i in 1:8) {
    CTDT[[i]][1] <- sum(GSummary[[i]][c("G1G2", "G2G1")]) / (n - 1)
    #CTDT[[i]][1]<-CTDT[[i]][1]*100
    CTDT[[i]][2] <- sum(GSummary[[i]][c("G1G3", "G3G1")]) / (n - 1)
    #CTDT[[i]][2]<-CTDT[[i]][2]*100
    CTDT[[i]][3] <- sum(GSummary[[i]][c("G2G3", "G3G2")]) / (n - 1)
    #CTDT[[i]][3]<-CTDT[[i]][3]*100
  }
  
  CTDT <- unlist(CTDT)
  
  names(CTDT) <- paste(
    "prop", rep(1:8, each = 3), ".",
    rep(c("Tr1221", "Tr1331", "Tr2332"), times = 8),
    sep = ""
  )
  
  return(CTDT*100)
}

#extractCTDT1(x)




col_n<-"tran_1"

for(k in 2:21){
  col_n<-paste0(col_n," ","tran_",k)
  
}

ls<-c(col_n)

for(i in 1: l){
  x<-toString(ncrna[i])
  d<-extractCTDT(x)
  temp<-d[[1]]
  for(j in 2:21){
    temp<-paste0(temp," ",d[[j]])
    
  }
  
  ls<-c(ls,temp)
  
  print(i)
}

write.csv(ls, "D:\\Research Work\\Disertation Project 2\\Raw Data\\featureExtraction\\projectDataNegative/transition.txt", row.names = FALSE)



#Distribution features



ncrna<-read.fasta(file = "D:\\Research Work\\Disertation Project 2\\Raw Data\\CDHIT\\negative\\negativeBac.faa",as.string = TRUE, seqtype = "AA")



#print(ncrna)
l<-length(ncrna)
min(getLength(ncrna))

extractCTDD_revised <- function(x) {
  
  AADict <- c(
    "A", "C", "D", "E", "F", "G", "H", "I",
    "L", "K", "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y" 
  )
  
  if (all(strsplit(x, split = "")[[1]] %in% AADict) == FALSE) {
    stop("x has unrecognized amino acid type")
  }
  
  group1 <- list(
    "hydrophobicity" = c("R", "K", "E", "D", "Q", "N"),
    "normwaalsvolume" = c("G", "A", "S", "T", "P", "D", "C"),
    "polarity" = c("L", "I", "F", "W", "C", "M", "V", "Y"),
    "polarizability" = c("G", "A", "S", "D", "T"),
    "charge" = c("K", "R"),
    "secondarystruct" = c("E", "A", "L", "M", "Q", "K", "R", "H"),
    "solventaccess" = c("A", "L", "F", "C", "G", "I", "V", "W"),
    "surfacetension" = c("G", "Q", "D", "N", "A", "H", "R")
  )
  
  group2 <- list(
    "hydrophobicity" = c("G", "A", "S", "T", "P", "H", "Y"),
    "normwaalsvolume" = c("N", "V", "E", "Q", "I", "L"),
    "polarity" = c("P", "A", "T", "G", "S"),
    "polarizability" = c("C", "P", "N", "V", "E", "Q", "I", "L"),
    "charge" = c(
      "A", "N", "C", "Q", "G", "H", "I", "L",
      "M", "F", "P", "S", "T", "W", "Y", "V"
    ),
    "secondarystruct" = c("V", "I", "Y", "C", "W", "F", "T"),
    "solventaccess" = c("R", "K", "Q", "E", "N", "D"),
    "surfacetension" = c("K", "T", "S", "E", "C")
  )
  
  group3 <- list(
    "hydrophobicity" = c("C", "L", "V", "I", "M", "F", "W"),
    "normwaalsvolume" = c("M", "H", "K", "F", "R", "Y", "W"),
    "polarity" = c("H", "Q", "R", "K", "N", "E", "D"),
    "polarizability" = c("K", "M", "H", "F", "R", "Y", "W"),
    "charge" = c("D", "E"),
    "secondarystruct" = c("G", "N", "P", "S", "D"),
    "solventaccess" = c("M", "S", "P", "T", "H", "Y"),
    "surfacetension" = c("I", "L", "M", "F", "P", "W", "Y", "V")
  )
  
  
  xSplitted <- strsplit(x, split = "")[[1]]
  n <- nchar(x)
  
  G <- vector("list", 8)
  for (i in 1:8) G[[i]] <- rep(NA, n)
  
  # Get groups for each property & each amino acid
  
  for (i in 1:8) {
    try(G[[i]][which(xSplitted %in% group1[[i]])] <- "G1")
    try(G[[i]][which(xSplitted %in% group2[[i]])] <- "G2")
    try(G[[i]][which(xSplitted %in% group3[[i]])] <- "G3")
  }
  
  # Compute Distribution
  
  D <- vector("list", 8)
  for (i in 1:8) D[[i]] <- matrix(ncol = 5, nrow = 3)
  
  for (i in 1:8) {
    for (j in 1:3) {
      inds <- which(G[[i]] == paste0("G", j))
      quartiles <- floor(length(inds) * c(0.25, 0.5, 0.75))
      
      quartiles[which(quartiles <= 0)] <- 1
      
      D[[i]][j, ] <- if (length(inds) > 0) {
        (inds[c(1, quartiles, length(inds))]) * 100 / n
      } else {
        0
      }
    }
  }
  
  D <- do.call(rbind, D)
  D <- as.vector(t(D))
  
  names(D) <- paste(
    rep(paste("prop", 1:8, sep = ""), each = 15),
    rep(rep(c(".G1", ".G2", ".G3"), each = 5), times = 8),
    rep(paste(".residue", c("0", "25", "50", "75", "100"), sep = ""), times = 24),
    sep = ""
  )
  
  flag<-matrix(0,120,1)
  lc<-c()
  
  i<-1
  while(i<=15){
    #print(i)
    
    if(flag[i,1]==0){
      z<-i
      count<-1
      while(count<=3){
        # print(z)
        lc<-c(lc,D[[z]])
        flag[z,1]<-1
        z<-z+5
        count<-count+1
      }
    }
    i<-i+1
    #print(i)
  }
  
  # lc 
  
  i<-16
  while(i<=30){
    #print(i)
    
    if(flag[i,1]==0){
      z<-i
      count<-1
      while(count<=3){
        # print(z)
        lc<-c(lc,D[[z]])
        flag[z,1]<-1
        z<-z+5
        count<-count+1
      }
    }
    i<-i+1
    #print(i)
  }
  
  # lc
  
  i<-31
  while(i<=45){
    #print(i)
    
    if(flag[i,1]==0){
      z<-i
      count<-1
      while(count<=3){
        # print(z)
        lc<-c(lc,D[[z]])
        flag[z,1]<-1
        z<-z+5
        count<-count+1
      }
    }
    i<-i+1
    #print(i)
  }
  
  # lc
  
  i<-46
  while(i<=60){
    #print(i)
    
    if(flag[i,1]==0){
      z<-i
      count<-1
      while(count<=3){
        # print(z)
        lc<-c(lc,D[[z]])
        flag[z,1]<-1
        z<-z+5
        count<-count+1
      }
    }
    i<-i+1
    #print(i)
  }
  
  #lc
  
  
  i<-61
  while(i<=75){
    #print(i)
    
    if(flag[i,1]==0){
      z<-i
      count<-1
      while(count<=3){
        # print(z)
        lc<-c(lc,D[[z]])
        flag[z,1]<-1
        z<-z+5
        count<-count+1
      }
    }
    i<-i+1
    #print(i)
  }
  
  #lc
  
  
  i<-76
  while(i<=90){
    #print(i)
    
    if(flag[i,1]==0){
      z<-i
      count<-1
      while(count<=3){
        # print(z)
        lc<-c(lc,D[[z]])
        flag[z,1]<-1
        z<-z+5
        count<-count+1
      }
    }
    i<-i+1
    #print(i)
  }
  
  #lc
  
  
  
  i<-91
  while(i<=105){
    #print(i)
    
    if(flag[i,1]==0){
      z<-i
      count<-1
      while(count<=3){
        # print(z)
        lc<-c(lc,D[[z]])
        flag[z,1]<-1
        z<-z+5
        count<-count+1
      }
    }
    i<-i+1
    #print(i)
  }
  
  #lc
  
  
  
  
  i<-106
  while(i<=120){
    #print(i)
    
    if(flag[i,1]==0){
      z<-i
      count<-1
      while(count<=3){
        # print(z)
        lc<-c(lc,D[[z]])
        flag[z,1]<-1
        z<-z+5
        count<-count+1
      }
    }
    i<-i+1
    #print(i)
  }
  
  #lc
  
  return (lc)
}






col_n<-"dist_1"

for(k in 2:105){
  col_n<-paste0(col_n," ","dist_",k)
  
}

ls<-c(col_n)

for(i in 1: l){
  x<-toString(ncrna[[i]])
  d<-extractCTDD_revised(x)
  temp<-d[[1]]
  for(j in 2:105){
    temp<-paste0(temp," ",d[[j]])
    
  }
  
  ls<-c(ls,temp)
  
  print(i)
}




write.csv(ls, "D:\\Research Work\\Disertation Project 2\\Raw Data\\featureExtraction\\projectDataNegative/distribution.txt", row.names = FALSE)




#Secondary structure features


ncrna<-read.fasta(file = "D:\\Research Work\\Disertation Project 2\\Raw Data\\CDHIT\\negative\\negativeBac.faa",as.string = TRUE, seqtype = "AA")



#print(ncrna)
l<-length(ncrna)
min(getLength(ncrna))

#BiocManager::install(c("DECIPHER", "BiocGenerics","parallel"))

library(RSQLite)
library(DECIPHER)


file = "D:\\Research Work\\Disertation Project 2\\Raw Data\\CDHIT\\negative\\negativeBac.faa"


aa = readAAStringSet(file)
length(aa)
#aa[[1]]
#dna <- readDNAStringSet(fas)
#aa <- translate(dna)
#aa
hec <- PredictHEC(aa)
ln<-length (hec)
#t<-hec[1]
#bb<-toString(t)
#bb[1]
# ss<-strsplit(t, "")[[1]]
# len<-length(ss)

# hec[1459]


l<-length(ncrna)

col_n<-"ss_1"

for(k in 2:6){
  col_n<-paste0(col_n," ","ss_",k)
  
}

ls<-c(col_n)

for(i in 1:l){
  t<-hec[i]
  data_psi<-strsplit(t, "")[[1]]
  len<-length(data_psi)
  add_ch<-""
  add_ch_exclude<-""
  flag<-""
  freq<-0
  x<-0
  y<-0
  z<-0
  SH<-0
  SE<-0
  SC<-0
  
  
  
  for(v in 1:len){
    if(toString(data_psi[v])=="H"){
      SH<-SH+v
      SH
      
    }
    if(toString(data_psi[v])=="E"){
      SE<-SE+v
      SE
      
    }
    if(toString(data_psi[v])=="C"){
      SC<-SC+v
      SC
      
    }
    add_ch<-paste0(add_ch,toString(data_psi[v]))
  }
  
  rr <- rle(strsplit(add_ch,"")[[1]])
  rr
  #SH<-sum(rr$lengths[which(rr$values == "H")])
  MH<-max(rr$lengths[which(rr$values == "H")])
  if (MH==-Inf || MH==Inf){
    MH<-0
  }
  
  CMVH<-SH/(len*(len-1))
  NMH<-MH/len
  
  #SE<-sum(rr$lengths[which(rr$values == "E")])
  ME<-max(rr$lengths[which(rr$values == "E")])
  ME
  if (ME==-Inf || ME==Inf){
    ME<-0
  }
  
  CMVE<-SE/(len*(len-1))
  NME<-ME/len
  
  #SC<-sum(rr$lengths[which(rr$values == "C")])
  MC<-max(rr$lengths[which(rr$values == "C")])
  
  CMVC<-SC/(len*(len-1))
  #NMC<-MC/len
  rr_len<-length(rr$values)
  rr_len
  for(v in 1:rr_len){
    if(rr$values[v]!="C")  {
      if(rr$values[v]!= flag){
        add_ch_exclude<-paste0(add_ch_exclude, toString(rr$values[v]))
        flag<-toString(rr$values[v])
        freq<-freq+1
      }
    }
  }
  add_ch_exclude
  freq
  
  #count_EHE<-str_count(add_ch_exclude, fixed("EHE"))
  count_EHE<-gregexpr("(?=EHE)",add_ch_exclude,perl=TRUE)
  count_EHE1<-count_EHE[[1]]
  count_EHE1
  if(count_EHE1[1]<0){
    count_EHE_len<-0
  }
  else{
    count_EHE_len<-length(count_EHE1)
  }
  
  count_EHE_len
  if(freq>2){
    f_EHE<-count_EHE_len/(freq-2)
  }
  else{
    f_EHE<-0
  }
  
  f_EHE
  add_line<-paste0(toString(CMVH), " ", toString(CMVE), " ", toString(CMVC), " ", toString(NMH), " ", toString(NME), " ", toString(f_EHE))
  add_line
  
  ls<-c(ls,add_line)
  print(i) 
  
}



write.csv(ls, "D:\\Research Work\\Disertation Project 2\\Raw Data\\featureExtraction\\projectDataNegative/secondary_struc.txt", row.names = FALSE)








