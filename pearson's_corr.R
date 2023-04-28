

## =============read from the original feature set=============================##

#file <-read.csv("D:\\Bacteriocin\\data\\featureExtraction\\validation_merged_file.csv", header = TRUE, sep = ",")
file <-read.csv("D:\\Research Work\\Disertation Project 1\\my paper\\BMC bioinformatics\\Final\\Revision_by_bmc_peer_review\\Raw Data\\featureExtraction\\dataSplit\\validate_train_data.csv", header = TRUE)
#file<-file %>% select(where(~ n_distinct(.) > 1))
#file$Output[file$Output== -1]<-0
#file<-as.numeric(file$Output)
#normalize <- function(x) {
# return ((x - min(x)) / (max(x) - min(x)))
#}
#file <- as.data.frame(lapply(file, normalize))
col <- ncol(file) 
# Put a flag with size of the input (col: number of columns)
col
flag = c(1:col)
flag[1:col]=0
#Rlist =list()
Rlist = c()
#index=1
col<-col-1
col


#============== Create a list of columns that we want to remove ==================
for(i in 1:col)
{
  for(j in 1:col)
  {
    
    if(flag[i]==0)
    {
      col2= file[[i]]
      col1 = file[[j]]
      i
      j
      col2
      col1
      corre<-abs (cor(col1,col2, method = "pearson"))
      
      res <- c(corre, i, j)
      if(corre >= 0.90 && i!=j)
      {
        flag[j]=1
        Rlist<-c(Rlist,j)
        #Rlist[index]=j
        #index=index+1
      }
      
    }
  }
}


##==================Remove the duplicates in our list====================================

Rlist <- unique(Rlist)
Rlist
Rlist<-sort(Rlist)
Rlist



##==========Remove the list from original file ============================



corrFST= file
corrFST= corrFST[,-Rlist]

length(corrFST)
#row = length(Rlist)
#corrFST= file
#count=0
#x=0
#for(i in 1:row)
#{
#  x=as.numeric(Rlist[i])-count
#  corrFST= corrFST[-x]
#  count=count+1
#}




#write.csv(corrFST, file = "D:\\Bacteriocin\\data\\featureExtraction\\CorrFeatureSet.csv", row.names = FALSE)
write.csv(corrFST, file = "D:\\Research Work\\Disertation Project 1\\my paper\\BMC bioinformatics\\Final\\Revision_by_bmc_peer_review\\Raw Data\\featureExtraction\\TCorrFeatureSet.csv", row.names = FALSE)


file_read <-read.csv("D:\\Research Work\\Disertation Project 1\\my paper\\BMC bioinformatics\\Final\\Revision_by_bmc_peer_review\\Raw Data\\featureExtraction\\TCorrFeatureSet.csv", header = TRUE, sep = ",")
length(file_read)
file_read$Output
