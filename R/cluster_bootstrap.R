# clustered bootstrap function
clusbootreg <- function(formula, data, cluster, reps, extract_from, extract_to){
  reg1<- lm(formula, data=data) #runs the reg
  clusters <- names(table(cluster)) #gives each cluster a name of the cluster itself.There are 1000 clusters in this example
  sterrs <- matrix(NA, nrow=reps, ncol= (extract_to - extract_from + 1)) #matrix to store coefficients after each rep, 1000 rows in this example
  for(i in 1:reps){
    set.seed(i+3)
    index <- sample(1:length(clusters), length(clusters), replace=TRUE) #sampling with replacement from clusters. no. of clusters here  = 1000
    aa<-clusters[index]  #aa is of "character" format
    bb<-table(aa) #will provide a count of each cluster
    bootdat <- NULL
    for(j in 1:max(bb)){
      cc<-data[cluster %in% names(bb[bb %in% j]),]   #The %in% operator in R is super useful for character vectors. First looks whether clusters
      #exist in the sample extracted
      for(k in 1:j){
        bootdat <- rbind(bootdat, cc)
      }
    }
    boot1<-lm(formula,data=subset(bootdat))
    sterrs[i,] <- c(coef(boot1)[extract_from:extract_to])
  }
  val <- cbind(coef(reg1)[extract_from:extract_to],apply(sterrs,2,sd))
  colnames(val) <- c("Estimate","Std. Error")
  return(val)
}
