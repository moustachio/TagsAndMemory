get_data <- function(filename,before_cols,after_cols,all_listen_cols){
  d <- fread(filename)
  
  ##lets get rid of rows where there are no listens before or after the tagged month
  d <- d[rowSums(d[,before_cols,with=F]) > 0 & rowSums(d[,after_cols,with=F]) > 0, ]
  
  ##give an id column
  d$id <- 1:nrow(d)
  
  ##add some summation columns
  d$before_sum <- rowSums(d[,before_cols,with=F])
  d$after_sum <-  rowSums(d[,after_cols,with=F])
  d$sum <- rowSums(d[,all_listen_cols,with=F])
  
  ##is there a tag?
  d$tagged <- d$V3 != -1
  return(d)
}

get_fair_samples <- function(bins,data){
  fair_sample = list()
  for(i in 2:length(bins) ){
    tagged_sample <- data[tagged==T & sum > bins[i-1] & sum <= bins[i],]
    untagged_sample <- data[tagged==F & sum > bins[i-1] & sum <= bins[i],]
    if(nrow(untagged_sample) > nrow(tagged_sample)){
      ##if untagged larger
      untagged_sample <- sample_n(untagged_sample, nrow(tagged_sample))
    } else {
      tagged_sample <- sample_n(tagged_sample,nrow(untagged_sample))
    }
    fair_sample <- c(fair_sample, list(untagged_sample), list(tagged_sample))
  }
  fair_sample_dt <- rbindlist(fair_sample)
}