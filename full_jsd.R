full_jsd <- function(dataFrame, targets, iterations=1000){
  dataFrame <- type_convert(dataFrame)
  dataFrame <- as.data.frame(dataFrame)
  ldf <- vector("list", length(targets))
  for (i in seq_along(targets)){
    #i <- 1
    if (any(grepl(targets[i],colnames(dataFrame)))){
      dataFrame_cell <- t(dataFrame[,grepl(targets[i],colnames(dataFrame))])
      probs <- dataFrame_cell/apply(dataFrame_cell, 1, sum)
      probs[is.na(apply(probs, 1, sum)),] <- rep(1/ncol(probs), ncol(probs))
      ldf[[i]] <- probs
    } else {
      print(paste0(targets[i], " not in dataframe columns!"))
    }
  ldf[sapply(ldf, is.null)] <- NULL  
  }
  set.seed(123)
  bootstrap <- function(x) {
    #x <- ldf[[1]]
    h <- function(x) {
      y <- x[x > 0]; -sum(y * log(y))
    }
    
    jsd <- function(p, q) {
      p <- p[[1]][p[[2]],]
      h(q %*% p) - q %*% apply(p, 1, h)
    }
    
    boot.jsd <- function(x) {
      boot.jsd <- jsd(as.matrix(x), rep(1/nrow(x), nrow(x)))
    }
    
    bs <- rsample::bootstraps(x, iterations)
    bs_jsd <- map_dbl(bs$splits, boot.jsd)
    #bs_jsd <- bs %>% do(boot.jsd(as.matrix(.)))
    
  }
  results_df <- data.frame(rep(0,iterations))
  colNam <- vector()
  for (i in seq_along(ldf)){
    colNam <- c(colNam,unique(str_split(row.names(ldf[[i]]), "\\.", simplify = T)[1]))
    results_df[,i] <- bootstrap(ldf[[i]])
  }
  colnames(results_df) <- colNam
  results_df
}