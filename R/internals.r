.count.lc.narm = function(x){
  return(c(sum(x=="crop", na.rm=T), sum(x=="grass", na.rm=T), sum(x=="shrub", na.rm=T), sum(x=="pine", na.rm=T), 
           sum(x=="oak", na.rm=T), sum(x=="sparseveg", na.rm=T), sum(x=="water", na.rm=T), 
           sum(x=="urban", na.rm=T), sum(x=="shrub.to.pine", na.rm=T), sum(x=="shrub.to.oak", na.rm=T)))
}

.sample.lc = function(x){
  if(length(x)!=7){
    return(NA)
  } else{
    return(sample(c("crop", "grass", "shrub", "pine", "oak", "sparseveg", "water", "urban"), size=1, p=x))  
  }
}

.neigh.transition = function(land, neigh.id, dist.th = 0, numeric = F){
  
  ## Do not count for the cell itself, so do not use first column of nn.idx data.frame
  neigh.target = matrix(land$is.target[neigh.id$nn.idx[,-1]], 
                        nrow=nrow(neigh.id$nn.idx), ncol=ncol(neigh.id$nn.idx)-1, byrow=F) 
  
  ## Restrict to those neighbors that are within the radius
  neigh.target = neigh.target  * (neigh.id$nn.dists[,-1] <=  dist.th)
  
  ## Compute the proportion of target neighbors within the corresponding neighborhood
  ptrans = apply(neigh.target, 1, sum, na.rm=T) 
  nneigh = apply(neigh.id$nn.dists[,-1] <= dist.th, 1, sum, na.rm=T) 
  ptrans = (ptrans/nneigh)^(1/5)
  
  ## Either it returns the probability of transition of each neighbor (numeric = TRUE)
  ## or a TRUE / FALSE vector resulting from comparing ptrans with random numbers (numeric = FALSE)
  if(numeric){
    return(ptrans)
  } else{
    ptrans = runif(length(ptrans), 0.0001, 1) <= ptrans  
    return(ptrans)
  }
}
