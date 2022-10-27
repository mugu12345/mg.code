vactor2QN=function(x,n){
  qs=quantile(x,seq(0,1,1/n))
  x1=rep(NA,length(x))
  for(i in 2:length(qs)){
    x1[which(x<=qs[i]&x>=qs[i-1])]=paste0('Q',i-1)
  }
  return(x1)
}
