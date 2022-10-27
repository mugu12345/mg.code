mg_group2datafreamBy0and1=function(group){
  gp=unique(group)
  dat=cbind()
  for(g in gp){
    dat=cbind(dat,ifelse(group==g,1,0))
  }
  colnames(dat)=gp
  return(as.data.frame(dat))
}
