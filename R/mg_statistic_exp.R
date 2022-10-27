mg_statistic_exp=function(exp){
  stic=t(apply(exp, 1, function(x){
    x=as.numeric(x)
    qx=quantile(x,na.rm = T)
    sdx=sd(x,na.rm = T)
    madx=mad(x,na.rm = T)
    menx=mean(x,na.rm = T)
    medx=median(x,na.rm = T)
    nac=sum(is.na(x))
    return(c(qx,sdx,madx,menx,medx,nac))
  }))
  colnames(stic)=c('0%','25%','50%','75%','100%','SD','MAD','Mean','Median','NA_count')
  stic1=t(apply(exp, 2, function(x){
    x=as.numeric(x)
    qx=quantile(x,na.rm = T)
    sdx=sd(x,na.rm = T)
    madx=mad(x,na.rm = T)
    menx=mean(x,na.rm = T)
    medx=median(x,na.rm = T)
    nac=sum(is.na(x))
    return(c(qx,sdx,madx,menx,medx,nac))
  }))
  colnames(stic1)=c('0%','25%','50%','75%','100%','SD','MAD','Mean','Median','NA_Count')

  return(list(Col=stic1,Row=stic))
}
