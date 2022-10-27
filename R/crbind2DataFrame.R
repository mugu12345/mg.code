crbind2DataFrame=function(dat,full=F){
  print(class(dat))
  if(class(dat)=='table'){
    if(!is.na(ncol(dat))){
      dat=apply(dat,2,function(x){
        return(x)
      })
    }
  }
  if(class(dat)!='data.frame'){
    dat1=as.data.frame(as.matrix(dat))
  }else{
    dat1=dat
  }
  dat1.class=apply(dat1, 2, class)
  #which(dat1.class!='numeric')
  #print(head(dat1))
  for(i in which(dat1.class!='numeric')){
    dat1[,i]=as.character(dat1[,i])
    if(full){
      dat1[,i]=as.numeric(dat1[,i])
    }else{
      dt=dat1[which(gsub(' ','',dat1[,i])!=''&!is.na(dat1[,i])),i]
      dt=dt[which(dt!='Inf'&dt!='NaN'&dt!='NA')]
      #dt[which(is.na(as.numeric(dt)))]
      if(sum(is.na(as.numeric(dt)))<length(dt)*0.1){
        #print(dat1[,i])
        dat1[,i]=as.numeric(dat1[,i])
      }
    }
  }
  return(dat1)
}
