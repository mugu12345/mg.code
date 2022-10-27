mg_str_outline=function(desc,n=15,isCut=F){
  ndesc=c()
  for(de in desc){
    if(nchar(de)>n&length(grep(' ',de))>0){
      de1=unlist(strsplit(de,' '))
      d2=paste0(de1[(ceiling(length(de1)/2)+1):length(de1)],collapse = ' ')
      if(nchar(d2)>n){
        d2=paste0(substr(d2,0,n-3),'...')
      }
      de2=paste0(paste0(de1[1:ceiling(length(de1)/2)],collapse = ' '),'\n'
                 ,d2)
      ndesc=c(ndesc,de2)
    }else{
      if(isCut&nchar(de)>n){
        ndesc=c(ndesc,paste0(substr(de,0,n-3),'...'))
      }else{
        ndesc=c(ndesc,de)
      }
    }
  }
  return(ndesc)
}
