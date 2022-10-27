matrix2vator=function(dat){
  val=c()
  if(is.null(dim(dat))){
    return(as.numeric(dat))
  }else{
    for(i in 1:nrow(dat)){
      val=c(val,as.numeric(dat[i,]))
    }
    val=val[!is.na(val)]
  }
  return(val)
}
