mg_createCoxModel_auto_step=function(dat,time,event){
  mods=list()
  cindx=c()
  for(did in c("both", "backward", "forward")){
    cx=createCoxModel(dat,time,event,isStep=T,direction=did)
    mods=c(mods,list(cx))
    if(is.null(cx)){
      cindx=c(cindx,NA)
    }else{
      c1=getC_index(cx$Score,os = time,status = event)
      cindx=c(cindx,c1$c.index)
    }
  }
  if(length(which.max(cindx))>0){
    return(mods[[which.max(cindx)]])
  }
  return(NULL)
}
