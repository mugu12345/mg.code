mg_predict_time_ymd=function(time,mks){
  mx=quantile(time,seq(0,1,0.1),na.rm=T)['80%']
  #mx=max(time,na.rm = T)
  if(mx<25){
  }else if(mx<365){
    mks=mks*12
  }else{
    mks=mks*365
  }
  mks=mks[which(mks<quantile(time,seq(0,1,0.01),na.rm=T)['95%'])]
  return(mks)
}
