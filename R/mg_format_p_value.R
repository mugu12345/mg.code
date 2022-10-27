mg_format_p_value=function(val,psig=T,rsig=F){
  if(is.na(val)) return('')
  if(psig){
    if(val<0.001){
      return('***')
    }else if(val<0.01){
      return('**')
    }else if(val<0.05){
      return('*')
    }else if(val<0.1){
      return('.')
    }
    if(rsig){
      return('-')
    }else{
      return('')
    }
  }else{
    return(signif(val,3))
  }
}
