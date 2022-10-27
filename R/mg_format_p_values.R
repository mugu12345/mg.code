mg_format_p_values=function(vals,psig=T,rsig=F){
  nvals=c()
  for(val in vals){
    nvals=c(nvals,mg_format_p_value(val,psig,rsig))
  }
  return(nvals)
}
