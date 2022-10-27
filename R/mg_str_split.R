mg_str_split=function(dat,intoColNames,sep=','){
  d.dat=data.frame(X=dat)
  tmp.dat=crbind2DataFrame(d.dat%>%tidyr::separate(X,into=intoColNames,sep=sep))
  return(tmp.dat)
}
