mg_ttest_DEG=function(exp,group,ulab,dlab,pair=F){
  ind1=which(group==ulab)
  ind2=which(group==dlab)
  eset=exp[,c(ind1,ind2)]
  result=t(apply(eset, 1, function(x){
    fc=mean(x[ind1],na.rm = T)/mean(x[ind2],na.rm = T)
    p=t.test(x[ind1],x[ind2],paired = pair)$p.value
    es=(mean(x[1:length(ind1)])-mean(x[(length(ind1)+1):ncol(eset)]))/(sd(x[1:length(ind1)])-sd(x[(length(ind1)+1):ncol(eset)]))
    return(c(log2(fc),es,p))
  }))
  colnames(result)=c('logFC','effectSize','p.value')
  row.names(result)=row.names(eset)
  result=crbind2DataFrame(result)
  result$FDR=p.adjust(result[,2])
  result$effectSize=effectSize
  result=result[order(result[,3]),]
  return(list(Exp=eset,Group=group[c(ind1,ind2)],DEG=result))
}
