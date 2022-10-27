mg_rank_DEG=function(exp,group,ulab,dlab){
  ind1=which(group==ulab)
  ind2=which(group==dlab)
  eset=exp[,c(ind1,ind2)]
  result=t(apply(eset, 1, function(x){
    fc=mean(x[ind1],na.rm = T)/mean(x[ind2],na.rm = T)
    p=wilcox.test(x[ind1],x[ind2])$p.value
    es=(mean(x[1:length(ind1)])-mean(x[(length(ind1)+1):ncol(eset)]))/(sd(x[1:length(ind1)])-sd(x[(length(ind1)+1):ncol(eset)]))
    return(c(log2(fc),es,p))
  }))
  colnames(result)=c('logFC','effect size','p.value')
  row.names(result)=row.names(eset)
  result=crbind2DataFrame(result)
  result$FDR=p.adjust(result[,3])
  result=result[order(result[,4]),]
  return(list(Exp=eset,Group=group[c(ind1,ind2)],DEG=result))
}
