mg_edgRSignal_DEG=function(exp,group,ulab,dlab){
  library(edgeR)
  ind1=which(group==ulab)
  ind2=which(group==dlab)
  eset=exp[,c(ind1,ind2)]
  group_list <- c(rep('G1',length(ind1)),rep('G0',length(ind2)))
  set.seed(123456)
  y <- DGEList(counts=eset,group=group_list)
  keep <- rowSums(cpm(y)>1) >= 1
  y <- y[keep, keep.lib.sizes=FALSE]
  y <- calcNormFactors(y)

  #y <- estimateCommonDisp(y)
  #y <- estimateTagwiseDisp(y)
  et <- exactTest(y)
  #FDR=p.adjust(et$table$PValue)#logFC     logCPM       PValue          FDR
  et$table$FDR=p.adjust(et$table$PValue)
  et=et[order(et$table$FDR),]
  return(list(Exp=eset,Group=group[c(ind1,ind2)],DEG=et$table))
}
