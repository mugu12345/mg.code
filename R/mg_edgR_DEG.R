mg_edgR_DEG=function(exp,group,ulab,dlab){
  library(edgeR)
  ind1=which(group==ulab)
  ind2=which(group==dlab)
  eset=exp[,c(ind1,ind2)]
  group_list <- c(rep('G1',length(ind1)),rep('G0',length(ind2)))
  y <- DGEList(counts=eset,group=group_list)
  y <- calcNormFactors(y)
  y <- estimateCommonDisp(y)
  y <- estimateTagwiseDisp(y)
  eset=cpm(y)
  et <- exactTest(y)
  #FDR=p.adjust(et$table$PValue)#logFC     logCPM       PValue          FDR
  et$table$FDR=p.adjust(et$table$PValue)
  et=et[order(et$table$FDR),]
  return(list(Exp=eset,Group=group[c(ind1,ind2)],DEG=et$table,Model=y))
}
