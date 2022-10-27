mg_Deseq2_DEG=function(exp,group,ulab,dlab){
  library(DESeq2)
  ind1=which(group==ulab)
  ind2=which(group==dlab)
  eset=exp[,c(ind1,ind2)]
  group_list <- c(rep('G1',length(ind1)),rep('G0',length(ind2)))
  set.seed(123456)
  dds <- DESeqDataSetFromMatrix(countData = eset,
                                colData = data.frame(row.names=factor(colnames(eset)), group_list=group_list),
                                design = ~ group_list)
  dds2 <- DESeq(dds)
  #resultsNames(dds2)
  res <-  results(dds2, contrast=c("group_list","G1","G0"))
  #resOrdered <- res[order(res$padj),]
  et=cbind(res$log2FoldChange,res$pvalue,res$padj)
  row.names(et)=row.names(res)[order(res$padj)]
  colnames(et)=c('logFC','PValue','FDR')
  et=crbind2DataFrame(et)
  eset=counts(dds2, normalized=TRUE)
  return(list(Exp=eset,Group=group[c(ind1,ind2)],DEG=et,Model=dds2))
}
