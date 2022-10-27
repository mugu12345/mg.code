exp_probe2symbol=function(datExpr,method=c('mean','median','max','min')[1]){
  library('hgu133plus2.db')
  ensg2gene=AnnotationDbi::select(hgu133plus2.db,keys = row.names(datExpr),keytype = 'PROBEID',columns = c('SYMBOL'))
  ensg2gene=ensg2gene[!is.na(ensg2gene[,2]),]
  merge.gene.exp=merge_data_by_group(datExpr,ensg2gene,method = method)
  detach('package:hgu133plus2.db')
  return(merge.gene.exp)
}
