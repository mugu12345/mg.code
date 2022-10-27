mg_get_coad_cms_subtype=function(symbol_expro,RNAseq=T){
  symbol_expro.idv=mg_idconvert_local(row.names(symbol_expro))
  symbol_expro.eid=exp_probe2symbol_v2(symbol_expro,anno = cbind(as.character(symbol_expro.idv$IDMap$BG)
                                                                 ,as.character(GSE17536.exp.idv$IDMap$EntrezID)))
  cms.res <- CMScaller::CMScaller(ExpressionSet(as.matrix(symbol_expro.eid)), RNAseq=RNAseq, doPlot=TRUE)
  return(data.frame(Sample=row.names(cms.res),CMS=cms.res$prediction))
}