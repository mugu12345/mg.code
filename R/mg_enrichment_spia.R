mg_enrichment_spia=function(genes,fcs,bg_genes,geneType='SYMBOL'){
  if(geneType=='SYMBOL'){
    library(hgu133plus2.db)
    cvt=AnnotationDbi::select(hgu133plus2.db,keys = bg_genes,columns = c('ENTREZID'),keytype = 'SYMBOL')
    g.ids=cvt$ENTREZID[match(genes,cvt$SYMBOL)]
    fcs=fcs[which(!is.na(g.ids))]
    names(fcs)=g.ids[which(!is.na(g.ids))]
  }else{
    g.ids=bg_genes[match(genes,bg_genes)]
    fcs=fcs[which(!is.na(g.ids))]
    names(fcs)=g.ids[which(!is.na(g.ids))]
  }
  t.inds.spia=SPIA::spia(de=fcs[which(!is.na(fcs)&!is.infinite(fcs))], all = unique(cvt$ENTREZID),organism="hsa",beta=NULL,nB=2000
                         ,plots=FALSE, verbose=TRUE,combine="fisher")
  return(t.inds.spia)
}
