ssGSEAScore_by_genes=function(gene.exp,genes){
  #library('GSVA')
  #library(GSEABase)
  #all.list=list()
  gs=GSEABase::GeneSet(setName='GeneSet', setIdentifier=paste0("101"),geneIds=unique(genes),GSEABase::SymbolIdentifier())

  gsc <- GSEABase::GeneSetCollection(list(gs))
  fl <- tempfile()
  GSEABase::toGmt(gsc, fl)
  cgeneset=GSEABase::getGmt(fl)
  ssGSEA.geneset <- GSVA::gsva(as.matrix(gene.exp), cgeneset,method='ssgsea',
                               min.sz=1, max.sz=5000, verbose=TRUE)
  #detach('package:GSVA')
  #detach('package:GSEABase')
  #row.names(ssGSEA.geneset)
  return(ssGSEA.geneset)
}
