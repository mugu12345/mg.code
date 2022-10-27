ssGSEAScore_by_muti_group_genes=function(gene.exp,genelist){
  #library('GSVA')
  #library(GSEABase)
  all.list=list()
  for(i in 1:length(genelist)){
    gs=GSEABase::GeneSet(setName=names(genelist)[i], setIdentifier=paste0("101"),geneIds=unique(genelist[[i]]),GSEABase::SymbolIdentifier())
    #gs
    all.list=c(all.list,list(gs))
  }
  #length(all.list)
  gsc <- GSEABase::GeneSetCollection(all.list)
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
