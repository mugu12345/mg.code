mg_gene2drug=function(genesymbols=NULL){
  drug2gene=readFST(paste0(MG_Grobal_baseFolder,'/source/ppi/drug2gene.fst'),data.frame = T)
  if(is.null(genesymbols)){
    return(drug2gene)
  }else{
    return(drug2gene[drug2gene$Gene%in%genesymbols,])
  }
}
