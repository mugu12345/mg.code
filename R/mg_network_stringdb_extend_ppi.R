mg_network_stringdb_extend_ppi=function(genesymbols,cut_score=400){
  string.gene.link=readFST(paste0(MG_Grobal_baseFolder,'/source/ppi/string.gene.link.fst'),data.frame = T)
  string.gene.link=string.gene.link[string.gene.link[,3]>=cut_score,]
  t.inds=unique(c(which(string.gene.link[,1]%in%genesymbols),which(string.gene.link[,2]%in%genesymbols)))
  string.gene.link=string.gene.link[t.inds,]
  return(string.gene.link)
}
