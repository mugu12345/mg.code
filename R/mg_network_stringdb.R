mg_network_stringdb=function(genesymbols,cut_score=400){
  string.gene.link=readFST(paste0(MG_Grobal_baseFolder,'/source/ppi/string.gene.link.fst'),data.frame = T)
  string.gene.link=string.gene.link[string.gene.link[,3]>=cut_score,]
  string.gene.link=string.gene.link[string.gene.link[,1]%in%genesymbols,]
  string.gene.link=string.gene.link[string.gene.link[,2]%in%genesymbols,]
  return(string.gene.link)
}
