parseGSEAResult=function(gsea.result.folder){
  if(file.exists(paste0(getwd(),'/',gsea.result.folder))){
    gsea.result.folder=paste0(getwd(),'/',gsea.result.folder)
  }
  #gsea.result.folder=paste0(paste0(gsea.outFolder,'/',gsea.result.kegg))
  edb.xml=XML::xmlParse(paste0(gsea.result.folder,'/edb/results.edb'),encoding="UTF-8")
  edb.xml.top = XML::xmlRoot(edb.xml)
  g.rnk=readMatrix(paste0(gsea.result.folder,'/edb/',XML::xmlGetAttr(edb.xml.top[[1]],'RANKED_LIST')),header = F)
  if(file.exists(paste0(gsea.result.folder,'/edb/',XML::xmlGetAttr(edb.xml.top[[1]],'TEMPLATE')))){
    TEMPLATE=readMatrix(paste0(gsea.result.folder,'/edb/',XML::xmlGetAttr(edb.xml.top[[1]],'TEMPLATE')),header = F,row=F)
    TEMPLATE=gsub('^# ','',TEMPLATE[2,1])
    TEMPLATE=unlist(strsplit(TEMPLATE,' '))
  }else{
    TEMPLATE=NULL
  }

  report.pathways=t(XML::xmlSApply(edb.xml.top, function(x){
    return(c(gsub('^gene_sets.gmt#','',XML::xmlGetAttr(x,'GENESET')),#RND_ES
             XML::xmlGetAttr(x,'ES'),
             XML::xmlGetAttr(x,'NES'),
             XML::xmlGetAttr(x,'NP'),
             XML::xmlGetAttr(x,'FDR'),
             XML::xmlGetAttr(x,'FWER'))
    )}))
  colnames(report.pathways)=c('Term','ES','NES','NP','FDR','FWER')
  report.pathways=crbind2DataFrame(report.pathways)
  return(list(EnrichTable=report.pathways,Nodes=edb.xml.top,Rank=g.rnk,TEMPLATE=TEMPLATE))
}
