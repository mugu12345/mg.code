immu_meta=function(exp,isTCGA=T){
  baseFolder=paste0(MG_Grobal_baseFolder,'/source')
  metagenes=read.csv(paste0(baseFolder,'/13_meta_immune_pmid_28428277.txt'),sep = '\t',stringsAsFactors = F)
  meta.score=cbind()
  for(g in unique(metagenes[,2])){
    ensgs=unique(metagenes[which(metagenes[,2]==g),3])
    ensgs=ensgs[!is.na(ensgs)]
    ensgs=intersect(ensgs,row.names(exp))
    meta.s1=ssGSEAScore_by_genes(exp,genes = ensgs)
    #meta.score=cbind(meta.score,apply(exp[match(ensgs,row.names(exp)),],2,median))
    meta.score=cbind(meta.score,meta.s1[1,])
  }
  colnames(meta.score)=unique(metagenes[,2])
  if(isTCGA){
    rnames=gsub('\\.','-',row.names(meta.score))
    row.names(meta.score)=rnames
  }
  return(meta.score)
}