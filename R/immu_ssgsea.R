immu_ssgsea=function(exp,isTCGA=T){
  baseFolder=paste0(MG_Grobal_baseFolder,'/source')
  imm.genes=read.csv(paste0(baseFolder,'/28_immune_pmid_28052254.txt'),sep = '\t',stringsAsFactors = F)
  all.list=list()
  for(s in unique(imm.genes$Cell.type)){
    inds=which(imm.genes$Cell.type==s)
    gs=GSEABase::GeneSet(setName=s, setIdentifier=paste0(s,"101"),geneIds=imm.genes$Metagene[inds],GSEABase::SymbolIdentifier()) 
    all.list <- c(all.list, list(gs))
  }
  gsc <- GSEABase::GeneSetCollection(all.list)
  fl <- tempfile()
  GSEABase::toGmt(gsc, fl)
  c2immue=GSEABase::getGmt(fl)
  ssGSEA.immue <- GSVA::gsva(as.matrix(exp), c2immue,method='ssgsea',
                     min.sz=10, max.sz=500, verbose=TRUE)
  ssGSEA.immue=t(ssGSEA.immue)
  if(isTCGA){
    rnames=gsub('\\.','-',row.names(ssGSEA.immue))
    row.names(ssGSEA.immue)=rnames
  }
  return(ssGSEA.immue)
}