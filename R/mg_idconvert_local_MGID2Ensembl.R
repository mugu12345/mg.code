mg_idconvert_local_MGID2Ensembl=function(mgids){
  baseFolder=paste0(MG_Grobal_baseFolder,'/source/')
  load(paste0(baseFolder,'gene_convert.Rdata'))
  #ENSG=Ensembl2Symbol[Ensembl2Symbol[,1]%in%mgids,]
  #Ensembl2Symbol[match('mgids',Ensembl2Symbol[,1]),]
  return(Ensembl2Symbol[match(mgids,Ensembl2Symbol[,1]),])
}
