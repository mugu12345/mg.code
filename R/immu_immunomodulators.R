immu_immunomodulators=function(exp_data){
  baseFolder='source'
  imm.genes=readMatrix(paste0(baseFolder,'/78_immunomodulators_pmcid_PMC5982584.txt'))
  all.gs=cbind()
  gnames=c()
  for(g in unique(imm.genes$`Super Category`)){
    if(g!='Other'){
      gs=row.names(imm.genes)[which(imm.genes$`Super Category`==g)]
      gs.score=ssGSEAScore_by_genes(exp_data,gs)
      all.gs=cbind(all.gs,gs.score[1,])
      gnames=c(gnames,g)
    }
  }
  all.gs=crbind2DataFrame(all.gs)
  colnames(all.gs)=gnames
  row.names(all.gs)=colnames(exp_data)
  return(all.gs)
}
