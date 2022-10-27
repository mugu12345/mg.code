mg_getPanCancerExpSamplesInfo=function(){
  dat=readMatrix(paste0(MG_Grobal_baseFolder,'/data_hub/pancancer_info/TcgaTargetGTEX_phenotype.txt'))
  return(dat)
}