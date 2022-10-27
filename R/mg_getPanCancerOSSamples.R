mg_getPanCancerOSSamples=function(samples=NULL){
  dat=readMatrix(paste0(MG_Grobal_baseFolder,'/data_hub/pancancer_info/TCGA_TARGET_survival_data_UCSC_pancancer.txt'))
  if(!is.null(samples)){
    return(dat[match(samples,row.names(dat)),])
  }
  return(dat)
}