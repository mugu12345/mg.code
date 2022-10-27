getTCGAClinicalBySamples=function(samples){
  cli=readMatrix(paste0(MG_Grobal_DBPath,'/TCGA/Matrix/Merge_clinical.txt'),row=F)
  sm1=substr(samples,1,12)
  return(cli[match(sm1,cli$A0_Samples),])
}