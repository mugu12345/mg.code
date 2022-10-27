getTCGAOSBySamples=function(samples){
  cli=readMatrix(paste0(MG_Grobal_DBPath,'/TCGA/Matrix/PMC6066282-TCGA-CDR-clinical.txt'),row=F)
  sm1=substr(samples,1,12)
  return(cli[match(sm1,cli$bcr_patient_barcode),24:31])
}