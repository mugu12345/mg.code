getTCGAMAFByCode=function(code){
  library(maftools)
  if(file.exists(paste0(MG_Grobal_DBPath,'/TCGA/Matrix/mutect2_mafs/TCGA.',code,'.mutect.somatic.maf'))){
    maf=read.maf(paste0(MG_Grobal_DBPath,'/TCGA/Matrix/mutect2_mafs/TCGA.',code,'.mutect.somatic.maf'),isTCGA = T)
    return(maf)
  }
  return(NULL)
}