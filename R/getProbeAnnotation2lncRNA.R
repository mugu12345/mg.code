getProbeAnnotation2lncRNA=function(gpl){
  if(file.exists(paste0(MG_Grobal_baseFolder,'/source/LncRNA_Re_anno/',gpl,'.txt'))){
    dat=readMatrix(paste0(MG_Grobal_baseFolder,'/source/LncRNA_Re_anno/',gpl,'.txt'))
    return(dat)
  }
  return(NULL)
}
