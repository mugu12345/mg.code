getTCGAmiRNAExpByCode=function(code,type='FPKM'){
  if(type=='FPKM'){
    cli=readMatrix(paste0(MG_Grobal_DBPath,'/TCGA/Matrix/miRNA_FPKM/TCGA-',code,'_fpkms.txt'))
  }else{
    cli=readMatrix(paste0(MG_Grobal_DBPath,'/TCGA/Matrix/miRNA_FPKM/TCGA-',code,'_counts.txt'))
  }
  return(cli)
}
