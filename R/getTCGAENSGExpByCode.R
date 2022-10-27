getTCGAENSGExpByCode=function(code,type='TPM'){
  if(type=='count'){
    cli=readMatrix(paste0(MG_Grobal_DBPath,'/TCGA/Matrix/mRNA_counts/TCGA-',code,'_counts.txt'))
  }else{
    cli=readMatrix(paste0(MG_Grobal_DBPath,'/TCGA/Matrix/mRNA_TPM/Merge_TCGA-',code,'_TPM.txt'))
  }
  return(cli)
}