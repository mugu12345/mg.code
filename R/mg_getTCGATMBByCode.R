mg_getTCGATMBByCode=function(code='BRCA'){
  #首先去掉 silent 和 intron（Nonsense Mutation：无义突变） 区间的突变，基因组区间取 38.4Mb，每个样本的 TMB 为所有突变数量除以 38.4Mb
  library(maftools)
  #remove.packages("maftools","/root/R/x86_64-redhat-linux-gnu-library/3.6")
  if(file.exists(paste0(MG_Grobal_DBPath,'/TCGA/Matrix/mutect2_mafs/TCGA.',code,'.mutect.somatic.maf'))){
    maf=read.maf(paste0(MG_Grobal_DBPath,'/TCGA/Matrix/mutect2_mafs/TCGA.',code,'.mutect.somatic.maf'))
    maf2tmb=tmb(maf,logScale = F)
    
    tmb.smp=cbind(as.character(maf2tmb$Tumor_Sample_Barcode),maf2tmb$total_perMB)
    tmb.smp=crbind2DataFrame(tmb.smp)
    tmb.smp[,1]=substr(tmb.smp[,1],1,12)
    colnames(tmb.smp)=c('Sample','TMB')
    #head(crbind2DataFrame(cbind(Sample=substr(maf@variant.classification.summary$Tumor_Sample_Barcode,1,12),TMB)))
    #head(tmb.smp)
    #paste0(colnames(maf@variant.classification.summary),collapse = "','")
    #mty=c('Frame_Shift_Del','Frame_Shift_Ins','In_Frame_Del','In_Frame_Ins','Missense_Mutation','Nonstop_Mutation','Splice_Site','Translation_Start_Site')
    #tb=apply(crbind2DataFrame(maf@variant.classification.summary)[,which(colnames(maf@variant.classification.summary)%in%mty)],1,sum)
    #substr(maf@variant.classification.summary$Tumor_Sample_Barcode,1,12)
    #TMB=tb/38.4
    #tmb=crbind2DataFrame(cbind(Sample=substr(maf@variant.classification.summary$Tumor_Sample_Barcode,1,12),TMB))
    return(tmb.smp)
  }
  return(NULL)
}
