mg_getTCGAMATHByCode=function(code='COAD'){#Mutant-allele tumor heterogeneity
  #MATH的意义，作者认为MATH能有效的代表肿瘤特异性特变位点的MAF值的分布的偏差，相当于说明MAF偏离该样本的MAF整体分布的程度（有点标准差的意思）
  #当然是MATH值越大，说明肿瘤异质性越高
  library(maftools)
  #remove.packages("maftools","/root/R/x86_64-redhat-linux-gnu-library/3.6")
  if(file.exists(paste0(MG_Grobal_DBPath,'/TCGA/Matrix/mutect2_mafs/TCGA.',code,'.mutect.somatic.maf'))){
    maf=read.maf(paste0(MG_Grobal_DBPath,'/TCGA/Matrix/mutect2_mafs/TCGA.',code,'.mutect.somatic.maf'))
    maf.smps=as.character(maf@variants.per.sample$Tumor_Sample_Barcode)
    maf.maths=c()
    for(s in maf.smps){
      tcga.ab.het = inferHeterogeneity(maf = maf, tsb = s)
      maf.maths=c(maf.maths,tcga.ab.het$clusterData$MATH[1])
    }
    math.smp=cbind(Sample=maf.smps,maf.maths)
    math.smp=crbind2DataFrame(math.smp)
    math.smp[,1]=substr(math.smp[,1],1,12)
    colnames(math.smp)=c('Sample','MATH')#Mutant-allele tumor heterogeneity
    return(math.smp)
  }
  return(NULL)
}