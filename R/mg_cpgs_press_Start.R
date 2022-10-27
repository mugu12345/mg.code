mg_cpgs_press_Start=function(data,promoter=T){
  #data=GSE59932.methy$Exp$GPL13534_485577_Data_col2
  nt=apply(data, 1, function(x){
    return(sum(is.na(x)))
  })
  dat=data[which(nt<ncol(data)*0.5),]
  cros=readMatrix(paste0(MG_Grobal_baseFolder,'/source/PMC3592906_CpG_CrossMap.txt'),row=F)
  dat=dat[which(!row.names(dat)%in%cros[,1]),]
  dat1=impute::impute.knn(as.matrix(dat))
  dat=dat1$data
  if(promoter){
    load(paste0(MG_Grobal_baseFolder,'/source/genome_info/cpg.450.27.cpg2enstbytss.Rdata'))
    dat=dat[row.names(dat)%in%cpg2enstbytss[,1],]
  }
  return(dat)
}
