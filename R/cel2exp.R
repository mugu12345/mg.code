cel2exp=function(filenames){
  chiptype=affyio::read.celfile.header(filenames[1])[["cdfName"]]
  pkgname <- gsub("[_-]", ".", paste("pd.", tolower(chiptype), sep = ""))
  #pkgname
  #print(chiptype)
  #expData=NULL
  print(paste0('platform cdf:',pkgname))
  tryCatch({
  if(pkgname=='pd.huex.1.0.st.v1'){
    pkgname='pd.huex.1.0.st.v2'
  }
  if(pkgname=='pd.primeview'){
    pkgname='primeviewcdf'
  }
  dt.raw=oligo::read.celfiles(filenames=filenames,pkgname=pkgname)
  dt=oligo::rma(dt.raw)
  expData=oligo::exprs(dt)
  print('oligo succ!')
  return(list(Exp=expData,platform=pkgname))
  },error = function(e) {
  dt=affy::ReadAffy(filenames =filenames)
  dt=affy::rma(dt)
  expData=affy::exprs(dt)
  #print(expData)
  print('affy succ!')
  return(list(Exp=expData,platform=pkgname))
  #  save(e,file=paste0(MG_Grobal_outFolder,'/',opt$errorfile))
  })
  #boxplot(dt,names=gsub('.CEL.gz','',colnames(dt)),outline=F,angle=75)
  #dim(dt)
  #pd.huex.1.0.st.v2
  #return(list(Exp=NULL,platform=pkgname))
}
