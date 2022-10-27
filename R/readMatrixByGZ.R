readMatrixByGZ=function(inpath,row=T,header=T){
  #inpath=paste0(MG_Grobal_DBPath,'/geo/GSMs/GSM1033655-tbl-1.txt.gz')
  if (!grepl("gz$", inpath)){
    return(readMatrix(inpath,row,header))
  }
  dat=data.table::fread(inpath, sep = "\t",header = header,stringsAsFactors = F,check.names = F,na.strings="NA")
  if(nrow(dat)==0){
    dat=as.data.frame(dat,stringsAsFactors=F)
    return(dat)
  }
  if(row){
    dat=as.data.frame(dat,stringsAsFactors=F)
    row.names(dat)=dat[,1]
    if(ncol(dat)>2){
      dat=dat[,-1]
    }else if(ncol(dat)>1){
      rns=row.names(dat)
      cls=colnames(dat)
      dat=data.frame(dat[,2],stringsAsFactors = F)
      row.names(dat)=rns
      colnames(dat)=cls[2]
    }
  }else{
    dat=as.data.frame(dat,stringsAsFactors=F)
  }
  return(dat)
}
