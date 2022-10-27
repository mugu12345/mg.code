writeMatrix=function(dat,outpath,row=T,header=T){
  if(row){
    write.table(cbind(Tag=row.names(dat),dat),file=outpath,sep="\t",quote = F,row.names=F,col.names = header)
  }else{
    write.table(dat,file=outpath,sep="\t",quote = F,row.names=F,col.names = header)
  }
}
