writeFST=function(dat,outpath,row=F){
  if(row) dat=cbind(Tag=row.names(dat),dat)
  tidyfst::export_fst(dat,outpath)
}
