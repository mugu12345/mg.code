readFST=function(inpath,data.frame=F){
  ft = tidyfst::import_fst(inpath,as.data.table = !data.frame)
  return(ft)
}
