mg_CpgAnnoPosition=function(cpgs){
  load(paste0(MG_Grobal_baseFolder,'/source/genome_info/cpg.450.27.anno.info.fmt.Rdata'))
  return(cpg.450.27.anno.info.fmt[match(cpgs,cpg.450.27.anno.info.fmt[,1]),])
}
