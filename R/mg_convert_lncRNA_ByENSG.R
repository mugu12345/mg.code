mg_convert_lncRNA_ByENSG=function(exp,Symbol=T){
  cut=c("__alignment_not_unique","__not_aligned","__too_low_aQual","__ambiguous","__no_feature")
  exp=exp[which(!row.names(exp)%in%cut),]
  genecod_ids=readMatrix(paste0(MG_Grobal_baseFolder,'/source/gencode.v22.ensg.genelen.tab'))
  head(genecod_ids)
  tps=c('lincRNA','sense_intronic','sense_overlapping','antisense','processed_transcript','3prime_overlapping_ncRNA')
  lnc_tb=genecod_ids[genecod_ids[,2]%in%tps,]
  row.names(exp)=gsub('\\..*','',row.names(exp))
  row.names(lnc_tb)=gsub('\\..*','',row.names(lnc_tb))
  if(Symbol){
    exp1=exp_probe2symbol_v2(exp,anno = data.frame(A=row.names(lnc_tb),B=lnc_tb[,3]))
  }else{
    exp1=exp[row.names(exp)%in%row.names(lnc_tb),]
  }
  return(exp1)
}
