immu_cytScore=function(exp_data=NULL){
  #每个患者的CYT评分被计算为GZMA和PRF1表达水平的平均值，用于评估肿瘤内免疫溶细胞t细胞活性
  #PMID：25594174
  #Paper:Molecular and genetic properties of tumors associated with local immune cytolytic activity
  inds=match(c('GZMA','PRF1'),row.names(exp_data))
  if(sum(is.na(inds))>0){
    print(paste0('not found gene:',c('GZMA','PRF1')[which(is.na(inds))]))
  }else{
    return(apply(exp_data[inds,],2,mean))
  }
  return(NULL)
}
