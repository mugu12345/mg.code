predictScoreByCoxModel=function(coxModel,dat){
  cls=colnames(dat)

  if(sum(is.na(coxModel$Genes%in%cls))>0){
    print(paste0('Gene:',paste0(coxModel$Genes[is.na(coxModel$Genes%in%cls)],collapse = ','),' not found'))
    return(NULL)
  }
  score=predictRiskScore(as.numeric(coxModel$Coef),coxModel$Genes,dat)
  names(score)=row.names(dat)
  return(score)
}

#predictScoreByCoxModel(m.res,t(ov.exp[match(sig.genes,row.names(ov.exp)),]))
#m.res$Score
