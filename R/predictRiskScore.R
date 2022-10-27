predictRiskScore=function(coefs,sigGenes,dat){
  #coefs=cox$coefficients
  #sigGenes=sig.genes
  pl=length(coefs)-length(sigGenes)
  p.cels=c()
  if(pl>0){
    p.cels=coefs[1:pl]
    l.cels=coefs[(pl+1):length(coefs)]
  }else{
    l.cels=coefs
  }
  cls=colnames(dat)
  if(sum(is.na(sigGenes%in%cls))>0){
    print(paste0('Gene:',paste0(sigGenes[is.na(sigGenes%in%cls)],collapse = ','),' not found'))
    return(NULL)
  }
  t.dat=dat[,match(sigGenes,cls)]
  t.score=as.matrix(t.dat)%*%as.numeric(l.cels)
  t.score=t.score[,1]
  if(length(p.cels)>0){
    for(pc in p.cels){
      t.score=pc+t.score
    }
  }
  return(t.score)
}
