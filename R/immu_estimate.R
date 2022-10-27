immu_estimate=function(exp,platform='illumina',isTCGA=T){#affymetrix
  library(estimate)
  inf=tempfile()
  ouf=tempfile()
  ouf2=tempfile()
  writeMatrix(exp,outpath = inf)
  outputGCT(inf, ouf)
  estimateScore(ouf,ouf2,platform=platform)
  est.score=t(read.csv(ouf2,sep = '\t',row.names = 1,check.names = F,skip = 2))
  est.score=est.score[-1,]
  rnames=row.names(est.score)
  if(isTCGA){
    rnames=gsub('\\.','-',row.names(est.score))
  }
  est.score=apply(est.score, 2, as.numeric)
  row.names(est.score)=rnames
  
  return(est.score)
}