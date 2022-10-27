mg_RT_PCT_RandomGeneMatrix=function(N=5,group=c('A','B'),Genes=c('GeneA','GeneB'),regulated=c(1,-1)){
  m=20+10*runif(length(Genes))
  GAPDH=c(rnorm(2*N,mean = mean(m)))
  dt.al=rbind()
  for(i in 1:length(m)){
    d1=c()
    d2=c()
    for(j in seq(0.1,10,0.1)){
      x1=rnorm(N,mean = m[i]-j)
      x2=rnorm(N,mean = m[i]+j)
      p=mg_RT_PCR_DEG(GAPDH,c(rep('B',N),rep('A',N)),c(x1,x2))
      if(p<0.05){
        d1=x1
        d2=x2
        break;
      }
    }
    if(regulated[i]<0){
      dt.al=rbind(dt.al,c(d2,d1))
    }else{
      dt.al=rbind(dt.al,c(d1,d2))
    }
  }
  row.names(dt.al)=Genes
  dt.al=rbind(GAPDH,dt.al)
  al.gp=c()
  for(u in group){
    al.gp=c(al.gp,rep(u,N))
  }
  colnames(dt.al)=al.gp
  fig=mg_RT_PCR_data_plot(GAPDH,t(dt.al[2:nrow(dt.al),]),al.gp)
  return(list(Exp=t(dt.al),Fig=fig))
}
