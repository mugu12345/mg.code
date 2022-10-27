mg_RT_PCR_DEG=function(GAPDH,group,dat){
  gp=unique(group)
  ref_m=c()
  for (g in gp) {
    ref_m=c(ref_m,mean(GAPDH[which(group==g)]))
  }
  dct=dat-ref_m[match(group,gp)]

  ddct=dct-mean(dct[which(group==gp[1])])
  e2ddt=2^(-ddct)
  return(t.test(e2ddt[which(group==gp[1])],e2ddt[which(group==gp[2])])$p.value)
}
