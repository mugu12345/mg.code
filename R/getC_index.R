getC_index=function(riskscore,os,status){
  inds=which(!is.na(riskscore)&!is.na(os)&!is.na(status))
  riskscore=riskscore[inds]
  os=os[inds]
  status=status[inds]
  c1=survcomp::concordance.index(x=riskscore, surv.time=os, surv.event=status,
                                 method="noether")
  #c2=concordance.index(x=riskscore[order(rnorm(length(riskscore)))], surv.time=os, surv.event=status,
  #                     method="noether")
  #p=min(cindex.comp(c1, c2)$p.value,cindex.comp(c2, c1)$p.value)
  return(c1)
}
