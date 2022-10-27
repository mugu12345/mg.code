mg_decenter_DEG=function(exp,groups,method=c('ttest','ttestPair','wilcox','limma','edgR','edgRSignal','DESeq2')){
  gp=unique(gsub(' ','',groups[,2]))
  for(i in 1:(length(gp)-1)){
    for(j in (i+1):length(gp)){
      #gsub(' ','',colnames(exp1))####################20200623 wangjy
      ind1=which(gsub(' ','',groups[,2])==gp[i])
      ind2=which(gsub(' ','',groups[,2])==gp[j])
      group1=gsub(' ','',groups[c(ind1,ind2),2])
      exp1=exp[match(gsub(' ','',groups[c(ind1,ind2),1]),gsub(' ','',colnames(exp))),]
      if(method=='ttest'){
        deg=mg_ttest_DEG(exp1,group1,gp[i],gp[j])
      }else if(method=='ttestPair'){
        deg=mg_ttest_DEG(exp1,group1,gp[i],gp[j],pair = T)
      }else if(method=='wilcox'){
        deg=mg_rank_DEG(exp1,group1,gp[i],gp[j])
      }else if(method=='limma'){
        deg=mg_limma_DEG(exp1,group1,gp[i],gp[j])
      }else if(method=='edgR'){
        deg=mg_edgR_DEG(exp1,group1,gp[i],gp[j])
      }else if(method=='edgRSignal'){
        deg=mg_edgR_DEG(exp1,group1,gp[i],gp[j])
      }else if(method=='DESeq2'){
        deg=mg_Deseq2_DEG(exp1,group1,gp[i],gp[j])
      }

    }
  }
}
