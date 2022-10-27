mg_RT_PCR_data_plot=function(GAPDH,GeneMatrix,group,col='jco',xangle=45){
  gp=unique(group)
  ref_m=c()
  for (g in gp) {
    ref_m=c(ref_m,mean(GAPDH[which(group==g)]))
  }
  dct=apply(GeneMatrix, 2, function(x){
    return(x-ref_m[match(group,gp)]  )
  })
  ddct=apply(dct, 2, function(x){
    return(x-mean(x[which(group==gp[1])]) )
  })
  e2ddt=2^(-ddct)
  e2ddt.test=apply(e2ddt, 2, function(x){
    ps=c(1)
    for(g in gp[2:length(gp)]){
      ps=c(ps,t.test(x[which(group==gp[1])],x[which(group==g)])$p.value)
    }
    return(ps)
  })
  e2ddt.test=crbind2DataFrame(e2ddt.test)
  e2ddt.test$Group=gp
  e2ddt.test_data_m=reshape2::melt(e2ddt.test, id.vars=c("Group"))
  basee2ddt=e2ddt
  e2ddt=crbind2DataFrame(e2ddt)
  e2ddt$Group=group
  print(e2ddt)
  data_m=reshape2::melt(e2ddt, id.vars=c("Group"))
  colnames(data_m)=c('Group','Gene','value')

  library(ggplot2)
  library(Rmisc)
  data_m.vt=summarySE(data_m, measurevar="value", groupvars=c("Group","Gene"))
  data_m.vt$pvalue=signif(e2ddt.test_data_m[match(paste0(data_m.vt[,1],data_m.vt[,2])
                                                  ,paste0(e2ddt.test_data_m[,1],e2ddt.test_data_m[,2])),3],3)
  data_m.vt$pvaluelabel=mg_format_p_values(data_m.vt$pvalue)

  plt=ggplot(data_m.vt, aes(x=Gene, y=value, fill=Group)) +
    geom_bar(position=position_dodge(), stat="identity",
             colour="black", # Use black outlines,
             size=.3) +      # Thinner lines
    geom_errorbar(aes(ymin=value-se, ymax=value+se),
                  size=.3,    # Thinner lines
                  width=.2,
                  position=position_dodge(.9)) +
    geom_text(aes(y=value+se,label=pvaluelabel)
              #,size = 10
              ,vjust=-1
              ,position=position_dodge(.9))+
    xlab("") +
    ylab("Relative mRNA expression") +
    #ggtitle("The Effect of Vitamin C on\nTooth Growth in Guinea Pigs") +
    theme_bw()+mg_get_ggsci_col(col,isFill = T)
  if(xangle==0){
    tx=element_text(colour="black",family="Times")
  }else{
    tx=element_text(angle=xangle,hjust = 1,colour="black",family="Times")
  }
  plt=plt+theme(axis.text.x=tx)

  return(list(Plot=plt,Exp=e2ddt))
}
