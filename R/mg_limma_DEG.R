mg_limma_DEG=function(exp,group,ulab,dlab){
  library(limma)
  ind1=which(group==ulab)
  ind2=which(group==dlab)

  sml <- c(rep('G1',length(ind1)),rep('G0',length(ind2)))    # set group names
  eset=exp[,c(ind1,ind2)]
  fl <- as.factor(sml)

  design <- model.matrix(~fl+0)
  colnames(design) <- levels(fl)
  cont.matrix<-makeContrasts(contrasts='G1-G0',levels=design)
  #print(head(eset))
  fit<-lmFit (eset,design)
  fit2 <- contrasts.fit(fit, cont.matrix)
  fit2 <- eBayes(fit2)
  #print(sml)
  tT <- topTable(fit2, adjust="fdr", sort.by="B", number=nrow(eset))
  regulated=ifelse(tT$logFC>0,'Up','Down')
  lfcs=c(log2(1.2),log2(1.3),log2(1.5),1)
  all.deg.cnt=cbind()
  for(lfc in lfcs){
    deg1=regulated[which(abs(tT$logFC)>lfc&tT$P.Value<0.05)]
    deg2=regulated[which(abs(tT$logFC)>lfc&tT$P.Value<0.01)]
    deg3=regulated[which(abs(tT$logFC)>lfc&tT$adj.P.Val<0.05)]
    deg4=regulated[which(abs(tT$logFC)>lfc&tT$adj.P.Val<0.01)]
    all.deg.cnt=cbind(all.deg.cnt,c(paste0(sum(deg1=='Up'),'|',sum(deg1=='Down'))
                                    ,paste0(sum(deg2=='Up'),'|',sum(deg2=='Down'))
                                    ,paste0(sum(deg3=='Up'),'|',sum(deg3=='Down'))
                                    ,paste0(sum(deg4=='Up'),'|',sum(deg4=='Down'))))
  }
  row.names(all.deg.cnt)=c('p<0.05','p<0.01','FDR<0.05','FDR<0.01')
  colnames(all.deg.cnt)=paste0(c('1.2','1.3','1.5','2'),'-fold')
  return(list(Exp=eset,Group=group[c(ind1,ind2)],DEG=tT,Summary=all.deg.cnt))
}
