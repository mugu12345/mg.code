mg_wgcna_get_power=function(exp,height=0,net_type='unsigned',blockSize = 7000){
  library(WGCNA)
  logs=c()
  #print('=====')
  filter.exp=exp
  sampleTree = hclust(dist(filter.exp), method = "average")
  #pdf(file = paste0(MG_GENE_FOLDER,'/SampleCluster.pdf'),width = 12,height = 6)
  #plot(sampleTree, main = "Sample clustering to detect outliers", sub="", xlab="")
  #dev.off()
  #height=140000
  #print(height)
  #print(min(sampleTree$height))
  if(height>min(sampleTree$height)){
    cte=cutree(tree = sampleTree,h = height)
    #plot(sampleTree)
    #abline(h=height)
    s.inds=match(names(which(cte==names(which.max(table(cte))))),row.names(filter.exp))
    #sum(sampleTree$height<height)
    logs=c(logs,paste0('remove samples:',(nrow(filter.exp)-length(s.inds)),' row count'))
    filter.exp=filter.exp[s.inds,]
    #print('==0')
  }
  #print('===1')
  powers = c(c(1:10), seq(from = 12, to=30, by=2))
  #print('===2')
  #print(head(filter.exp))
  sft = pickSoftThreshold(filter.exp, powerVector=powers,
                          networkType=net_type, verbose=5,blockSize = blockSize)
  #print('===3')
  cutPower=sft$powerEstimate
  if(is.na(cutPower)){
    cutPower=0
  }
  print(paste0('power succed:',cutPower,',starting plot'))
  logs=c(logs,paste0('power succed:',cutPower,',starting plot'))

  #library(customLayout)
  #lay1 <- lay_new(
  #  matrix(1:1, nc = 1),
  #  widths = c(5),
  #  heights = c(4))
  #lay2 <- lay_new(
  #  matrix(1:2, nc = 2),
  #  widths = c(2.5,2.5),
  #  heights = c(4))

  #cl = lay_bind_row(lay1, lay2,heights = c(0.8,1))
  #lay_set(cl)

  layout(matrix(c(1,1,2,3),2,2,byrow=T),widths=c(1,1),heights=c(0.8,1))

  mai=par('mai')

  #pdf(file = paste0(MG_GENE_FOLDER,'/powers.pdf'),width = 9,height = 8)
  mai1=mai
  mai1[1]=0
  par(mai=mai1)
  plot(sampleTree, main = "Sample clustering to detect outliers", sub="", xlab="")
  if(height>min(sampleTree$height)){
    abline(h=height,col='red')
  }
  par(mai=mai)

  #lay_show(cl)
  #par(mfrow = c(1,2))

  cex1 = rep(0.9,length(sft$fitIndices[,1]))
  col1=rep('red',length(sft$fitIndices[,1]))
  if(cutPower>0){
    cex1[which(sft$fitIndices[,1]==cutPower)]=1.5
    col1[which(sft$fitIndices[,1]==cutPower)]='blue'
  }
  plot(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
       xlab="Soft Threshold (power)",
       ylab="Scale Free Topology Model Fit,signed R^2",type="n",
       main = paste("Scale independence"))
  text(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
       labels=powers,cex=cex1,col=col1)
  abline(h=0.85,col="red")

  plot(sft$fitIndices[,1], sft$fitIndices[,5],
       xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n",
       main = paste("Mean connectivity"))
  text(sft$fitIndices[,1], sft$fitIndices[,5], labels=powers,
       cex=cex1, col=col1)
  arg=paste0('height=',height,',net_type=',net_type,',blockSize = ',blockSize)
  return(list(cutPower=cutPower,log=logs,Exp=filter.exp,arg=arg))
}
