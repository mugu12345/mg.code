mg_tmp_submap <- function(data.mkr,data.rnk,cand.cls.label.mkr,cand.cls.label.rnk,ES.matrix,num.subcls.mkr,num.subcls.rnk,ntag=100,weighted.score.type=1)
{
  for (rnk in 1:num.subcls.rnk)
  {
    # convert rnk cls to binary
    #cls.bin.rnk<-cand.cls.label.rnk
    cls.bin.rnk<-ifelse(cand.cls.label.rnk==rnk,1,0)
    #cls.bin.rnk[cls.bin.rnk!=rnk]<-0
    #cls.bin.rnk[cls.bin.rnk==rnk]<-1
    cls1=ifelse(cls.bin.rnk==1,1,NA)
    cls0=ifelse(cls.bin.rnk==0,1,NA)

    #cls0[cls0==0]<-1
    # order gene in data.rnk
    data<-as.matrix(data.rnk)
    #dim(data)
    data1.tr <-apply(t(data)*cls1, 2, na.omit)
    data1.tr <- as.matrix(data1.tr)

    data0.tr <- apply(t(data)*cls0, 2, na.omit)
    data0.tr <- as.matrix(data0.tr)

    mean1<-apply(data1.tr, 2, mean)
    mean0<-apply(data0.tr, 2, mean)

    sd1<-apply(data1.tr,2,sd)
    sd0<-apply(data0.tr,2,sd)
    s2n<-(mean1-mean0)/(sd1+sd0)

    #s2n[,]
    #order.gene.s2n.rnk<-cbind(order(s2n,decreasing=T))
    #order.gene.s2n.rnk<-cbind(order.gene.s2n.rnk,s2n[order.gene.s2n.rnk[,1]])
    order.gene.s2n.rnk<-cbind(order(s2n,decreasing=T),sort(s2n,decreasing=T))
    #dim(order.gene.s2n.rnk)
    #    rm(s2n)
    #    gc()

    for (mkr in 1:num.subcls.mkr)
    {
      print(paste("### rank: ",rnk,"/",num.subcls.rnk,", mkr: ",mkr,"/",num.subcls.mkr," ###",sep=""))
      # convert mkr cls to binary
      cls.bin.mkr<-ifelse(cand.cls.label.mkr==mkr,1,0)
      cls1=ifelse(cls.bin.mkr==1,1,NA)
      cls0=ifelse(cls.bin.mkr==0,1,NA)
      data<-as.matrix(data.mkr)
      data1.tr<-apply(t(data)*cls1,2,na.omit)
      data0.tr<-apply(t(data)*cls0,2,na.omit)
      data1.tr <- as.matrix(data1.tr)
      data0.tr <- as.matrix(data0.tr)
      mean1<-apply(data1.tr,2,mean)
      mean0<-apply(data0.tr,2,mean)
      sd1<-apply(data1.tr,2,sd)
      sd0<-apply(data0.tr,2,sd)
      s2n<-(mean1-mean0)/(sd1+sd0)
      order.gene.mkr<-order(s2n,decreasing=T)
      marker<-order.gene.mkr[1:ntag]
      # ES
      tag.indicator <- sign(match(order.gene.s2n.rnk[,1], marker, nomatch=0))
      no.tag.indicator <- 1 - tag.indicator
      N <- length(order.gene.s2n.rnk[,1])
      Nh <- length(marker)
      Nm <-  N - Nh
      if (weighted.score.type == 0){
        correl.vector <- rep(1, N)
      }else{
        alpha <- weighted.score.type
        correl.vector <- abs(order.gene.s2n.rnk[,2]**alpha)
      }
      sum.correl.tag    <- sum(correl.vector[tag.indicator == 1])
      norm.tag    <- 1.0/sum.correl.tag
      norm.no.tag <- 1.0/Nm
      RES <- cumsum(tag.indicator * correl.vector * norm.tag - no.tag.indicator * norm.no.tag)
      max.ES <- max(RES)
      min.ES <- min(RES)
      if (max.ES > - min.ES){
        ES.matrix[mkr,rnk] <- max.ES
      }else{
        ES.matrix[mkr,rnk] <- min.ES
      }
    }      # loop end num.subcls.rnk
  }        # loop end num.subcls.mkr
  return(ES.matrix)
}  # end of submap

