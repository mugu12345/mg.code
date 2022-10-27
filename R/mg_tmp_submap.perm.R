mg_tmp_submap.perm <- function(data.mkr,data.rnk,cand.cls.label.mkr,cand.cls.label.rnk,ntag=100,nperm=1000,weighted.score.type=1)
{
  # cls label, box
  #data.mkr=data.A
  #data.rnk=data.B
  #cand.cls.label.mkr=cand.cls.label.A
  #cand.cls.label.rnk=cand.cls.label.B

  mkr.table.clustid<-table(cand.cls.label.mkr)
  cand.cls.box.mkr<-as.numeric(rownames(mkr.table.clustid))

  rnk.table.clustid<-table(cand.cls.label.rnk)
  cand.cls.box.rnk<-as.numeric(rownames(rnk.table.clustid))

  num.subcls.mkr<-length(cand.cls.box.mkr)
  num.subcls.rnk<-length(cand.cls.box.rnk)

  # initialize
  perm.ES.matrices<-array(0,c(num.subcls.mkr,num.subcls.rnk,(nperm+1)))


  # observed ES matrix
  ES.matrix<-matrix(0,nrow=num.subcls.mkr,ncol=num.subcls.rnk)

  perm.ES.matrices[,,1]<-mg_tmp_submap(data.mkr,data.rnk,cand.cls.label.mkr,cand.cls.label.rnk,ES.matrix,num.subcls.mkr,num.subcls.rnk,ntag,weighted.score.type)

  # permutated ES matrices
  for (i in 2:(nperm+1))
  {
    print(paste("start perm ",i,"-1",sep=""))
    perm.cand.cls.label.rnk<-sample(cand.cls.label.rnk)
    perm.ES.matrices[,,i]<-mg_tmp_submap(data.mkr,data.rnk,cand.cls.label.mkr,perm.cand.cls.label.rnk,ES.matrix,num.subcls.mkr,num.subcls.rnk,ntag,weighted.score.type)
  }

  return(perm.ES.matrices)
}    ## end of submap.perm (Main)
