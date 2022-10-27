mg_submap=function(original.data.A,original.data.B,original.data.A.group,original.data.B.group
                   ,ntag=100,nperm=50,nperm.fisher=1000,weighted.score.type=1,null.dist=c('each',"pool")[2]
                   #,output.filename="SubMap"
                   #,p.corr="FDR",clust.row=1,clust.col=1,nom.p.mat="T",create.legend="T"
                   ,rnd.seed=47365321){
  #original.data.A
  #original.data.A.group
  #original.data.B
  #original.data.B.group

  set.seed(rnd.seed)
  ntag<-as.integer(ntag)
  nperm<-as.integer(nperm)
  nperm.fisher<-as.integer(nperm.fisher)
  weighted.score.type<-as.integer(weighted.score.type)
  rnd.seed<-as.numeric(rnd.seed)
  if (clust.row=="NA"){clust.row<-NA}else{clust.row<-as.integer(clust.row)}
  if (clust.col=="NA"){clust.col<-NA}else{clust.col<-as.integer(clust.col)}
  null.dist.fig <- "F"   # null distribution for Fisher statistic
  # Read input data & clustids
  #original.data.A<-read.gct(filename=input.data.A)
  #original.data.B<-read.gct(filename=input.data.B)

  cmp.gs=intersect(row.names(original.data.A),row.names(original.data.B))
  data.A<-original.data.A[match(cmp.gs,row.names(original.data.A)),]
  data.B<-original.data.B[match(cmp.gs,row.names(original.data.B)),]
  num.intersection<-length(data.A[,1])
  if (num.intersection<=ntag){stop(paste("### Number of intersection genes is ",num.intersection," (too small) ###",sep=""))}
  cand.cls.label.A=as.numeric(as.factor(original.data.A.group))
  cand.cls.label.B=as.numeric(as.factor(original.data.B.group))
  mp_a=crbind2DataFrame(unique(cbind(as.character(original.data.A.group),cand.cls.label.A)))
  mp_b=crbind2DataFrame(unique(cbind(as.character(original.data.B.group),cand.cls.label.B)))
  mp_a=mp_a[order(mp_a[,2]),]
  mp_b=mp_b[order(mp_b[,2]),]

  # Read clustid
  #cand.cls.label.A<-read.cls(filename=input.cls.A)
  #cand.cls.label.B<-read.cls(filename=input.cls.B)

  # Mutual mapping to generate ES matrices
  ES.1<-mg_tmp_submap.perm(data.A,data.B,cand.cls.label.A,cand.cls.label.B,ntag,nperm,weighted.score.type)
  ES.2<-mg_tmp_submap.perm(data.B,data.A,cand.cls.label.B,cand.cls.label.A,ntag,nperm,weighted.score.type)

  n.row<-length(ES.1[,1,1])
  n.col<-length(ES.1[1,,1])

  # Convert ES matrices into nominal p-values
  nom.p.matrix.1<-array(9,c(n.row,n.col,(nperm+1)))
  nom.p.matrix.2<-array(9,c(n.col,n.row,(nperm+1)))

  if (null.dist=="each")
  {
    for (r in 1:n.row)
    {
      for (c in 1:n.col)
      {
        each.ES.1<-ES.1[r,c,]
        each.ES.2<-ES.2[c,r,]
        ES.rank.1<-rank(each.ES.1)
        ES.rank.2<-rank(each.ES.2)
        nom.p.matrix.1[r,c,]<-((nperm+2-ES.rank.1)/(nperm+1))
        nom.p.matrix.2[c,r,]<-((nperm+2-ES.rank.2)/(nperm+1))
      }
    }
  }

  if (null.dist=="pool")
  {
    perm.ES.vector.1<-as.vector(ES.1[,,2:(nperm+1)])
    perm.ES.vector.2<-as.vector(ES.2[,,2:(nperm+1)])
    num.element<-length(perm.ES.vector.1)
    nom.p.perm.ES.vector.1<-((num.element+2-rank(perm.ES.vector.1))/(num.element+1))
    nom.p.perm.ES.vector.2<-((num.element+2-rank(perm.ES.vector.2))/(num.element+1))
    nom.p.perm.ES.1<-array(nom.p.perm.ES.vector.1,c(n.row,n.col,nperm))
    nom.p.perm.ES.2<-array(nom.p.perm.ES.vector.2,c(n.col,n.row,nperm))

    for (r in 1:n.row)
    {
      for (c in 1:n.col)
      {
        rank.comb.ES.vector.1<-rank(c(ES.1[r,c,1],perm.ES.vector.1))
        rank.comb.ES.vector.2<-rank(c(ES.2[c,r,1],perm.ES.vector.2))
        nom.p.matrix.1[r,c,1]<-((num.element+2-rank.comb.ES.vector.1[1])/(num.element+1))
        nom.p.matrix.2[c,r,1]<-((num.element+2-rank.comb.ES.vector.2[1])/(num.element+1))
      }
    }
    nom.p.matrix.1[,,2:(nperm+1)]<-nom.p.perm.ES.1
    nom.p.matrix.2[,,2:(nperm+1)]<-nom.p.perm.ES.2
  }
  # compute nominal-p of Fisher's statistics from "each cell" or "pool of cells"
  nom.fisher.p.matrix<-matrix(9,nrow=n.row,ncol=n.col)
  each.perm.fisher.stat<-vector(length=nperm.fisher,mode="numeric")

  if (null.dist=="each")
  {
    for (r in 1:n.row)
    {
      for (c in 1:n.col)
      {
        obs.fisher<-(-log(nom.p.matrix.1[r,c,1])-log(nom.p.matrix.2[c,r,1]))
        perm.p.pool.1<-nom.p.matrix.1[r,c,2:(nperm+1)]
        perm.p.pool.2<-nom.p.matrix.2[c,r,2:(nperm+1)]
        for (f in 1:nperm.fisher)
        {
          rand.nom.p.1<-sample(perm.p.pool.1,1,replace=T)
          rand.nom.p.2<-sample(perm.p.pool.2,1,replace=T)
          each.perm.fisher.stat[f]<-(-log(rand.nom.p.1)-log(rand.nom.p.2))
        }
        comb.vector<-c(obs.fisher,each.perm.fisher.stat)
        fisher.rank<-rank(comb.vector)
        nom.fisher.p.matrix[r,c]<-((nperm.fisher+2-fisher.rank[1])/(nperm.fisher+1))
      }
    }
  }

  if (null.dist=="pool")
  {
    perm.p.pool.1<-as.vector(nom.p.matrix.1[,,2:(nperm+1)])
    perm.p.pool.2<-as.vector(nom.p.matrix.2[,,2:(nperm+1)])
    for (f in 1:nperm.fisher)
    {
      rand.nom.p.1<-sample(perm.p.pool.1,1,replace=T)
      rand.nom.p.2<-sample(perm.p.pool.2,1,replace=T)
      each.perm.fisher.stat[f]<-(-log(rand.nom.p.1)-log(rand.nom.p.2))
    }

    for (r in 1:n.row)
    {
      for (c in 1:n.col)
      {
        obs.fisher<-(-log(nom.p.matrix.1[r,c,1])-log(nom.p.matrix.2[c,r,1]))
        comb.vector<-c(obs.fisher,each.perm.fisher.stat)
        fisher.rank<-rank(comb.vector)
        nom.fisher.p.matrix[r,c]<-((nperm.fisher+2-fisher.rank[1])/(nperm.fisher+1))
      }
    }
  }


  # p-value correction (SA matrix)
  bonf.SA.matrix<-fdr.SA.matrix<-matrix(9,nrow=n.row,ncol=n.col)
  bonf.SA.matrix<-nom.fisher.p.matrix*(n.row*n.col)
  bonf.SA.matrix[bonf.SA.matrix>1]<-1
  rank.matrix<-matrix(rank(nom.fisher.p.matrix),nc=length(nom.fisher.p.matrix[1,]))
  fdr.SA.matrix<-nom.fisher.p.matrix*(n.row*n.col)/rank.matrix
  fdr.SA.matrix[fdr.SA.matrix>1]<-1
  # output: text
  header=rbind("*** Subbclass Mappping Results ***","",
               paste("input.data.A:  Genes=",nrow(original.data.A),',Samples=',ncol(original.data.A),sep=""),
               paste("input.data.B:  Genes=",nrow(original.data.B),',Samples=',ncol(original.data.B),sep=""),
               paste("# of intersection genes:  ",num.intersection,sep=""),
               paste("input.cls.A:  ",paste0(unique(original.data.A.group),collapse = ','),sep=""),
               paste("input.cls.B:  ",paste0(unique(original.data.B.group),collapse = ','),sep=""),
               paste("# of marker genes:  ",ntag,sep=""),
               paste("# of permutation for nominal-p of ES:  ",nperm,sep=""),
               paste("# of permutation for nominal-p of Fisher's statistics:  ",nperm.fisher,sep=""),
               paste("SNR weight for ES (1=yes, 0=no):  ",weighted.score.type,sep=""),
               paste("choice of null distribution:  ",null.dist,sep=""),
               #paste("p-value correction method (for Fisher's statistics):  ",p.corr,sep=""),
               ""
  )
  print(header)
  #outputTable.file <- paste(output.filename,"_SubMapResult.txt",sep="")

  #write.table(header,outputTable.file,quote=F,sep="\t",row.names=F,col.names=F)

  #row.label<-paste("A",as.character(seq(1:n.row)),sep="")
  row.label<-mp_a[,1]
  #col.label<-paste("B",as.character(seq(1:n.col)),sep="")
  col.label<-mp_b[,1]

  rownames(bonf.SA.matrix)<-rownames(fdr.SA.matrix)<-rownames(nom.fisher.p.matrix)<-rownames(nom.p.matrix.1)<-colnames(nom.p.matrix.2)<-row.label

  colnames(bonf.SA.matrix)<-colnames(fdr.SA.matrix)<-colnames(nom.fisher.p.matrix)<-colnames(nom.p.matrix.1)<-rownames(nom.p.matrix.2)<-col.label

  results<-list(Bonferroni.SA.matrix=as.data.frame(bonf.SA.matrix)
                ,FDR.SA.matrix=as.data.frame(fdr.SA.matrix)
                ,nominal.p.matrix.Fisher=as.data.frame(nom.fisher.p.matrix)
                ,nominal.p.matrix.ES.A.on.B=as.data.frame(nom.p.matrix.1[,,1])
                ,nominal.p.matrix.ES.B.on.A=as.data.frame(t(nom.p.matrix.2[,,1]))
                ,each.perm.fisher.stat=each.perm.fisher.stat)
  return(results)

  #################
  # if (p.corr=="Bonferroni")
  # {
  #
  #   results<-list(SA.matrix=as.data.frame(bonf.SA.matrix)
  #                 ,nominal.p.matrix.Fisher=as.data.frame(nom.fisher.p.matrix)
  #                 ,nominal.p.matrix.ES.A.on.B=as.data.frame(nom.p.matrix.1[,,1])
  #                 ,nominal.p.matrix.ES.B.on.A=as.data.frame(t(nom.p.matrix.2[,,1])))
  # }
  #
  # if (p.corr=="FDR")
  # {
  #   results<-list(SA.matrix=as.data.frame(fdr.SA.matrix)
  #                 ,nominal.p.matrix.Fisher=as.data.frame(nom.fisher.p.matrix)
  #                 ,nominal.p.matrix.ES.A.on.B=as.data.frame(nom.p.matrix.1[,,1])
  #                 ,nominal.p.matrix.ES.B.on.A=as.data.frame(t(nom.p.matrix.2[,,1])))
  # }
  #
  # if (p.corr=="both")
  # {
  #   results<-list(Bonferroni.SA.matrix=as.data.frame(bonf.SA.matrix)
  #                 ,FDR.SA.matrix=as.data.frame(fdr.SA.matrix)
  #                 ,nominal.p.matrix.Fisher=as.data.frame(nom.fisher.p.matrix)
  #                 ,nominal.p.matrix.ES.A.on.B=as.data.frame(nom.p.matrix.1[,,1])
  #                 ,nominal.p.matrix.ES.B.on.A=as.data.frame(t(nom.p.matrix.2[,,1])))
  # }
  #
  # write.table(capture.output(results),outputTable.file,quote=F,sep="\t",row.names=F,col.names=F,append=T)
  #
  # # output: gct for SA matrix
  # if (p.corr=="Bonferroni")
  # {
  #   mg_out_GCTfile(bonf.SA.matrix,outpath = paste(output.filename,"_Bonferroni_SAmatrix.gct",sep=""))
  # }
  #
  # if (p.corr=="FDR")
  # {
  #   mg_out_GCTfile(fdr.SA.matrix,outpath = paste(output.filename,"_FDR_SAmatrix.gct",sep=""))
  # }
  #
  # if (p.corr=="both")
  # {
  #   mg_out_GCTfile(bonf.SA.matrix,outpath = paste(output.filename,"_Bonferroni_SAmatrix.gct",sep=""))
  #   mg_out_GCTfile(fdr.SA.matrix,outpath = paste(output.filename,"_FDR_SAmatrix.gct",sep=""))
  # }
  #
  # # output: gct for each nominal-p matrices of ES
  # if (nom.p.mat=="T")
  # {
  #   mg_out_GCTfile(nom.p.matrix.1[,,1],outpath = paste(output.filename,"_nominal_p_matrix_AonB.gct",sep=""))
  #   mg_out_GCTfile(nom.p.matrix.2[,,1],outpath = paste(output.filename,"_nominal_p_matrix_BonA.gct",sep=""))
  # }
  # # output: heatmap of SA matrix
  # min.matrix<-max.matrix<-9
  #
  # if (p.corr=="Bonferroni")
  # {
  #   if (min(bonf.SA.matrix)==max(bonf.SA.matrix))
  #   {
  #     min.matrix<-.9999999999
  #     max.matrix<-1
  #   }
  #   else
  #   {
  #     min.matrix<-min(bonf.SA.matrix)
  #     max.matrix<-max(bonf.SA.matrix)
  #   }
  #
  #   if (capabilities("png"))
  #   {
  #     png(paste(output.filename,"_Bonferroni_SAmatrix.png",sep=""))
  #   }
  #   else
  #   {
  #     pdf(paste(output.filename,"_Bonferroni_SAmatrix.pdf",sep=""))
  #   }
  #
  #   heatmap(bonf.SA.matrix,Rowv=clust.row,Colv=clust.col,scale="none",col=rainbow(1000,start=(0+.7*min.matrix),end=.7*max.matrix,alpha=1))
  #
  #   dev.off()
  # }
  #
  # if (p.corr=="FDR")
  # {
  #   if (min(fdr.SA.matrix)==max(fdr.SA.matrix))
  #   {
  #     min.matrix<-.9999999999
  #
  #     max.matrix<-1
  #   }
  #   else
  #   {
  #     min.matrix<-min(fdr.SA.matrix)
  #
  #     max.matrix<-max(fdr.SA.matrix)
  #   }
  #
  #   if (capabilities("png"))
  #   {
  #     png(paste(output.filename,"_FDR_SAmatrix.png",sep=""))
  #   }
  #   else
  #   {
  #     pdf(paste(output.filename,"_FDR_SAmatrix.pdf",sep=""), bg="white")
  #   }
  #
  #   heatmap(fdr.SA.matrix,Rowv=clust.row,Colv=clust.col,scale="none",col=rainbow(1000,start=(0+.7*min.matrix),end=.7*max.matrix,alpha=1))
  #
  #   dev.off()
  # }
  #
  # if (p.corr=="both")
  # {
  #   if (min(bonf.SA.matrix)==max(bonf.SA.matrix))
  #   {
  #     min.matrix<-.9999999999
  #
  #     max.matrix<-1
  #   }
  #   else
  #   {
  #     min.matrix<-min(bonf.SA.matrix)
  #
  #     max.matrix<-max(bonf.SA.matrix)
  #   }
  #
  #   if (capabilities("png"))
  #   {
  #     png(paste(output.filename,"_Bonferroni_SAmatrix.png",sep=""))
  #   }
  #   else
  #   {
  #     pdf(paste(output.filename,"_Bonferroni_SAmatrix.pdf",sep=""))
  #
  #   }
  #
  #   heatmap(bonf.SA.matrix,Rowv=clust.row,Colv=clust.col,scale="none",col=rainbow(1000,start=(0+.7*min.matrix),end=.7*max.matrix,alpha=1))
  #
  #   dev.off()
  #
  #   if (min(fdr.SA.matrix)==max(fdr.SA.matrix))
  #   {
  #     min.matrix<-.9999999999
  #
  #     max.matrix<-1
  #   }
  #   else
  #   {
  #     min.matrix<-min(fdr.SA.matrix)
  #
  #     max.matrix<-max(fdr.SA.matrix)
  #   }
  #
  #   if (capabilities("png"))
  #   {
  #     png(paste(output.filename,"_FDR_SAmatrix.png",sep=""))
  #   }
  #   else
  #   {
  #     pdf(paste(output.filename,"_FDR_SAmatrix.pdf",sep=""))
  #   }
  #
  #   heatmap(fdr.SA.matrix,Rowv=clust.row,Colv=clust.col,scale="none",col=rainbow(1000,start=(0+.7*min.matrix),end=.7*max.matrix,alpha=1))
  #
  #   dev.off()
  # }
  #
  # # output: heatmap of each nominal-p matrices of ES
  # if (nom.p.mat=="T")
  # {
  #   if (capabilities("png"))
  #   {
  #     png(paste(output.filename,"_nominal_p_matrix_AonB.png",sep=""))
  #   }
  #   else
  #   {
  #     pdf(paste(output.filename,"_nominal_p_matrix_AonB.pdf",sep=""))
  #
  #   }
  #
  #   heatmap(nom.p.matrix.1[,,1],Rowv=NA,Colv=NA,scale="none",col=rainbow(1000,start=(0+.7*min(nom.p.matrix.1[,,1])),end=.7*max(nom.p.matrix.1[,,1]),alpha=1))
  #
  #   dev.off()
  #
  #   t.nom.p.matrix.2<-t(nom.p.matrix.2[,,1])
  #
  #   if (capabilities("png"))
  #   {
  #     png(paste(output.filename,"_nominal_p_matrix_BonA.png",sep=""))
  #   }
  #   else
  #   {
  #     pdf(paste(output.filename,"_nominal_p_matrix_BonA.pdf",sep=""))
  #   }
  #
  #   heatmap(t.nom.p.matrix.2,Rowv=NA,Colv=NA,scale="none",col=rainbow(1000,start=(0+.7*min(t.nom.p.matrix.2)),end=.7*max(t.nom.p.matrix.2),alpha=1))
  #
  #   dev.off()
  # }
  #
  # # legend of heatmap
  # if (create.legend=="T")
  # {
  #   if (capabilities("png"))
  #   {
  #     png("legend.png")
  #   }
  #   else
  #   {
  #     pdf("legend.pdf")
  #
  #   }
  #
  #   par(plt=c(.1,.9,.45,.5))
  #
  #   a=matrix(seq(1:1000),nc=1)
  #
  #   image(a,col=rainbow(1000,start=0,end=.7,alpha=1),xlim=c(0,1),yaxt="n")
  #
  #   box()
  #
  #   dev.off()
  # }
  #
  # # histgram of pooled Fisher's statistics
  # if (null.dist.fig=="T")
  # {
  #   if (capabilities("png"))
  #   {
  #     png("null.distribution.of.fisher.png")
  #   }
  #   else
  #   {
  #     pdf("null.distribution.of.fisher.pdf")
  #   }
  #
  #   hist(each.perm.fisher.stat,br=30,probability=T,main="Null distribution of Fisher's statistics (pooled)")
  #
  #   lines(density(each.perm.fisher.stat),col="red",lwd=2)
  #
  #   dev.off()
  # }

  #  return(nom.fisher.p.matrix)
}
