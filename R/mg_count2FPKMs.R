mg_count2FPKMs=function(exp,gff3=NULL,gene_len=NULL){
  cut=c("__alignment_not_unique","__not_aligned","__too_low_aQual","__ambiguous","__no_feature")
  exp=exp[which(!row.names(exp)%in%cut),]

  if(is.null(gff3)&!is.null(gene_len)){
    exp1=exp[row.names(exp)%in%gene_len[,1],]
    ct=apply(exp1,2,sum)
    anno=gene_len[match(row.names(exp1),gene_len[,1]),]
    lens=anno[,2]
    exp1=exp1/lens
    exp1=t(t(exp1)/ct)
    exp1=exp1*1e9
    return(exp1)
  }else if(is.null(gff3)){
    anno=readMatrix(paste0(MG_Grobal_baseFolder,'/source/gencode.v22.ensg.genelen.tab'))
    row.names(anno)=gsub('\\..*','',row.names(anno))
    row.names(exp)=gsub('\\..*','',row.names(exp))
    exp1=exp[row.names(exp)%in%row.names(anno),]
    anno=anno[match(row.names(exp1),row.names(anno)),]
    if(nrow(anno)==60483){
      ct=apply(exp1[which(anno[,2]=='protein_coding'),],2,sum)
    }else{
      ct=apply(exp1,2,sum)
    }
    lens=anno[,4]
    exp1=exp1/lens
    exp1=t(t(exp1)/ct)
    exp1=exp1*1e9
    return(exp1)
  }else{
    library(GenomicFeatures)
    txdb <- makeTxDbFromGFF(gff3,format="gff3")
    exons_gene <- exonsBy(txdb, by = "gene")
    exons_gene_lens <- lapply(exons_gene,function(x){sum(width(reduce(x)))})
    exons_gene_lens=unlist(exons_gene_lens)
    exp=exp[row.names(exp)%in%names(exons_gene_lens),]
    ct=apply(exp,2,sum)
    lens=exons_gene_lens[match(row.names(exp),names(exons_gene_lens))]
    exp1=exp/lens
    exp1=t(t(exp1)/ct)
    exp1=exp1*1e9
    return(exp1)
  }
}




