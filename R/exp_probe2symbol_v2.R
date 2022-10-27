exp_probe2symbol_v2=function(datExpr,anno=NULL,method=c('mean','median','max','min')[1],GPL=NULL,rm_muti=T){

  if(is.null(anno)&!is.null(GPL)){
    if(file.exists(paste0(MG_Grobal_baseFolder,'/source/GPL_anno/',GPL,'-tbl-1.txt.Rdata'))){
      load(paste0(MG_Grobal_baseFolder,'/source/GPL_anno/',GPL,'-tbl-1.txt.Rdata'))
      load(paste0(MG_Grobal_baseFolder,'/source/gene_convert.Rdata'))
      symb=Symbol2Symbol[match(dat.anno$GeneID,Symbol2Symbol$V1),2]
      anno=data.frame(dat.anno$ProbeID,symb)
    }else if(GPL=='pd.hugene.1.0.st.v1'|GPL=='GPL6244'){
      library(hugene10sttranscriptcluster.db)
      anno=toTable(hugene10sttranscriptclusterSYMBOL)

    }else if(GPL=='pd.hta.2.0'){
      library(hta20transcriptcluster.db)
      anno=toTable(hta20transcriptclusterSYMBOL)
    }else if(GPL=='pd.huex.1.0.st.v2'){
      #print('======')
      load(paste0(MG_Grobal_baseFolder,'/source/GPL_anno/pd.huex.1.0.st.v2.Rdata'))
      load(paste0(MG_Grobal_baseFolder,'/source/gene_convert.Rdata'))
      symb=Symbol2Symbol[match(dat.anno[,2],Symbol2Symbol$V1),2]
      anno=data.frame(dat.anno[,1],symb)
      #print(anno)
    }else{
      print(paste0('Not Found platform annotation:',GPL))
      return(NULL)
    }
  }
  else if(is.null(anno)&is.null(GPL)){
    return(datExpr)
  }
  merge.gene.exp=merge_data_by_group(datExpr,anno,method = method,rm_muti=rm_muti)
  #row.names(merge.gene.exp)=genes
  #colnames(merge.gene.exp)=colnames(datExpr)
  return(merge.gene.exp)
}
