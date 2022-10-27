enrichmentNTA=function(genes,minNum=10,maxNum = 500, sigMethod = "top", fdrMethod = "BH", fdrThr = 0.5,
                       topThr = 10, reportNum = 20, perNum = 1000){
  #library('WebGestaltR')
  refFile <- system.file("extdata", "referenceGenes.txt", package="WebGestaltR")
  refGenes=read.csv(refFile,sep = '\t',stringsAsFactors = F,header = F)[,1]
  cm.genes=intersect(genes,refGenes)
  if(length(cm.genes)<length(genes)*0.1){
    library(hgu133plus2.db)
    columns(hgu133plus2.db)
    tryCatch({
      eid=AnnotationDbi::select(hgu133plus2.db,keys = genes,keytype = 'ENTREZID',columns = c('SYMBOL'))
      genes1=unique(eid$SYMBOL)
      cm.genes1=intersect(genes1,refGenes)
    },error=function(e){cm.genes1=c();
    genes1=c()
    })
    tryCatch({
      eid=AnnotationDbi::select(hgu133plus2.db,keys = genes,keytype = 'ENSEMBL',columns = c('SYMBOL'))
      genes2=unique(eid$SYMBOL)
      cm.genes2=intersect(genes2,refGenes)
    },error=function(e){
      cm.genes2=c();
      genes2=c()
    })
    if(length(genes1)>length(genes2)){
      genes3=genes1
    }else{
      genes3=genes2
    }
    cm.genes1=intersect(genes3,refGenes)
    if(length(cm.genes)<length(cm.genes1)){
      genes=genes3
      cm.genes=cm.genes1
    }
  }
  if(length(cm.genes)>0){
    fld=paste0(tempfile())
    dir.create(fld)
    tmp=paste0(fld,'/gene.txt')
    write.table(data.frame(genes),file=tmp,row.names = F,col.names = F,sep = '\t')
    result=rbind()
    print('network_PPI_BIOGRID')
    enrichResult <- WebGestaltR::WebGestaltR(enrichMethod="NTA", organism="hsapiens"
                                             ,enrichDatabase="network_PPI_BIOGRID"
                                             ,highlightSeedNum=10
                                             ,networkConstructionMethod="Network_Retrieval_Prioritization"
                                             ,minNum=minNum,maxNum = maxNum, sigMethod = sigMethod, fdrMethod = fdrMethod, fdrThr = fdrThr,
                                             topThr = topThr, reportNum = reportNum, perNum = perNum
                                             , interestGeneFile=tmp,
                                             interestGeneType="genesymbol", isOutput=F,
                                             outputDirectory=fld, projectName=NULL)
    if(!is.null(enrichResult)){
      enrichResult$DB=rep('network_PPI_BIOGRID',nrow(enrichResult))
      result=rbind(result,enrichResult)
    }
    unlink(fld,recursive = T)
    #detach('package:WebGestaltR')
    return(result)
  }else{
    #detach('package:WebGestaltR')
    return(data.frame())
  }
}
