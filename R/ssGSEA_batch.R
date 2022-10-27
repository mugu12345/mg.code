ssGSEA_batch=function(exp,idType='SYMBOL',dbs=c('KEGG','BP','CC','MF','hallmark'),TCGAcode=NULL){
  if(!is.null(TCGAcode)){
    #TCGAcode='LUAD'
    load(paste0('source/ssGSEA/',TCGAcode,'.ssgsea.RData'))
    return(list(KEGG=ssGSEA_TCGA_Matrix$KEGG,BP=ssGSEA_TCGA_Matrix$GO_BP,CC=ssGSEA_TCGA_Matrix$GO_CC,MF=ssGSEA_TCGA_Matrix$GO_MF,ID_Map=ssGSEA_TCGA_Matrix$ID_Map))
  }else{
    id=idType
    #library('GSVA')
    #library(GSEABase)
    all.exp=exp
    baseFolder='source/'
    if(id=='SYMBOL'){
      c2KEGG <- GSEABase::getGmt(paste0(baseFolder,"c2.cp.kegg.v7.0.symbols.gmt"),
                                 collectionType=GSEABase::BroadCollection(category="c2"),
                                 geneIdType=GSEABase::SymbolIdentifier())
      c5BP<-GSEABase::getGmt(paste0(baseFolder,"c5.bp.v7.0.symbols.gmt"),
                             collectionType=GSEABase::BroadCollection(category="c5"),
                             geneIdType=GSEABase::SymbolIdentifier()
      )
      c5CC<-GSEABase::getGmt(paste0(baseFolder,"c5.cc.v7.0.symbols.gmt"),
                             collectionType=GSEABase::BroadCollection(category="c5"),
                             geneIdType=GSEABase::SymbolIdentifier()
      )
      c5MF<-GSEABase::getGmt(paste0(baseFolder,'c5.mf.v7.0.symbols.gmt'),
                             collectionType=GSEABase::BroadCollection(category="c5"),
                             geneIdType=GSEABase::SymbolIdentifier()
      )
      hf<-GSEABase::getGmt(paste0(baseFolder,'h.all.v7.0.symbols.gmt'),
                           collectionType=GSEABase::BroadCollection(category="c7"),
                           geneIdType=GSEABase::SymbolIdentifier()
      )
    }else{
      c2KEGG <- GSEABase::getGmt(paste0(baseFolder,"c2.cp.kegg.v7.0.entrez.gmt"),
                                 collectionType=GSEABase::BroadCollection(category="c2"),
                                 geneIdType=GSEABase::SymbolIdentifier())
      c5BP<-GSEABase::getGmt(paste0(baseFolder,'c5.bp.v7.0.entrez.gmt'),
                             collectionType=GSEABase::BroadCollection(category="c5"),
                             geneIdType=GSEABase::SymbolIdentifier()
      )
      c5CC<-GSEABase::getGmt(paste0(baseFolder,'c5.cc.v7.0.entrez.gmt'),
                             collectionType=GSEABase::BroadCollection(category="c5"),
                             geneIdType=GSEABase::SymbolIdentifier()
      )
      c5MF<-GSEABase::getGmt(paste0(baseFolder,'c5.mf.v7.0.entrez.gmt'),
                             collectionType=GSEABase::BroadCollection(category="c5"),
                             geneIdType=GSEABase::SymbolIdentifier()
      )
      hf<-GSEABase::getGmt(paste0(baseFolder,'h.all.v7.0.symbols.gmt'),
                           collectionType=GSEABase::BroadCollection(category="c7"),
                           geneIdType=GSEABase::SymbolIdentifier()
      )
    }
    if('KEGG' %in% dbs){
      ssGSEA.kegg <- GSVA::gsva(as.matrix(all.exp), c2KEGG,method='ssgsea',
                                min.sz=10, max.sz=500, verbose=TRUE)
    }else{ssGSEA.kegg=NULL;}
    if('BP' %in% dbs){
      ssGSEA.bp <- GSVA::gsva(as.matrix(all.exp), c5BP,method='ssgsea',
                              min.sz=10, max.sz=500, verbose=TRUE)
    }else{ssGSEA.bp=NULL;}
    if('CC' %in% dbs){
      ssGSEA.cc <- GSVA::gsva(as.matrix(all.exp), c5CC,method='ssgsea',
                              min.sz=10, max.sz=500, verbose=TRUE)
    }else{ssGSEA.cc=NULL;}
    if('MF' %in% dbs){
      ssGSEA.mf <- GSVA::gsva(as.matrix(all.exp), c5MF,method='ssgsea',
                              min.sz=10, max.sz=500, verbose=TRUE)
    }else{ssGSEA.mf=NULL;}
    if('hallmark' %in% dbs){
      ssGSEA.hf <- GSVA::gsva(as.matrix(all.exp), hf,method='ssgsea',
                              min.sz=10, max.sz=500, verbose=TRUE)
    }else{ssGSEA.hf=NULL;}
    #detach('package:GSVA')
    #detach('package:GSEABase')

    return(list(KEGG=ssGSEA.kegg,BP=ssGSEA.bp,CC=ssGSEA.cc,MF=ssGSEA.mf,hallmark=ssGSEA.hf))
  }
}
