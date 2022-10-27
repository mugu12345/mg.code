mg_clusterProfiler=function(genes,keytype='SYMBOL',minGSSize=10,maxGSSize = 500, pAdjustMethod = "BH",pvalueCutoff=0.05,qvalueCutoff = 0.5){
  library(org.Hs.eg.db)
  library(clusterProfiler)
  eg <- bitr(genes, fromType=keytype, toType=c("ENTREZID","ENSEMBL"), OrgDb="org.Hs.eg.db");

  if(!file.exists(paste0('source/genome_info/KEGG_Map.RData'))){
    kegg.idmap=mg_mysql_query_248('SELECT * FROM `gene2keggmap` WHERE gene2keggmap.GeneID IS NOT NULL')
    kegg.idmap=kegg.idmap[,c('KEGGName','GeneID','pathwayid','info','one_type','two_type')]
    kegg.idmap[,1]=gsub('hsa:','',kegg.idmap[,1])
    save(kegg.idmap,file=paste0('source/genome_info/KEGG_Map.RData'))
  }else{
    load(file=paste0('source/genome_info/KEGG_Map.RData'))
  }
  #print('加载KEGG数据库完成，正在进行KEGG富集分析...')
  print('start KEGG....')
  kegg_enrich=clusterProfiler::enricher(eg$ENTREZID, pvalueCutoff = pvalueCutoff
                                        , pAdjustMethod = pAdjustMethod
                                        ,minGSSize = minGSSize, maxGSSize = maxGSSize
                                        , qvalueCutoff = qvalueCutoff
                                        ,TERM2GENE=data.frame(term =kegg.idmap[,3],gene=kegg.idmap[,1]),
                                        TERM2NAME = data.frame(term =kegg.idmap[,3],name=kegg.idmap[,4]))

  print('start GO....')
  if(!file.exists(paste0('source/genome_info/GO_Map.RData'))){
    library(org.Hs.eg.db)
    symbols <- keys(org.Hs.eg.db, keytype="ENTREZID")
    goAnno <- select(org.Hs.eg.db, keys = symbols, keytype = 'ENTREZID', columns = c("GOALL", "ONTOLOGYALL"))
    goAnno <- unique(goAnno[!is.na(goAnno$GOALL), ])
    GO2GENE.bp <- unique(goAnno[goAnno$ONTOLOGYALL == 'BP',
                                c(2, 1)])
    g2n=go2term(GO2GENE.bp[,1])
    GO2GENE.bp$Name=g2n[match(GO2GENE.bp[,1],g2n[,1]),2]
    GO2GENE.cc <- unique(goAnno[goAnno$ONTOLOGYALL == 'CC',
                                c(2, 1)])
    g2n=go2term(GO2GENE.cc[,1])
    GO2GENE.cc$Name=g2n[match(GO2GENE.cc[,1],g2n[,1]),2]

    GO2GENE.mf <- unique(goAnno[goAnno$ONTOLOGYALL == 'MF',
                                c(2, 1)])
    g2n=go2term(GO2GENE.mf[,1])
    GO2GENE.mf$Name=g2n[match(GO2GENE.mf[,1],g2n[,1]),2]
    GO2GeneData=list(BP=GO2GENE.bp,CC=GO2GENE.cc,MF=GO2GENE.mf)

    save(GO2GeneData,file=paste0('source/genome_info/GO_Map.RData'))
  }else{
    load(file=paste0('source/genome_info/GO_Map.RData'))
  }
  print('Succ KEGG,Start GO_BP')
  bp_enrich=clusterProfiler::enricher(eg$ENTREZID, pvalueCutoff = pvalueCutoff
                                      , pAdjustMethod = pAdjustMethod
                                      ,minGSSize = minGSSize, maxGSSize = maxGSSize
                                      , qvalueCutoff = qvalueCutoff
                                      ,TERM2GENE=data.frame(term =GO2GeneData$BP[,1],gene=GO2GeneData$BP[,2]),
                                      TERM2NAME = data.frame(term =GO2GeneData$BP[,1],name=GO2GeneData$BP[,3]))
  print('Succ GP_BP,Start GO_CC')
  cc_enrich=clusterProfiler::enricher(eg$ENTREZID, pvalueCutoff = pvalueCutoff
                                      , pAdjustMethod = pAdjustMethod
                                      ,minGSSize = minGSSize, maxGSSize = maxGSSize
                                      , qvalueCutoff = qvalueCutoff
                                      ,TERM2GENE=data.frame(term =GO2GeneData$CC[,1],gene=GO2GeneData$CC[,2]),
                                      TERM2NAME = data.frame(term =GO2GeneData$CC[,1],name=GO2GeneData$CC[,3]))
  print('Succ GO_CC,Start GO_MF')
  mf_enrich=clusterProfiler::enricher(eg$ENTREZID, pvalueCutoff = pvalueCutoff
                                      , pAdjustMethod = pAdjustMethod
                                      ,minGSSize = minGSSize, maxGSSize = maxGSSize
                                      , qvalueCutoff = qvalueCutoff
                                      ,TERM2GENE=data.frame(term =GO2GeneData$MF[,1],gene=GO2GeneData$MF[,2]),
                                      TERM2NAME = data.frame(term =GO2GeneData$MF[,1],name=GO2GeneData$MF[,3]))
  if(nrow(kegg_enrich)>0){
    kegg_enrich=setReadable(kegg_enrich,OrgDb = 'org.Hs.eg.db',keyType = 'ENTREZID')
  }
  if(nrow(bp_enrich)>0){
    bp_enrich=setReadable(bp_enrich,OrgDb = 'org.Hs.eg.db',keyType = 'ENTREZID')
  }
  if(nrow(cc_enrich)>0){
    cc_enrich=setReadable(cc_enrich,OrgDb = 'org.Hs.eg.db',keyType = 'ENTREZID')
  }
  if(nrow(mf_enrich)>0){
    mf_enrich=setReadable(mf_enrich,OrgDb = 'org.Hs.eg.db',keyType = 'ENTREZID')
  }

  enrich_tab=rbind(cbind(summary(kegg_enrich),DB=rep('pathway_KEGG',nrow(kegg_enrich)))
                   ,cbind(summary(bp_enrich),DB=rep('geneontology_Biological_Process',nrow(bp_enrich)))
                   ,cbind(summary(cc_enrich),DB=rep('geneontology_Cellular_Component',nrow(cc_enrich)))
                   ,cbind(summary(mf_enrich),DB=rep('geneontology_Molecular_Function',nrow(mf_enrich)))
  )
  enrich_tab=crbind2DataFrame(enrich_tab)
  colnames(enrich_tab)[c(2,5,6,9)]=c('description','pValue','FDR','size')
  enrich_tab$enrichmentRatio=unlist(lapply(strsplit(enrich_tab[,3],'/'), function(x){
    x1=as.numeric(x)
    return(x1[1]/x1[2])
  }))
  #enrich_tab=enrich_tab[which(enrich_tab[,5]<pCut&enrich_tab[,6]<fdrCutoff&enrich_tab[,7]<pvalueCutoff),]
  return(list(KEGG=kegg_enrich,GO_BP=bp_enrich,GO_CC=cc_enrich,GO_MF=mf_enrich
              ,Enrich_tab=enrich_tab,PathwayClass=unique(kegg.idmap[,c(3,5,6)])))
}
