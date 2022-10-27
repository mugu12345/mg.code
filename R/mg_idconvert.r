mg_idconvert=function(ids){
  #ids=ids[512:591]
  #ids='BRCA1'
  pids=paste0(ids,collapse = ',')
  r <- httr::POST(paste0(MG_Grobal_baseURL,"/pubmed/oldIdsToGeneIdAllType"), body = list(oldIds = pids),encode = "form")
  status=httr::http_status(r)
  if(status$category=='Success'){
    parseByDB=function(smpls,db){
      gs=names(smpls)
      search.ones=rbind()
      if(length(gs)>0){
        for(i in 1:length(gs)){
          mp=smpls[[i]]
          for(j in 1:mp$genesList$searchTotal){
            shd=mp$genesList$searchData[j,]
            search.ones=rbind(search.ones,c(gs[i],unlist(shd$name),unlist(shd$gene2AliasesEntrezGeneSet),unlist(shd$geneID),db))
          }
        }
      }
      return(search.ones)
    }

    text=httr::content(r,'text')
    text=jsonlite::fromJSON(text)
    if(text$status!=200){
      return(NULL)
    }
    mapAll=rbind(
      parseByDB(text$res$aggs$Genebank2Symbol,'GeneBank')
      ,parseByDB(text$res$aggs$ENSEMBLProtein2Symbol,'ENSGP')
      ,parseByDB(text$res$aggs$Protein2Symbol,'Uniprot')
      ,parseByDB(text$res$aggs$Ensembl2Symbol,'ENSG')
      ,parseByDB(text$res$aggs$REFSEQ2Symbol,'Refseq')
      ,parseByDB(text$res$aggs$ENSEMBLmRNA2Symbol,'ENST')
      ,parseByDB(text$res$aggs$REFSEQProteins2Symbol,'RefseqP')
      ,parseByDB(text$res$aggs$Entrez2Symbol,'Entrez')
      ,parseByDB(text$res$aggs$Symbol2Symbol,'SYMBOL')
    )
    if(is.null(mapAll)||nrow(mapAll)==0){
      return(list(MapAll=NULL,IDMap=NULL,MutiMap=NULL))
    }
    colnames(mapAll)=c('SearchID','GeneSymbol','EntrezID','MG_ID','Database')
    dbs=table(mapAll[,4])
    db.inds=order(dbs,decreasing = T)

    tb.cout=table(mapAll[,1])
    tb.mgs=names(tb.cout)[which(tb.cout>1)]
    tb.ugs=names(tb.cout)[which(tb.cout==1)]
    tb.mgs.uiq=rbind()
    for(g in tb.mgs){
      gm=mapAll[which(mapAll[,1]==g),]
      gm.inds=match(gm[4],names(dbs)[db.inds])
      tb.mgs.uiq=rbind(tb.mgs.uiq,gm[gm.inds[!is.na(gm.inds)][1],])
    }
    if(length(tb.mgs)>0){
      colnames(tb.mgs.uiq)=c('SearchID','GeneSymbol','EntrezID','MG_ID','Database')
      uni.map=rbind(
        mapAll[match(tb.ugs,mapAll[,1]),]
        ,tb.mgs.uiq)
    }else{
      inds=match(tb.ugs,mapAll[,1])
      if(length(inds)==1){
        uni.map=rbind(mapAll[inds,])
      }else{
        uni.map=mapAll[inds,]
      }
    }
    if(nrow(uni.map)>1){
      uni.map=cbind(BG=ids,uni.map[match(ids,uni.map[,1]),])
    }else{
      uni.map=cbind(BG=ids,uni.map)
    }
    return(list(MapAll=mapAll,IDMap=uni.map,MutiMap=mapAll[mapAll[,1]%in%tb.mgs,]))
  }
  return(NULL)
}
#mg_idconvert(c('ENSG00000148584','Q9NQ94','CR749684.1','29974','ENST00000600686','ENSP00000470909','NP_570602.2','NM_144670.6','AKAP12'))
