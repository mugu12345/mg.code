mg_idconvert_local=function(ids,batch=T){
  if(batch){
    baseFolder=paste0(MG_Grobal_baseFolder,'/source/')
    load(paste0(baseFolder,'gene_convert.Rdata'))
  }
  GeneBank=genebank2symbol[genebank2symbol[,2]%in%ids,]
  ENST=ENSEMBLmRNA2Symbol[ENSEMBLmRNA2Symbol[,2]%in%ids,]
  Refseq=REFSEQ2Symbol[REFSEQ2Symbol[,2]%in%ids,]
  ENSG=Ensembl2Symbol[Ensembl2Symbol[,2]%in%ids,]
  Entrez=Entrez2Symbol[Entrez2Symbol[,2]%in%ids,]
  Uniprot=Protein2Symbol[Protein2Symbol[,2]%in%ids,]
  ENSGP=ENSEMBLProtein2Symbol[ENSEMBLProtein2Symbol[,2]%in%ids,]
  RefseqP=REFSEQProteins2Symbol[REFSEQProteins2Symbol[,2]%in%ids,]
  SYMBOL=Symbol2Symbol[Symbol2Symbol[,2]%in%ids,]

  mach_all=rbind(cbind(GeneBank[,1],GeneBank[,2],rep('GeneBank',nrow(GeneBank))),
                 cbind(ENST[,1],ENST[,2],rep('ENST',nrow(ENST))),
                 cbind(Refseq[,1],Refseq[,2],rep('Refseq',nrow(Refseq))),
                 cbind(ENSG[,1],ENSG[,2],rep('ENSG',nrow(ENSG))),
                 cbind(Entrez[,1],Entrez[,2],rep('Entrez',nrow(Entrez))),
                 cbind(Uniprot[,1],Uniprot[,2],rep('Uniprot',nrow(Uniprot))),
                 cbind(ENSGP[,1],ENSGP[,2],rep('ENSGP',nrow(ENSGP))),
                 cbind(RefseqP[,1],RefseqP[,2],rep('RefseqP',nrow(RefseqP))),
                 cbind(SYMBOL[,1],SYMBOL[,2],rep('SYMBOL',nrow(SYMBOL)))
  )
  #head(mach_all)
  if(!is.null(mach_all)&nrow(mach_all)>0){
    mach_all=data.frame(SearchID=mach_all[,2],
                        GeneSymbol=Symbol2Symbol[match(mach_all[,1],Symbol2Symbol[,1]),2],
                        EntrezID=Entrez2Symbol[match(mach_all[,1],Entrez2Symbol[,1]),2],
                        MG_ID=mach_all[,1],
                        Database=mach_all[,3],stringsAsFactors = F)
  }else{
    return(NULL)
  }
  mapAll=mach_all

  dbs=table(mapAll[,5])
  db.inds=order(dbs,decreasing = T)

  tb.cout=table(mapAll[,1])
  tb.mgs=names(tb.cout)[which(tb.cout>1)]
  tb.ugs=names(tb.cout)[which(tb.cout==1)]
  tb.mgs.uiq=rbind()
  for(g in tb.mgs){
    gm=mapAll[which(mapAll[,1]==g),]
    gm.inds=match(gm[,5],names(dbs)[db.inds])
    tb.mgs.uiq=rbind(tb.mgs.uiq,gm[gm.inds[!is.na(gm.inds)][1],])
  }
  if(length(tb.mgs)){
    colnames(tb.mgs.uiq)=c('SearchID','GeneSymbol','EntrezID','MG_ID','Database')
    uni.map=rbind(
      mapAll[match(tb.ugs,mapAll[,1]),]
      ,tb.mgs.uiq)
  }else{
    uni.map=mapAll[match(tb.ugs,mapAll[,1]),]
  }
  uni.map=cbind(BG=ids,uni.map[match(ids,uni.map[,1]),])
  return(list(MapAll=mapAll,IDMap=uni.map,MutiMap=mapAll[mapAll[,1]%in%tb.mgs,]))

}

