
mg_idconvert_ensg2ensgv33=function(ensgs){
  load(paste0(MG_Grobal_baseFolder,'/source/gencode.v33.id.tab.RData'))
  unensg=setdiff(ensgs,gencode.v33.id.tab[,2])
  ndt=cbind(ET=gencode.v33.id.tab[,2],gencode.v33.id.tab)
  all.ensg.sym=rbind()
  unensg3=NULL
  if(length(unensg)>0){
    library(EnsDb.Hsapiens.v79)
    inputs.cv=ensembldb::select(EnsDb.Hsapiens.v79, keys= unensg
                                , keytype = "GENEID", columns = c("GENEID","SYMBOL","ENTREZID"))
    unensg1=setdiff(unensg,inputs.cv$GENEID)
    all.ensg.sym=rbind(all.ensg.sym,inputs.cv)
    if(length(unensg1)>0){
      library(EnsDb.Hsapiens.v86)
      inputs.cv=ensembldb::select(EnsDb.Hsapiens.v86, keys= unensg1
                                  , keytype = "GENEID", columns = c("GENEID","SYMBOL","ENTREZID"))
      unensg2=setdiff(unensg1,inputs.cv$GENEID)
      all.ensg.sym=rbind(all.ensg.sym,inputs.cv)
      if(length(unensg2)>0){
        library(EnsDb.Hsapiens.v75)
        inputs.cv=ensembldb::select(EnsDb.Hsapiens.v75, keys= unensg2
                                    , keytype = "GENEID", columns = c("GENEID","SYMBOL","ENTREZID"))
        unensg3=setdiff(unensg2,inputs.cv$GENEID)
        all.ensg.sym=rbind(all.ensg.sym,inputs.cv)
      }
    }
  }
  if(!is.null(all.ensg.sym)){
    all.ensg.sym=crbind2DataFrame(all.ensg.sym)
    all.ensg.sym$MG_ID=NA
    if(sum(!is.na(all.ensg.sym[,3]))>0){
      idv1=mg_idconvert_local(all.ensg.sym[!is.na(all.ensg.sym[,3]),3])
      gid2mgid=crbind2DataFrame(cbind(idv1$IDMap[!is.na(idv1$IDMap$MG_ID),1],idv1$IDMap$MG_ID[!is.na(idv1$IDMap$MG_ID)]))
      all.ensg.sym$MG_ID=gid2mgid[match(all.ensg.sym[,3],gid2mgid[,1]),2]
    }
    all.ensg.sym2=all.ensg.sym[which(is.na(all.ensg.sym$MG_ID)),]
    idv2=mg_idconvert_local(all.ensg.sym2[,2])
    sym2mgid=crbind2DataFrame(cbind(idv2$IDMap[!is.na(idv2$IDMap$MG_ID),1],idv2$IDMap$MG_ID[!is.na(idv2$IDMap$MG_ID)]))
    all.ensg.sym2$MG_ID=sym2mgid[match(all.ensg.sym2[,2],sym2mgid[,1]),2]
    all.ensg.sym$MG_ID[match(all.ensg.sym2[which(!is.na(all.ensg.sym2$MG_ID)),1]
                             ,all.ensg.sym[,1])]=all.ensg.sym2$MG_ID[which(!is.na(all.ensg.sym2$MG_ID))]
    all.ensg.sym$ENSG_v1=mg_idconvert_local_MGID2Ensembl(all.ensg.sym$MG_ID)[,2]
    ndt=rbind(ndt,cbind(ET=all.ensg.sym[,1]
                        ,gencode.v33.id.tab[match(all.ensg.sym$ENSG_v1,gencode.v33.id.tab[,2]),]))
  }
  return(crbind2DataFrame(ndt[match(ensgs,ndt[,1]),]))
}





