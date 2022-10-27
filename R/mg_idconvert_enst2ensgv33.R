mg_idconvert_enst2ensgv33=function(ensts){
  load(paste0(MG_Grobal_baseFolder,'/source/gencode.v33.id.tab.RData'))
  unensg=setdiff(ensts,gencode.v33.id.tab[,1])
  ndt=gencode.v33.id.tab[,1:2]
  #ndt=cbind(ET=gencode.v33.id.tab[,2],gencode.v33.id.tab)
  all.ensg.sym=rbind()
  unensg3=NULL
  if(length(unensg)>0){
    library(EnsDb.Hsapiens.v79)
    inputs.cv=ensembldb::select(EnsDb.Hsapiens.v79, keys= unensg
                                , keytype = "TXID", columns = c("GENEID","SYMBOL","ENTREZID","TXID"))
    unensg1=setdiff(unensg,inputs.cv$TXID)
    all.ensg.sym=rbind(all.ensg.sym,inputs.cv)
    if(length(unensg1)>0){
      library(EnsDb.Hsapiens.v86)
      inputs.cv=ensembldb::select(EnsDb.Hsapiens.v86, keys= unensg1
                                  , keytype = "TXID", columns = c("GENEID","SYMBOL","ENTREZID","TXID"))
      unensg2=setdiff(unensg1,inputs.cv$TXID)
      all.ensg.sym=rbind(all.ensg.sym,inputs.cv)
      if(length(unensg2)>0){
        library(EnsDb.Hsapiens.v75)
        inputs.cv=ensembldb::select(EnsDb.Hsapiens.v75, keys= unensg2
                                    , keytype = "TXID", columns = c("GENEID","SYMBOL","ENTREZID","TXID"))
        unensg3=setdiff(unensg2,inputs.cv$TXID)
        all.ensg.sym=rbind(all.ensg.sym,inputs.cv)
      }
    }
  }
  if(!is.null(all.ensg.sym)){
    colnames(ndt)=c('TXID','GENEID')
    ndt=crbind2DataFrame(rbind(ndt,all.ensg.sym[,c(4,1)]))
  }
  ensgs=ndt[match(ensts,ndt[,1]),2]

  ensgs_dt=mg_idconvert_ensg2ensgv33(ensgs[!is.na(ensgs)])
  dt=crbind2DataFrame(cbind(ENST_IN=ensts,ensgs_dt[match(ensgs,ensgs_dt[,1]),]))
  dt[which(is.na(dt[,4])),4]=dt[which(is.na(dt[,4])),2]
  return(crbind2DataFrame(dt[,c(1,4:7)]))
}
