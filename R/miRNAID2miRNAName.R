miRNAID2miRNAName=function(miRNAID){
  load(paste0(MG_Grobal_baseFolder,'/source/miRbase.hsa.idmap.RData'))
  mid=miRbase.has.idmap$IDmap[match(miRNAID,miRbase.has.idmap$IDmap[,1]),2]
  if(sum(is.na(mid))>0){
    mid[is.na(mid)]=miRbase.has.idmap$IDAliases[match(miRNAID[is.na(mid)],miRbase.has.idmap$IDAliases[,1]),2]
  }
  return(cbind(SearchID=miRNAID,miRbaseID=mid))
}
