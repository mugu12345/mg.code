miRNA2Mature=function(miRNAID){
  load(paste0(MG_Grobal_baseFolder,'/source/miRbase.hsa.idmap.RData'))
  #head(miRbase.has.idmap$IDmap)
  #miRbase.has.idmap$miRNAMap
  mid=miRbase.has.idmap$miRNAMap[match(miRNAID,miRbase.has.idmap$miRNAMap[,1]),c(5,7)]
  return(cbind(SearchID=miRNAID,miRbaseID5p=mid[,1],miRbaseID3p=mid[,2]))
}
