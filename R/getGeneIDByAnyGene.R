
getGeneIDByAnyGene=function(gene){
  gs=mg_idconvert(c(gene))
  if(!is.null(gs$IDMap)){
    return(gs$IDMap[1,5])
  }
  return(NULL)
}
