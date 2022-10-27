mg_clusterProfiler_dotplot=function(enrich,color = c("qvalue","p.adjust","pvalue")[2], showCategory = 10,low_col='blue',high_col='red'){
  enrich1=summary(enrich)
  if(nrow(enrich1)>showCategory){
    enrich1=enrich1[1:showCategory,]
  }

  gr=unlist(lapply(enrich1$GeneRatio,function(x){
    x1=as.numeric(unlist(strsplit(x,'/')))
    return(x1[1]/x1[2])
  }))
  if(color=='p.adjust'){
    enrich_color=enrich1$p.adjust
    fdr=T
  }else if(color=='qvalue'){
    enrich_color=enrich1$qvalue
    fdr=T
  }else{
    enrich_color=enrich1$pvalue
    fdr=F
  }
  return(mg_dotplot(enrich_desc=mg_str_outline(enrich1$Description,40)
                    ,enrich_ratio=gr
                    ,enrich_size = enrich1$Count
                    ,enrich_color=enrich_color
                    ,isFDR = fdr))
}
