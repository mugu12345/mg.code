mg_dotplot=function(enrich_desc,enrich_ratio,enrich_size,enrich_color,isFDR=T,low_col='blue',high_col='red'){
  library(ggplot2)
  pathway_data=crbind2DataFrame(cbind(enrich_desc,enrich_ratio,enrich_size,enrich_color))
  pathway_data[,1]=factor(pathway_data[,1], levels = unique(pathway_data[,1]))
  if(isFDR){
    colnames(pathway_data)=c('Desc','Ratio','size','FDR')
  }
  bubble=ggplot(data = pathway_data, aes(x = Ratio, y = Desc))
  bubble=bubble+xlab('Enrichment Ratio')
  if(isFDR){
    bubble=bubble+geom_point(aes(size = size,color = -log10(FDR))) +scale_color_gradient(low = low_col, high = high_col)
  }else{
    bubble=bubble+geom_point(aes(size = size,color = -log10(pvalue))) +scale_color_gradient(low = low_col, high = high_col)
  }
  bubble=bubble+mg_get_ggplot_theme_bw()
  bubble=bubble+ylab('')
  return(bubble)
}
