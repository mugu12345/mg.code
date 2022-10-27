mg_enrich_circle=function(enrich_tab,gene_exp=NULL){#gene_exp是一个两列的数据框，第一列为基因
  #,enrich_tab是一个数据框，desc,size,pvalue,geneids
#  tryCatch({
    
  library(circlize)
  
  
  link_all=rbind()
  for(i in 1:nrow(enrich_tab)){
    #print(enrich_tab[i,])
    link_all=rbind(link_all,cbind(rep(enrich_tab[i,1],enrich_tab[i,2])
    ,gsub(' ','',unlist(strsplit(enrich_tab[i,4],'/')))
    ,rep(enrich_tab[i,3],enrich_tab[i,2])
    ,(1:enrich_tab[i,2])
    ,rep(enrich_tab[i,2],enrich_tab[i,2])
    ))
  }
  print('====format success')
  #print(link_all)
  link_all=crbind2DataFrame(link_all)
  link_all=link_all[order(link_all[,3]),]
  link_all[,3]=-log10(link_all[,3]+0.000000001)
  cir_ids1=sort(table(link_all[,1]))
  cir_ids2=sort(table(link_all[,2]))

  circle_plot=function(link_all,cir_ids1,cir_ids2){
  
    facing='inside'
    adj=par("adj")
    if(length(cir_ids2)>24){
      facing='clockwise'
      adj=c(0, 1)
    }
  xMax=c(as.numeric(cir_ids1),rep(sum(cir_ids1)/length(cir_ids2),length(cir_ids2)))

#link_all[which(link_all[,3]>5),3]=5

  col_fun = colorRamp2(c(1, length(cir_ids2)), c("darkgreen", "orange"))
  getGeneColor=function(gene){
    ind=which(names(cir_ids2)==gene)
    if(is.null(gene_exp)){
      return(col_fun(ind))
    }else{
      fc=as.numeric(gene_exp[match(names(cir_ids2),gene_exp[,1]),2])
      col_fun1=colorRamp2(c(min(fc,na.rm = T), median(fc,na.rm = T),max(fc,na.rm = T)), c("green",'white',"blue"))
      if(is.na(fc[ind])){
        return('grey')
      }else{
        return(col_fun1(fc[ind]))
      }
    }
  }

#col_fun1 = colorRamp2(c(1, length(cir_ids1)), c("blue", "darkgreen"))
#col_fun(cir_ids2)
col_fun_p = colorRamp2(c(min(link_all[,3]), max(link_all[,3])), c('white', "red"))
circos.clear() 
circos.par(canvas.xlim =c(-1,1),canvas.ylim = c(-1,1),cell.padding = c(0,0,0,0)
           ,track.margin=c(0,0)
           ,start.degree = 90
           ,gap.degree=0,clock.wise = T)
fa = factor(c(names(cir_ids1),names(cir_ids2)), levels = c(names(cir_ids1),names(cir_ids2)))
circos.initialize(factors = fa, xlim = cbind(rep(0,length(xMax)),xMax)) # 初始化
circos.trackPlotRegion(factors = fa, ylim =c(0,10), track.height = 0.1,
                       bg.border = NA,
                       bg.col=NA
                       ,panel.fun = function(x, y) {
                         index=get.cell.meta.data("sector.index")
                         
                         if(length(which(names(cir_ids1)==index))>0){
                           circos.rect(get.cell.meta.data("xlim")[1], get.cell.meta.data("ylim")[1]+get.cell.meta.data("ylim")[2]*0.2
                                       ,get.cell.meta.data("xlim")[2],get.cell.meta.data("ylim")[2]
                                       ,sector.index = get.cell.meta.data("sector.index")
                                       ,col=mg_colors[which(names(cir_ids1)==index)]
                           )
                         }else{
                           
                           circos.text(get.cell.meta.data("xcenter"), get.cell.meta.data("ylim")[2]*1.2
                                       ,labels = index
                                       ,sector.index =get.cell.meta.data("sector.index")
                                       ,facing=facing
                                       ,niceFacing=T
                                       ,cex = 0.5,adj = adj
                                       ,col ='black')
                           
                           
                           circos.rect(get.cell.meta.data("xlim")[1], get.cell.meta.data("ylim")[2]/2
                                       ,get.cell.meta.data("xlim")[2],get.cell.meta.data("ylim")[2]
                                       ,sector.index = get.cell.meta.data("sector.index")
                                       ,col=getGeneColor(index)
                           )
                           #index='ALDH2'
                           lnk=link_all[which(link_all[,2]==index),]
                           xspan=get.cell.meta.data("xlim")[2]/nrow(lnk)
                           
                           for(j in 1:nrow(lnk)){
                            xxlim=get.cell.meta.data('xlim', sector.index = lnk[j,1])
                            span=xxlim[2]/lnk[j,5]
                            circos.link(sector.index1=get.cell.meta.data("sector.index"), c((j-1)*xspan, j*xspan)
                                        ,sector.index2=lnk[j,1], c((lnk[j,4]-1)*span,lnk[j,4]*span)
                                        ,col=mg_colors[which(names(cir_ids1)==lnk[j,1])]
                                        ,arr.lty=5,border ='black'
                                        )
                            circos.rect((j-1)*xspan, get.cell.meta.data("ylim")[1]
                                        ,j*xspan,get.cell.meta.data("ylim")[2]/2
                                        ,sector.index = get.cell.meta.data("sector.index")
                                        ,col=col_fun_p(lnk[j,3])
                            )
                            tx=''
                            if(lnk[j,3]>=3){
                              tx='***'
                            }
                            else if(lnk[j,3]>=2){
                              tx='**'
                            }
                            else if(lnk[j,3]>1.30103){
                              tx='*'
                            }
                            if(tx!=''){
                              circos.text((j-1)*xspan+xspan/2, get.cell.meta.data("ylim")[2]/4
                                          ,labels = tx
                                          ,sector.index =get.cell.meta.data("sector.index")
                                          ,facing='inside',niceFacing=T
                                          ,cex = 0.5
                                          ,col ='black')
                            }
                           }
                         }
                       })
}
library(gridBase)
library(grid)
#mg_colors[which(names(cir_ids1)==index)]
lgd_lines = ComplexHeatmap::Legend(at = names(cir_ids1), type = "boxplot", 
                                   legend_gp = grid::gpar(col = mg_colors[1:length(cir_ids1)], lwd = 2), title_position = "topleft", 
                                   title = "Term")
lgd_links=NULL
  if(!is.null(gene_exp)){
    fc=as.numeric(gene_exp[match(names(cir_ids2),gene_exp[,1]),2])
    col_fun1=colorRamp2(c(min(fc,na.rm = T), median(fc,na.rm = T),max(fc,na.rm = T)), c("green",'white',"blue"))
    lgd_links =ComplexHeatmap::Legend(at = c(signif(min(fc,na.rm = T),2),signif(median(fc,na.rm = T),2), signif(max(fc,na.rm = T),2))
                                      , col_fun = col_fun1, 
                       title_position = "topleft", title = colnames(gene_exp)[2]
                       ,direction = "horizontal")
    
  }
plot.new()
circle_size = grid::unit(1, "snpc") # snpc unit gives you a square region

pushViewport(viewport(x = 0, y = 0.5, width = circle_size, height = circle_size,
                      just = c("left", "center")))
par(omi = gridOMI(), new = TRUE)
circle_plot(link_all,cir_ids1,cir_ids2)
upViewport()
if(!is.null(lgd_links)){
  lgd_list_horizontal = ComplexHeatmap::packLegend(lgd_lines, lgd_links)
  ComplexHeatmap::draw(lgd_list_horizontal, x = circle_size, just = "left")
}else{
ComplexHeatmap::draw(lgd_lines, x = circle_size, just = "left")
}
#ComplexHeatmap::draw(lgd_lines, x = grid::unit(1, "npc") - grid::unit(2, "mm"), y = grid::unit(4, "mm"), 
#     just = c( "bottom"))

}
#plot_cor_circle_label(methy_r_all.melt)