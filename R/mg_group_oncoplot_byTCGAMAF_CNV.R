mg_group_oncoplot_byTCGAMAF_CNV=function(tcga_code='COAD',group,top=5
                                         ,is_split=F
                                         ,show_pvalue=F,full=F
                                         ,genes=NULL
){#group two columns,col1: TCGA Sample,col2 Group,col3:color
  library(ComplexHeatmap)
  maf=getTCGAMAFByCode(tcga_code)
  gene.mut=crbind2DataFrame(cbind(as.character(maf@data$Hugo_Symbol),
                                  as.character(maf@data$Tumor_Sample_Barcode),
                                  as.character(maf@data$Variant_Classification)))
  #smps=unique(gene.mut[,2])
  #group=cbind(smps,c('H',rep(c('H','L'),length(smps)/2)))
  uri=paste0('http://218.108.182.182:9000/pubmed/getTCGAFileByTable/TCGA-',tcga_code
             ,'?data_category=Copy%20Number%20Variation&data_format=TXT&data_type=Gene%20Level%20Copy%20Number%20Scores&experimental_strategy=Genotyping%20Array&platform=Affymetrix%20SNP%206.0&workflow_type=GISTIC%20-%20Copy%20Number%20Score&fileCount=1')
  r <- httr::GET(uri,add_headers=c('If-Match:*'))
  status=httr::http_status(r)
  print(paste0('statrus:',status))
  text=httr::content(r,'text',encoding = 'utf-8')
  text=rjson::fromJSON(text)
  cnv=readMatrix(paste0(MG_Grobal_DBPath,'/TCGA/Backup_Files/',text$res$mergeFile[[1]]$url))
  #colnames(cnv)
  ensg=gsub('\\..*','',row.names(cnv))
  idmap=mg_idconvert_local(ensg)
  #idmap$IDMap$BG
  cnv.inds=which(!is.na(idmap$IDMap$GeneSymbol))
  cnv=cnv[cnv.inds,]
  row.names(cnv)=idmap$IDMap$GeneSymbol[cnv.inds]
  colnames(cnv)=substr(colnames(cnv),1,12)
  #head(cnv[,1:10])
  cnv.melt=reshape2::melt(cbind(V1=row.names(cnv),cnv[,3:ncol(cnv)]))
  cnv.melt[,3]=ifelse(cnv.melt[,3]>0,'CNV_Amp',ifelse(cnv.melt[,3]<0,'CNV_Del',''))
  colnames(cnv.melt)=c('V1','V2','V3')
  cnv.melt=cnv.melt[cnv.melt[,3]!='',]
  #head(gene.mut)
  mg_melt=rbind(gene.mut,cnv.melt)
  #head(mg_melt)
  mg_melt=mg_melt[mg_melt$V2%in%substr(group[,1],1,12),]
  #dim(mg_melt)

  gene.mut.mtx=reshape2::dcast(mg_melt,V1 ~ V2 ,fun.aggregate =function(x){
    if(length(x)>1){
      return('Multi_Hit')
    }else{
      return(paste0(x,collapse = ''))
    }
  })
  #head(gene.mut.mtx[,1:10])
  #gene.mut.mtx[,2]
  row.names(gene.mut.mtx)=gene.mut.mtx[,1]
  gene.mut.mtx=gene.mut.mtx[,-1]
  x.group=group[match(colnames(gene.mut.mtx),substr(group[,1],1,12)),2]

  g.pls=apply(gene.mut.mtx, 1, function(x){
    #x=gene.mut.mtx[1,]

    #CNV_Amp
    cdel=grep('CNV_Del',x)
    cdel_x=x
    cdel_x[cdel]='CNV_Del'
    camp=grep('CNV_Amp',x)
    camp_x=x
    camp_x[camp]='CNV_Amp'

    mx_CNV_Del=as.matrix(table(x.group,ifelse(cdel_x=='CNV_Del','L','M')))
    #print(mx_CNV_Del)
    mx_CNV_Amp=as.matrix(table(x.group,ifelse(camp_x=='CNV_Amp','L','M')))
    mx_mut=as.matrix(table(x.group,ifelse(gsub('CNV_Amp','',gsub('CNV_Del','',x))=='','L','M')))
    ps=c()
    if(ncol(mx_mut)==2&min(apply(mx_mut, 2,sum))>0){
      p=fisher.test(mx_mut)$p.value
    }else{
      p=NA
    }
    ps=c(ps,p)
    if(ncol(mx_CNV_Del)==2&min(apply(mx_CNV_Del, 2,sum))>0){
      p=fisher.test(mx_CNV_Del)$p.value
    }else{
      p=NA
    }
    ps=c(ps,p)
    if(ncol(mx_CNV_Amp)==2&min(apply(mx_CNV_Amp, 2,sum))>0){
      p=fisher.test(mx_CNV_Amp)$p.value
    }else{
      p=NA
    }
    ps=c(ps,p)
    return(ps)
  })
  g.pls=t(g.pls)
  colnames(g.pls)=c('Mut','CNV_Del','CNV_Amp')
  if(is.null(genes)){
    genes=unique(c(row.names(g.pls)[order(g.pls[,1],na.last = T)][1:top]
                   ,row.names(g.pls)[order(g.pls[,2],na.last = T)][1:top]
                   ,row.names(g.pls)[order(g.pls[,3],na.last = T)][1:top]))

  }
  mat=gene.mut.mtx[which(row.names(gene.mut.mtx)%in%genes),]
  min_palues=g.pls[match(row.names(mat),row.names(g.pls)),]

  #if(show_pvalue){
  #  lbs=paste0('p=',round(min_palues,3))
  #  lbs[which(lbs=='p=0')]='p<0.001'
  #  row.names(mat)=paste0(row.names(mat),'(',lbs,')')
  #}else{
  #  row.names(mat)=paste0(row.names(mat),mg_format_p_values(min_palues))
  #}


  #Missense_Mutation, CNV_Amp, Nonsense_Mutation, Nonstop_Mutation, CNV_Del, Splice_Site, Frame_Shift_Del
  #指定变异的样子，x,y,w,h代表变异的位置(x,y)和宽度(w)，高度(h)
  afun=c('Missense_Mutation','CNV_Amp','Nonsense_Mutation','CNV_Del'
         ,'Frame_Shift_Del','Nonstop_Mutation','Splice_Site','Frame_Shift_Ins','In_Frame_Ins'
         ,'In_Frame_Del','Multi_Hit')
  cols=c(mg_colors[1:(length(afun)-1)],'#000000')
  names(cols)=afun
  heatmap_legend_param <- list(title = "Alternations",
                               at = afun,
                               labels = afun)

  alter_fun <- list(
    background = function(x, y, w, h) {
      grid.rect(x, y, w-unit(0.5, "mm"), h-unit(0.5, "mm"),
                gp = gpar(fill = "#CCCCCC", col = '#CCCCCC'))
    },
    Multi_Hit=function(x, y, w, h) {
      grid.rect(x, y, w-unit(0.5, "mm"), h-unit(0.5, "mm"),
                gp = gpar(fill = "#000000", col = '#000000'))
    },
    Missense_Mutation=function(x, y, w, h) {
      grid.rect(x, y+0.25*h, w-unit(0.5, "mm"), h*0.5-unit(0.5, "mm"),
                gp = gpar(fill =  cols[1], col =cols[1]))
    },
    CNV_Amp=function(x, y, w, h) {
      grid.rect(x, y-0.25*h, w-unit(0.5, "mm"), h*0.5-unit(0.5, "mm"),
                gp = gpar(fill = cols[2], col =cols[2]))
    },
    Nonsense_Mutation=function(x, y, w, h) {
      grid.rect(x, y+0.25*h, w-unit(0.5, "mm"), h*0.5-unit(0.5, "mm"),
                gp = gpar(fill =  cols[3], col =cols[3]))
    },
    CNV_Del=function(x, y, w, h) {
      grid.rect(x, y-0.25*h, w-unit(0.5, "mm"), h*0.5-unit(0.5, "mm"),
                gp = gpar(fill =  cols[4], col =cols[4]))
    },
    Frame_Shift_Del=function(x, y, w, h) {
      grid.rect(x, y+0.25*h, w-unit(0.5, "mm"), h*0.5-unit(0.5, "mm"),
                gp = gpar(fill =  cols[5], col =cols[5]))
    },
    Nonstop_Mutation=function(x, y, w, h) {
      grid.rect(x, y+0.25*h, w-unit(0.5, "mm"), h*0.5-unit(0.5, "mm"),
                gp = gpar(fill = cols[6], col =cols[6]))
    },
    Splice_Site=function(x, y, w, h) {
      grid.rect(x, y+0.25*h, w-unit(0.5, "mm"), h*0.5-unit(0.5, "mm"),
                gp = gpar(fill = cols[7], col =cols[7]))
    },
    Frame_Shift_Ins=function(x, y, w, h) {
      grid.rect(x, y+0.25*h, w-unit(0.5, "mm"), h*0.5-unit(0.5, "mm"),
                gp = gpar(fill = cols[8], col =cols[8]))
    },
    In_Frame_Ins=function(x, y, w, h) {
      grid.rect(x, y+0.25*h, w-unit(0.5, "mm"), h*0.5-unit(0.5, "mm"),
                gp = gpar(fill = cols[9], col =cols[9]))
    },
    In_Frame_Del=function(x, y, w, h) {
      grid.rect(x, y+0.25*h, w-unit(0.5, "mm"), h*0.5-unit(0.5, "mm"),
                gp = gpar(fill = cols[10], col =cols[10]))
    }
  )
  #for(a in 1:length(afun)){
  #  mtu=function(x, y, w, h) {
  #    grid.rect(x, y, w-unit(0.5, "mm"), h-unit(0.5, "mm"),
  #              gp = gpar(fill = cols[a], col = cols[a]))
  #  }
  #  alter_fun=c(alter_fun,list(mtu))
  #}
  #names(alter_fun)=c("background",afun)
  anno_df = data.frame(
    Mut=c('p<0.001','p<0.01','p<0.05','p<0.1','p>0.1','p>0.1')[match(mg_format_p_values(min_palues[,1]),c('***','**','*','.','-',''))],
    Del=c('p<0.001','p<0.01','p<0.05','p<0.1','p>0.1','p>0.1')[match(mg_format_p_values(min_palues[,2]),c('***','**','*','.','-',''))],
    Amp=c('p<0.001','p<0.01','p<0.05','p<0.1','p>0.1','p>0.1')[match(mg_format_p_values(min_palues[,3]),c('***','**','*','.','-',''))]
  )
  row_anno_color=c('#ff0000','#ff3333','#ff6666','#ff9999','#ffcccc')
  names(row_anno_color)= c('p<0.001','p<0.01','p<0.05','p<0.1','p>0.1')
  #anno_df[anno_df=='']='p>0.1'
  row_ha = rowAnnotation(df = anno_df,
                         col = list(Mut = row_anno_color,Del = row_anno_color,
                                    Amp = row_anno_color
                         )
                         ,show_legend = F
  )
  #anno_df = data.frame(foo = 1:10,
  #                     bar = sample(letters[1:3], 10, replace = TRUE))
  #c('p<0.001','p<0.01','p<0.05','p<0.1','p>0.1')
  ugp=unique(group[,2])
  if(ncol(group)<3){
    group_col=mg_colors[as.numeric(as.factor(ugp))]
  }else{
    group_col=group[match(ugp,group[,2]),3]
    group_col[is.na(group_col)]='#000000'
  }
  names(group_col)=ugp

  #c('***','**','*','.','-','')
  lgd1 = Legend(labels = names(row_anno_color), legend_gp = gpar(fill = row_anno_color)
                , title = "Mut_Del_Amp pvalue"
                #,labels_gp = gpar(col = "red", fontsize = 14)
  )
  lgd2 = Legend(labels = names(cols)
                , legend_gp = gpar(fill = cols)
                , title = "Alternations"
                #,labels_gp = gpar(col = "red", fontsize = 14)
  )
  lgd3 = Legend(labels = names(group_col)
                , legend_gp = gpar(fill = group_col)
                , title = "Group"
                #,labels_gp = gpar(col = "red", fontsize = 14)
  )
  pd = packLegend(lgd1, lgd2, lgd3
                  #, direction = "horizontal"
  )

  darw_Het=function(t.inds,left_anno=T){
    ha = HeatmapAnnotation(df = data.frame(
      Group=x.group[t.inds]),
      col = list(Group =group_col)
      ,show_legend = F
    )
    if(left_anno){
      left_anno=row_ha
    }else{
      left_anno=NULL
    }
    p1=ComplexHeatmap::oncoPrint(mat[,t.inds],alter_fun = alter_fun
                                 , col = cols
                                 ,column_title =paste0('Top',top,' of ',tcga_code,' Gene Alternations')
                                 ,heatmap_legend_param = heatmap_legend_param
                                 ,row_names_side = "left"
                                 ,pct_side = "right"
                                 ,bottom_annotation=ha
                                 ,left_annotation = left_anno
                                 ,column_split = as.factor(x.group)[t.inds]
                                 ,show_heatmap_legend =F
                                 ,show_row_names = !is.null(left_anno)
    )
    return(p1)
  }

  if(is_split){
    ht_list = NULL  ## Heatmap(...) + NULL gives you a HeatmapList object
    for(s in unique(x.group)) {
      ht_list = ht_list + darw_Het(which(x.group==s),is.null(ht_list))
    }
  }else{
    ht_list=darw_Het(1:length(x.group),T)
  }
  pall=draw(ht_list,annotation_legend_list  = pd)

  return(list(Plot=pall,Matrix=gene.mut.mtx,pvalues=g.pls))
}
