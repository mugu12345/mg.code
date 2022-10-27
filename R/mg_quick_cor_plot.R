mg_quick_cor_plot=function(dat1,dat2=NULL,method=c('pearson','spearman','kendall')[1],R_range=NULL
                           ,R_range_cols=NULL,P_range=NULL,P_range_cols=NULL
                           ,cell_width=NULL
                           ,xlab='X value'
                           ,ylab='Y value'
                           ,cluster_row=F
                           ,cluster_col=F
){
  if(is.null(cell_width)){
    cell_width=2
  }
  cell_height=cell_width
  if(is.vector(dat1)){
    t.dat1=cbind(dat1)
    colnames(t.dat1)=c(xlab)
  }else{
    t.dat1=dat1
  }
  if(is.null(dat2)&ncol(t.dat1)>1){
    cr=mg_muti_cor_plot(t.dat1,NULL,method)
    if(ncol(t.dat1)==2){
      g1=mg_cor_point(t.dat1[,1],t.dat1[,2],xlab = colnames(t.dat1)[1],ylab = colnames(t.dat1)[2],method = ifelse(method=='pearson','Pearson','other'))
    }else{
      tp='pie'
      if(ncol(t.dat1)>30){
        tp='color'
      }else if(ncol(t.dat1)>10){
        tp='circle'
      }
      row_inds=1:nrow(cr$r)
      col_inds=1:ncol(cr$r)
      if(cluster_col){
        d <- dist(cr$r)
        hc1 <- hclust(d,"single")
        col_inds=hc1$order
        row_inds=col_inds
      }
      g1=mg_plot_hetmap_cor(cr$r[row_inds,col_inds],cr$p[row_inds,col_inds],type = tp
                            ,leg_title1 = 'correlation',leg_title2 = '-log10(p value)'
                            ,split1_range = R_range
                            ,split1_range_cols = R_range_cols
                            ,split2_range = P_range
                            ,split2_range_cols = P_range_cols
                            ,cell_width = cell_width

                            #,split2_range_cols = c(mg_colors[3],mg_colors[4],mg_colors[5])
      )
    }
  }else{
    if(is.vector(dat2)){
      t.dat2=cbind(dat2)
      colnames(t.dat2)=c(xlab)
    }else{
      t.dat2=dat2
    }
    cr=mg_muti_cor_plot(t.dat1,t.dat2,method)
    if(ncol(t.dat1)==1&ncol(t.dat2)==1){
      g1=mg_cor_point(t.dat1[,1],t.dat2[,1],xlab = colnames(t.dat1)[1],ylab = colnames(t.dat2)[1],method = ifelse(method=='pearson','Pearson','other'))
    }else{
      show_text_p=matrix(NA,nrow = nrow(cr$r),ncol = ncol(cr$r))
      cor_p=matrix(NA,nrow = nrow(cr$r),ncol = ncol(cr$r))
      for(i in 1:nrow(cr$r)){
        for(j in 1:ncol(cr$r)){
          if(cr$p[i,j]==0){
            cor_p[i,j]=6
            show_text_p[i,j]='***'
          }else{
            cor_p[i,j]=-log10(cr$p[i,j])
            show_text_p[i,j]=mg_format_p_value(cr$p[i,j])
            if(show_text_p[i,j]=='-'|show_text_p[i,j]=='.'){
              show_text_p[i,j]=''
            }
          }
        }
      }
      colnames(cor_p)=colnames(cr$r)
      row.names(cor_p)=row.names(cr$r)

      print(dim(cr$p))
      row_inds=1:nrow(cr$r)
      col_inds=1:ncol(cr$r)
      if(cluster_col&nrow(cr$r)>2){
        d <- dist(t(cr$r))
        hc1 <- hclust(d,"single")
        col_inds=hc1$order
      }
      if(cluster_row&ncol(cr$r)>2){
        d <- dist(cr$r)
        hc1 <- hclust(d,"single")
        row_inds=hc1$order
      }
      plt_p=matrix(NA,nrow = nrow(cr$r),ncol = ncol(cr$r),dimnames = list(row.names(cr$r)[row_inds]
                                                                          ,colnames(cr$r)[col_inds]))
      plt_r=matrix(NA,nrow = nrow(cr$r),ncol = ncol(cr$r),dimnames = list(row.names(cr$r)[row_inds]
                                                                          ,colnames(cr$r)[col_inds]))
      plt_show_text_p=matrix(NA,nrow = nrow(cr$r),ncol = ncol(cr$r),dimnames = list(row.names(cr$r)[row_inds]
                                                                                    ,colnames(cr$r)[col_inds]))
      for(ii in col_inds){
        plt_p[,ii]=cor_p[row_inds,ii]
        plt_show_text_p[,ii]=show_text_p[row_inds,ii]
        plt_r[,ii]=cr$r[row_inds,ii]
      }

      g1=mg_plot_split_hetmap(plt_p,plt_r,type = 'bt',leg_title2 = 'correlation',leg_title1 = '-log10(p value)'
                              ,show_text1 = plt_show_text_p
                              ,split2_range = R_range,split2_range_cols = R_range_cols
                              ,split1_range = P_range,split1_range_cols = P_range_cols
                              ,cell_height=cell_height
                              ,cell_width =cell_width
      )
    }
  }
  return(list(R=cr,plot=g1))
}
