getmHeatmapAnno=function(name,size,values,type=c('p','l','b','h')
                         ,bk=c(0,1,2),col=c("blue", "white", "red"),gr=T,pch=20
                         ,bar_width =1){
  if(is.null(col)){
    col=c("blue", "white", "red");
  }
  if(type=='x'){
    if(is.null(bk)){
      if(class(values)=='character'){
        ves=unique(values)
        col1=structure(mg_get_ggsci_colN(num=length(ves)), names = ves)
      }else{
        if(length(col)==2){
          bk=c(min(values,na.rm = T),max(values,na.rm = T))
        }else if(length(col)>2){
          bk=c(min(values,na.rm = T),median(values,na.rm = T),max(values,na.rm = T))
          col=col[1:3]
        }else{
          bk=c(min(values,na.rm = T),median(values,na.rm = T),max(values,na.rm = T))
          col=c("blue", "white", "red");
        }
        col1=circlize::colorRamp2(bk, col)
      }
      return(list(Name=name,Height=size,Value=values,Type=type,col=col1,pch=pch,gr=gr,bar_width =bar_width ))
    }else if(length(bk)==length(col)){
      if(gr){
        col1=circlize::colorRamp2(bk, col)
      }else{
        col1=structure(col, names = bk)
      }
      return(list(Name=name,Height=size,Value=values,Type=type,col=col1,pch=pch,gr=gr,bar_width =bar_width ))
    }else{
      print('bk length not equel col')
      return(NULL)
    }
  }else{
    col1=col
    #print(list(Name=name,Height=size,Value=values,Type=type,col=col1,pch=pch,gr=gr,bar_width =bar_width ))
    return(list(Name=name,Height=size,Value=values,Type=type,col=col1,pch=pch,gr=gr,bar_width =bar_width ))
  }
}
