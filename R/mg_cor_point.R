# mg_cor_point!
#
# This is an example function named 'Scatter plot of correlation'
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
mg_cor_point=function(x,y,method='Pearson',top_col='#D55E00',right_col='#009E73'
                      ,ylab='y expression',xlab='x expression',title=NULL
                      ,marginal.type=c("histogram", "boxplot", "density", "violin", "densigram")[1]){
  library(ggstatsplot)
  dat=data.frame(X=x,Y=y)
  tp='nonparametric'
  if(method=='Pearson'|method=='pearson'){
    tp='parametric'
  }
  g1=ggscatterstats(data = dat,
                    x = X,
                    y = Y
                    ,type = tp
                    ,xfill = top_col
                    ,yfill = right_col
                    ,xlab = xlab
                    ,ylab=ylab
                    ,marginal.type = marginal.type
                    ,title = title)
  return(g1)
}





