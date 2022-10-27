mg_nomogram_buti=function(cox_model,cut.time,title='Nomogram'){
  library(regplot)
  regplot(cox_model#对观测2的六个指标在列线图上进行计分展示
          ,observation=pbc[2,] #也可以不展示
          #预测3年和5年的死亡风险，此处单位是day
          ,title=title
          ,failtime = cut.time
          ,prfail = TRUE #cox回归中需要TRUE
          ,showP = T #是否展示统计学差异
          ,droplines = F#观测2示例计分是否画线
          #,colors = mg_colors[1:3] #用前面自己定义的颜色
          #,rank="decreasing") #根据统计学差异的显著性进行变量的排序
          #,interval="confidence"
          #,rank="decreasing"
          #,clickable=T
          ,points=TRUE)

}
