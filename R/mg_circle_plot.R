mg_circle_plot=function(axis_ids#圆圈分割成多少个对应的ID,逆时针
                        ,xlims=NULL#定义每个被分割的圈的区间范围，为行数与axis_ids长度一致的两列矩阵，
                        ,track_list#有多少个track
                        ,start.degree = 90#绘制其实点
                        ,cell.padding=c(0.01,0,0.01,0),track.margin=c(0,0,0,0)#跟CSS一样下，左，上，右
                        ,gap.degree=1#分割的圈的间隔
                        ,canvas.xlim =c(-1,1)#圈图绘制位置，比如0,-1就是右半边
                        ,canvas.ylim = c(-1,1)#圈图绘制位置，比如0,-1就是上半边
                        ){
  fa = factor(axis_ids, levels = axis_ids)
  #circos.initialize(factors = f2, xlim = c(0, 1))
  library(circlize) 
  circos.clear()  #这个命令用于清空画布，画错时要运行此命令重新再画。
  #整体布局
  circos.par(canvas.xlim =canvas.xlim,canvas.ylim = canvas.ylim,cell.padding = cell.padding
             ,track.margin=track.margin
             ,start.degree = start.degree,gap.degree=gap.degree,clock.wise = T)
  #fa = factor(axis_ids,levels = fa)
  circos.initialize(factors = fa, xlim = c(0,1)) # 初始化
  
  #chordDiagram(data.frame(methy_r_all.melt[methy_r_all.melt[,1]%in%tp.gos,1]
  #                        ,methy_r_all.melt[methy_r_all.melt[,1]%in%tp.gos,3]),transparency=0.5)
  for(i in length(track_list)){
    mp_plot_circle_track(track_list[1])
  }
}