mg_plot_circle_Points=function(data){#data是个六列矩阵，第一列为索引列,第二列为x值,第三列为y值,第四列为颜色,第五列为点的类型，第六列为点的大小
  for(i in 1:nrow(data)){
      row=data[i,]
      #print(row)
      if(ncol(data)<4){
        row[4]='black'
      }
      if(ncol(data)<5){
        row[5]=20
      }
      if(ncol(data)<6){
        row[6]=1
      }
      print(row)
      circos.points(as.numeric(row[2]), as.numeric(row[3]), sector.index = row[1], pch = as.numeric(row[5]), col = row[4],cex =as.numeric(row[6]))
  }
}