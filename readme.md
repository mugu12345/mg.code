#install packages

devtools::install_github('mugu12345/mg.code')


![image](https://user-images.githubusercontent.com/116777684/198206496-189af2de-0bb8-43e6-b05a-45e94e92a36b.png)


library(mg.code)


1、森林图的绘制
#data preparation：单因素/多因素cox分析与预后的关系
data.sig


![image](https://user-images.githubusercontent.com/116777684/198207530-1d2ed52f-02f6-4d2a-86d9-832d1cefdfb7.png)


#森林图的绘制


mg_forestplot_v2(data.sig,xlog = T,colgap = 8,lineheight = 10)


![image](https://user-images.githubusercontent.com/116777684/198207165-134a6fb3-e7e2-4718-8cea-b7b41f3f685b.png)


2、单因素cox分析
#data preparation：表达谱+生存数据


![image](https://user-images.githubusercontent.com/116777684/198207982-32a28e31-a372-40b5-983c-7d119726e60d.png)


![image](https://user-images.githubusercontent.com/116777684/198208025-0af4b7eb-7503-4f98-aa9c-8c30f752b2ce.png)


tcga.hub.cox <- cox_batch(dat[,tcga_cli$Samples],
                          time = tcga_cli$OS.time,
                          event = tcga_cli$OS)
                          
                          
![image](https://user-images.githubusercontent.com/116777684/198208170-bd98e9dc-726d-4606-9bb6-a2f350564eb2.png)


3、相关性分析散点图
#数据准备：两列数值


![image](https://user-images.githubusercontent.com/116777684/198209668-d129a4c9-2f70-4184-9176-59e3fd9ffff7.png)


mg_cor_point(x=dat$FCGR2B,y = dat$CD1C,method = 'pearson',top_col = 'red',right_col = 'blue',ylab = 'CD1C',xlab = 'FCGR2B',marginal.type=c("histogram", "boxplot", "density", "violin", "densigram")[1])


![image](https://user-images.githubusercontent.com/116777684/198209784-3f03f31e-58b0-4c7e-8190-bf1b0ef5ab49.png)


mg_cor_point(x=dat$FCGR2B,y = dat$CD1C,method = 'pearson',top_col = 'red',right_col = 'blue',ylab = 'CD1C',xlab = 'FCGR2B',marginal.type=c("histogram", "boxplot", "density", "violin", "densigram")[2])


![image](https://user-images.githubusercontent.com/116777684/198209925-2ea947d9-51be-417e-a2e8-460b184fcecd.png)


mg_cor_point(x=dat$FCGR2B,y = dat$CD1C,method = 'pearson',top_col = 'red',right_col = 'blue',ylab = 'CD1C',xlab = 'FCGR2B',marginal.type=c("histogram", "boxplot", "density", "violin", "densigram")[3])


![image](https://user-images.githubusercontent.com/116777684/198210046-99f72ac4-0fab-4af3-a013-a7fbc6afc36f.png)


mg_cor_point(x=dat$FCGR2B,y = dat$CD1C,method = 'pearson',top_col = 'red',right_col = 'blue',ylab = 'CD1C',xlab = 'FCGR2B',marginal.type=c("histogram", "boxplot", "density", "violin", "densigram")[4])


![image](https://user-images.githubusercontent.com/116777684/198210150-3a03d677-f6c2-419b-9aee-551f216c9610.png)


mg_cor_point(x=dat$FCGR2B,y = dat$CD1C,method = 'pearson',top_col = 'red',right_col = 'blue',ylab = 'CD1C',xlab = 'FCGR2B',marginal.type=c("histogram", "boxplot", "density", "violin", "densigram")[5])


![image](https://user-images.githubusercontent.com/116777684/198210218-d158b1cf-190f-4636-8df5-1b6ca0135f2f.png)







