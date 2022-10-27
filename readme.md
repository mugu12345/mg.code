#install packages

devtools::install_github('mugu12345/mg.code')


![image](https://user-images.githubusercontent.com/116777684/198206496-189af2de-0bb8-43e6-b05a-45e94e92a36b.png)


library(mg.code)
1、森林图的绘制
#data preparation：单因素/多因素cox分析与预后的关系
data.sig


![image](https://user-images.githubusercontent.com/116777684/198207530-1d2ed52f-02f6-4d2a-86d9-832d1cefdfb7.png)


#森林图的绘制（一）


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




