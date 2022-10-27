

mg_WGCNA_getModule=function(exp,power,blockSize=7000,minModuleSize=30,deepSplit=2,mergeCutHeight = 0.25,net_type='unsigned'){
  #exp=filter.exp
  #outFolder=MG_GENE_FOLDER
  #power=5
  library(WGCNA)
  print('adjacency staring')
  adjacency = adjacency(exp, power = power,type = net_type)
  print('adjacency success')
  TOM = TOMsimilarity(adjacency)
  print('TOM success')
  dissTOM = 1-TOM
  plotTOM = dissTOM^7
  # Set diagonal to NA for a nicer plot
  diag(plotTOM) = NA
  # Call the plot function
  geneTree = hclust(as.dist(dissTOM), method = "average")

  # We like large modules, so we set the minimum module size relatively high:
  #minModuleSize = 30
  # Module identification using dynamic tree cut:
  dynamicMods = cutreeDynamic(dendro = geneTree, distM = dissTOM
                              ,deepSplit = deepSplit, pamRespectsDendro = FALSE,
                              minClusterSize = minModuleSize)
  print('dynamicMods success')
  # Convert numeric lables into colors
  dynamicColors = labels2colors(dynamicMods)
  #table(dynamicColors)
  dynamicColors = labels2colors(dynamicMods)

  MEList = moduleEigengenes(exp, colors = dynamicColors)
  MEs = MEList$eigengenes
  # Calculate dissimilarity of module eigengenes
  MEDiss = 1-cor(MEs)
  # Cluster module eigengenes
  METree = hclust(as.dist(MEDiss), method = "average")
  #MEDissThres = 0.25
  # Call an automatic merging function
  merge = mergeCloseModules(exp, dynamicColors, cutHeight = mergeCutHeight, verbose = 3)
  # The merged module colors
  mergedColors = merge$colors
  table(mergedColors)

  MEs=merge$newMEs
  Modules=cbind(dynamicColors,mergedColors)
  row.names(Modules)=colnames(exp)

  #moduleLabels = net$colors
  #moduleColors = labels2colors(moduleLabels)
  # Plot the dendrogram and the module colors underneath
  #plotDendroAndColors(geneTree, Modules,
  #                    c("Dynamic Module",'Merged Module'),
  #                    dendroLabels = FALSE, hang = 0.03,
  #                    addGuide = TRUE, guideHang = 0.05)
  arg=paste0('power=',power,',blockSize=',blockSize,',minModuleSize=',minModuleSize,',deepSplit=',deepSplit,',mergeCutHeight =',mergeCutHeight)

  return(list(MEs=MEs,Modules=Modules,Tree=geneTree,plotTOM=plotTOM,TOM=TOM,arg=arg))
  #TOMplot(plotTOM, net$dendrograms, moduleColors,
  #        main = "Network heatmap plot, all genes")

}


