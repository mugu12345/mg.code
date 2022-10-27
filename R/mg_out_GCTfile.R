mg_out_GCTfile=function(exp_data,outpath){
  if(class(exp_data)%in%c('matrix','data.frame')){
    exp.data=crbind2DataFrame(exp.data)
  }else{
    exp.data <- readMatrix(exp.data)
  }
  exp.data1 <- cbind(NAME = rownames(exp.data), Description = rownames(exp.data),
                     exp.data)
  column1 <- colnames(exp.data1)
  column1[1] <- "NAME"
  column1[2] <- "Description"
  exp.data1$NAME <- factor(exp.data1$NAME)
  exp.data1$Description <- factor(exp.data1$Description)
  class(exp.data1)
  levels(exp.data1[, 1]) <- c(levels(exp.data1[, 1]), "NAME")
  levels(exp.data1[, 2]) <- c(levels(exp.data1[, 2]), "Description")
  exp.data2 <- rbind(column1, exp.data1)
  row1 <- rep("", length(1:ncol(exp.data)))
  row1_2 <- data.frame(row1, row1)
  row1_2 <- t(row1_2)
  No_gene <- nrow(exp.data1)
  No_sample <- (ncol(exp.data1) - 2)
  GCT <- matrix(c("#1.2", No_gene, "", No_sample), nrow = 2,
                ncol = 2)
  gct <- cbind(GCT, row1_2)
  colnames(gct) <- colnames(exp.data2)
  tmp <- rbind(gct, exp.data2)
  writeMatrix(tmp,outpath = outpath,row=F,header = F)

}
