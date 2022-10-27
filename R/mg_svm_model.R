mg_svm_model <- function (cost = 1, gamma = 1,dat,group) {
  library(e1071)
  library(ROCR)
  #dat=data.frame(Gene=as.numeric(pre.lst.gene.exp[1,]))
  dat=crbind2DataFrame(dat)
  #dat$predict_value=as.factor(ifelse(group==unique(group)[1],unique(group)[1],unique(group)[2]))
  dat$predict_value=factor(group)
  model <- svm (predict_value ~ ., data = dat, probability=TRUE,
                cost = cost, gamma = gamma)
  return(model)
}
