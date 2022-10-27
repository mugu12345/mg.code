get_best_calibrate=function(cox_result,u_time){
  set.seed(123)
  ms=c()
  best_cal=c()
  points=c(2,3,4,5)
  for(m1 in floor(sum(cox_result$n)/points)){
    tryCatch({
      cal <- calibrate(cox_result, u=u_time, cmethod='KM', m=m1, B=sum(cox_result$n))
      mls=(cal[,8]-cal[,7])*(cal[,8]-cal[,7])
      if(sum(!is.na(mls))>2){
        ms1=mean(mls,na.rm = T)
        ms=c(ms,ms1)
        best_cal=c(best_cal,m1)
      }
    }, error = function(e) {
    }, finally = {})
  }
  cal <- calibrate(cox_result, u=u_time, cmethod='KM', m=best_cal[which.min(ms)], B=sum(cox_result$n))
  return(cal)
}
