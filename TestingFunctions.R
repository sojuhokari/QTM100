#This function performs a one-sample t-test on repeated samples from a single quantitative variable
#variable - variable of interest
#sample.size - sample size
#alpha - level of significance
#num.reps - number of samples to draw
inference.means<-function(variable,sample.size,alpha,num.reps){
  
  samp.est<-rep(NA,num.reps)
  stdev<-rep(NA,num.reps)
  se.xbar<-rep(NA,num.reps)
  test.stat<-rep(NA,num.reps)
  p.val<-rep(NA,num.reps)
  decision<-rep(NA,num.reps)
  lcl<-rep(NA,num.reps)
  ucl<-rep(NA,num.reps)
  capture<-rep(NA,num.reps)
  true.mean<-mean(variable)
  
  for(i in 1:num.reps){
    samp<-sample(variable,sample.size)
    samp.est[i]<-mean(samp)
    stdev[i]<-sd(samp)
    se.xbar[i]<-stdev[i]/sqrt(sample.size)
    test.stat[i]<-(samp.est[i]-true.mean)/se.xbar[i]
    df<-sample.size-1
    p.val[i]<-2*pt(abs(test.stat[i]),df,lower.tail=FALSE)
    t.score<-qt(1-alpha/2,df)
    lcl[i]<-samp.est[i]-t.score*se.xbar[i]
    ucl[i]<-samp.est[i]+t.score*se.xbar[i]
    
    decision[i]<-ifelse(p.val[i]<=alpha,"reject Ho","fail to reject Ho")
    capture[i]<-ifelse(lcl[i]<=true.mean & ucl[i]>=true.mean,"yes","no")
  }
  
  results<-data.frame(samp.est=round(samp.est,4),
                      test.stat=round(test.stat,4),
                      p.val=round(p.val,4),
                      decision=decision,
                      lcl=round(lcl,4),
                      ucl=round(ucl,4),
                      capture=capture)
  return(results)
}






#This function plots confidence interval results
#results - an object created by either inference.means or inference.proportions
#true.val - the true population parameter value
plot.ci<-function(results,true.val){
  par(mar=c(4, 1, 2, 1), mgp=c(2.7, 0.7, 0),xpd=T)
  k <- length(results$lcl)
  xR <- c(min(results$lcl),max(results$ucl))
  yR <- c(0, 41*k/40)
  plot(xR, yR, type='n', xlab='', ylab='', axes=FALSE)
  cols<-ifelse(results$capture=="yes","white","firebrick2")
  segments(results$lcl,1:k,results$ucl,1:k,col=cols,lwd=4)
  points(results$samp.est,1:k,pch=20,col="black")
  segments(results$lcl,1:k,results$ucl,1:k,col="black")
  segments(true.val,0-42/40,true.val,42*k/40, lty=2, col="royalblue3")
  axis(1)
  text(true.val,yR[2],paste("true =",round(true.val,4)),col="royalblue3",pos=3)
}

