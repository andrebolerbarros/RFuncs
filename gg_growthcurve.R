gg_growthcurve <- function (Time=NA,Well=NA, background="min", 
                            blank=NA, transp=0.7,linetype="solid",
                            col_points="black",col_line="red",pt_size=1,
                            line_size=1){
  
  require(ggplot2)
  require(growthcurver)
  
  if(!length(Time)==length(Well)) stop("Time and Well should have same length!")
  if(!is.numeric(Time)) stop("Time must be numeric!")
  if(!is.numeric(Well)) stop("Well must be numeric!")
  
  m0<- SummarizeGrowth(Time, Well,bg_correct = background)
  
  ss1<-data.frame(Time=m0$data$t,OD=m0$data$N,Exp=predict(m0$model))

  ggplot(ss1,aes(x=Time,y=OD))+
    geom_point(alpha=transp,colour=col_points,size=pt_size)+
    geom_line(aes(y=Exp), color=col_line,linetype=linetype,size=line_size)+
    theme_minimal()
}
