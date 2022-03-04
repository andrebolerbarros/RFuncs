gg_growthcurve <- function (Curve=NA, transp=0.7,linetype="solid",
                            col_points="black",col_line="red",pt_size=1,
                            line_size=1){
  
  require(ggplot2)
  require(growthcurver)
  
  if(!class(Curve)=="gcfit") stop("This is not a SummarizeGrowthCurve object!")
 
  ss1<-data.frame(Time=Curve$data$t,OD=Curve$data$N,Exp=predict(Curve$model))

  ggplot(ss1,aes(x=Time,y=OD))+
    geom_point(alpha=transp,colour=col_points,size=pt_size)+
    geom_line(aes(y=Exp), color=col_line,linetype=linetype,size=line_size)+
    theme_minimal()
}
