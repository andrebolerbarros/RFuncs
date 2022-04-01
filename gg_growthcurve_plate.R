gg_growthcurve_plate<- function (
  df=NA, #Data-frame with ODs and corresponding wells
  metadata=NA, #Metadata data-frame, with wells per rows
  plate="full", #Is a full plate or only some wells?
  correction=c("min","none","blank"), #Background correction used in GrowthCurveR package. Default is min.
  blank=NA, #Necessary in case you choose "blank" in the correction
  color_by=NULL, #Column that should be used for color. Needs to be in character
  shape_by=NULL, #Column that should be used for shape. Needs to be in character
  line_colour="darkgrey", #Color for the curve. Default is darkgrey. Not designed to be a variable!
  pt_size=1, #size for the points in the plots
  line_size=1, #line size for the growth curves
  vars=NULL #variables that should be used to group the parameters in the final data.frame
  )
{
  #import the libraries
  library(growthcurver)
  library(ggplot2)
  library(reshape2)
  library(dplyr) 
  
  #Warnings & General Errors
  if (!("Wells" %in% colnames(metadata))) {stop("There is no column called wells in metadata")}
  if (!(color_by %in% colnames(metadata))) {stop("Color variable is not in metadata")}
  if (!(shape_by %in% colnames(metadata))) {stop("Shape variable is not in metadata")}
  if (all((vars %in% colnames(metadata)))==F) {stop("vars not in metadata")}
  
  #Create a vector with letters & numbers to show a plot similar to 96 Well-plate
  wells<-paste(rep(LETTERS[seq( from = 1, to = 8 )],each=12),
               rep(seq(1:12),times=8),sep="")
  if (plate=="full") {
    wells<-wells
    
  } else if (plate=="partial") {
    wells<-wells[wells %in% metadata$Wells]
  } else {
    stop("Invalid plate atribute")
  }
  
  #Order the columns of our plate accordingly with the plate setup
  df<-df[,colnames(df) %in% c(colnames(df)[1],wells)]
  df<-df[,c(colnames(df)[1],wells)]
  
  #Organize the metadata information based on the plate setup
  metadata$Wells<-factor(metadata$Wells,levels=wells)
  metadata<-metadata[order(metadata$Wells),]
  
  #Create a dataframe to save information regarding the parameters of growthcurves
  gc_info <- data.frame(well=NA, k=NA, n0=NA,
                        r=NA, sigma=NA,t_mid=NA,t_gen=NA,
                        auc_l=NA,  auc_e=NA,note=NA)
  
  
  
  #Create a dataframe to save information to plot the growthcurves
  gc_plot<- data.frame(Well=NA,Time=NA,OD=NA,Exp=NA,metadata[1,])
  
  #Perform a cycle to run through the the wells
  
  for (i in wells) {
    
    #Calculate the parameters individually for each combination of time & well. Important to assign the right method for correction and the corresponding columns, if you are to choose the blank option
    calc<-SummarizeGrowth(df[,1],df[,colnames(df)==i],bg_correct = correction,blank=blank)
    
    #Save and collect the information regarding the estimates produced by the model & bind with the data.frame produced from the previous iteration
    temp1<-data.frame(well=NA, k=calc$vals$k, n0=calc$vals$n0,
                      r=calc$vals$r, sigma=calc$vals$sigma,t_mid=calc$vals$t_mid,t_gen=calc$vals$t_gen,
                      auc_l=calc$vals$auc_l,  auc_e=calc$vals$auc_e,note=calc$vals$note)
    
    
    
    gc_info<-rbind(gc_info,temp1)
    #Create a temporary dataset, that you store the points & predicted growth curve, alongside with the information from the metadata
    temp<-data.frame(Well=i,Time=calc$data$t,OD=calc$data$N,Exp=predict(calc$model),
                     metadata[metadata$Wells==i,])
    gc_plot<-rbind(gc_plot,temp)
    
  }
  
  gc_info<-gc_info[-1,]
  gc_plot<-gc_plot[-1,]
  
  
  #This will allow for the right order to appear on the data.frame, an order similar to the plate structures
  gc_info$well<-wells
  
  gc_plot$Well<-factor(gc_plot$Well,levels=wells)
  
  #Produce the first plot, containing the real points & growth curves predicted, in a similar structure as a 96-well plate
  if (is.null(color_by)) {cl1= NULL} else {cl1= gc_plot[,which(colnames(gc_plot)==color_by)]}
  if (is.null(shape_by)) {shp1=NULL} else {shp1=gc_plot[,which(colnames(gc_plot)==shape_by)]}
  
  
  g1<-ggplot(data=gc_plot,aes(x=Time,y=OD,color=cl1,shape=shp1))+
    geom_point(alpha=0.5,size=pt_size)+xlab("Time")+ylab("OD")+
    ggtitle("Growth Curves - Plate Plot")+
    geom_line(aes(y=Exp),colour=line_colour,size=line_size)+
    facet_wrap(~Well,ncol=12,nrow=8)+
    theme_classic()+  
    theme(strip.text = element_text(size=12),
          plot.title = element_text(hjust=0.5,size=20),
          legend.title = element_blank())
  
  
  #Just to confirm everything is right with the well order, in order to merge the parameter info with metadata
  if(all(metadata$Wells==gc_info$well)==T) {
    print("Everything seems to be right")
  } else {
    stop("Ups! Something's not right here!")
  }
  
  comp0<-cbind(gc_info,metadata)
  comp1<-cbind(gc_info[,-10],metadata)
  comp2<-melt(comp1,id=c("well","Wells",vars))
  
  
  if (is.null(color_by)) {cl2= NULL} else {cl2= comp2[,which(colnames(comp2)==color_by)]}
  if (is.null(shape_by)) {shp2=NULL} else {shp2=comp2[,which(colnames(comp2)==shape_by)]}
  
  g2<-ggplot(data = comp2,aes(x=cl2,y=value,
                              color=cl2,shape=shp2))+
    geom_point(alpha=0.7,size=pt_size)+ggtitle("Growth Curves - Parameters")+
    facet_wrap(~variable,scales = "free_y")+xlab("")+ylab("")+
    theme_minimal()+
    theme(strip.background=element_rect(fill="black"),
          plot.title = element_text(hjust=0.5,size=20),
          strip.text = element_text(color="white"),legend.title = element_blank())
  
  tbl<- comp2 %>%
    group_by(variable,comp2[,c(vars)]) %>%
    summarise(Min=min(value,na.rm=T),Q1=quantile(value,0.25,na.rm=T), 
              Median=median(value,na.rm = T), Q3=quantile(value,0.75,na.rm=T), 
              Max=max(value,na.rm=T),Mean=mean(value,na.rm=T),sd=sd(value,na.rm=T))
  
  colnames(tbl)<-c("Parameters",vars,"Min","Q1","Median","Q3","Max","Mean","SD")
  
  return(list(df_curves=gc_plot,
              Plot_curves=g1,
              df_Param =comp0,
              Summ_Param =tbl,
              Plot_Param =g2))
  
}
