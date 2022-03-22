# gg_growthcurve_plate

### Description
This function is used to obtain the growthcurve plots and the data used to predict them, as well as the parameters of the growth curves per each well and their exploratory data analysis summary based on user-defined variables.

### Usage
`gg_growthcurve_plate (df, metadata,  plate, correction, blank, j,  color_by, shape_by, line_colour, pt_size, line_size, vars)`  

### Arguments
- **df:** Data-frame with ODs and corresponding wells. It's design for time & Well names in columns, ODs for wells per timepoint per row;
- **metadata:** Data-frame with the Wells and corresponding information. It's designed to have wells per row, with columns with the experimentally relevant information;
- **plate:** Is the plate full or just some wells? Default is `full`. If `partial`, the `metadata` should only contain the relevant wells;
- **correction:** Background correction used in the `GrowthCurveR` package. Default is the same as in said package, `min`;
- **blank:** Wells corresponding to the blanks, in case the background correction selected was `blank`;
- **j:** Corresponds to the index of the metadata's column that contain the info regarding the wells. Default is `1`;
- **color_by:** Metadata's column that should be used for point color. *Needs to be in character*;
- **shape_by:** Metadata's column that should be used for point shape. *Needs to be in character*;
- **line_colour:** Color for the curves. Default is `darkgrey`. *Not designed to be a variable!*
- **pt_size:** Size of the points in the plots. Default is `1`. *Not designed to be a variable!*
- **line_size:** Size of the curves in the plots. Default is `1`. *Not designed to be a variable!*
- **vars:** Variables that are given by the user to group the parameters & perform some statistical analysis. 

### Value
This function gives 3 outputs:
- **df_curves:** A `data.frame` containing information regarding the obtained values and the predicted ones (the ones used to make the growth curves);
- **Plot_curves:** A `ggplot` object, corresponding plots with the points & growthcurves;
- **df_Param:** A `data.frame` containing the parameter information from the growth curves for each well;

### Author(s)
André Boler Barros

### Example

```
library(growthcurver)
df<-growthdata #Using the information from the growthcurver package
metadata<-data.frame(Bacteria=rep(rep(c("WT","BactA","BactC","BactD"),each=4),times=6),
                     Wells=colnames(df)[-1]) #Creating metadata information, 

metadata$Bacteria<-factor(metadata$Bacteria,levels=c("WT","BactA","BactB","BactC","BactD")) #Ordering the variable 'Bacteria'
p1<-gg_growthcurve_plate(df=df,metadata = metadata,plate="partial",correction="min",j=2,color_by = "Bacteria",vars="Bacteria",shape=NULL,pt_size=2)
```
