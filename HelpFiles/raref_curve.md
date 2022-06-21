# raref_curve

### Description
This function is used to perform rarefaction curves before rarefying our microbiome samples. Right now, only for `phyloseq` objects and only outputs number of RSVs (Ribossomal Sequence Variants) and Shannon Index

### Usage
`gg_growthcurve_plate (obj=NA,by=500)`  

### Arguments
- **obj:** a `phyloseq` object;
- **by:** the interval between steps of sequencing depth to produce the rarefaction curves. Default is `500`

### Value
This function gives 2 outputs:
- **RSVs:** A `ggplot2` plot for the number of RSVs with the increasing sequencing depth;
- **ShannonIndex:** A `ggplot2` plot for the Shannon Index with the increasing sequencing depth;
- 
### Author(s)
André Boler Barros

### Notes
- It takes a while to run.
- The `rgseed` is defined as `1`

### Example

```
data("GlobalPatterns")
GP = GlobalPatterns)

GP<-prune_samples(sample_data(GP)$SampleType=="Soil",GP) #To make it feasible to run the example

curv<-raref_curve(GP,by=5000)
```
