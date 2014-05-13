## https://personality-project.org/r/r.guide.html#oneway (cleaned up)

## datafilename="/Users/bill/Desktop/R.tutorial/datasets/recall1.data"
datafilename <- "/Users/raymondleclair/Projects/4-RStudio/Eye-Tracking-Analysis/Data/recall1.data"
recall.data=read.table(datafilename,header=TRUE)
recall.data # show the data

raw=recall.data[,1:8]        # just trial data 
## First set some specific paremeters for the analysis -- this allows
numcases=27                  # How many subjects are there?
numvariables=8               # How many repeated measures are there?
numreplications=2            # How many replications/subject?
numlevels1=2                 # specify the number of levels for within subject variable 1
numlevels2=2                 # specify the number of levels for within subject variable 2

stackedraw=stack(raw)        # convert the data array into a vector 
## add the various coding variables for the conditions
## make sure to check that this coding is correct
recall.raw.df=data.frame(recall=stackedraw,
  subj=factor(rep(paste("subj", 1:numcases, sep=""), numvariables)),
  replication=factor(rep(rep(c("1","2"), c(numcases, numcases)),
    numvariables/numreplications)),
  time=factor(rep(rep(c("short", "long"),
    c(numcases*numreplications, numcases*numreplications)),numlevels1)),
  study=rep(c("d45", "d90") ,c(numcases*numlevels1*numreplications,
    numcases*numlevels1*numreplications)))

recall.aov= aov(recall.values ~ time * study + Error(subj/(time * study)), data=recall.raw.df) 
## do the ANOVA
summary(recall.aov)                                #show the output
print(model.tables(recall.aov,"means"),digits=3)   #show the cell means for the anova table
