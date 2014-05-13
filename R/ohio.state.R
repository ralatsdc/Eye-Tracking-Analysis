## http://star.psy.ohio-state.edu/coglab/Demos/r.anova.html (cleaned up)

## read the data into a table
## datafilename="https://personality-project.org/r/datasets/R.appendix5.data"
datafilename <- "/Users/raymondleclair/Projects/4-RStudio/Eye-Tracking-Analysis/Data/R.appendix5.data"
data.ex5 <- read.table(datafilename,header=T)

## show the data
data.ex5

## estimate the model
aov.ex5 <- aov(Recall~(Task*Valence*Gender*Dosage)+Error(Subject/(Task*Valence))+(Gender*Dosage),data.ex5)

## report the summary
summary(aov.ex5)

## report the means and the number of subjects/cell
print(model.tables(aov.ex5,"means"),digits=3)

## graphical summary of means of the 36 cells
boxplot(Recall~Task*Valence*Gender*Dosage,data=data.ex5) 

## graphical summary of means of 18 cells
boxplot(Recall~Task*Valence*Dosage,data=data.ex5) 
