#Quick Heatmap Tool utilizing the heatmap.3 function
#Author: Jayit Biswas
#Date: 4/2/2016
#Version: Alpha

#If not already installed
#install.packages("gplots")
#install.packages("devtools")

#Load necessary packages
library("gplots")
library("devtools")

#Load latest version of heatmap.3 function
#source_url("https://raw.githubusercontent.com/obigriffith/biostar-tutorials/master/Heatmaps/heatmap.3.R")
source("C:\\Users\\Jayit\\Documents\\R_stuff\\heatmap.3.R")

name <- "test_2.csv"
print(name)
data <- read.csv(file = paste("C:\\Users\\Jayit\\Desktop\\",name,sep=""), sep = ",")
dat <- as.matrix(data)
rownames(dat) <- dat[,1]
dat <- dat[,2:ncol(data)]

key <- 1


for(i in 1:ncol(dat)){
  if (colnames(dat)[i] == "CGap1"){
    cgap1 <- i
  } else if (colnames(dat)[i] == "CGap2"){
    cgap2 <- i
  }
}

for(i in 1:nrow(dat)){
  if (rownames(dat)[i] == "RGap1"){
    rgap1 <- i
  } else if (rownames(dat)[i] == "RGap2"){
    rgap2 <- i
  } else if (rownames(dat)[i] == "Key"){
    key <- i
  }
}

colorswap <- function(x){
  for (i in 1:ncol(x)){
    for (j in 1:nrow(x)){
      if (is.na(x[j,i]) || x[j,i] == "") {
        x[j,i] = "yellow"
      }  else if (as.numeric(x[j,i]) == 0){
        x[j,i] = "ghostwhite"
      } else if (as.numeric(x[j,i]) == 0.5) {
        x[j,i] = "gray"
      } else if (as.numeric(x[j,i]) == 1) {
        x[j,i] = "black"
      } else if (as.numeric(x[j,i]) == 2) {
        x[j,i] = "red"
      } else if (as.numeric(x[j,i]) == 3) {
        x[j,i] = "darkred"
      } else if (as.numeric(x[j,i]) == 4) {
        x[j,i] = "green"
      } else if (as.numeric(x[j,i]) == 5) {
        x[j,i] = "darkgreen"
      } else if (as.numeric(x[j,i]) == 6) {
        x[j,i] = "blue"
      } else if (as.numeric(x[j,i]) == 7) {
        x[j,i] = "darkblue"
      } else if (as.numeric(x[j,i]) == 8) {
        x[j,i] = "violet"
      } else if (as.numeric(x[j,i]) == 9) {
        x[j,i] = "darkviolet"
      } 
    }
  }
  return(x)
}

clab <- as.matrix(dat[1:(rgap1-1),(cgap1+1):(cgap2-1)])
rlab <- as.matrix(dat[(rgap1+1):(rgap2-1),1:(cgap1-1)])

if (ncol(clab) == 1) {
  clabelfinal <- clab 
} else {
  clabelfinal <-  as.matrix(t(clab))
}
rlabelfinal <-  as.matrix(t(rlab))

colnames(clabelfinal) <- dat[1:(rgap1-1),cgap1]
clabelfinal = colorswap(clabelfinal)
rownames(rlabelfinal) <- dat[rgap1, 1:(cgap1-1)]
rlabelfinal = colorswap(rlabelfinal)

keytext <- dat[key,1:11]

datamat <- apply(dat[(rgap1+1):(rgap2-1),(cgap1+1):(cgap2-1)],2,as.numeric) 
rownames(datamat) <- rownames(dat)[(rgap1+1):(rgap2-1)]
colnames(datamat) <- colnames(dat)[(cgap1+1):(cgap2-1)]

main_title <- "Heatmap Tool: test_2"

myclust=function(c) {hclust(c,method="ward.D2")}
dev.off()
colside <- clabelfinal
rowside <- rlabelfinal
#colside <- as.matrix(clabelfinal[,1:(nrow(clabels)-1)])
# png("heatmaptool_test_2",    # create PNG for the heat map        
#     width = 7*300,        # 5 x 300 pixels
#     height = 7*300,
#     res = 300,            # 300 pixels per inch
#     pointsize = 6)        # smaller font size
# par(cex.main=1)
heatmap.3(datamat, hclustfun=myclust,Rowv=TRUE, margins=c(12,12), Colv=TRUE, ColSideColors=colside, RowSideColors=rowside, key=TRUE,
          density.info="none", trace="none", main=main_title, col=rev(heat.colors(200)),
          ColSideColorsSize=ncol(clabelfinal), RowSideColorsSize=nrow(rlabelfinal), KeyValueName="Prob. Response")
legend("topright",legend= keytext#c("0","0.5","1","2","3","4","5","6","7","8","9")
         , fill=c("ghostwhite","gray","black","red","darkred","green","darkgreen","blue","darkblue","violet","darkviolet"), border=FALSE, bty="n", y.intersp = 0.9, cex=0.7)

 #dev.off()
