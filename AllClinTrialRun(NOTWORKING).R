library("tm")
library("SnowballC")
library("ggplot2")
library("wordcloud")
library("stringi")

setwd("/afs/athena.mit.edu/user/w/i/williame/Downloads")

thingcsv <- read.csv("allInclusionExclusionBulletPoint.csv", encoding = "latin1")
thingcsv <- as.data.frame(thingcsv[,4])
thingcsv[] <- lapply(thingcsv, as.character)
for (l in 1:nrow(thingcsv)) {
  thingcsv[l,] <- stringi::stri_trans_general(thingcsv[l,], "latin-ascii")
}
setwd("/afs/athena.mit.edu/user/w/i/williame/Downloads/ClusterSentAllCTFinal2")
n <- 1
for (i in 1:nrow(thingcsv)) {
  if (!is.na(thingcsv[i,1])) {
    if (thingcsv[i,1] != "" & thingcsv[i,1] != "#NAME?" & thingcsv[i,1] != "All") {
      n <- as.character(n)
      str <- paste("allsent", n, ".txt", sep = "")
      write.table(as.data.frame(thingcsv[i,1]),file = str, row.names = F, col.names = F)
      n <- as.numeric(n)
      n <- n + 1
    }
  }
}


allsentdocs <- Corpus(DirSource("/afs/athena.mit.edu/user/w/i/williame/Downloads/ClusterSentAllCTFinal2"))
writeLines(as.character(allsentdocs[[25]]))

allsentdocs <- tm_map(allsentdocs,content_transformer(tolower))
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
allsentdocs <- tm_map(allsentdocs, toSpace, '"')
allsentdocs <- tm_map(allsentdocs, removeWords, stopwords("english"))
allsentdocs <- tm_map(allsentdocs, stripWhitespace)
allsentdocs <- tm_map(allsentdocs,stemDocument)
allsentdtm <- DocumentTermMatrix(allsentdocs)
allsentm <- as.matrix(allsentdtm)
rownames(allsentm) <- paste(substring(rownames(allsentm),1,3),rep("..",nrow(allsentm)), substring(rownames(allsentm), nchar(rownames(allsentm))-12,nchar(rownames(allsentm))-4))
allsentd <- dist(allsentm)
setwd("/afs/athena.mit.edu/user/w/i/williame/Downloads")
write.csv(as.matrix(allsentd), file ="distmatclusterallsentfinal2.csv")
allsentgroups <- hclust(allsentd,method="ward.D")
allsentgroupsplot <- as.dendrogram(allsentgroups)

plot(allsentgroups, hang=-1)
par(mfrow=c(3,1))

plot(allsentgroupsplot, main="Main")
plot(cut(allsentgroupsplot, h=4000)$upper, 
     main="Upper tree of cut at h=1000")
plot(cut(allsentgroupsplot, h=4000)$lower[[2]], 
     main="Second branch of lower tree with cut at h=3000")
library("ggplot2")
library("ggdendro")

ggdendrogram(allsentgroups)
allsentdata <- dendro_data(allsentgroupsplot, type = "rectangle")
head(allsentdata$labels)

allsentdatalabel<- allsentdata[["labels"]][["label"]]

allsentdatalabeldf <- as.data.frame(allsentdatalabel)
allsentdatalabeldf <- as.data.frame(cbind(allsentdatalabeldf, rep("n/a", times = nrow(allsentdatalabeldf))))
gsuballsen <- function(x) {gsub("all .. ", "", x)} #this might not be right
allsentdatalabeldf$allsentdatalabel <- lapply(allsentdatalabeldf$allsentdatalabel, gsuballsen)
setwd("C:/Users/AI/Downloads/ClusterSentAllCTFinal2")
colnames(allsentdatalabeldf)[c(1,2)] <- c("file", "text")
allsentdatalabeldf[] <- lapply(allsentdatalabeldf, as.character)
for (i in 1:nrow(allsentdatalabeldf)) {
  allsentdatalabeldf[i,2] <- as.character(read.table(paste(allsentdatalabeldf[i,1],".txt",sep=""), header = F)[1,1])
  
} 

setwd("/afs/athena.mit.edu/user/w/i/williame/Downloads")

write.csv(allsentdatalabeldf, file = "allsentclusterresultsclean.csv")


pdf("pdfgraphallsentfinal2.pdf", width=40, height=15)
plot(allsentgroups, hang=-1)
dev.off()


