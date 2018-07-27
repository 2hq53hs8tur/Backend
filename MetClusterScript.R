library(data.table)
library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)
setwd("/afs/athena.mit.edu/user/w/i/williame/Downloads")
mt <- read.csv("sample_output_full (1).csv", header = F, stringsAsFactors = F)
setwd("/afs/athena.mit.edu/user/w/i/williame/Downloads/ClusterMetFinal2")
mtcol <- rep("f", times = nrow(mt))
mt <- cbind(mt,mtcol)
mt[] <- lapply(mt, as.character)
for (i in 1:nrow(mt)) {
  for (j in 1:ncol(mt)) {
    if (!is.na(mt[i,j])) {
      if (mt[i,j] %like% "Processing text") {
        mt[i, ncol(mt)] <- "t"
      }
    }
  }
}
n <- 1
k <- 1
p <- 1
for (i in 1:nrow(mt)) {
  if (!is.na(mt[i,ncol(mt)])) { 
    if ((mt[i,ncol(mt)] != "f") & i > n) {
      k <- i - 1
      p <- as.character(p)
      str <- paste("mt", p, ".txt", sep = "")
      write.table(as.data.frame(mt[(n+1):k,]),file = str, row.names = F, col.names = F)
      n <- k + 1
      p <- as.numeric(p)
      p <- p + 1
    }
  }
}

mt[1,ncol(mt)] != "f"

metdocs <- Corpus(DirSource("/afs/athena.mit.edu/user/w/i/williame/Downloads/ClusterMetFinal2"))
writeLines(as.character(metdocs[[25]]))

metdocs <- tm_map(metdocs,content_transformer(tolower))
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
metdocs <- tm_map(metdocs, toSpace, '"')
metdocs <- tm_map(metdocs, removeWords, stopwords("english"))
metdocs <- tm_map(metdocs, stripWhitespace)
metdocs <- tm_map(metdocs,stemDocument)
metdtm <- DocumentTermMatrix(metdocs)
metm <- as.matrix(metdtm)
rownames(metm) <- paste(substring(rownames(metm),1,3),rep("..",nrow(metm)), substring(rownames(metm), nchar(rownames(metm))-12,nchar(rownames(metm))-4))
metd <- dist(metm)
setwd("/afs/athena.mit.edu/user/w/i/williame/Downloads")
write.csv(as.matrix(metd), file ="distmatclustermetfinal2.csv")
metgroups <- hclust(metd,method="ward.D")
metgroupsplot <- as.dendrogram(metgroups)
plot(metgroups, hang=-1)
par(mfrow=c(3,1))

plot(metgroupsplot, main="Main")
plot(cut(metgroupsplot, h=100)$upper, 
     main="Upper tree of cut at h=1000")
plot(cut(metgroupsplot, h=100)$lower[[2]], 
     main="Second branch of lower tree with cut at h=1000")
library("ggplot2")
library("ggdendro")

ggdendrogram(metgroups)
metdata <- dendro_data(metgroupsplot, type = "rectangle")
head(metdata$labels)

metdatalabel<- metdata[["labels"]][["label"]]

metdatalabeldf <- as.data.frame(metdatalabel)
metdatalabeldf <- as.data.frame(cbind(metdatalabeldf, rep("n/a", times = nrow(metdatalabeldf))))
gsubmet <- function(x) {gsub("mt\\d+ .. ", "", x)}
metdatalabeldf$metdatalabel <- lapply(metdatalabeldf$metdatalabel, gsubmet)
setwd("/afs/athena.mit.edu/user/w/i/williame/Downloads/ClusterMetFinal3Text")
n <- 1
k <- 1
p <- 1
for (i in 1:nrow(mt)) {
  if (!is.na(mt[i,ncol(mt)])) { 
    if ((mt[i,ncol(mt)] != "f") & i > n) {
      k <- i - 1
      p <- as.character(p)
      str <- paste("mt", p, "text", ".txt", sep = "")
      write.table(as.data.frame(mt[n:k,]),file = str, row.names = F, col.names = F)
      n <- k + 1
      p <- as.numeric(p)
      p <- p + 1
    }
  }
}
colnames(metdatalabeldf)[c(1,2)] <- c("file", "text")
metdatalabeldf[] <- lapply(metdatalabeldf, as.character)
gsubproc <- function(x) {gsub("\'Processing text_000N_\\d+.tx.1:", "", x)}
test1 <- gsubproc(as.character(read.table(paste(metdatalabeldf[1,1],"text",".txt",sep=""), header = F)[1,2]))
for (i in 1:nrow(metdatalabeldf)) {
  metdatalabeldf[i,2] <- gsubproc(as.character(read.table(paste(metdatalabeldf[i,1],"text",".txt",sep=""), header = F, fill = T)[1,2]))
  
} 

setwd("/afs/athena.mit.edu/user/w/i/williame/Downloads")

write.csv(metdatalabeldf, file = "metclusterresultsclean3.csv")


pdf("pdfgraphsmetfinal1.pdf", width=40, height=15)
plot(metgroups, hang=-1)
dev.off()


