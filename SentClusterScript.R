########FINAL STUFF############

library("tm")
library("SnowballC")
library("ggplot2")
library("wordcloud")




###SENTENCE###

setwd("C:/Users/AI/Downloads")
ctclnb <- read.csv("ctsampleinds_cleaned_nobool.csv", header = F)


setwd("C:/Users/AI/Downloads/ClusterSentFinal1")

n <- 1
for (i in 1:nrow(ctclnb)) {
  for (j in 1:ncol(ctclnb)) {
    if (!is.na(ctclnb[i,j])) {
      if (ctclnb[i,j] != "") {
        n <- as.character(n)
        str <- paste("sent", n, ".txt", sep = "")
        write.table(as.data.frame(ctclnb[i,j]),file = str, row.names = F, col.names = F)
        n <- as.numeric(n)
        n <- n + 1
      }
    }
  }
}


sentdocs <- Corpus(DirSource("C:/Users/AI/Downloads/ClusterSentFinal1"))
writeLines(as.character(sentdocs[[25]]))

sentdocs <- tm_map(sentdocs,content_transformer(tolower))
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
sentdocs <- tm_map(sentdocs, toSpace, '"')
sentdocs <- tm_map(sentdocs, removeWords, stopwords("english"))
sentdocs <- tm_map(sentdocs, stripWhitespace)
sentdocs <- tm_map(sentdocs,stemDocument)
sentdtm <- DocumentTermMatrix(sentdocs)
sentm <- as.matrix(sentdtm)
rownames(sentm) <- paste(substring(rownames(sentm),1,3),rep("..",nrow(sentm)), substring(rownames(sentm), nchar(rownames(sentm))-12,nchar(rownames(sentm))-4))
sentd <- dist(sentm)
setwd("C:/Users/AI/Downloads")
write.csv(as.matrix(sentd), file ="distmatclustersentfinal1.csv")
sentgroups <- hclust(sentd,method="ward.D")
sentgroupsplot <- as.dendrogram(sentgroups)

plot(sentgroups, hang=-1)
par(mfrow=c(3,1))

plot(sentgroupsplot, main="Main")
plot(cut(sentgroupsplot, h=4000)$upper, 
     main="Upper tree of cut at h=1000")
plot(cut(sentgroupsplot, h=4000)$lower[[2]], 
     main="Second branch of lower tree with cut at h=3000")
library("ggplot2")
library("ggdendro")

ggdendrogram(sentgroups)
sentdata <- dendro_data(sentgroupsplot, type = "rectangle")
head(sentdata$labels)

sentdatalabel<- sentdata[["labels"]][["label"]]

sentdatalabeldf <- as.data.frame(sentdatalabel)
sentdatalabeldf <- as.data.frame(cbind(sentdatalabeldf, rep("n/a", times = nrow(sentdatalabeldf))))
gsubsen <- function(x) {gsub("sen .. ", "", x)}
sentdatalabeldf$sentdatalabel <- lapply(sentdatalabeldf$sentdatalabel, gsubsen)
setwd("C:/Users/AI/Downloads/ClusterSentFinal1")
colnames(sentdatalabeldf)[c(1,2)] <- c("file", "text")
sentdatalabeldf[] <- lapply(sentdatalabeldf, as.character)
test1 <- as.character(read.table(paste(sentdatalabeldf[1,1],".txt",sep=""), header = F)[1,1])
for (i in 1:nrow(sentdatalabeldf)) {
  sentdatalabeldf[i,2] <- as.character(read.table(paste(sentdatalabeldf[i,1],".txt",sep=""), header = F)[1,1])
  
} 

setwd("C:/Users/AI/Downloads")

write.csv(sentdatalabeldf, file = "sentclusterresultsclean.csv")

pdf("pdfgraphsentfinal2.pdf", width=40, height=15)
plot(sentgroups, hang=-1)
dev.off()

