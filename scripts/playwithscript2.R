###USE THE PART TOWARDS THE END WITH THE NUMBERS AND NUMBERS BETWEEN###

library(data.table)
require(data.table)

getwd()
setwd("/afs/athena.mit.edu/user/w/i/williame/Downloads")
clusterres <- read.csv("sentclusterresultsclean.csv", stringsAsFactors = F)
clusterres <- as.data.frame(clusterres[,3])

write.table(as.data.frame(clusterres[3809:3812,]),file = "playwith2.txt", row.names = F, col.names = F)


b <- read.csv("playwith2.txt", header = F)
b[] <- lapply(b, as.character)


l <- as.data.frame(matrix(0, ncol = 1, nrow = (nrow(b))))


for (i in 1:nrow(b)) {
  if (length(unlist(strsplit (b[i,1], " "))) > ncol(l)) {
    l <- as.data.frame(matrix(0, ncol = length(unlist(strsplit (b[i,1], " "))), nrow = (nrow(b))))
  }
  
}
n <- 1
for (i in 1:nrow(b)) {
  n <- as.numeric(as.numeric(ncol(l)) - as.numeric(length(unlist(strsplit(b[i,1], " ")))))
  l[i,] <- c(unlist(strsplit (b[i,1], " ")), rep("IGNORE", times = as.numeric(n)))
}


row1 <- unlist(strsplit(as.character(print(b[1,])), " "))
results <- rep('IGNORE', times = ncol(l))
n <- 1
k <- 1
g <- 1
p <- 0

#TO-DO: Remove any single words that have repeats elsewhere

while (n <= length(row1)) {
  while (k <= length(row1)) {
    p <- 0 
    if (n > 0) {
      if (k > 0) {
        for (i in 1:nrow(l)) {
          for (v in 1:ncol(l)) {
            for (w in 1:ncol(l)) {  
              if (nchar(paste(row1[n:k], collapse = ' ')) <= 3) {
                p <- p
              }
              else if ((paste(l[i, v:w], collapse = ' ')) == (paste(row1[n:k], collapse = ' '))) {
                p <- p + 1
                print (p)
              }
            } 
          }
        }
      }
    }
    if (p >= nrow(b)) {
      results[g] <- paste(row1[n:k], collapse = ' ')
      if (k == length(row1)) {
        n <- k
      }
      if (k < length(row1)) {
        k <- k + 1
      }
    }
    if ((p < nrow(b))) {
      if (n < length(row1)) { 
        n <- k + 1
        k <- n
        g <- g + 2
      }
    }
    if (n == length(row1)) {
      n <- n + 1
      k <- k + 1
    }
  }
}

results <- unlist(strsplit(results, " "))

results <- as.data.frame((results))
results <- cbind(results, rep("f", times = nrow(results)))
colnames(results)[ncol(results)] <- "tf"
results[] <- lapply(results, as.character)
results <- results[complete.cases(results),]

for (i in 2:nrow(results)) {
  if (results[i,1] == "IGNORE") {
    if (results[i,1] == results[(i-1),1]) {
      results[i,2] <- "t"
    }
  }
}

igcheck <- 0
results <- subset(results, tf == "f")  
results <- as.data.frame(results[,-(ncol(results))])
colnames(results)[1] <- "Template"
results <- as.character(results[,1])
if (length(results) < ncol(l)) {
  for (i in (length(results)+1):(ncol(l))) {
    results[i] <- "IGNORE"
    igcheck <- 1
  }
}

seq <- as.data.frame(t(as.data.frame(results)))
seq <- seq[,(1:(ncol(l)))]
seq [] <- lapply(seq, as.character)
l[] <- lapply(l, as.character)

p <- list(as.character(print(seq[1,])))
p <- as.data.frame(p)
colnames(p)[1] <- "col1"
p <- cbind(p, rep("f", times =nrow(p)))
colnames(p)[2] <- "tf"
p[,2] <- sapply(p[,2], as.character)
for (i in 2:nrow(p)) {
  if ((trimws(p[i,1], which = "both")) == "IGNORE") {
    if (p[i,1] == p[(i-1),1])
      p[i,2] <- "t"
  }
}


p <- subset(p, tf == "f")
p <- as.character(print(p[,1]))
p <- paste(p,collapse=" ")
w <- strsplit(p, "IGNORE")
w <- as.data.frame(w[[1]])
colnames(w)[1] <- "col1"
w <- subset(w, col1 != " ")
w <- subset(w, col1 != "")
w <- as.character(w[,1])
w[1]
w[2]
w <- gsub(" ","", w)


non2 <- as.data.frame(matrix(0, ncol = ncol(l), nrow = nrow(l)), stringsAsFactors = F)
non2[] <- lapply(non2, as.numeric)

g <- 1 

for (j in 1:nrow(l)) {
  if (j > 0) {
    for (i in 1:length(w)) {
      if (i > 0) {
        for (k in 1:ncol(l)) {
          for (m in 1:ncol(l)) {
            if (gsub(" ","",paste(l[j,k:m],collapse=" ")) == w[i]) {
                if (g == 1) {
                  non2[j, g] <- k
                  non2[j, g+1] <- m
                  g <- g + 2
                }
                else if ((k > as.numeric(non2[j, (g-1)])) & (m > as.numeric(non2[j, (g-1)]))) {
                  non2[j, g] <- k
                  non2[j, g+1] <- m
                  g <- g + 2
                }
            }
          }
        }
      }
    }
  }
  g <- 1
}
 
non3 <- as.data.frame(matrix("IGNORE", ncol = ncol(l), nrow = nrow(l)), stringsAsFactors = F)

g <- 1




for (m in 1:nrow(non2)) {
  q <- as.character(print(non2[m,]))
  q <- paste(q,collapse=" ")
  q <- strsplit(q, "0")
  q <- q[[1]][1]
  q <- unlist(strsplit(q, " "))
  q <- list(q)
  q <- as.data.frame(q[[1]])
  colnames(q)[1] <- "col1"
  q <- subset(q, col1 != "")
  q[,1] <- sapply(q[,1], as.character)
  q[,1] <- sapply(q[,1], as.numeric)
  i <- 1
  while (i <= nrow(q)) {
    if ((i == 1) & (as.numeric(q[i,1]-1) >=1)) {
      non3[m,g] <- paste(as.character(l[m,1:(as.numeric(q[i,1]-1))]), collapse = " ")
      g <- g + 1
    }
    if ((i %% 2 == 0) & (i != nrow(q)) & ((as.numeric(q[i+1,1]-1)) >= (as.numeric(q[i,1]+1)))) {
      non3[m,g] <- paste(as.character(l[m,(as.numeric(q[i,1]+1)):(as.numeric(q[i+1,1]-1))]), collapse = " ")
      g <- g + 1
    }
    if ((i %% 2 == 0) & (i == nrow(q)) & (as.numeric(q[i,1]+1) <= as.numeric(ncol(l)))) {
      non3[m,g] <- paste(as.character(l[m,(as.numeric(q[i,1]+1)):(as.numeric(ncol(l)))]), collapse = " ")
      g <- g + 1
    }
    if (i < nrow(q)) {
      i <- i + 1
    }
    else {
      break
    }
  }
  g <- 1
}


for (i in 1:ncol(seq)) {
  if (seq[1,i] == "IGNORE") {
    seq[1,i] <- "BLANK" 
  }
}

non3 <- as.data.frame(t(non3))
non3 <- cbind(non3, rep("f", times = nrow(non3)))
colnames(non3)[ncol(non3)] <- "tf"
non3[] <- lapply(non3, as.character)

for (i in 1:nrow(non3)) {
  if (paste(as.character(print(non3[i,1:(ncol(non3)-1)])), collapse = " ") == paste(as.character(rep("IGNORE", times = ncol(non3)-1)), collapse = " ")) {
    non3[i,ncol(non3)] <- "t" 
  }
}

non3 <- subset(non3, tf == "f")  
non3 <- non3[,-(ncol(non3))]
non3 <- as.data.frame(t(non3))

seq <- as.data.frame(t(seq))
seq <- cbind(seq, rep("f", times = nrow(seq)))
colnames(seq)[ncol(seq)] <- "tf"
seq[] <- lapply(seq, as.character)

for (i in 2:nrow(seq)) {
  if (seq[i,1] == "BLANK") {
    if (seq[i,1] == seq[(i-1),1]) {
      seq[i,2] <- "t"
    }
  }
}

if (seq[nrow(seq),1] == "BLANK") {
  if (igcheck == 1) {
    seq[nrow(seq),2] <- "t" 
  }
}

seq <- subset(seq, tf == "f")  
seq <- as.data.frame(seq[,-(ncol(seq))])




colnames(seq)[1] <- "Template"
seq <- as.data.frame(t(seq))


template <- seq



fillins <- non3

for (j in 1:ncol(fillins)) {
  fillinstest <- as.character(fillins[,j])
  for (i in 1:length(fillinstest)) {
    fillinstest[i] <- gsub("IGNORE", "", fillinstest[i])
  }
  fillins[,j] <- fillinstest
}


#write.table(template, file = "template4-18.csv")
#write.table(fillins, file = "fillins4-18.csv")
