# set working directory
setwd("/Users/aliciang/Downloads/FIT3152-DATA-
ANALYTICS/FIT3152_Assignment3")
# clean up the environment before starting
install.packages("ggplot2")
install.packages("igraph")
rm(list = ls())
library(slam)
library(tm)
library(SnowballC)
library(ggplot2)
library(igraph)
#Get file path to folder "NgChenTing31861148" where the
documents are located
cname = file.path(".", " NgChenTing31861148")
# Turn all documents into a Corpus
docs = Corpus(DirSource((cname)))
# Specific Transformations
toSpace <- content_transformer(function(x, pattern)
  gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "’")
toSpace <- content_transformer(function(x, pattern)
  gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "-")
toSpace <- content_transformer(function(x, pattern)
  gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "–")
toSpace <- content_transformer(function(x, pattern)
  gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "“")
toSpace <- content_transformer(function(x, pattern)
  gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "”")
toSpace <- content_transformer(function(x, pattern)
  gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "one")
toSpace <- content_transformer(function(x, pattern)
  gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "two")
toSpace <- content_transformer(function(x, pattern)
  gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "also")
toSpace <- content_transformer(function(x, pattern)
  gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "get")
toSpace <- content_transformer(function(x, pattern)
  gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "just")
toSpace <- content_transformer(function(x, pattern)
  gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "see")
#Tokenisation
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, content_transformer(tolower))
#Filter words
# Remove stop words and white space
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace)
# Stem
docs <- tm_map(docs, stemDocument, language = "english")
# Create DTM
dtm = DocumentTermMatrix(docs)
dtm = as.data.frame(as.matrix(dtm))
write.csv(dtm, "OriginalDTM.csv")
dim(dtm)
Tokens
dtm = DocumentTermMatrix(docs)
dtms <- removeSparseTerms(dtm, 0.45)
dtms = as.matrix(dtms)
write.csv(dtms,"SparsedMovie.csv")
dim(dtms)
words
freq <- colSums(as.matrix(dtms))
ord = order(freq)
freq[tail(ord,20)]
# Orginial DTM has 1707
# 20 columns means 19
### QUESTION 4
##############################################################
##
# Calculates Inverse Document Frequency
( idf <- log( ncol(dtms) / ( 1 + rowSums(dtms != 0) ) ) )
# Converts the IDF values into a diagonal matrix
( idf <- diag(idf) )
# Calculates the TF-IDF matrix
tf_idf <- crossprod(dtms, idf)
colnames(tf_idf) <- rownames(dtms)
# Calculates the cosine distance between documents
cosine_dist = 1-crossprod(tf_idf) /
  (sqrt(colSums(tf_idf^2)%*%t(colSums(tf_idf^2))))
# remove NaN using 0
cosine_dist[is.na(cosine_dist)] <- 0
# Converts the cosine distance matrix into a distance object
cosine_dist <- as.dist(cosine_dist)
# Creating dendrogram
cluster1 <- hclust(cosine_dist, method = "ward.D")
plot(cluster1)
# Boxing clusters
rect.hclust(cluster1, k = 3, border = "red")
## QUANTITATIVE MEASURE using confusion matrix##
topics =
  c("Horror","Horror","Romance","Horror","Law","Law","Horror",
    "Law","Law","Romance","Law","Romance","Romance","Horror","Roma
nce")
groups = cutree(cluster1, k = 3)
table(GroupNames = topics, Clusters = groups)
### QUESTION 5
##############################################################
##
# Convert Document Term Matrix to Binary Matrix
dtmsx = as.matrix(dtms)
dtmsx = as.matrix((dtmsx > 0) + 0)
# Multiply Binary Matrix by its transpose
ByAbsMatrix = dtmsx %*% t(dtmsx)
# Make leading diagonal zero to complete Abstract Network Data
diag(ByAbsMatrix) = 0
# Important nodes
max(colSums(ByAbsMatrix))
closeness = format(closeness(ByAbs), digits = 2)
degree = as.table(degree(ByAbs))
betweenness = as.table(betweenness(ByAbs))
# Create an Abstracts Network Plot
ByAbs = graph_from_adjacency_matrix(ByAbsMatrix, mode =
                                      "undirected", weighted = TRUE)
plot(ByAbs)
mean(E(ByAbs)$weight)
ReScaledWidth <- c(E(ByAbs)$weight)/5
# Only highlight red for edges where sharing tokens > 6
plot(ByAbs, edge.width = ReScaledWidth,
     edge.color = ifelse(ReScaledWidth > 2, "red", "grey"))
E(ByAbs)$weight
# Heatmap of Network Matrix
palf <- colorRampPalette(c("white","purple"))
heatmap(ByAbsMatrix[,15:1], Rowv = NA, Colv = NA, col =
          palf(100),
        scale="none", margins=c(10,10), main = "Heatmap of
Abstract Network Data" )
### QUESTION 6
##############################################################
##
# start with original document-term matrix
dtmsx = as.matrix(dtms)
# convert to binary matrix
dtmsx = as.matrix((dtmsx > 0) + 0)
# multiply transpose binary matrix by binary matrix
ByTokenMatrix = t(dtmsx) %*% dtmsx
# make leading diagonal zero
diag(ByTokenMatrix) = 0
# Network Plot for Tokens
ByAbs = graph_from_adjacency_matrix(ByTokenMatrix, mode = "undirected",weighted = TRUE)
plot(ByAbs)
# Highest value in Token Matrix
max(colSums(ByTokenMatrix))
# Importance of nodes
d = as.table(degree(ByAbs))
b = as.table(betweenness(ByAbs))
c = as.table(closeness(ByAbs))
e = as.table(evcent(ByAbs)$vector)
stats = as.data.frame(rbind(d,b,c,e))
stats = as.data.frame(t(stats))
colnames(stats) = c("degree", "betweenness", "closeness",
                    "eigenvector")
# Clustering using Greedy
cfg = cluster_fast_greedy(as.undirected(ByAbs))
g_cfg = plot(cfg,
             as.undirected(ByAbs),vertex.label=V(ByAbs)$role,main="Fast Greedy for Token Clustering")
### QUESTION 7
##############################################################
##
## PRE_PROCESSING ##
dtmsa = as.data.frame(dtms) # clone dtms
dtmsa$ABS = rownames(dtmsa) # adds a new column named "ABS" to
contain names
dtmsb = data.frame()        # empty data frame
# Iterate over DTM and extract their corresponding weight,
document name
# and token name to sppend to the dtmsb data frame
for (i in 1:nrow(dtmsa)){
  for (j in 1:(ncol(dtmsa)-1)){
    touse = cbind(dtmsa[i,j], dtmsa[i,ncol(dtmsa)],
                  colnames(dtmsa[j]))
    dtmsb = rbind(dtmsb, touse ) } } # close loops
colnames(dtmsb) = c("weight", "abs", "token")
dtmsc = dtmsb[dtmsb$weight != 0,] # delete 0 weights
# put colunms in order: abs, token, weight
dtmsc = dtmsc[,c(2,3,1)]
# Importance of nodes
d = as.table(degree(ByAbs))
b = as.table(betweenness(ByAbs))
c = as.table(closeness(ByAbs))
e = as.table(evcent(ByAbs)$vector)
stats = as.data.frame(rbind(d,b,c,e))
stats = as.data.frame(t(stats))
colnames(stats) = c("degree", "betweenness", "closeness",
                    "eigenvector")
# Creating Bipartite Network
g <- graph.data.frame(dtmsc, directed=FALSE)
bipartite.mapping(g)
# Adding type, colour, shape to vertices and edges
V(g)$type <- bipartite_mapping(g)$type
V(g)$color <- ifelse(V(g)$type, "pink", "darkslategray1")
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
E(g)$color <- "lightgray"
plot(g)
# Closeness of each node in Network
max(closeness(g))
# Clustering using Greedy
cfg = cluster_fast_greedy(as.undirected(g))
g_cfg = plot(cfg,
             as.undirected(g),vertex.label=V(g)$role,main="Fast Greedy")