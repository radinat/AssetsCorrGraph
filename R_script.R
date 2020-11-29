# Fileado Case Study
# Part 1
# v.2.0

# Clear workspace and set working directory ----
rm(list = ls()) # Clear workspace
dev.off(dev.list()["RStudioGD"]) # Clear plots
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path) # Get location of current script
setwd(fileloc) # Set working directory to script location
rm(fileloc) # Remove fileloc variable


# Load libraries ----
library(readxl) 
library(quantmod)
library(igraph)
library(dplyr)
library(lubridate)


# Import the available list of tickers ----
equity.tickers=read_excel("List_of_assets.xlsx",sheet=1)
fixed.income.tickers=read_excel("List_of_assets.xlsx",sheet=2)
commodities.tickers=read_excel("List_of_assets.xlsx",sheet=3)

# What we have?
dim(equity.tickers) # 131 - choose 30 of them
dim(fixed.income.tickers) # 10 - choose 2 of them
dim(commodities.tickers) # 3 - choose 1 of them

# Create Environments #трябва да се създадат предварително, ако зареждаме данните от rdata файловете
equity=list()
equity$tickers=equity.tickers$Ticker
equity$env=new.env()

fixed_income=list()
fixed_income$tickers=fixed.income.tickers$Ticker
fixed_income$env=new.env()

commodities=list()
commodities$tickers=commodities.tickers$Ticker
commodities$env=new.env()

# Download historical information ----
# For stocks:
getSymbols(equity$tickers, env=equity$env, from="2006-04-28", "getSymbols.warning4.0"=FALSE) 
rm(.getSymbols, envir=equity$env) 
save(list=ls(equity$env), file='equity.RData',envir=equity$env)

# There is no data for ticker AGN in yahoo finance. Removed it from Excel
# list of assets, because it brings an error when we load the data.

# For fixed income ETFs:
getSymbols(fixed_income$tickers, env=fixed_income$env, from="2006-04-28", "getSymbols.warning4.0"=FALSE) 
rm(.getSymbols, envir=fixed_income$env) 
save(list=ls(fixed_income$env), file='fixed_income.RData',envir=fixed_income$env)

# For commodity ETFs:
getSymbols(commodities$tickers, env=commodities$env, from="2006-04-28", "getSymbols.warning4.0"=FALSE) 
rm(.getSymbols, envir=commodities$env) 
save(list=ls(commodities$env), file='commodities.RData',envir=commodities$env)



# Load data (not downloading it every time) ----
load("equity.RData", envir=equity$env)
load('fixed_income.RData', envir=fixed_income$env)
load('commodities.RData', envir=commodities$env)

# Create data frames with adjusted prices ----
eq=lapply(names(equity$env), get, envir=equity$env)
fi=lapply(names(fixed_income$env), get, envir=fixed_income$env)
com=lapply(names(commodities$env), get, envir=commodities$env)

# Equity data frame
eq_dd=list() 
for(i in 1:length(eq)){
  eq_dd[[i]]=eq[[i]][,6] 
} 
eq_dd=do.call(merge,eq_dd)
colnames(eq_dd)=gsub(".Adjusted","",colnames(eq_dd))
eq_dd=as.data.frame(eq_dd)
                    
# Fixed income ETFs data frame
fi_dd=list() 
for(i in 1:length(fi)){
  fi_dd[[i]]=fi[[i]][,6] 
} 
fi_dd=do.call(merge,fi_dd)
colnames(fi_dd)=gsub(".Adjusted","",colnames(fi_dd))
fi_dd=as.data.frame(fi_dd)
                    
# Commodities data frame
com_dd=list() 
for(i in 1:length(com)){
  com_dd[[i]]=com[[i]][,6] 
} 
com_dd=do.call(merge,com_dd)
colnames(com_dd)=gsub(".Adjusted","",colnames(com_dd)) 
com_dd=as.data.frame(com_dd)

rm(eq,fi,com,i)
rm(commodities.tickers,equity.tickers,fixed.income.tickers)

# Extract date
date=ymd(rownames(com_dd))
first_date=min(date)
last_date=max(date)
#Начална и крайна на какъв принцип са избирани?

# Data preparation
sapply(eq_dd, class)
sapply(fi_dd, class)
sapply(com_dd, class)

# Missing values and dates of first observations available
# Loops for dates
# Equity:
eq_first=vector("numeric")
for (i in 1:ncol(eq_dd)) {
  eq_first[i]=min(which(!is.na(eq_dd[,i])))
}
eq_date=date[eq_first]
eq_date

# To remove equities with not enough observations:
torem=which(eq_first>1)
eq_dd=eq_dd[,-torem]
eq_date=eq_date[-torem]

# Fixed income ETFs:
fi_first=vector("numeric")
for (j in 1:ncol(fi_dd)) {
  fi_first[j]=min(which(!is.na(fi_dd[,j])))
}
fi_date=date[fi_first]
# Remove assets with too many missing values
torem=which(fi_first>500) #500?
fi_dd=fi_dd[,-torem]
fi_date=fi_date[-torem]

# Impute missing values
library(imputeTS)
library(forecast)

toimpn=names(which(colSums(is.na(fi_dd))>0))
for (i in toimpn) {
  name=i
  print(name)
  curr_df = fi_dd[i]
  first=vector("numeric")
  for (k in 1:ncol(curr_df)) {
    first[k]=min(which(!is.na(curr_df[,k])))
  }
  print(first)
  last=vector("numeric")
  for (k in 1:ncol(curr_df)) {
    last[k]=max(which(!is.na(curr_df[,k])))
  }
  print(last)
  bcData <- curr_df[first:last,] #back forecasting data
  x <- bcData
  h <- first-1
  f <- frequency(x)
  print(h)
  revx <- ts(rev(x), frequency=f)
  # Forecast
  fc <- forecast(auto.arima(revx), h)$mean
  for (j in 1:nrow(curr_df)) {
    if (is.na(curr_df[j,1])) {
      fi_dd[name][[1]][[j]] = fc[j]
    } 
  }
}


# Commodities
com_first=vector("numeric")
for (k in 1:ncol(com_dd)) {
  com_first[k]=min(which(!is.na(com_dd[,k])))
}
com_date=date[com_first]

summary(eq_date)
# Auxiliary tables
aux_eq=data.frame(row.names=names(eq_dd), missing_values=colSums(is.na(eq_dd)),
                  first_date=eq_date)
aux_fi=data.frame(row.names=names(fi_dd), missing_values=colSums(is.na(fi_dd)),
                  first_date=fi_date)
aux_com=data.frame(row.names=names(com_dd), missing_values=colSums(is.na(com_dd)),
                  first_date=com_date)

# Bind data frames
all_dd=cbind(eq_dd,fi_dd,com_dd)

# Calculate returns for equities
rets=function(x){ 
  lx=dplyr::lag(x) 
  r=(x-lx)/lx 
  return (r[-1]) 
}
eq_rr=as.data.frame(sapply(eq_dd,rets)) 
rdate=date[-1]

# Calculate returns for all classes
all_rr=as.data.frame(sapply(all_dd,rets)) 
rdate=date[-1]

# Correlation matrix for equities
eq_cc=cor(eq_rr, use = "pairwise.complete.ob")

#Correlation matrix for all classes
all_cc=cor(all_rr, use  = "pairwise.complete.ob")

# Trying clustering for equities
library(psych)
library(maptree)
library(dendextend)
library(cluster)
library(rpart)
#Автоматично инсталиране на пакети?

dissimilarity <- dist((1-abs(eq_cc)), method = "euclidean") # dissimilarity matrix
clus <- hclust(dissimilarity, method = "complete" )
# Dendrogram object
dend <- as.dendrogram(clus)

# Find optimal number of clusters
op_k = kgs(clus, dissimilarity, maxclus = NULL)
plot (names (op_k), op_k, xlab="Number of Clusters", ylab="Penalty")
k_clus = as.integer(names(op_k[which(op_k == min(op_k))])) 

# Cut the dendrogram to obtain exactly the optimal number of clusters
cutClus <- cutree(dend, k = k_clus) #4 #5 #9
# Plot dendrogram
windows()
par(xpd=TRUE)
plot(dend, main="Cluster Dendrogram", xlab="", ylab="Distance")
rect.dendrogram(dend , k = k_clus, border = 2:(k_clus+1))
abline(h = heights_per_k.dendrogram(dend)[[k_clus]], col = 'red', lwd=2) 
color_branches(dend, k = k_clus, col=2:(k_clus+1))
color_labels(dend, k = k_clus, col=2:(k_clus+1))
labels_cex(dend) <- 0.8

# Visualization 2
library(factoextra)
windows()
fviz_cluster(list(data = all_cc, cluster = cutClus))

# Summarize results in a table
cluster_dd=data.frame(tickers=rownames(eq_cc), cluster=cutClus)
freq=as.data.frame(count(cluster_dd, cluster))
colnames(freq) <- c("Cluster", "Number of Assets")

# Cluster analysis for all classes
dissimilarity <- dist((1-abs(all_cc)), method = "euclidean") # dissimilarity matrix
clus <- hclust(dissimilarity, method = "complete" )
# Dendrogram object
dend <- as.dendrogram(clus)

# Find optimal number of clusters
op_k = kgs(clus, dissimilarity, maxclus = NULL)
plot (names (op_k), op_k, xlab="Number of Clusters", ylab="Penalty")
k_clus = as.integer(names(op_k[which(op_k == min(op_k))])) 

# Cut the dendrogram to obtain exactly the optimal number of clusters
cutClus <- cutree(dend, k = k_clus) #4 #5 #9
# Plot dendrogram
windows()
par(xpd=TRUE)
plot(dend, main="Cluster Dendrogram", xlab="", ylab="Distance")
rect.dendrogram(dend , k = k_clus, border = 2:(k_clus+1))
abline(h = heights_per_k.dendrogram(dend)[[k_clus]], col = 'red', lwd=2) 
color_branches(dend, k = k_clus, col=2:(k_clus+1))
color_labels(dend, k = k_clus, col=2:(k_clus+1))
labels_cex(dend) <- 0.8

# Visualization 2
library(factoextra)
windows()
fviz_cluster(list(data = all_cc, cluster = cutClus))

# Other -------
X=cor(all_rr)
L = eigen(X, symmetric=TRUE)
L$values
L$vectors

N = 6  # (use 1st 10 eigenvectors, set N larger to reduce regularization)
P = L$vectors[, 1:N] %*% ((1 / L$values[1:N]) * t(L$vectors[, 1:N]))
P = P / tcrossprod(sqrt(diag(P)))

threshold = 0.80
Q = P * (P > quantile(P, probs=threshold))                           # thresholded precision matrix
g = graph.adjacency(Q, mode="undirected", weighted=TRUE, diag=FALSE) # ...expressed as a graph

x = groups(cluster_louvain(g))
i = unlist(lapply(x, length))
d = order(i, decreasing=TRUE)
x = x[d]
i = i[d]
j = i > 1
s = sum(j)
names(x)[j] = seq(1, s)
names(x)[! j] = s + 1
grp = as.integer(rep(names(x), i))
clrs = c(rainbow(s),'gray')[grp[order(unlist(x))]]
g = set_vertex_attr(g, "color", value=clrs)
gg=graph_from_data_frame(P)
library(threejs)
windows()
graphjs(g, vertex.size=0.2, vertex.shape=colnames(X), edge.alpha=0.5)
plot(g)

