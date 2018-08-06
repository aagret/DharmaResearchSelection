
library(data.table)
library(quantmod)

hist <- database[GICS_Sector_Name=="Materials",.(Ticker, Date, PX_LAST)]
setkey(hist, Ticker, Date)

hist[,PX_LAST:= ROC(PX_LAST), by= Ticker]
hist[is.na(PX_LAST), PX_LAST:=0]

hist <- as.xts(dcast(hist, Date ~Ticker))

# replace NA by previous value
for(j in 1:ncol(hist)) hist[, j] = na.locf(hist[, j]) 
hist = hist[, apply(hist, 2, function(x) ! any(is.na(x)))]

colnames(hist) = gsub(" Equity", "", colnames(hist))


X = cor(hist)
L = eigen(X, symmetric=TRUE)

plot(L$values, ylab="eigenvalues")
abline(v=10)

N = 10  # (use 1st 10 eigenvectors, set N larger to reduce regularization)
P = L$vectors[, 1:N] %*% ((1 / L$values[1:N]) * t(L$vectors[, 1:N]))
P = P / tcrossprod(sqrt(diag(P)))


library(igraph)

threshold = 0.95
Q = P * (P > quantile(P, probs=threshold))                           # thresholded precision matrix
g = graph.adjacency(Q, mode="undirected", weighted=TRUE, diag=FALSE) # ...expressed as a graph

# The rest of the code lumps any singletons lacking edges into a single 'unassociated' group shown in gray
# (also assigning distinct colors to the other groups).
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
clrs = c(rainbow(s), "gray")[grp[order(unlist(x))]]
g = set_vertex_attr(g, "color", value=clrs)

library(threejs)
graphjs(g, vertex.size=0.2, vertex.shape=colnames(X), edge.alpha=0.5)

