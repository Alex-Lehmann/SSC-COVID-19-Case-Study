Davies_Bouldin = function(A, SS, m) {
    # A - the centres of the clusters
    # SS - the within sum of squares
    # m - the sizes of the clusters
  N = nrow(A)
  S = sqrt(SS/m)
  M = as.matrix(dist(A))
  R = matrix(0, N, N)
  for (i in 1:(N-1)){
    for (j in (i+1):N) {
      R[i,j] = (S[i] + S[j])/M[i,j]
      R[j,i] = R[i,j]
    }
  }
  return(mean(apply(R, 1, max)))
}

census = read.csv("data/LASSO Maps Data.csv")
toCluster = census %>% select(-Unit_ID)

n = 8
bestClusters = kmeans(toCluster, 2, 34)
bestDBI = Davies_Bouldin(bestClusters$centers, bestClusters$withinss, bestClusters$size)
DBI = c(bestDBI, rep(-1, n-1))
for (i in 3:n){
  newClusters = kmeans(toCluster, i, 34)
  DBI[i-2] = Davies_Bouldin(newClusters$centers, newClusters$withinss, newClusters$size)
  if (DBI[i-2] < bestDBI){
    bestClusters = newClusters
    bestDBI = DBI[i-2]
  }
}
bestClusters$cluster

# Drop Toronto
toCluster = census %>% filter(Unit_ID != 3595) %>% select(-Unit_ID)

bestClusters = kmeans(toCluster, 2, 33)
bestDBI = Davies_Bouldin(bestClusters$centers, bestClusters$withinss, bestClusters$size)
DBI = c(bestDBI, rep(-1, n-1))
for (i in 3:n){
  newClusters = kmeans(toCluster, i, 33)
  DBI[i-2] = Davies_Bouldin(newClusters$centers, newClusters$withinss, newClusters$size)
  if (DBI[i-2] < bestDBI){
    bestClusters = newClusters
    bestDBI = DBI[i-2]
  }
}
bestClusters$cluster
