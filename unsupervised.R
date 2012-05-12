#This script evaluates unsupervised learning methods on classifying ANES open ended responses

#For now, let's just work with the terrorism data

data <- as.matrix(terrorism$Verbatim[!is.na(terrorism$Verbatim)])

#cleaning corpus
#data.stopwords <- c(stopwords('english'),'data','firefox','private')
data.corpus <- tm_map(data.corpus, stripWhitespace)
data.corpus <- tm_map(data.corpus, removePunctuation)
data.corpus <- tm_map(data.corpus, tolower)
#data.corpus <- tm_map(data.corpus, removeWords, data.stopwords)
#data.corpus <- tm_map(data.corpus, stem <- Document)

data.dtm <- DocumentTermMatrix(data.corpus,
                               control = list(weighting = weightTfIdf,
                                              stopwords = TRUE))

data.dtm.mat <- unique(as.matrix(data.dtm))

cosine.dist <- function(p, q){
  1 - ((p%*%q) / (sqrt(p%*%p * q%*%q)))
}

cosdist.mat <- matrix(NA, nrow=nrow(data.dtm.mat), ncol=nrow(data.dtm.mat))

for (j in 1:nrow(data.dtm.mat)){
  for (k in 1:j){
    cosdist.mat[j,k] <- cosdist.mat[k,j] <- cosine.dist(data.dtm.mat[j,], data.dtm.mat[k,])
  }}

for(j in 1:nrow(cosdist.mat)){
  for(k in 1:j){
    cosdist.mat[j,k]<- cosdist.mat[k,j]<- cosdist.mat[k,j] + runif(1, min=0, max=1e-6)
  }
}

diag(cosdist.mat) <- 0

#TODO: something wrong with NA, use replace() hack for now
temp = cosdist.mat
cosdist.mat <- replace(cosdist.mat, is.na(cosdist.mat), 1)

cos.mds <- cmdscale(cosdist.mat)

colnames(cos.mds) = c("x","y")

fig <- ggplot(data = as.data.frame(cos.mds), aes(
  x = x,
  y = y,
  label = row.names(data))) +
    geom_point(aes(alpha = 0.5)) +
    opts(legend.position = "none")
ggsave(fig, file= "/Users/Rebecca/Dropbox/research/ANES/plots/p_cos_mds.pdf")

cos.sam <- sammon(cosdist.mat)

cos.sam <- as.data.frame(cos.sam$points)
colnames(cos.sam) = c("x","y")

fig <- ggplot(data = cos.sam, aes(
  x = x,
  y = y,
  label = row.names(data))) +
    geom_point(aes(alpha = 0.5)) +
    opts(legend.position = "none")
ggsave(fig, file= "/Users/Rebecca/Dropbox/research/ANES/plots/p_cos_sam.pdf")


###################
#NUMBER OF CLUSTERS
#chosen arbitrarily
nclust = 8
###################

#constructing clustering models
m1 <- kmeans(cosdist.mat, centers = nclust, iter.max = 10)
#m2 <- multmixEM(data.dtm.mat, k = nclust, lambda=NULL, theta=NULL, maxit=1000, epsilon=1e-08, verb=FALSE)

#c <- apply(m2$posterior, 1, which.max)

#justin grimmer's code for monroe, colaresi, and quinn "lexical feature selection"
fightin <- function(clust.num, clustering, data){
  topic <- clust.num
  cluster <- clustering
  strength=500
  aa <- which(cluster==topic)
  bb <- which(cluster!= topic)
  sub <- data[aa,]
  
  if(is.null(nrow(sub))==F){
    sum.in <- apply(sub, 2, sum)
    overall <- apply(sub, 2, mean)
  }
  
  if(is.null(nrow(sub))==T){
    sum.in<- sub
    overall<- sub
  }
  
  prior <- overall*strength +1
  sum.out <- apply(data[bb,], 2, sum)
  tots.in <- sum(sum.in + prior)
  tots.out <- sum(sum.out + prior)
  prop.in <- (sum.in + prior)/tots.in
  prop.out <- (sum.out + prior)/tots.out
  odds.in <- (prop.in)/(1 -prop.in)
  odds.out <- (prop.out)/(1 -prop.out)
  log.odds <- log(odds.in) - log(odds.out)
  vars <- 1/(sum.in + prior) + 1/(sum.out + prior)
  scores <- log.odds/vars
  return(scores)
}

#ncol = number of clusters
#n most frequent numbers
nfreq = 20

#k-means
m1.words <- matrix(NA, nrow = nfreq, ncol = nclust)
m1.scores <- matrix(NA, nrow = nfreq, ncol = nclust)
m1.mat <- matrix(NA, nrow = nfreq * nclust, ncol = 3)
for(j in 1:nclust){
  temp <- fightin(j, m1$cluster, data.dtm.mat)
  m1.words[,j] <- colnames(data.dtm.mat)[order(temp, decreasing=T)[1:nfreq]]
  m1.scores[,j] <- temp[order(temp, decreasing=T)[1:nfreq]]
}

m1.mat[,1] = as.character(m1.words)
m1.mat[,2] = as.character(m1.scores)
m1.mat[,3] = rep(nclust:1, each=nfreq)

m1.df <- as.data.frame(m1.mat, stringsAsFactors = FALSE)
names(m1.df) = c("words","scores","cluster")
m1.df$scores <- as.numeric(m1.df$scores)

#plotting kmeans in-cluster word frequencies
p <- ggplot(data = m1.df, aes(
  x = log(scores),
  y = reorder(words, log(scores), max),
  label = words)) +
    geom_text(hjust =1, aes(size = as.numeric(log(scores)))) +
    facet_wrap (~ cluster, scales = "free_y") +
    ylab("Words in order of log(Scores)") +
    xlab("log(Scores)") +
    opts(title = "Clusters Defined by Word Frequencies", 
         axis.text.y = theme_blank(),
         axis.ticks = theme_blank(),
         legend.position = "none")
ggsave(filename="/Users/Rebecca/Dropbox/research/ANES/plots/p_kmeans_freq.pdf", plot=p, device=pdf)

d <- as.data.frame(cos.mds)
colnames(d) = c("x","y")
d$cluster1 <- m1$cluster
#d$cluster2 <- c

p <- ggplot(data = d, aes(
  x = x,
  y = y,
  color = factor(cluster1))) +
    geom_point(alpha = 0.6) +
    scale_colour_brewer(name = "Clusters", palette = "Set1") +
    opts(title = "Classical MDS using k-means and cosine distance",
         legend.position=c(.9,.75), 
         legend.background = theme_rect(fill="white"))
ggsave(filename="/Users/Rebecca/Dropbox/research/ANES/plots/p_kmeans_cosdist_clusters.pdf",plot = p, device = pdf)
