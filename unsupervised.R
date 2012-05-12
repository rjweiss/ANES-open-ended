#Terrorism
#For now, let's just work with the terrorism data


#cleaning corpus
data.stopwords <- c(stopwords('english'),'data','firefox','private')
data.corpus <- tm_map(data.corpus, stripWhitespace)
data.corpus <- tm_map(data.corpus, removePunctuation)
data.corpus <- tm_map(data.corpus, tolower)
#data.corpus <- tm_map(data.corpus, removeWords, data.stopwords)
data.corpus <- tm_map(data.corpus, stem <- Document)

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

cos.mds <- cmdscale(cosdist.mat)

fig <- ggplot(data = as.data.frame(cos.mds), aes(
  		x = x,
			y = y,
			label = row.names(data))) +
	geom_point(aes(alpha = 0.5)) +
	opts(legend.position = "none")
	ggsave(fig, file= "p_cos_mds.pdf")

cos.sam <- sammon(cosdist.mat)

cos.sam.dat <- as.data.frame(cos.sam$points)

fig <- ggplot(data = cos.sam.dat, aes(
  					x = x,
						y = y,
						label = row.names(data))) +
		geom_point(aes(alpha = 0.5)) +
		opts(legend.position = "none")
ggsave(fig, file= "p_cos_sam.pdf")