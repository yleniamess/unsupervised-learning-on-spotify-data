 
# COLLECT & ARRANGE DATA ----
library(spotifyr)                                       
get_spotify_authorization_code(source("set_token.R"))   # access authorization credentials

playlist_username <- 'Spotify'
playlist_uris <- c('37i9dQZF1DXcF6B6QPhFDv',      # Rock This
                   '37i9dQZF1DX0BcQWzuB7ZO',      # Dance Hits
                   '37i9dQZF1DXcA6dRp8rwj6',      # Beats & Rhymes
                   '37i9dQZF1DWZeKCadgRdKQ')      # Deep Focus
playlist_audio_features <- get_playlist_audio_features(playlist_username, playlist_uris)

library(tidyr)
# unnest "track.artists" variable, getting one row of output for each element of the list
my_dataset <- unnest(data = playlist_audio_features, cols = track.artists, keep_empty = TRUE)

my_dataset <- data.frame(
  "song" = my_dataset$track.name,
  "artist" = my_dataset$name,
  "playlist" = my_dataset$playlist_name,
  "acousticness" = my_dataset$acousticness,
  "danceability" = my_dataset$danceability,
  "energy" = my_dataset$energy,
  "loudness" = my_dataset$loudness,
  "tempo" = my_dataset$tempo,
  "valence" = my_dataset$valence
)

library(tidyverse)
my_dataset <- my_dataset %>% distinct()                  # remove duplicate rows
my_dataset <- my_dataset[!duplicated(my_dataset$song),]  # keep only one row per song

rock <- sample_n(subset(my_dataset, playlist == "Rock This"), 50)
dance <- sample_n(subset(my_dataset, playlist == "Dance Hits"), 50)
hiphop <- sample_n(subset(my_dataset, playlist == "Beats & Rhymes"), 50)
cinematic <- sample_n(subset(my_dataset, playlist == "Deep Focus"), 50)

my_dataset <- rbind(rock, dance, hiphop, cinematic)      # combine dataframes
my_dataset <- sample_n(my_dataset, size = 200)

write.csv(my_dataset, "spotify_dataset.csv", row.names = FALSE)

# MY DATASET ----

data <- read.csv("spotify_dataset.csv")
# convert first column to row name/index
my_dataset <- data[,-1]
rownames(my_dataset) <- data[,1]

attach(my_dataset)

head(my_dataset)
str(my_dataset)

songs = row.names(my_dataset) # units
names(my_dataset)             # variables

my_dataset <- my_dataset[,-c(1,2)] # drop non-numeric variable

# UNIVARIATE ANALYSIS ----

summary(my_dataset)  # main statistical indices of each variable

boxplot(acousticness, main = "Boxplot of acousticness")
boxplot(danceability, main = "Boxplot of danceability")
boxplot(energy, main = "Boxplot of energy")
boxplot(loudness, main = "Boxplot of loudness")
boxplot(tempo, main = "Boxplot of tempo")
boxplot(valence, main = "Boxplot of valence")

# FIT UNIVARIATE MODELS
library(gamlss)

# Fit real-valued continuous distributions models on "loudness"
par(mfrow = c(3,2))
fit.NO <- histDist(loudness, family=NO, nbins = 30, main="Normal distribution")
fit.NET <- histDist(loudness, family=NET, nbins = 30, main="Normal Exponential t distribution")
fit.TF <- histDist(loudness, family=TF, nbins = 30, main="t-distribution")
fit.GU <- histDist(loudness, family=GU, nbins = 30, main="Gumbel distribution")
fit.ST5 <- histDist(loudness, family=ST5, nbins = 30, main="Skew t type 5 distribution")
fit.SHASH <- histDist(loudness, family=SHASH, nbins = 30, main="Shash distribution")

AIC(fit.NO,fit.NET,fit.TF,fit.GU,fit.ST5, fit.SHASH)

fit.info <- function(distribution, parameter) {
  cbind(logLik(distribution), distribution$sbc)
}

fit.criteria <- rbind(fit.info(fit.NO), 
                      fit.info(fit.NET), 
                      fit.info(fit.TF), 
                      fit.info(fit.GU), 
                      fit.info(fit.ST5), 
                      fit.info(fit.SHASH))
colnames(fit.criteria) <- c("logLik", "BIC")
row.names(fit.criteria) <- c("fit.NO","fit.NET","fit.TF","fit.GU","fit.ST5", "fit.SHASH")

fitted(fit.SHASH, "mu")[1]    # ML estimated 1st parameter
fitted(fit.SHASH, "sigma")[1] # ML estimated 2nd parameter
fitted(fit.SHASH, "nu")[1]    # ML estimated 3rd parameter
fitted(fit.SHASH, "tau")[1]   # ML estimated 4th parameter

LR.test(fit.NO,fit.SHASH) # Likelihood-ratio test

# Fit continuous distributions in the real positive line on "tempo"
par(mfrow = c(3,2))
fit.EXP <- histDist(tempo, family=EXP, nbins = 30, main="Exponential distribution")
fit.GA <- histDist(tempo, family=GA, nbins = 30, main="Gamma distribution")
fit.IG <- histDist(tempo, family=IG, nbins = 30, main="Inverse Gaussian distribution")
fit.LOGNO <- histDist(tempo, family=LOGNO, nbins = 30, main="Log-Normal distribution")
fit.WEI <- histDist(tempo, family=WEI, nbins = 30, main="Weibull distribution")
fit.BCPE <- histDist(tempo, family=BCPE, nbins = 30, main="Box-Cox Power Exponential")

AIC(fit.EXP,fit.GA,fit.IG,fit.LOGNO,fit.WEI,fit.BCPE)

fit.criteria <- rbind(fit.info(fit.EXP), 
                      fit.info(fit.GA), 
                      fit.info(fit.IG), 
                      fit.info(fit.LOGNO), 
                      fit.info(fit.WEI),
                      fit.info(fit.BCPE))
colnames(fit.criteria) <- c("logLik", "BIC")
row.names(fit.criteria) <- c("fit.EXP","fit.GA","fit.IG","fit.LOGNO","fit.WEI","fit.BCPE")

fitted(fit.BCPE, "mu")[1]    # ML estimated 1st parameter
fitted(fit.BCPE, "sigma")[1] # ML estimated 2nd parameter
fitted(fit.BCPE, "nu")[1]    # ML estimated 3rd parameter
fitted(fit.BCPE, "tau")[1]   # ML estimated 4th parameter

LR.test(fit.GA,fit.BCPE)     # Likelihood-ratio test

# Fit continuous distributions in [0,1] & in the real positive line on "valence"
par(mfrow = c(3,2))
fit.BE <- histDist(valence, family=BE, nbins = 30, main="Beta distribution")
fit.LOGITNO <- histDist(valence, family=LOGITNO, nbins = 30, main="Logit-Normal distribution")
fit.GB1 <- histDist(valence, family=GB1, nbins = 30, main="Generalized Beta type 1 distribution")
fit.IG <- histDist(valence, family=IG, nbins = 30, main="Inverse Gaussian distribution")
fit.LOGNO <- histDist(valence, family=LOGNO, nbins = 30, main="Log-Normal distribution")
fit.WEI <- histDist(valence, family=WEI, nbins = 30, main="Weibull distribution")

AIC(fit.BE,fit.LOGITNO,fit.GB1,fit.IG,fit.LOGNO,fit.WEI)

fit.criteria <- rbind(fit.info(fit.BE), 
                      fit.info(fit.LOGITNO), 
                      fit.info(fit.GB1),
                      fit.info(fit.IG),
                      fit.info(fit.LOGNO), 
                      fit.info(fit.WEI))
colnames(fit.criteria) <- c("logLik", "BIC")
row.names(fit.criteria) <- c("fit.BE","fit.LOGITNO","fit.GB1","fit.IG","fit.LOGNO","fit.WEI")

fitted(fit.LOGITNO, "mu")[1]    # ML estimated 1st parameter
fitted(fit.LOGITNO, "sigma")[1] # ML estimated 2nd parameter

# MIXTURE DISTRIBUTIONS
library(gamlss.mx)

XX <- tempo
fit.GA.2 <- gamlssMXfits(n = 5, XX~1, family = GA, K = 2) 
fit.GA.3 <- gamlssMXfits(n = 5, XX~1, family = GA, K = 3)
fit.GA.4 <- gamlssMXfits(n = 5, XX~1, family = GA, K = 4)

mu.hat1 <- exp(fit.GA.2[["models"]][[1]][["mu.coefficients"]])    
sigma.hat1 <- exp(fit.GA.2[["models"]][[1]][["sigma.coefficients"]])
mu.hat2 <- exp(fit.GA.2[["models"]][[2]][["mu.coefficients"]])    
sigma.hat2 <- exp(fit.GA.2[["models"]][[2]][["sigma.coefficients"]])
hist(XX, breaks = 50,freq = FALSE, main = "Histogram of tempo", xlab = "tempo")
lines(seq(min(XX),max(XX),length=length(XX)),fit.GA.2[["prob"]][1]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat1, sigma = sigma.hat1),lty=2,lwd=3,col="coral2")
lines(seq(min(XX),max(XX),length=length(XX)),fit.GA.2[["prob"]][2]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat2, sigma = sigma.hat2),lty=2,lwd=3,col="deepskyblue3")
lines(seq(min(XX),max(XX),length=length(XX)),
      fit.GA.2[["prob"]][1]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat1, sigma = sigma.hat1) +
      fit.GA.2[["prob"]][2]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat2, sigma = sigma.hat2),
      lty = 1, lwd = 3, col = 1)

mu.hat1 <- exp(fit.GA.3[["models"]][[1]][["mu.coefficients"]])    
sigma.hat1 <- exp(fit.GA.3[["models"]][[1]][["sigma.coefficients"]])
mu.hat2 <- exp(fit.GA.3[["models"]][[2]][["mu.coefficients"]])    
sigma.hat2 <- exp(fit.GA.3[["models"]][[2]][["sigma.coefficients"]])
mu.hat3 <- exp(fit.GA.3[["models"]][[3]][["mu.coefficients"]])    
sigma.hat3 <- exp(fit.GA.3[["models"]][[3]][["sigma.coefficients"]])
hist(XX, breaks = 50,freq = FALSE, main = "Histogram of tempo", xlab = "tempo")
lines(seq(min(XX),max(XX),length=length(XX)),fit.GA.3[["prob"]][1]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat1, sigma = sigma.hat1),lty=2,lwd=3,col="coral2")
lines(seq(min(XX),max(XX),length=length(XX)),fit.GA.3[["prob"]][2]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat2, sigma = sigma.hat2),lty=2,lwd=3,col="deepskyblue3")
lines(seq(min(XX),max(XX),length=length(XX)),fit.GA.3[["prob"]][3]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat3, sigma = sigma.hat3),lty=2,lwd=3,col="khaki")
lines(seq(min(XX),max(XX),length=length(XX)),
      fit.GA.3[["prob"]][1]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat1, sigma = sigma.hat1) +
      fit.GA.3[["prob"]][2]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat2, sigma = sigma.hat2) +
      fit.GA.3[["prob"]][3]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat3, sigma = sigma.hat3),
      lty = 1, lwd = 3, col = 1)

mu.hat1 <- exp(fit.GA.4[["models"]][[1]][["mu.coefficients"]])    
sigma.hat1 <- exp(fit.GA.4[["models"]][[1]][["sigma.coefficients"]])
mu.hat2 <- exp(fit.GA.4[["models"]][[2]][["mu.coefficients"]])    
sigma.hat2 <- exp(fit.GA.4[["models"]][[2]][["sigma.coefficients"]])
mu.hat3 <- exp(fit.GA.4[["models"]][[3]][["mu.coefficients"]])    
sigma.hat3 <- exp(fit.GA.4[["models"]][[3]][["sigma.coefficients"]])
mu.hat4 <- exp(fit.GA.4[["models"]][[4]][["mu.coefficients"]])    
sigma.hat4 <- exp(fit.GA.4[["models"]][[4]][["sigma.coefficients"]])
hist(XX, breaks = 50,freq = FALSE, main = "Histogram of tempo", xlab = "tempo")
lines(seq(min(XX),max(XX),length=length(XX)),fit.GA.4[["prob"]][1]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat1, sigma = sigma.hat1),lty=2,lwd=3,col="coral2")
lines(seq(min(XX),max(XX),length=length(XX)),fit.GA.4[["prob"]][2]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat2, sigma = sigma.hat2),lty=2,lwd=3,col="deepskyblue3")
lines(seq(min(XX),max(XX),length=length(XX)),fit.GA.4[["prob"]][3]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat3, sigma = sigma.hat3),lty=2,lwd=3,col="khaki")
lines(seq(min(XX),max(XX),length=length(XX)),fit.GA.4[["prob"]][4]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat4, sigma = sigma.hat4),lty=2,lwd=3,col="darkblue")
lines(seq(min(XX),max(XX),length=length(XX)),
      fit.GA.4[["prob"]][1]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat1, sigma = sigma.hat1) +
      fit.GA.4[["prob"]][2]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat2, sigma = sigma.hat2) +
      fit.GA.4[["prob"]][3]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat3, sigma = sigma.hat3) +
      fit.GA.4[["prob"]][4]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat4, sigma = sigma.hat4),
      lty = 1, lwd = 3, col = 1)

AIC(fit.GA, fit.GA.2, fit.GA.3, fit.GA.4)

fit.GA$sbc
fit.GA.2$sbc
fit.GA.3$sbc
fit.GA.4$sbc

fit.GA.2$prob # mixture weights

# PRINCIPAL COMPONENT ANALYSIS ----

round(apply(my_dataset, 2, mean), 4)
round(apply(my_dataset, 2, var), 4)

df.scaled <- scale(my_dataset)

cor_matrix <- cor(df.scaled) # correlation matrix since data have been standardized
eigen <- eigen(cor_matrix)   # compute eigenvectors & eigenvalues

phi <- eigen$vectors                    # extract loadings
phi <- -phi                             # use positive-pointing vector
row.names(phi) <- colnames(my_dataset)  # rename rowes in phi matrix
colnames(phi) <- c("PC1", "PC2", "PC3", 
                   "PC4", "PC5", "PC6") # rename columns in phi matrix

# selecting number of PCs
PVE <- eigen$values/sum(eigen$values)
(PVE <- round(PVE, 2))
cumsum(PVE)

library(tidyverse)
screeplot <- qplot(c(1:6), PVE) +
  geom_line() +
  xlab("Principal Components") +
  ylab("PVE") +
  ggtitle("Scree Plot") +
  ylim(0,1)

cum.screeplot <- qplot(c(1:6), cumsum(PVE)) +
  geom_line() +
  xlab("Principal Components") +
  ylab(NULL) +
  ggtitle("Cumulative Scree Plot") +
  ylim(0,1)

library(gridExtra)
grid.arrange(screeplot, cum.screeplot, ncol=2) 

# calculating principal component scores
PC1 <- df.scaled %*% phi[,1]
PC2 <- df.scaled %*% phi[,2]
PC3 <- df.scaled %*% phi[,3]

# creating data frame with principal component scores
PC <- data.frame(PC1, PC2, PC3)
PC <- data.frame(Songs = row.names(my_dataset), PC1, PC2, PC3)

ggplot(PC, aes(PC1, PC2)) + 
  modelr::geom_ref_line(h = 0) +
  modelr::geom_ref_line(v = 0) +
  geom_text(aes(label = Songs), size = 3) +
  xlab("First Principal Component") + 
  ylab("Second Principal Component") + 
  ggtitle("First Two Principal Components of Spotify Playlists Data")

ggplot(PC, aes(PC2, PC3)) + 
  modelr::geom_ref_line(h = 0) +
  modelr::geom_ref_line(v = 0) +
  geom_text(aes(label = Songs), size = 3) +
  xlab("Second Principal Component") + 
  ylab("Third Principal Component") + 
  ggtitle("Second & Third Principal Component of Spotify Playlists Data")

# alternative PCA via princomp() - biplot 
pca <- princomp(my_dataset, cor = TRUE, fix_sign = FALSE)
print(pca$loadings, cutoff = 0)

pca$loadings <- -pca$loadings    # PCs loadings matrix (sign adjusted)   
pca$scores <- -pca$scores        # PCs scores matrix (sign adjusted)

biplot(pca, scale = 0, pc.biplot = FALSE, 
       col = c("#D0A9F5", "#000000"),
       cex = 1.2,
       xlabs = rep("●", 200),
       xlab = "PC1",
       ylab = "PC2")

# CLUSTERING ANALYSIS ----
df.scaled <- scale(my_dataset)      # standardize dataset

fviz_pca_ind(pca,                   # visualize data in pca space
             title = "PCA - Spotify Playlists data", 
             palette = "jco",
             geom = "point", 
             ggtheme = theme_classic(),
             legend = "bottom")

levels(data$playlist) <- factor(c("Hip-Hop", "Electro-Dance", "Ambient Music", "Rock")) # rename levels
genre <- as.numeric(data$playlist)

pairs(df.scaled, gap=0, pch=16, col = genre)

# Hopkins statistic
library(clustertend)

set.seed(123)
hopkins(df.scaled, 
        n = nrow(df.scaled)-1, # number of points selected from data (all rows minus 1)
        header = TRUE          # 1st column is not deleted in the calculation
        )

# VAT algorithm 
library(factoextra)

fviz_dist(dist(df.scaled), show_labels = FALSE) +
  labs(title = "Spotify Playlists Data")

# Elbow method
fviz_nbclust(df.scaled, FUNcluster = hcut, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow method ~ hierarchical clustering")

fviz_nbclust(df.scaled, FUNcluster = kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow method ~ k-means clustering")

fviz_nbclust(df.scaled, FUNcluster = cluster::pam, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow method ~ k-medoids clustering")

# Silhouette method
fviz_nbclust(df.scaled, FUNcluster = hcut, method = "silhouette") +
  labs(subtitle = "Silhouette method ~ hierarchical clustering")

fviz_nbclust(df.scaled, FUNcluster = kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method ~ k-means clustering")

fviz_nbclust(df.scaled, FUNcluster = cluster::pam, method = "silhouette") +
  labs(subtitle = "Silhouette method ~ k-medoids clustering")

# Gap statistic
set.seed(123)

fviz_nbclust(df.scaled, FUNcluster = hcut, method = "gap_stat", nboot = 500) +
  labs(subtitle = "Gap statistic method ~ hierarchical clustering")

fviz_nbclust(df.scaled, FUNcluster = kmeans, nstart = 25, method = "gap_stat", nboot = 500) +
  labs(subtitle = "Gap statistic method ~ k-means clustering")

fviz_nbclust(df.scaled, FUNcluster = cluster::pam, method = "gap_stat", nboot = 500) +
  labs(subtitle = "Gap statistic method ~ k-medoids clustering")

# NbClust function
library("NbClust")

nb.hclust <- NbClust(df.scaled,                      
                     distance = "euclidean",  
                     min.nc = 2, max.nc = 10, 
                     method = "ward.D2"
                     )

nb.kmeans <- NbClust(df.scaled,                      
                     distance = "euclidean",
                     min.nc = 2, max.nc = 10, 
                     method = "kmeans"
                     )

# Comparing clustering algorithms
library(clValid)

clust.methods <- c("hierarchical","kmeans","pam")
internal <- clValid(df.scaled, 
                    nClust = 2:6,
                    clMethods = clust.methods, 
                    validation = "internal")

summary(internal)

stability <- clValid(df.scaled, 
                     nClust = 2:6, 
                     clMethods = clust.methods,
                     validation = "stability")
summary(stability)

# Performing partitioning clustering:

# K-means algorithm with K = 4
set.seed(123)

kmeans.clust <- kmeans(df.scaled,
                       centers = 2,
                       iter.max = 10,
                       nstart = 50
                       )

# Accessing to kmeans() results
kmeans.clust$size            # cluster size
kmeans.clust$centers         # cluster means
kmeans.clust$cluster[1:9]    # cluster membership number of the first 9 observations
kmeans.clust$totss           # total sum of squares
kmeans.clust$withinss        # within-cluster sum of squares for each cluster
kmeans.clust$tot.withinss    # total within-cluster sum of squares
kmeans.clust$betweenss       # between-cluster sum of squares (totss-tot.withinss)

(kmeans.clust$betweenss/kmeans.clust$totss)*100
(kmeans.clust$withinss/kmeans.clust$totss)*100

# Internal cluster validation
kmeans <- eclust(df.scaled, "kmeans", k = 2, nstart = 25, graph = FALSE)
fviz_silhouette(kmeans, palette = "magma")      # silhouette plot
silhouette_info <- kmeans$silinfo               # silhouette information
head(silhouette_info$widths, 10)                # silhouette widths of each observation
silhouette_info$clus.avg.widths                 # svg. silhouette width of each cluster
silhouette_info$avg.width                       # total avg. (mean of all individual silhouette widths)

sil <- kmeans$silinfo$widths[, 1:3]             # silhouette width of observations
neg_sil_index <- which(sil[, "sil_width"] < 0)  # units with negative silhouette widths
sil[neg_sil_index, , drop = FALSE] 
low_sil_index <- which(sil[, "sil_width"] < 0.30)  # units with low silhouette widths
sil[low_sil_index, , drop = FALSE]

library(fpc)
kmeans_stats <- cluster.stats(dist(df.scaled), kmeans$cluster)
kmeans_stats$dunn             # Dunn index
kmeans_stats$min.separation   # inter-cluster separation
kmeans_stats$max.diameter     # intra-cluster compactness

# Mean of the original variables for each cluster
aggregate(my_dataset, by = list(cluster = kmeans.clust$cluster), mean)

# Adding the point classifications (cluster memberships) to the original data
head(cbind(my_dataset, cluster = kmeans.clust$cluster))

# Visualizing K-means clusters in the original space
clusters <- kmeans.clust$cluster
pairs(df.scaled, gap = 0, pch = clusters, col = c("#F5D0A9","#D0A9F5")[clusters])

# Visualizing K-means clusters
fviz_cluster(kmeans.clust,
             geom = "point",
             data = df.scaled,
             palette = c("#F5D0A9","#D0A9F5"),
             ellipse.type = "euclid",           # Concentration ellipse
             star.plot = TRUE,                  # Add segments from centroids to items
             repel = TRUE,                      # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
             )

# K-medoids clustering with K = 2
library(cluster)
pam.clust <- pam(df.scaled, k = 2)

# Accessing pam() results
pam.clust$id.med                                # id cluster medoids
pam.clust$medoids                               # cluster medoids matrix
pam.clust$clustering[1:15]                      # cluster membership number for the first 15 observations
pam.clust$objective                             # build & swap phases
pam.clust$clusinfo                              # cluster info

# Internal cluster validation
fviz_silhouette(pam.clust, palette = "jco")     # silhouette plot
pam_silhouette <- pam.clust$silinfo             # silhouette info
pam_silhouette$clus.avg.widths                  # average silhouette widths of each cluster
head(pam_silhouette$widths, 10)                 # silhouette widths of each observation
pam_silhouette$avg.width                        # total avg. (mean of all individual silhouette widths)

pam.sil <- pam.clust$silinfo$widths[, 1:3]              # silhouette width of observations
neg_pam_sil_index <- which(pam.sil[, "sil_width"] < 0)  # units with negative silhouette widths
pam.sil[neg_pam_sil_index, , drop = FALSE] 
low_pam_sil_index <- which(pam.sil[, "sil_width"] < 0.30)  # units with low silhouette widths
pam.sil[low_pam_sil_index, , drop = FALSE] 

library(fpc)
pam_stats <- cluster.stats(dist(df.scaled), pam.clust$cluster)
pam_stats$dunn             # Dunn index
pam_stats$min.separation   # inter-cluster separation
pam_stats$max.diameter     # intra-cluster compactness

# Adding the point classifications to the original data
head(cbind(my_dataset, cluster = pam.clust$cluster))

# Visualizing PAM clusters in the original space
pclusters <- pam.clust$cluster
pairs(df.scaled, gap = 0, pch = pclusters, col = c("#086A87","#DF0174")[pclusters])

# Visualizing PAM clusters
fviz_cluster(pam.clust2,
             geom = "point",
             palette = c("#086A87","#DF0174"), # Color palette
             ellipse.type = "t",               # Concentration ellipse
             repel = TRUE,                     # Avoid label overplotting (slow)
             ggtheme = theme_classic()
             )

# External cluster validation
levels(data$playlist) <- factor(c("Hip-Hop", "Electro-Dance", "Ambient Music", "Rock")) # rename levels
table(data$playlist, kmeans$cluster) # confusion matrix kmeans

genre <- as.numeric(data$playlist)
km.external.stats <- cluster.stats(dist(df.scaled),
                                   genre,
                                   kmeans$cluster)

km.external.stats$corrected.rand   # Rand index
km.external.stats$vi               # Meila's vi index
adjustedRandIndex(data$playlist, kmeans$cluster)

# CA ON PCA OUTPUT ----
# Comparing clustering algorithms
library(clValid)

clust.methods <- c("hierarchical","kmeans","pam")
internal <- clValid(PC, 
                    nClust = 2:6,
                    clMethods = clust.methods, 
                    validation = "internal")

optimalScores(internal)

stability <- clValid(PC, 
                     nClust = 2:6, 
                     clMethods = clust.methods,
                     validation = "stability")
optimalScores(stability)

# Hierarchical clustering with K = 2
# computing dissimilarities using distance measures for continuous quantitative variables
eucl.dist <- stats::dist(PC, method = "euclidean")

# reformatting the distance vector into distance matrix
eucl.dist.matrix <- round(as.matrix(eucl.dist)[1:5, 1:5], 2)

# hierarchical clustering
hclust <- hclust(d = eucl.dist, method = "ward.D2")

# producing dendrogram
library(factoextra)
fviz_dend(hclust, cex = 0.2)

# verifying the cluster tree:
coph.dist <- cophenetic(hclust)  # computing cophenetic distance
cor(eucl.dist, coph.dist)        # correlation between cophenetic and original distances

# executing again with the average linkage method
hclust2 <- hclust(eucl.dist, method = "average")
cor(eucl.dist, cophenetic(hclust2))

# cutting dendrogram into 2 groups
groups <- cutree(hclust, k = 2)
# number of members in each cluster
table(groups)

# mean of PCs for each cluster
aggregate(PC, by = list(cluster = pca.hc$cluster), mean)

# adding the point classifications (cluster memberships) to the original data
head(cbind(PC, cluster = groups))

# visualizing easily the result of the cuts
fviz_dend(hclust, k = 2, # Cut in four groups
          cex = 0.2, # label size
          k_colors = c("#E1F5A9", "#F7BE81"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
          )

# Internal cluster validation
pca.hc <- eclust(PC, "hclust", k = 2, hc_metric = "euclidean",
                 hc_method = "ward.D2", graph = FALSE)

hc_silhouette <- pca.hc$silinfo                      # silhouette info
hc_silhouette$clus.avg.widths                        # average silhouette widths of each cluster
head(hc_silhouette$widths, 10)                       # silhouette widths of each observation
hc_silhouette$avg.width                              # total avg. (mean of all individual silhouette widths)

hc.sil <- pca.hc$silinfo$widths[, 1:3]                  # silhouette width of observations
neg_hc_sil_index <- which(hc.sil[, "sil_width"] < 0)    # units with negative silhouette widths
hc.sil[neg_hc_sil_index, , drop = FALSE]
low_hc_sil_index <- which(hc.sil[, "sil_width"] < 0.30) # units with low silhouette widths
hc.sil[low_hc_sil_index, , drop = FALSE]

pca_hc_stats <- cluster.stats(dist(PC), pca.hc$cluster)
pca_hc_stats$dunn             # Dunn index
pca_hc_stats$min.separation
pca_hc_stats$max.diameter

# MODEL-BASED CLUSTERING ----
library(mclust)

df.scaled <- scale(my_dataset)

# Fit the parsimonious models
model <- Mclust(df.scaled, G = 1:9, modelNames = NULL)

# Visualize the top three BIC models
summary(model$BIC)

# Plot of the BIC values for all the fitted models
plot(model, what = "BIC", ylim = range(model$BIC, na.rm = TRUE), legendArgs = list(x = "bottomleft"))

# Visualize the main output
summary(model)

# Useful quantities
model$modelName                       # Selected parsimonious configuration
model$G                               # Optimal number of clusters
head(round(model$z, 6), 20)           # Probality to belong to a given cluster
head(model$classification, 20)        # Cluster assignment of each observation
head(round(model$uncertainty, 6), 20) # Uncertainty associated with classification

probabilities <- as.matrix(round(model$z, 6)) 
colnames(probabilities) <- c("pi1", "pi2", "pi3", "pi4", "pi5", "pi6")
classification <- as.matrix(model$classification) 
colnames(classification) <- "Classification"
uncertainty <- as.matrix(round(model$uncertainty, 6))
colnames(uncertainty) <- "Uncertainty"

model_output <- cbind(probabilities, classification, uncertainty)
head(model_output, 30)

PC[model_output[,"Uncertainty"] > 0.3,]
PC[model_output[,"Uncertainty"] > 0.50,]

uncertain_units <- cbind(PC[model_output[,"Uncertainty"] > 0.50,], 
      Uncertainty = model_output[model_output[,"Uncertainty"] > 0.50, 8], 
      Classification = model_output[model_output[,"Uncertainty"] > 0.50, 7])

model_output[model_output[,"Uncertainty"] > 0.50,1:6]

# Pairs plot with classification
pairs(df.scaled, gap=0, pch = 16, col = model$classification)

# Mean of the original variables for each cluster
aggregate(df.scaled, by = list(cluster = model$classification), mean)

# Confusion matrix (External Cluster Validation)
table(data$playlist, model$classification)

# Adjusted (or Corrected) Rand Index
adjustedRandIndex(data$playlist, model$classification)

# Visualizing the obtained results in the PC space
library(factoextra)

# BIC values used for choosing the number of clusters
fviz_mclust(model, "BIC", palette = "rickandmorty")

# Classification: plot showing the clustering
fviz_mclust(model, "classification", geom = "point", pointsize = 1.5, palette = "lancet")

# Classification uncertainty
# larger symbols indicate the more uncertain observations
fviz_mclust(model, "uncertainty", palette = "lancet")

# Plot the data using only two variables of interest
fviz_mclust(model, "uncertainty", palette = "lancet", choose.vars = c("acousticness", "energy"))


