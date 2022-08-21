# Set working directory:
# Session -> Set Working Directory -> To Source File Location

# Read file
Music <- read.csv("../datasets/music_filtered.csv",sep = ",", dec = ".")



## -------  PREPROCESSING -------

quantitative_var <- c("popularity", "acousticness", "danceability", 
                      "duration_ms", "energy", "instrumentalness", "liveness",         
                      "loudness", "speechiness", "tempo", "valence")

# Select quantitative values
X <- subset(Music, select = quantitative_var)
head(X)
dim(X)
n <- nrow(X)
p <- ncol(X)

# Replace "?" values in tempo by NA
library(naniar)            
X <- X %>% replace_with_na(replace = list(tempo = '?'))
X$tempo <- as.numeric(X$tempo)

# Replace "-1" values in duration_ms by NA
X <- X %>% replace_with_na(replace = list(duration_ms = -1))


attach(X)



## ------- MISSING DATA -------

table(Music$mode) 
table(Music$key)

# No missing data in qualitative variables

summary(X)

# Missing data in tempo and duration


# Frequency table with the number of missing values in each observation
n_miss <- apply(is.na(X),1,sum)
table(n_miss)

# Imputation with PPM
library(mice)
X_imp <- mice(X, m=1, method="pmm")
X_imp <- complete(X_imp)

new_miss <- apply(is.na(X_imp),1,sum)
table(new_miss)

# Comparison original and input data
obs_miss <- which(n_miss == 1 | n_miss == 2)
X[obs_miss,]
X_imp[obs_miss,]

# Plot tempo
library(ggplot2)
data_label <- rep('Default', n)
data_label[obs_miss] <- 'Input'
ggplot(cbind(X_imp, data_label), aes(x=tempo, y=liveness,
                                     col=data_label)) +
  geom_point() +
  labs(colour = 'Data')

# Plot duration
ggplot(cbind(X_imp, data_label), aes(x=duration_ms, y=liveness,
                                     col=data_label)) +
  geom_point() +
  labs(colour = 'Data')


# Update missing values
X <- X_imp


attach(X)



## ------- GRAPHICAL ANALYSIS-------


# Get updated data frame with missing values and preprocessing
music_genre <- Music$music_genre
X_y <- cbind(X, music_genre)
attach(X_y)


# Pair plot
library(GGally)
library(dplyr)
music_sampled <- sample_n(X_y, 500)
ggpairs(music_sampled, columns = c(1:11), aes(color = music_genre),
        upper = list(continuous = "points"))


boxplot(popularity ~ music_genre)
boxplot(acousticness ~ music_genre)
boxplot(danceability ~ music_genre)
boxplot(duration_ms ~ music_genre)
boxplot(energy ~ music_genre)
boxplot(instrumentalness ~ music_genre)
boxplot(liveness ~ music_genre)
boxplot(loudness ~ music_genre)
boxplot(speechiness ~ music_genre)
boxplot(tempo ~ music_genre)
boxplot(valence ~ music_genre)

hist(popularity)
hist(popularity[music_genre == "Classical"])
hist(popularity[music_genre == "Electronic"])
hist(popularity[music_genre == "Hip-Hop"])
hist(popularity[music_genre == "Jazz"])


hist(danceability)
hist(danceability[music_genre=="Classical"])
hist(danceability[music_genre=="Electronic"])
hist(danceability[music_genre=="Hip-Hop"])
hist(danceability[music_genre=="Jazz"])


hist(acousticness)
hist(acousticness[music_genre=="Classical"])
hist(acousticness[music_genre=="Electronic"])
hist(acousticness[music_genre=="Hip-Hop"])
hist(acousticness[music_genre=="Jazz"])


hist(energy)
hist(energy[music_genre=="Classical"])
hist(energy[music_genre=="Electronic"])
hist(energy[music_genre=="Hip-Hop"])
hist(energy[music_genre=="Jazz"])



hist(instrumentalness)
hist(instrumentalness[music_genre == "Classical"])
hist(instrumentalness[music_genre == "Electronic"])
hist(instrumentalness[music_genre == "Hip-Hop"])
hist(instrumentalness[music_genre == "Jazz"])


hist(loudness)
hist(loudness[music_genre=="Classical"])
hist(loudness[music_genre=="Electronic"])
hist(loudness[music_genre=="Hip-Hop"])
hist(loudness[music_genre=="Jazz"])


hist(speechiness)
hist(speechiness[music_genre=="Classical"])
hist(speechiness[music_genre=="Electronic"])
hist(speechiness[music_genre=="Hip-Hop"])
hist(speechiness[music_genre=="Jazz"])



hist(valence)
hist(valence[music_genre=="Classical"])
hist(valence[music_genre=="Electronic"])
hist(valence[music_genre=="Hip-Hop"])
hist(valence[music_genre=="Jazz"])


# PCP
ggparcoord(music_sampled, columns = c(1:11), groupColumn = "music_genre")

library(andrews)
andrews(df <- music_sampled, clr = 11)



## ------- TRANSFORMATIONS ------

library(e1071)

# 0:100, not skewed
hist(popularity, freq = F) 
skewness(popularity)

# 0:1, bridge
hist(acousticness, freq = F)
skewness(acousticness)

# 0:1, not skewed
hist(danceability, freq = F)
skewness(danceability)

# Strange
hist(duration_ms, freq = F)
skewness(duration_ms)

# 0:1, uniform
hist(energy, freq = F)
skewness(energy)

# 0:1, bridge 
hist(instrumentalness, freq = F)
skewness(instrumentalness)

# 0:1, right skewed
hist(liveness, freq = F)
skewness(liveness)
hist(log(liveness), freq = F)
skewness(log(liveness))

X$liveness <- log(liveness)

# -50:0, shifted to right
hist(loudness, freq = F)
skewness(loudness)
exponent <- function(a, pow) (abs(a)^pow)*sign(a)
hist(exponent(loudness, 1/3))
skewness(exponent(loudness, 1/3))

X$loudness <- exponent(loudness, 1/3)

# 0:1, rigth skewed
hist(speechiness, freq = F)
skewness(speechiness)
hist(log(speechiness), freq = F)
skewness(log(speechiness))

X$speechiness <- log(speechiness)

# Wide, symmetric
hist(tempo, freq = F)
skewness(tempo)

# 0:1, uniform
hist(valence, freq = F)
skewness(valence)



# Update X_y
X_y <- cbind(X, music_genre)

attach(X_y)




## ------- CHARACTERISTICS -------


# Study with all the observations in the dataset            

# Calculation of mean vector, covariance matrix and correlation matrix
m_all <- colMeans(X)
S_all <- cov(X)
R_all <- cor(X)

# Graphical representation of the correlation matrix
library(corrplot)
corrplot(R_all)




# Study per music genre in the dataset                  

# Data matrix per music genre
X_classical <- X[music_genre=="Classical",]
X_electronic <- X[music_genre=="Electronic",]
X_hiphop <- X[music_genre=="Hip-Hop",]
X_jazz <- X[music_genre=="Jazz",]

# Calculation of mean vector, covariance matrix and correlation matrix per genre
m_classical <- colMeans(X_classical)
S_classical <- cov(X_classical)
R_classical <- cor(X_classical)
corrplot(R_classical)

m_electronic <- colMeans(X_electronic)
S_electronic <- cov(X_electronic)
R_electronic <- cor(X_electronic)
corrplot(R_electronic)

m_hiphop <- colMeans(X_hiphop)
S_hiphop <- cov(X_hiphop)
R_hiphop <- cor(X_hiphop)
corrplot(R_hiphop)

m_jazz <- colMeans(X_jazz)
S_jazz <- cov(X_jazz)
R_jazz <- cor(X_jazz)
corrplot(R_jazz)



## ------- OUTLIERS -------

library(rrcov)


# MCD estimator
mcd_estimator <- function(X, h=0.85){
  # Obtain the mean vector, sample covariance matrix and sample correlation matrix of the data set
  m <- colMeans(X)
  S <- cov(X)
  R <- cor(X)
  
  # Obtain the robust mean vector, covariance matrix and correlation matrix of the data set
  MCD_est <- CovMcd(X, alpha=h, nsamp="deterministic", tolSolve = 1e-20)
  m_MCD <- MCD_est$center
  S_MCD <- MCD_est$cov
  R_MCD <- cov2cor(S_MCD)
  
  out <- list(m=m, S=S, R=R, 
              MCD_est=MCD_est, m_MCD=m_MCD, S_MCD=S_MCD, R_MCD=R_MCD)
  return (out)
  
}

# Compare eigenvalues of both covariance matrices
show_eigenvalues <- function(S, S_MCD){
  eval_S <- eigen(S)$values
  eval_S_MCD <- eigen(S_MCD)$values
  df1 = data.frame(Number=1:length(eval_S), Eigenvalues=eval_S,
                   Origin = rep('From S', length(eval_S)))
  df2 = data.frame(Number=1:length(eval_S_MCD), Eigenvalues=eval_S_MCD,
                   Origin = rep('From S MCD', length(eval_S_MCD)))
  ggplot(data=rbind(df1, df2), aes(x=Number, y=Eigenvalues, col=Origin)) + geom_line() +
    geom_point(data=df1, aes(x=Number, y=Eigenvalues, col=Origin)) +
    geom_point(data=df2, aes(x=Number, y=Eigenvalues, col=Origin)) +
    ggtitle("Comparison of eigenvalues") +
    theme(plot.title = element_text(hjust = 0.5))
}

# Obtain possible outliers
get_outliers <- function(MCD_est){
  X_sq_Mah_MCD <- MCD_est$mah
  outliers_Mah_MCD <- which(X_sq_Mah_MCD>qchisq(.99,p))
  
  out <- list(mh_distances=X_sq_Mah_MCD, outliers = outliers_Mah_MCD)
  return (out)
}

# Show the squared/log Mahalanobis distances with the final set of non-outliers
show_mh_distance <- function(X_sq_Mah_MCD, outliers_Mah_MCD, n, log = FALSE){
  outlier_label <- rep('Non-outlier', n)
  outlier_label[outliers_Mah_MCD] <- 'Outlier'
  # Square
  if (!log){
    ggplot() +
      geom_point(data = data.frame(x=1:n, y=X_sq_Mah_MCD, col=outlier_label),
                 aes(x=x, y=y, col=col)) +
      labs(colour="Outlier label", x='Observation', y='Squared Mahalanobis distance') +
      ggtitle("Squared Mahalanobis distances") +
      geom_abline(intercept = qchisq(.99,p), slope = 0) +
      theme(plot.title = element_text(hjust = 0.5))
    # Log
  }else{
    ggplot() +
      geom_point(data = data.frame(x=1:n, y=log(X_sq_Mah_MCD), col=outlier_label),
                 aes(x=x, y=y, col=col)) +
      labs(colour="Outlier label", x='Observation', y='Log of squared Mahalanobis distance') +
      ggtitle("Log of squared Mahalanobis distances") +
      geom_abline(intercept = log(qchisq(.99,p)), slope = 0) +
      theme(plot.title = element_text(hjust = 0.5))
  }
}


# PCP plot
show_pcp <- function(X, outliers_Mah_MCD, n_points=0){
  outlier_label <- rep('Non-outlier', nrow(X))
  if (n_points>0){
    indexes <- which.maxn(classical_out$mh_distances, n_points)
    outlier_label[indexes] <- 'Outlier'
  }else{
    outlier_label[outliers_Mah_MCD] <- 'Outlier'
  }
  ggparcoord(cbind(X, outlier_label), columns = c(1:11),
             groupColumn = "outlier_label") + labs(colour="Outlier label")
}

# Pairs plot
show_pairs <- function(X, outliers_Mah_MCD){
  outlier_label <- rep('Non-outlier', nrow(X))
  outlier_label[outliers_Mah_MCD] <- 'Outlier'
  ggpairs(cbind(X, outlier_label), columns = c(1:11), aes(color = outlier_label),
          upper = list(continuous = "points"))
}

# Andrews plot
show_andrews <- function(X, outliers_Mah_MCD){
  out_X <- rep(1, nrow(X))
  out_X[outliers_Mah_MCD] <- 2
  andrews(as.data.frame(cbind(X,as.factor(out_X))),clr=9,ymax=4)
}

# Remove outliers
remove_outliers <- function(X, distances, n_points){
  if (n_points >= 1){
    indexes <- which.maxn(classical_out$mh_distances, n_points)
    return (X[-c(indexes), ])
  }
  else{
    return (X)
  }
}


library(doBy)


## ------- Classical -------

rownames(X_classical) <- NULL
n_class <- nrow(X_classical)

classical <- mcd_estimator(X_classical, h=0.95)
show_eigenvalues(classical$S, classical$S_MCD)
corrplot(classical$R)
corrplot(classical$R_MCD)
classical_out <- get_outliers(classical$MCD_est)
show_mh_distance(classical_out$mh_distances, classical_out$outliers, n_class)
show_mh_distance(classical_out$mh_distances, classical_out$outliers, n_class, 
                 log = TRUE)

# Remove outliers
n_points_class <- 3

# show_pairs(X_classical, classical_out$outliers)
show_pcp(X_classical, classical_out$outliers, n_points_class)

# Filter
X_classical_filtered <- remove_outliers(X_classical, classical_out$mh_distances, 
                                        n_points_class)
dim(X_classical)
dim(X_classical_filtered)




# ------- Electronic -------

rownames(X_electronic) <- NULL
n_elec <- nrow(X_electronic)

electronic <- mcd_estimator(X_electronic)
show_eigenvalues(electronic$S, electronic$S_MCD)
corrplot(electronic$R)
corrplot(electronic$R_MCD)
electronic_out <- get_outliers(electronic$MCD_est)
show_mh_distance(electronic_out$mh_distances, electronic_out$outliers, n_elec)
show_mh_distance(electronic_out$mh_distances, electronic_out$outliers, n_elec, 
                 log = TRUE)

# Remove outliers
n_points_elec <- 4

# show_pairs(X_electronic, electronic_out$outliers)
show_pcp(X_electronic, electronic_out$outliers, n_points_elec)

# Filter
X_electronic_filtered <- remove_outliers(X_electronic, electronic_out$mh_distances, 
                                         n_points_elec)
dim(X_electronic)
dim(X_electronic_filtered)


# ------- Hip-Hop -------

rownames(X_hiphop) <- NULL
n_hiphop <- nrow(X_hiphop)

hiphop <- mcd_estimator(X_hiphop, h = 0.9)
show_eigenvalues(hiphop$S, hiphop$S_MCD)
corrplot(hiphop$R)
corrplot(hiphop$R_MCD)
hiphop_out <- get_outliers(hiphop$MCD_est)
show_mh_distance(hiphop_out$mh_distances, hiphop_out$outliers, n_hiphop)
show_mh_distance(hiphop_out$mh_distances, hiphop_out$outliers, n_hiphop, 
                 log = TRUE)

# Remove outliers
n_points_hip <- 0

# show_pairs(X_hiphop, hiphop_out$outliers)
show_pcp(X_hiphop, hiphop_out$outliers, n_points_hip)

# Filter
X_hiphop_filtered <- remove_outliers(X_hiphop, hiphop_out$mh_distances, 
                                     n_points_hip)
dim(X_hiphop)
dim(X_hiphop_filtered)


# ------- Jazz -------

rownames(X_jazz) <- NULL
n_jazz <- nrow(X_jazz)

jazz <- mcd_estimator(X_jazz, h=0.95)
show_eigenvalues(jazz$S, jazz$S_MCD)
corrplot(jazz$R)
corrplot(jazz$R_MCD)
jazz_out <- get_outliers(jazz$MCD_est)
show_mh_distance(jazz_out$mh_distances, jazz_out$outliers, n_jazz)
show_mh_distance(jazz_out$mh_distances, jazz_out$outliers, n_jazz, 
                 log = TRUE)

# Remove outliers
n_points_jazz <- 9

# show_pairs(X_jazz, jazz_out$outliers)
show_pcp(X_jazz, jazz_out$outliers, n_points_jazz)

# Filter
X_jazz_filtered <- remove_outliers(X_jazz, jazz_out$mh_distances, 
                                   n_points_jazz)
dim(X_jazz)
dim(X_jazz_filtered)



## ------- NEW DATA -------

X <- rbind(X_classical_filtered, X_electronic_filtered,
           X_hiphop_filtered, X_jazz_filtered)
dim(X)

music_genre <- c(rep('Classical', nrow(X_classical_filtered)),
                 rep('Electronic', nrow(X_electronic_filtered)),
                 rep('Hip-Hop', nrow(X_hiphop_filtered)),
                 rep('Jazz', nrow(X_jazz_filtered)))
length(music_genre)

X_y <- cbind(X, music_genre)
dim(X_y)

# Write to csv
# write.csv(X,"../datasets/X.csv", row.names = FALSE)
# write.csv(X_y,"../datasets/X_y.csv", row.names = FALSE)



# Set working directory:
# Session -> Set Working Directory -> To Source File Location

####### ------ Dimension Reduction Techniques ------ #######

library(glue)
library(GGally)

#X = read.csv("../datasets/X.csv",sep = ",", dec = ".")
#X_y = read.csv("../datasets/X_y.csv",sep = ",", dec = ".")
#attach(X_y)

n <- nrow(X)
p <- ncol(X)



## ------- PCA -------


# Obtaining PCs, scale advisable
X_pc <- prcomp(X, scale=TRUE)
dim(X_pc$x)
head(X_pc$x)


# Variance explained
library(factoextra)
fviz_eig(X_pc, ncp=p, addlabels=T)
variance_explain <- get_eigenvalue(X_pc)
variance_explain 
# Explain at least a percentage of variance
min_variance <- 75 # in percentage
last_pc <- which(variance_explain$cumulative.variance.percent > min_variance)[1]
last_pc


# Plot two PC components
plot_two_pc <- function(idx1, idx2){
  ggplot(data.frame(x = X_pc$x[, idx1], y = X_pc$x[, idx2], col = music_genre), 
         aes(x = x, y = y, col = col)) +
    labs(x = glue('PC{idx1}'), y = glue('PC{idx2}')) + 
    geom_point()
}

plot_two_pc(1, 2)


# Values of PCs
dim(X_pc$rotation)
X_pc$rotation
X_pc$rotation[, 1:last_pc]


# Plot scores one or two PCs components
plot_scores_pc <- function(idx, idx2 = 0){
  # One PCA
  if(idx2 == 0){
    df <- data.frame(x = 1:p, y = X_pc$rotation[, idx])
    labels <- labs(x = "Variables", y = "Score")
    title <- ggtitle(glue("Weights for PC{idx}"))
    
    # Two PCAs
  }else{
    df <- data.frame(x = X_pc$rotation[, idx], y = X_pc$rotation[, idx2])
    labels <- labs(x = glue('PC{idx}'), y = glue('PC{idx2}'))
    title <- ggtitle(glue("Weights for PC{idx} and PC{idx2}"))
  }
  
  ggplot(df, aes(x = x, y = y)) + geom_point() +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
    geom_text(aes(label = colnames(X)), hjust=0.5, vjust=1.5) +
    labels + title + theme(plot.title = element_text(hjust = 0.5))
}

plot_scores_pc(1)

# devtools::install_github("richardjtelford/ggbiplot", ref = "experimental")
# Biplot
library(ggbiplot)
ggbiplot(X_pc, choices = c(1, 2)) + # PCs to print in vector format c(pc1, pc2)
  theme(aspect.ratio=1)


# Pairs plot with PC
plot_pc_pairs <- function(last_pc){
  ggpairs(data.frame(X_pc$x[, 1:last_pc], col = music_genre), 
          columns = c(1:last_pc), 
          aes(col = col),
          upper = list(continuous = "points"))
}

plot_pc_pairs(last_pc)


# Corrplot X and PCs
library(corrplot)
corrplot(cor(X, X_pc$x), is.corr=T)
corrplot(cor(X, X_pc$x[,1:last_pc]), is.corr=T)




## ------- ICA -------


# Obtain ICs components
library(fastICA)
X_ic <- fastICA(scale(X), n.comp = p, alg.typ = "parallel", fun = "logcosh", alpha = 1,
                method = "C", row.norm = FALSE, maxit = 200,
                tol = 0.0001, verbose = TRUE)


# Rename columns
Z <- X_ic$S
dim(Z)
head(Z)
colnames(Z) <- sprintf("IC%d",seq(1,11))


# Ensure S_z=I
cov(Z)
Z <- Z * sqrt((n-1)/n)
cov(Z)


# Sort IC components by neg-entropy
neg_entropy <- function(z){1/12 * mean(z^3)^2 + 1/48 * mean(z^4)^2}
Z_neg_entropy <- apply(Z,2,neg_entropy)
ic_sort <- sort(Z_neg_entropy,decreasing=TRUE,index.return=TRUE)$ix
ic_sort
Z_ic_imp <- Z[,ic_sort]
Z_ic_imp


# Plot sorted IC components by neg-entropy
df <- data.frame(x = 1:p, y = Z_neg_entropy[ic_sort])
ggplot(data = df, aes(x = x, y = y)) + geom_line() +
  geom_point(data = df, aes(x = x, y = y)) +
  geom_text(aes(label = ic_sort), hjust=0.5, vjust=-1) +
  labs(x = 'Dimensions', y = 'Neg-entropy') +
  ggtitle("Sorted Neg-entropies of ICs components") +
  theme(plot.title = element_text(hjust = 0.5))
labs(x = 'Index', y = 'Neg-entropy')


# Plot two IC components, index by decreasing order of neg-entropy
plot_two_ic <- function(idx1, idx2){
  ggplot(data.frame(x = Z_ic_imp[, idx1], y = Z_ic_imp[, idx2], col = music_genre), 
         aes(x = x, y = y, col = col)) +
    labs(x = glue('IC{ic_sort[idx1]}'), y = glue('IC{ic_sort[idx2]}')) + 
    geom_point()
}

plot_two_ic(1, 2)

# Points with large negative values of the first IC
which(Z_ic_imp[, 1] < (-5))

# Points with large positive values of the second IC
which(Z_ic_imp[, 2] > 5)


# Pairs plot with ICs
plot_ic_pairs <- function(last_ic){
  ggpairs(data.frame(Z_ic_imp[, 1:last_ic], col = music_genre), 
          columns = c(1:last_ic), 
          aes(col = col),
          upper = list(continuous = "points"))
}

plot_ic_pairs(5)


# Corrplot X and ICs
corrplot(cor(X, Z_ic_imp),is.corr=T)

# Plot the correlations between the PCs and the ICs sorted by neg-entropy
corrplot(cor(X_pc$x, Z_ic_imp), is.corr=T)



## ------- NEW DATA -------

# Write to csv PCA
X_pca <- X_pc$x
#write.csv(X_pca, "../datasets/X_pca.csv", row.names = FALSE)

X_y_pca <- data.frame(X_pc$x, music_genre = music_genre)
#write.csv(X_y_pca, "../datasets/X_y_pca.csv", row.names = FALSE)


# Write to csv ICA
X_ica <- data.frame(Z_ic_imp)
#write.csv(X_ica,"../datasets/X_ica.csv", row.names = FALSE)

X_y_ica <- data.frame(Z_ic_imp, music_genre = music_genre)
#write.csv(X_y_ica,"../datasets/X_y_ica.csv", row.names = FALSE)


# To read new data frames
#X_pca = read.csv("../datasets/X_pca.csv",sep = ",", dec = ".")
#X_y_pca = read.csv("../datasets/X_y_pca.csv",sep = ",", dec = ".")

#X_ica = read.csv("../datasets/X_ica.csv",sep = ",", dec = ".")
#X_y_ica = read.csv("../datasets/X_y_ica.csv",sep = ",", dec = ".")



####### ------ Unsupervised Learning ------ #######

## The datasets are read again in case it is only needed to execute one section of the code

library(cluster)    
library(factoextra) 

#X = read.csv("../datasets/X.csv",sep = ",", dec = ".")
#X_y = read.csv("../datasets/X_y.csv",sep = ",", dec = ".")
attach(X)

n <- nrow(X)
p <- ncol(X)

#X_y_ica = read.csv("../datasets/X_y_ica.csv",sep = ",", dec = ".")
#X_y_pca = read.csv("../datasets/X_y_pca.csv",sep = ",", dec = ".")

#X_ica = read.csv("../datasets/X_ica.csv",sep = ",", dec = ".")
#X_pca = read.csv("../datasets/X_pca.csv",sep = ",", dec = ".")


X_scaled = scale(X)



### ESTIMATION OF K
# We already know that the true value of K is 4 but we will check this in any case


## Elbow method

K <- 1:10
tot_within_ss <- sapply(K, function(k) {
  cl <- kmeans(X_scaled, k, nstart = 25)
  cl$tot.withinss
})


elbow_df <- data.frame(
  k = K,
  tot_within_ss = tot_within_ss
)

ggplot(elbow_df, aes(x = k, y = tot_within_ss)) +
  geom_line() + geom_point()+
  scale_x_continuous(breaks = 1:10)


## Silhouette method

fviz_nbclust(X_scaled, kmeans, method = "silhouette", k.max=10)


# try with pca

tot_within_ss_pca <- sapply(K, function(k) {
  cl <- kmeans(X_pca, k, nstart = 25)
  cl$tot.withinss
})

elbow_df_pca <- data.frame(
  k = K,
  tot_within_ss_pca = tot_within_ss_pca
)

ggplot(elbow_df_pca, aes(x = k, y = tot_within_ss_pca)) +
  geom_line() + geom_point()+
  scale_x_continuous(breaks = 1:10)


fviz_nbclust(X_pca, kmeans, method = "silhouette", k.max=10)



### PARTITIONAL CLUSTERING

# K-Means with K=4


kmeans.re <- kmeans(X_scaled, centers = 4, nstart=25)


# Confusion Matrix
cm <- table(X_y$music_genre, kmeans.re$cluster)
cm

#k means plot

fviz_cluster(kmeans.re, X_scaled, ellipse.type = "norm")+theme_minimal()


### HIERARCHICAL CLUSTERING

dist_mat <- dist(X_scaled, method = 'euclidean')

# hcl correspond to the case using complete linkage while hclust_ward uses ward linkage
hcl <- hclust(dist_mat)
hclust_ward <- hclust(dist_mat, method = 'ward.D')

cut = cutree(hcl, k = 4)
cut_ward = cutree(hclust_ward, k = 4)

table(cut)

table(cut_ward)

plot(hclust_ward)
rect.hclust(hclust_ward , k = 4, border = 2:6)
abline(h = 4, col = 'red')

## Visualization
suppressPackageStartupMessages(library(dendextend))
avg_dend_obj <- as.dendrogram(hclust_ward)
avg_col_dend <- color_branches(avg_dend_obj, h = 3)
plot(avg_col_dend)


#Comparison with k means

table(kmeans.re$cluster, cut_ward)

#Confusion matrix hierarchical
cm_h <- table(X_y$music_genre, cut_ward)
cm_h



####### ------ Supervised Learning ------ #######

## The datasets are read again in case it is only needed to execute one section of the code

col_blue <- "deepskyblue2"

#music <- read.csv("../datasets/X_y.csv")
music = X_y
quantitative_var <- c("popularity", "acousticness", "danceability", 
                      "duration_ms", "energy", "instrumentalness", "liveness",         
                      "loudness", "speechiness", "tempo", "valence")
X <- subset(music, select = quantitative_var)
Y <- music["music_genre"]

n <- nrow(X)
n_train <- floor(.7*n)
n_test <- n - n_train


#####################################################
# Divide the dataset into training and test samples #
#####################################################

# Fix the seed
set.seed(1)

# Obtain the training and test partitions
i_train <- sort(sample(1:n,n_train))
X_train <- X[i_train,]
Y_train <- Y[i_train,]
i_test <- setdiff(1:n,i_train)
X_test <- X[i_test,]
Y_test <- Y[i_test,]


#######
# KNN #
#######

library(class)
library(ggplot2)

# Standardize the variables in the dataset as euclidean distance is used
stan_X_train <- scale(X_train)
stan_X_test <- scale(X_test)

# Estimate the unknown parameters with the training sample
LER <- rep(NA,30)
for (i in 1 : 30){
  knn_output <- knn.cv(stan_X_train,Y_train,k=i)
  LER[i] <- mean(knn_output!=Y_train)
}

plot(1:30,LER,pch=20,type="b", col=col_blue,
     xlab="k",ylab="LER",main="LOOCV error rate for each k in KNN")

k <- which.min(LER) # k that gives the minimum LOOCV error rate

# Classify the observations in the test sample
knn_Y_test <- knn(stan_X_train,stan_X_test,Y_train,k=k,prob=TRUE)

# Confusion table
table(Y_test,knn_Y_test)

# Obtain the Test Error Rate (TER)
TER_knn <- mean(Y_test!=knn_Y_test)


######################################
# Linear Discriminant Analysis (LDA) #
######################################

library(MASS)

# Estimate the parameters of the model with the training sample
lda_train <- lda(Y_train~.,data=as.data.frame(X_train))

# Classify the observations in the test sample
lda_test <- predict(lda_train,newdata=as.data.frame(X_test))

# Confusion table
lda_Y_test <- lda_test$class
table(Y_test,lda_Y_test)

# Obtain the Test Error Rate (TER)
TER_lda <- mean(Y_test!=lda_Y_test)


#########################################
# Quadratic Discriminant Analysis (QDA) #
#########################################

# Estimate the parameters of the model with the training sample
qda_train <- qda(Y_train~.,data=as.data.frame(X_train))

# Classify the observations in the test sample
qda_test <- predict(qda_train,newdata=as.data.frame(X_test))

# Confusion table
qda_Y_test <- qda_test$class
table(Y_test,qda_Y_test)

# Obtain the Test Error Rate (TER)
TER_qda <- mean(Y_test!=qda_Y_test)


####################
# Naive Bayes (NB) #
####################

library(naivebayes)

# Estimate the parameters of the model with the training sample
nb_train <- gaussian_naive_bayes(as.data.frame(X_train),Y_train)

# Classify the observations in the test sample
nb_test <- predict(nb_train,newdata=as.matrix(X_test),type="prob")
nb_Y_test <- c("Classical","Jazz","Electronic","Hip-hop")[apply(nb_test,1,which.max)]

# Confusion table
table(Y_test,nb_Y_test)

# Obtain the Test Error Rate (TER)
TER_nb <- mean(Y_test!=nb_Y_test)


#######################
# Logistic regression #
#######################

library(nnet)

# Estimate the parameters of the model with the training sample
lr_train <- multinom(Y_train~.,data=as.data.frame(X_train))
step_lr_train <- step(lr_train,direction="backward",trace=0)

# Classify the observations in the test sample 
lr_test <- predict(lr_train,newdata=X_test)
step_lr_test <- predict(step_lr_train,newdata=X_test)

# Confusion table
table(Y_test,lr_test)
table(Y_test,step_lr_test)

# Obtain the Test Error Rate (TER)
TER_lr <- mean(Y_test!=lr_test)
TER_step_lr <- mean(Y_test!=step_lr_test)


