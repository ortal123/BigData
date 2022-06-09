

# Big Data - Exercise 2
# GitHub: https://github.com/JuditHalperin/BigData/tree/main/Ex2

# Yudit Halperin - 324216589
# Ortal Calfon (Peretz) - 315011189
# Asnat Berlin - 211825401



# Load libraries
library(stringr)
library(dplyr)
library(ggplot2)
library(mosaic)
library(dplyr)
library(stringr)
library(xtable)
library(gridExtra)
library(stopwords)
library(quanteda)
library(caret)
library(rpart)


# Set rounding to 2 digits
options(digits = 2)


# Input data
input <- 'short_profiles'
profiles <- read.csv(file.path(paste0('Ex2/data/', input, '.csv')), header = TRUE, stringsAsFactors = FALSE)
str(profiles)


# Output file
pdf(file.path('Ex2/output/Week5_dating.pdf'))


# Combine the essays of each profile into one paragraph
essays <- select(profiles, starts_with("essay"))
essays <- apply(essays, MARGIN = 1, FUN = paste, collapse=" ")


# Remove HTML characters and stop-words
html <- c( "<a[^>]+>", "class=[\"'][^\"']+[\"']", "&[a-z]+;", "\n", "\\n", "<br ?/>", "</[a-z]+ ?>" )
html.pat <- paste0( "(", paste(html, collapse = "|"), ")" )
essays <- str_replace_all(essays, html.pat, " ")

stop.words <-  c( "a", "am", "an", "and", "as", "at", "are", "be", "but", "can", "do", "for", "have", "i'm", "if", "in", "is", "it", "like", "love", "my", "of", "on", "or", "so", "that", "the", "to", "with", "you", "i" )
stop.words.pat <- paste0( "\\b(", paste(stop.words, collapse = "|"), ")\\b" )
essays <- str_replace_all(essays, stop.words.pat, " ")


# Remove common words used by men / women
#male.words <- subset(essays, profiles$sex == "m") %>% str_split(" ") %>% unlist() %>% table() %>% sort(decreasing=TRUE) %>% names()
#female.words <- subset(essays, profiles$sex == "f") %>% str_split(" ") %>% unlist() %>% table() %>% sort(decreasing=TRUE) %>% names()

# Words in the males top 500 that weren't in the females' top 500:
#top.male.words <- setdiff(male.words[1:500], female.words[1:500])
# Words in the male top 500 that weren't in the females' top 500:
#top.female.words <- setdiff(female.words[1:500], male.words[1:500])

#top.male.words.pat <- paste0( "\\b(", paste(top.male.words, collapse = "|"), ")\\b" )
#essays <- str_replace_all(essays, top.male.words.pat, " ")

#top.female.words.pat <- paste0( "\\b(", paste(top.female.words, collapse = "|"), ")\\b" )
#essays <- str_replace_all(essays, top.female.words.pat, " ")


# Tokenize essay texts
all.tokens <- tokens(essays, what = "word", remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove_hyphens = TRUE)
rm(essays)


# Lower case the tokens
all.tokens <- tokens_tolower(all.tokens)


# Use quanteda's built-in stopword list for English
all.tokens <- tokens_select(all.tokens, stopwords(), selection = "remove")


# Perform stemming on the tokens
all.tokens <- tokens_wordstem(all.tokens, language = "english")


# remove single-word tokens after stemming (meaningless)
all.tokens <- tokens_select(all.tokens, "^[a-z]$", selection = "remove", valuetype = "regex")


# Create a bag-of-words model = document-term frequency matrix
all.tokens.dfm <- dfm(all.tokens, tolower = FALSE)
rm(all.tokens)


# Trim some features (because 99.90% of the cells are zeros)
sparsity(all.tokens.dfm)
all.tokens.dfm <- dfm_trim(all.tokens.dfm, min_docfreq = 10, min_termfreq = 20, verbose = TRUE)


# Transform to a matrix and inspect
all.tokens.dfm <- as.matrix(all.tokens.dfm)
dim(all.tokens.dfm)


# TF function
tf <- function(row) {
  row / (sum(row) + 1)
}

# IDF function
idf <- function(column) {
  
  size <- length(column)
  doc.count <- length(which(column > 0))
  log10(size / doc.count)
}

# TF-IDF function
tf.idf <- function(tf, idf) {
  
  tf * idf
}


# TF-IDF
tf.mat <- apply(all.tokens.dfm, 1, tf)
idf.mat <- apply(all.tokens.dfm, 2, idf)
tf.idf.mat <- apply(tf.mat, 2, tf.idf, idf = idf.mat)


# Transpose the matrix
tf.idf.mat <- t(tf.idf.mat)


# Clean memory
rm(tf.mat)
rm(idf.mat)
gc()


"
# PCA to test the data
pca <- prcomp(tf.idf.mat)

pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
pca.data <- data.frame(Sample=rownames(pca$x), X=pca$x[,1], Y=pca$x[,2])

ggplot(data=pca.data, aes(x=X, y=Y, label=Sample)) +
  geom_text() +
  xlab(paste('PC1 - ', pca.var.per[1], '%', sep='')) +
  ylab(paste('PC2 - ', pca.var.per[2], '%', sep='')) +
  theme_bw() +
  ggtitle('PCA Graph')
"


# Convert the matrix to a dataframe
df <- as.data.frame(tf.idf.mat, row.names = NULL, optional = FALSE, make.names = TRUE)


# Rectifying the names of the variables
names(df) <- make.names(names(df), unique = TRUE) 


# 10-fold cross validation 3 times (we use trainControl instead of createMultiFolds(df, k = 10, times = 3))
cross_validation <- trainControl(method = "repeatedcv", number = 10, repeats = 3)


# Add labels of male / female to the DFM data frame
df <- cbind(Label = profiles$sex, Data = df)
    

# Train the model
trainmodel <- train(df[,-1], df[,1], trControl = cross_validation, method = "rpart")


# Test the model by calculating a confusion matrix
confMat <- confusionMatrix(trainmodel)








# Remove the batch effect: find and eliminate “male words” and “female words”

# Retrieving words that split the tree-words identified by either woman or men
words <- names(trainmodel$finalModel$variable.importance)
subWords <- list()
for (word in words) {
  subWords <- c(subWords,substring(word,6))
}

# Removing words identified by either woman or men from the dataframe
df_filtered <- as.data.frame(all.tokens.dfm, row.names = NULL, optional = FALSE, make.names = TRUE, headers = FALSE)
df_filtered <- df_filtered[,!(names(df_filtered) %in% subWords)]


# Applying TF-IDF on filtered dataframe
tf.df <- apply(df_filtered, 1, tf)
idf.df <- apply(df_filtered, 2, idf)
tf.idf.df <- apply(tf.df, 2, tf.idf, idf = idf.df)


# Transpose the dataframe
tf.idf.df <- t(tf.idf.df)


# Kmeans with 2,3,4 and 10 clusters
twoClusters <- kmeans(tf.idf.df, 2)
threeClusters <- kmeans(tf.idf.df, 3)
fourClusters <- kmeans(tf.idf.df, 4)
tenClusters <- kmeans(tf.idf.df, 10)


# Calculating the pca
pca <- prcomp(tf.idf.df)
pca.data <- data.frame(Sample = rownames(pca$x), X = pca$x[,1], Y = pca$x[,2])
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var / sum(pca.var)*100, 1)


# Plotting 
colors <- c("cluster1", "cluster2", "cluster3", "cluster4", "cluster5", "cluster6", "cluster7", "cluster8", "cluster9", "cluster10")
barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")
ggplot(data=pca.data, aes(x=X, y=Y, label=Sample ,colour = colors[twoClusters$cluster])) +
  geom_point() + 
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("KMEANS K=2")

ggplot(data=pca.data, aes(x=X, y=Y, label=Sample ,colour = colors[threeClusters$cluster])) +
  geom_point() + 
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("KMEANS K=3")

ggplot(data=pca.data, aes(x=X, y=Y, label=Sample ,colour = colors[fourClusters$cluster])) +
  geom_point() + 
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("KMEANS K=4")

ggplot(data=pca.data, aes(x=X, y=Y, label=Sample ,colour = colors[tenClusters$cluster])) +
  geom_point() + 
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("KMEANS K=10")


dev.off()




