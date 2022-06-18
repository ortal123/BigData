

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
profiles <- read.csv(file.path('Ex2/data/profiles.csv'), header = TRUE, stringsAsFactors = FALSE)
str(profiles)


# Shuffle the data and pick less rows
profiles <- profiles[sample(1:nrow(profiles)), ]
profiles <- profiles[1:500,]


# Get labels
labels <- profiles$sex


# Output file
pdf(file.path('Ex2/output/Week5_dating.pdf'))


# Combine the essays of each profile into one paragraph
essays <- select(profiles, starts_with("essay"))
essays <- apply(essays, MARGIN = 1, FUN = paste, collapse = " ")


# Clean
rm(profiles)
gc()


# Remove HTML characters
html <- c( "<a[^>]+>", "class=[\"'][^\"']+[\"']", "&[a-z]+;", "\n", "\\n", "<br ?/>", "</[a-z]+ ?>" )
html.pat <- paste0( "(", paste(html, collapse = "|"), ")" )
essays <- str_replace_all(essays, html.pat, " ")


# Remove stop-words
stop.words <-  c( "a", "am", "an", "and", "as", "at", "are", "be", "but", "can", "do", "for", "have", "i'm", "if", "in", "is", "it", "like", "love", "my", "of", "on", "or", "so", "that", "the", "to", "with", "you", "i" )
stop.words.pat <- paste0( "\\b(", paste(stop.words, collapse = "|"), ")\\b" )
essays <- str_replace_all(essays, stop.words.pat, " ")


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


# TF-IDF functions
tf_function <- function(row) { row / (sum(row) + 1) }
idf_function <- function(column) { log10(length(column) / length(which(column > 0))) }
tf_idf_function <- function(tf, idf) { tf * idf }


# Apply TF-IDF
tf <- apply(all.tokens.dfm, 1, tf_function)
idf <- apply(all.tokens.dfm, 2, idf_function)
tf.idf <- apply(tf, 2, tf_idf_function, idf = idf)
tf.idf <- t(tf.idf) # transpose


# Clean
rm(tf)
rm(idf)
gc()


# Convert matrix to dataframe
df <- as.data.frame(tf.idf, row.names = NULL, optional = FALSE, make.names = TRUE)
names(df) <- make.names(names(df), unique = TRUE) 


# 10-fold cross validation 3 times
# we use trainControl instead of createMultiFolds(df, k = 10, times = 3)
cross_validation <- trainControl(method = "repeatedcv", number = 10, repeats = 3)


# Add labels of male / female to the DFM data frame
df <- cbind(Label = labels, Data = df)
    

# Train the model
trainmodel <- train(df[,-1], df[,1], trControl = cross_validation, method = "rpart")
rm(df)


# Test the model by calculating a confusion matrix
confusionMatrix(trainmodel)


# Remove the batch effect: find and eliminate “male words” and “female words”
words_identified_by_gender <- list()
for (word in names(trainmodel$finalModel$variable.importance)) {
  words_identified_by_gender <- c(words_identified_by_gender, substring(word, 6))
}
words_identified_by_gender

all.tokens.dfm <- as.data.frame(all.tokens.dfm, row.names = NULL, optional = FALSE, make.names = TRUE, headers = FALSE)
all.tokens.dfm <- all.tokens.dfm[,!(names(all.tokens.dfm) %in% words_identified_by_gender)]


# Apply TF-IDF on filtered dataframe
tf <- apply(all.tokens.dfm, 1, tf_function)
idf <- apply(all.tokens.dfm, 2, idf_function)
tf.idf <- apply(tf, 2, tf_idf_function, idf = idf)
tf.idf <- t(tf.idf) # transpose


# Clean
rm(all.tokens.dfm)
rm(tf)
rm(idf)
gc()


# Calculate PCA
pca <- prcomp(tf.idf)
pca.data <- data.frame(Sample = rownames(pca$x), X = pca$x[,1], Y = pca$x[,2])
pca.var <- pca$sdev ^ 2
pca.var.per <- round(pca.var / sum(pca.var) * 100, 1)


# Cluster the applicants to 2,3,4 and 10 clusters using Kmeans
colors <- paste0("cluster #", 1:10)

for (k in c(2, 3, 4, 10)) {
  
  clusters <- kmeans(tf.idf, k)
  
  print(ggplot(data = pca.data, aes(x = X, y = Y, label = Sample ,colour = colors[clusters$cluster])) +
          geom_point() + theme_bw() + ggtitle(paste0("Kmeans: K = ", k)) +
          theme(legend.position = "bottom", panel.background = element_rect(fill = "grey")) +
          xlab(paste0("PC1 (", pca.var.per[1], "%)")) + ylab(paste0("PC2 (", pca.var.per[2], "%)")))
}


dev.off()






