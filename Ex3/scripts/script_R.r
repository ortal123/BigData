

# Big Data - Exercise 3 (Hackathon)
# GitHub: https://github.com/JuditHalperin/BigData/tree/main/Ex3

# Yudit Halperin - 324216589
# Ortal Calfon (Peretz) - 315011189
# Asnat Berlin - 211825401
# Naama Shenberger - 211983747



# Loud libraries
library(RMySQL)
library(sqldf)
library(recommenderlab)
library(stringr)
library(ggplot2)
library(qpcR)


# Connect to the MYSQL database
mydb <- dbConnect(MySQL(), user = 'root', password = 'Yeuda0522', dbname = 'bx_db', host = 'localhost')
tables <- dbListTables(mydb)
dbSendQuery(mydb, "SET GLOBAL local_infile = true;")


# Create ratings dataframe
ratings <- dbGetQuery(mydb, "select * from `bx_book_ratings` b where b.`Book_Rating` > 0 and b.`Book_Rating` <= 10 and `ISBN` REGEXP '^[A-Za-z0-9]+$'")


# Remove the RMySQL package to use the sqldf package
detach("package:RMySQL", unload = TRUE)


# Query reatings
books_with_ratings <- sqldf("select `ISBN` from ratings group by `ISBN` having count(*) > 30;")
people_with_ratings <- sqldf("select `User_ID` from ratings group by `User_ID` having count(*) > 30 and count(*) < 300;")
ratings <- sqldf("select * from ratings where `User_ID` in people_with_ratings and `ISBN` in books_with_ratings;")
print(dim(ratings))


# Create dataframes of people that rated books and the of the rated books
library(RMySQL)
people <- dbGetQuery(mydb, "select * from `bx_users` b where b.`User_ID` in (select rating.`User_ID` from `bx_book_ratings` rating)")
books <- dbGetQuery(mydb, "select * from `bx_books` b where b.`ISBN` in (select rating.`ISBN` from `bx_book_ratings` rating)")


# Remove duplicate books
detach("package:RMySQL", unload = TRUE)
books <- sqldf("SELECT * FROM books b GROUP BY b.`Book_Title`, b.`Book_Author`, b.Publisher, b.`Year_Of_Publication` HAVING COUNT(b.`Book_Author`) > 0 AND COUNT(b.`Year_Of_Publication`) > 0 AND COUNT(b.`Publisher`) > 0 AND COUNT(b.`Book_Title`) > 0;")


# Remove ratings that contain people or books that were removed
ratings <- sqldf("SELECT * FROM ratings  where `ISBN` in (select `ISBN` from books) and `User_ID` in (select `User_ID` from people)")
print(dim(ratings))


# Convert ratings into realRatingMatrix
bmatrix <- as(ratings, "realRatingMatrix")
print(dim(bmatrix@data))


# Clean
rm(people)
rm(people_with_ratings)
rm(books_with_ratings)
rm(ratings)
rm(books)
gc()


# 5-fold cross validation sets for training (80%) and testing (20%) data
eval_sets <- evaluationScheme(data = bmatrix, method = "cross-validation", train = 0.8, given = -1, goodRating = 6, k = 5)


# Knn function
.knn <- function(sim, k)
  lapply(1:nrow(sim), FUN = function(i)
    head(order(sim[i,], decreasing = TRUE, na.last = NA), k))


# Predict function
my.predict <- function(model, newdata, n = 10, data = NULL, type = c("topNList", "ratings", "ratingMatrix"), ...) {
  
  type <- match.arg(type)
  newdata_id <- NULL
  
  if (is.numeric(newdata)) {
    if (model$sample)
      stop("(EE) User id in newdata does not work when sampling is used!")
    
    newdata_id <- newdata
    newdata <- model$data[newdata, ]
  }
  
  else {
    if (ncol(newdata) != ncol(model$data))
      stop("(EE) number of items in newdata does not match model.")
    
    if (!is.null(model$normalize))
      newdata <- normalize(newdata, method = model$normalize)
  }
  
  # Divide model$data into sections by rows
  cat('(II) running similarity() calculation\n')
  sim <- similarity(newdata, model$data, method = model$method, min_matching = model$min_matching_items, min_predictive = model$min_predictive_items)
  cat('(II) similarity() done\n')
  
  if (!is.null(newdata_id))
    sim[cbind(seq(length(newdata_id)), newdata_id)] <- NA
  
  cat(paste('(II) creating knn with', model$nn ,'neighbors\n'))
  neighbors <- .knn(sim, model$nn)
  cat('(II) knn done\n')
  
  if (model$weighted) {
    cat ('(II) weigh the ratings by similarities\n')
    s_uk <- sapply(1:nrow(sim), FUN = function(i) sim[i, neighbors[[i]]])
    if (!is.matrix(s_uk)) s_uk <- as.matrix(t(s_uk))
    ratings <- t(sapply(1:nrow(newdata), FUN = function(i) {
      r_neighbors <- as(model$data[neighbors[[i]]], "dgCMatrix")
      Cols.1 <- drop(as(crossprod(r_neighbors, s_uk[, i]), "matrix"))
      Cols.2 <- drop(as(crossprod(!dropNAis.na(r_neighbors), s_uk[, i]), "matrix"))
      Cols.1 / Cols.2
    }))
    ratings[!is.finite(ratings)] <- NA
    cat ('(II) done weigh the ratings\n')
  }
  
  else {
    cat ("(II) copy the ratings across the user's knn\n")
    ratings <- t(sapply(1:nrow(newdata), FUN = function(i) {
      r_neighbors <- as(model$data[neighbors[[i]]], "dgCMatrix")
      Cols.1 <- colSums(r_neighbors)
      Cols.2 <- colSums(!dropNAis.na(r_neighbors))
      Cols.1 / Cols.2
    }))
    ratings[!is.finite(ratings)] <- NA
    cat ("(II) copy the ratings ... done\n")
  }
  
  rownames(ratings) <- rownames(newdata)
  ratings <- new("realRatingMatrix", data = dropNA(ratings), normalize = getNormalize(newdata))
  
  cat ('(II) de-normalize the ratings (back to rating scale)\n')
  ratings <- denormalize(ratings)
  cat ('(II) de-normalize done\n')
  
  returnRatings(ratings, newdata, type, n)
}


# Number of items we want recommendations for
items_to_recommend <- 10


# Build the ubcf recommendation model
eval_recommender_ubcf <- Recommender(data = getData(eval_sets, "train"), method = "UBCF", parameter = NULL)


# Make the weights false for my.predict()
eval_recommender_ubcf@model$weighted <- FALSE


# Predicit based on the recommender model
eval_prediction_ubcf <- my.predict(eval_recommender_ubcf@model, newdata = getData(eval_sets, "known"), n = items_to_recommend, type = "ratings")


# Get accuracy for ubcf model
eval_accuracy_ubcf <- calcPredictionAccuracy(x = eval_prediction_ubcf, data = getData(eval_sets, "unknown"), byUser = TRUE)


# Total results for predictions
eval_accuracy_ubcf_total <- calcPredictionAccuracy(x = eval_prediction_ubcf, data = getData(eval_sets, "unknown"), byUser = FALSE)
print("total RMSE for ubcf")
print(eval_accuracy_ubcf_total[1])


# Remove rows with Nan
eval_accuracy_ubcf <- na.omit(eval_accuracy_ubcf)


# Head of the accuracy data
print(head(eval_accuracy_ubcf))


# Save data
# save(file = 'Ex3/output/ubcfmodel.rdata', bmatrix, eval_sets, eval_recommender_ubcf, eval_accuracy_ubcf)


# Create a recommender model for ibcf
eval_recommender_ibcf <- Recommender(data = getData(eval_sets, "train"), method = "IBCF", parameter = NULL)


# Predicitons for the ibcf model
eval_prediction_ibcf <- predict( eval_recommender_ibcf, newdata = getData(eval_sets, "known"), n = items_to_recommend, type = "ratings")


# Get accuracy for ibcf model
eval_accuracy_ibcf <- calcPredictionAccuracy(x = eval_prediction_ibcf, data = getData(eval_sets, "unknown"), byUser = TRUE)


# Total results for predictions
eval_accuracy_ibcf_total <- calcPredictionAccuracy(x = eval_prediction_ibcf, data = getData(eval_sets, "unknown"), byUser = FALSE)
print("total RMSE for ibcf")
print(eval_accuracy_ibcf_total[1])


# Remove rows with Nan
eval_accuracy_ibcf <- na.omit(eval_accuracy_ibcf)


# Head of the accuracy data
print(head(eval_accuracy_ibcf))


# Output PDF histograms
pdf('Ex3/output/histogram.pdf')


# Convert data to dataframe
accuracy_ubcf <- eval_accuracy_ubcf[,1]
df_ubcf <- as.data.frame(x = accuracy_ubcf)


# Plot histogram for ubcf
ggplot(df_ubcf, aes(x = accuracy_ubcf)) + geom_histogram(color = "darkgreen", fill = "green") + ggtitle("UBCF - RMSE Histogram") + labs(y = "Bin", x = "RMSE")


# Histogram for text file
hist_ubcf <- hist(x = as.numeric(df_ubcf$accuracy_ubcf), plot = FALSE)


# Convert data to dataframe
accuracy_ibcf <- eval_accuracy_ibcf[,1]
df_ibcf <- as.data.frame(x = accuracy_ibcf)


# Plot histogram for ibcf
ggplot(df_ibcf, aes(x = accuracy_ibcf)) + geom_histogram(color = "darkgreen", fill = "green") + ggtitle("IBCF - RMSE Histogram") + labs(y = "Bin", x = "RMSE")


# Histogram for text file
hist_ibcf <- hist(x = as.numeric(df_ibcf$accuracy_ibcf), plot = FALSE)


# RMSE histogram (6.b)
rmse_hist <- as.data.frame(row.names=as.character( hist_ubcf$mids), qpcR:::cbind.na( hist_ubcf$counts,hist_ibcf$counts))
colnames(rmse_hist) <- c("N.UBCF", "N.IBCF")
print(rmse_hist)


# Back up data for text file
# save(file = "Ex3/output/part1text.rdata", rmse_hist, eval_accuracy_ubcf_total, eval_accuracy_ibcf_total)


# By user RMSE chart
V.RMSE_data_frame <- transform(merge(df_ubcf, df_ibcf, by = 0, all = TRUE), row.names = Row.names, Row.names = NULL)
colnames(V.RMSE_data_frame) <- c("UBCF", "IBCF")
print(head(V.RMSE_data_frame))


# Nested RMSE list by user
V.RMSE<-list(list(df_ubcf), list(df_ibcf))
names(V.RMSE) <- c("UBCF", "IBCF")


# Close output
dev.off()


# Save data
save(file = 'Ex3/output/model.rdata', bmatrix, eval_sets, eval_recommender_ubcf, eval_recommender_ibcf, V.RMSE, V.RMSE_data_frame)


# Clean
rm(eval_prediction_ibcf)
rm(eval_prediction_ubcf)
rm(bmatrix)
rm(V.RMSE_data_frame)
rm(V.RMSE)
rm(df_ubcf)
rm(df_ibcf)
gc()


# Put all of the books into a dataframe
library(RMySQL)
books <- dbGetQuery(mydb, "select * from `bx_books`")
detach("package:RMySQL", unload=TRUE)


# ubcf recomenadations
top_ten_ubcf_predict <- predict(eval_recommender_ubcf, newdata = getData(eval_sets, "known")[1:279], n = items_to_recommend)
top_ten_ubcf_predict <- as(top_ten_ubcf_predict, "list")


# Find book title based on isbn
find_book_name<-function(x) {
  temp <- sqldf(sprintf("select `Book_Title` from `books` where `ISBN` LIKE '%s';",x))
  substr(temp, 1, 12)
}


# Apply previous function on all of the users
for (i in 1:279) {
  top_ten_ubcf_predict[[i]] <- lapply(top_ten_ubcf_predict[[i]], FUN = find_book_name)
}


# Convert results into matrix
ubcf_recommended <- do.call(rbind, top_ten_ubcf_predict)


# Column names
colnames(ubcf_recommended) <- paste0('book ', 1:10)


# Print ubcf recomendation
print("ubcf recomendations")
print(ubcf_recommended)


# Save to csv
write.csv(ubcf_recommended, "Ex3/output/ubcf_recomendations.csv")


# ibcf recommendations
top_ten_ibcf_predict <- predict(eval_recommender_ibcf, newdata = getData(eval_sets, "known")[1:279], n = items_to_recommend)
top_ten_ibcf_predict<-as(top_ten_ibcf_predict, "list")


# Apply previous function on all of the users
for (i in 1:279) {
  top_ten_ibcf_predict[[i]] <- lapply(top_ten_ibcf_predict[[i]], FUN = find_book_name)
}


# Convert results into matrix
ibcf_recommended<-do.call(rbind, top_ten_ibcf_predict)


# Column names
colnames(ibcf_recommended) <- paste0('book ', 1:10)


# Print ibcf recomendation
print("ibcf recomendations")
print(ibcf_recommended)


# Save to csv
write.csv(ibcf_recommended, "Ex3/output/ibcf_recomendations.csv")


# Back up data for text file
# save(file = "Ex3/output/part2text.rdata", ubcf_recommended, ibcf_recommended)









