
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


#connecting to the MYSQL database
mydb = dbConnect(MySQL(), user='root', password='project', dbname='hackathon', host='localhost')

#list of tables in the database
tables<-dbListTables(mydb)

dbSendQuery(mydb, "SET GLOBAL local_infile = true;")

#query to put all of the ratings into a dataframe to use inside of R
ratings<-dbGetQuery(mydb, "select * from `bx-book-ratings` b where b.`Book-Rating`>0 and b.`Book-Rating` <=10 and  `ISBN` REGEXP '^[A-Za-z0-9]+$'")


print(head(ratings))
print("first ratings")

#removing the RMYSQL package so we use the sqldf package
detach("package:RMySQL", unload=TRUE)
books_with_ratings<-sqldf("select `ISBN` from ratings group by `ISBN` having count(*)>4;")

people_with_ratings<-sqldf("select `User-ID` from ratings group by `User-ID` having count(*)>4 and count(*)<600;")

ratings<-sqldf("select * from ratings where `User-ID` in people_with_ratings and `ISBN` in books_with_ratings; ")
print(dim(ratings))


library(RMySQL)
#query to put all of the people that rated books into a dataframe
people<-dbGetQuery(mydb, "select * from `bx-users` b
                   where b.`User-ID` in (
                     select rating.`User-ID` from `bx-book-ratings` rating)")
print("people")


#query to put all of the rated books into a dataframe
books<-dbGetQuery(mydb, "select * from `bx-books` b where b.`ISBN` in (
    select rating.`ISBN` from `bx-book-ratings` rating
    )")
print("first books")

#removing the RMYSQL package so we use the sqldf package
detach("package:RMySQL", unload=TRUE)

#removing duplicate books
books<-sqldf("SELECT *
FROM books b
GROUP BY
    b.`Book-Title`, b.`Book-Author`, b.Publisher, b.`Year-Of-Publication`
HAVING  COUNT(b.`Book-Author`) > 0
    AND COUNT(b.`Year-Of-Publication`) > 0
    AND COUNT(b.`Publisher`)> 0
AND COUNT(b.`Book-Title`)>0;")
print("second books")

#removing the ratings that contain people or books that we removed
ratings<-sqldf("SELECT * FROM ratings  where `ISBN` in (select `ISBN` from books) 
               and `User-ID` in (select `User-ID` from people)")

print(dim(ratings))
print("second ratings")


print(head(ratings))

print(head(books))

print(head(people))





#converting ratings into realRatingMatrix
bmatrix <- as(ratings, "realRatingMatrix")
print(dim(bmatrix@data))

rm(people)
rm(people_with_ratings)
rm(books_with_ratings)
rm(ratings)
rm(books)

#5 fold cross validation sets for training and testing data
eval_sets <- evaluationScheme(data = bmatrix, method = "cross-validation",
                              train = 0.8, given = -1,
                              goodRating = 6, k = 5)

gc()

#knn for my predict
.knn <- function(sim, k)
  lapply(1:nrow(sim), FUN = function(i)
    head(order(sim[i,], decreasing = TRUE, na.last = NA), k))


# Predict:

my.predict <- function (model, newdata, n = 10, data = NULL,
                        type = c("topNList", "ratings", "ratingMatrix"), ...)
{
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
  #divide model$data into sections by rows
  cat('(II) running similarity() calculation\n')
  sim <- similarity(newdata, model$data, method = model$method,
                    min_matching = model$min_matching_items,
                    min_predictive = model$min_predictive_items)
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
  ratings <- new("realRatingMatrix", data = dropNA(ratings),
                 normalize = getNormalize(newdata))
  
  cat ('(II) de-normalize the ratings (back to rating scale)\n')
  ratings <- denormalize(ratings)
  cat ('(II) de-normalize done\n')
  
  returnRatings(ratings, newdata, type, n)
}

#amount of items we want recommendations for
items_to_recommend <- 10

#building the ubcf recommendation model
eval_recommender_ubcf <- Recommender(data = getData(eval_sets, "train"),
                                method = "UBCF", parameter = NULL)

#making the weights false so my.predict will work
eval_recommender_ubcf@model$weighted<-FALSE

#prediciting based on the recommender model
eval_prediction_ubcf <- my.predict( eval_recommender_ubcf@model,
                             newdata = getData(eval_sets, "known"),
                             n = items_to_recommend,
                             type = "ratings")

#getting the accuracy results for ubcf model
eval_accuracy_ubcf <- calcPredictionAccuracy(x = eval_prediction_ubcf,
                                         data = getData(eval_sets, "unknown"),
                                         byUser = TRUE)
#total results for predictions
eval_accuracy_ubcf_total <- calcPredictionAccuracy(x = eval_prediction_ubcf,
                                                   data = getData(eval_sets, "unknown"),
                                                   byUser = FALSE)
print("total RMSE for ubcf")
print(eval_accuracy_ubcf_total[1])

#removing rows with Nan
eval_accuracy_ubcf<- na.omit(eval_accuracy_ubcf)

#printing the head of the accuracy data
print(head(eval_accuracy_ubcf))





#saving data
save(file='ubcfmodel.rdata',bmatrix,eval_sets,eval_recommender_ubcf, eval_accuracy_ubcf)



#creating a recommender model for ibcf
eval_recommender_ibcf <- Recommender(data = getData(eval_sets, "train"),method = "IBCF", parameter = NULL)

#predicitons for the ibcf model
eval_prediction_ibcf <- predict( eval_recommender_ibcf,
                             newdata = getData(eval_sets, "known"),
                             n = items_to_recommend,
                             type = "ratings")

#the prediction results for ibcf
eval_accuracy_ibcf <- calcPredictionAccuracy(x = eval_prediction_ibcf,
                                         data = getData(eval_sets, "unknown"),
                                         byUser = TRUE)
#total results for predictions
eval_accuracy_ibcf_total <- calcPredictionAccuracy(x = eval_prediction_ibcf,
                                             data = getData(eval_sets, "unknown"),
                                             byUser = FALSE)
print("total RMSE for ibcf")
print(eval_accuracy_ibcf_total[1])
#removing Nan from the data
eval_accuracy_ibcf<- na.omit(eval_accuracy_ibcf)

#printing the head of the ibcf data
print(head(eval_accuracy_ibcf))



#opening the pdf for the histograms
pdf('histogram.pdf')

#converting the data into a data frame so we can plot a histogram
accuracy_ubcf<-eval_accuracy_ubcf[,1]
df_ubcf<-as.data.frame(x=accuracy_ubcf)

#plotting a histogram for ubcf, using ggplot
ggplot(df_ubcf, aes(x= accuracy_ubcf)) + geom_histogram(color="darkblue", fill="blue")+ggtitle("UBCF RMSE Histogram")+labs(y="bin",x="RMSE")

#histogram for text file
hist_ubcf<-hist(x=as.numeric(df_ubcf$accuracy_ubcf),plot=FALSE )


#converting the data into a dataframe for creating a histogram
accuracy_ibcf<-eval_accuracy_ibcf[,1]
df_ibcf<-as.data.frame(x=accuracy_ibcf)

#plotting a histogram with ggplot
ggplot(df_ibcf, aes(x= accuracy_ibcf)) + geom_histogram(color="darkblue", fill="blue")+ggtitle("IBCF RMSE Histogram") +labs(y="bin",x="RMSE")

#histogram for text file
hist_ibcf<-hist(x=as.numeric(df_ibcf$accuracy_ibcf),plot=FALSE )

rmse_hist <- as.data.frame(row.names=as.character( hist_ubcf$mids)
                     , qpcR:::cbind.na( hist_ubcf$counts,hist_ibcf$counts))

colnames(rmse_hist) <- c("N.UBCF", "N.IBCF")
print(rmse_hist)

#backing up data for text file
save(file="part1text.rdata",rmse_hist,eval_accuracy_ubcf_total,eval_accuracy_ibcf_total)

#by user RMSE chart
V.RMSE_data_frame<- transform(merge(df_ubcf,df_ibcf,by=0,all=TRUE), row.names=Row.names, Row.names=NULL)
colnames(V.RMSE_data_frame) <- c("UBCF", "IBCF")
print(head(V.RMSE_data_frame))

#nested RMSE list by user
V.RMSE<-list(list(df_ubcf),list(df_ibcf))
names(V.RMSE) <- c("UBCF", "IBCF")

dev.off()
#saving data
save(file='model.rdata',bmatrix,eval_sets,eval_recommender_ubcf, eval_recommender_ibcf, V.RMSE,V.RMSE_data_frame)


rm(eval_prediction_ibcf)
rm(eval_prediction_ubcf)
rm(bmatrix)
rm(V.RMSE_data_frame)
rm(V.RMSE)
rm(df_ubcf)
rm(df_ibcf)
gc()

library(RMySQL)
#query to put all of the  books into a dataframe
books<-dbGetQuery(mydb, "select * from `bx-books`")

#removing the RMYSQL package so we use the sqldf package
detach("package:RMySQL", unload=TRUE)

#ubcf recomenadations

top_ten_ubcf_predict <- predict(eval_recommender_ubcf,
               newdata = getData(eval_sets, "known")[1:500], n = items_to_recommend)

top_ten_ubcf_predict<-as(top_ten_ubcf_predict, "list")

#find book title based on isbn
find_book_name<-function(x)
  
{
  temp<-sqldf(sprintf("select `Book-Title` from `books` where `ISBN` LIKE '%s';",x))
  substr(temp,1,12)
}

#apply previous function on all of the users
for (i in 1:500)
{
  top_ten_ubcf_predict[[i]]<-lapply(top_ten_ubcf_predict[[i]],FUN=find_book_name)
  
}

#converts results into matrix
ubcf_recommended<-do.call(rbind, top_ten_ubcf_predict)

#updating the column names
colnames(ubcf_recommended) <- c("book 1", "book 2","book 3","book 4","book 5","book 6","book 7","book 8","book 9","book 10")
print("ubcf recomendations")
print(ubcf_recommended)

###ibcf recommendations

top_ten_ibcf_predict <- predict(eval_recommender_ibcf,
                                newdata = getData(eval_sets, "known")[1:500], n = items_to_recommend)

top_ten_ibcf_predict<-as(top_ten_ibcf_predict, "list")
#apply previous function on all of the users
for (i in 1:500)
{
  top_ten_ibcf_predict[[i]]<-lapply(top_ten_ibcf_predict[[i]],FUN=find_book_name)
  
}

#converts results into matrix
ibcf_recommended<-do.call(rbind, top_ten_ibcf_predict)

#updating the column names
colnames(ibcf_recommended) <- c("book 1", "book 2","book 3","book 4","book 5","book 6","book 7","book 8","book 9","book 10")
print("ibcf recomendations")
print(ibcf_recommended)




#backing up data for text file
save(file="part2text.rdata",ubcf_recommended,ibcf_recommended)




