setwd('C:/Users/Bensabat Elliot/Desktop/movieRE')
library(recommenderlab)
library(openxlsx)
library(data.table)
library(proxy)

loadData <- function() {
  xlsxFile <- system.file("movieLens100k.xlsx", package = "openxlsx")
  if(!exists('movies')) {
    movies <<- read.xlsx("movieLens100k.xlsx", sheet = 2)
  }
  if(!exists('ratings')) {
    ratings <<- read.xlsx("movieLens100k.xlsx", sheet = 3)
  }
}

loadData()
ratings <- ratings[(which(ratings$movieId < 1000000)),] #for memory purpose

#change data format to matrix for recommenderLab
ratings_matrix <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
rownames(ratings_matrix) <- ratings_matrix[,1]
ratings_matrix <- ratings_matrix[,-1]
colnames(ratings_matrix) <- paste("movie", colnames(ratings_matrix), sep="")
rownames(ratings_matrix) <- paste("user", rownames(ratings_matrix), sep="")
ratings_matrix <- as.matrix(ratings_matrix)
ratings_matrix <- as(ratings_matrix, "realRatingMatrix")
#ratings_matrix <- normalize(ratings_matrix) #normalization is done automatically in a "RealRatingMatrix"


recommenderRegistry$get_entries(dataType = "realRatingMatrix")
r <- Recommender(ratings_matrix, method="UBCF")
names(getModel(r))
getModel(r)$method
recom <- predict(r, ratings_matrix[rownames(ratings_matrix) == "user500",], n=5)
as(recom, "list")
recom <- predict(r, ratings_matrix[rownames(ratings_matrix) == "user500",], type="ratings")
as(recom, "matrix")[,1:10] #NAs for item rated already
recom <- predict(r, ratings_matrix[500:501], type="ratingMatrix") #complete matrix

#Evaluation of predicted ratings
#90% training set, evaluation based on 15 items, goodRating >= 4
e <- evaluationScheme(ratings_matrix, method="split", train=0.9, given=-1, goodRating=4)
#e <- evaluationScheme(Jester5k[1:1000], method="split", train=0.9,given=15, goodRating=5)
r1 <- Recommender(getData(e, "train"), "UBCF")
r2 <- Recommender(getData(e, "train"), "IBCF")
#predicted ratings for the known part of the test data (15 items for each user)
p1 <- predict(r1, getData(e, "known"), type="ratings")
p2 <- predict(r2, getData(e, "known"), type="ratings")
#Error between prediction and unknown part of test data
error <- rbind(UBCF = calcPredictionAccuracy(p1, getData(e, "unknown")),
               IBCF = calcPredictionAccuracy(p2, getData(e, "unknown")))
error

#Evaluation of top-N recommender algorithm
#4 fold-cross validation, for test users all but 3 items are withheld for evaluation
scheme <- evaluationScheme(ratings_matrix, method="cross", k=4, given=-1, goodRating=4)
#We evaluate top-1, top-3, top-5, top-10, top-15 and top-20 recommendation lists.
results <- evaluate(scheme, method="POPULAR", type = "topNList", n=c(1,3,5,10,15,20))
#Results for 1st of 4 run
getConfusionMatrix(results)[[1]]
#Average result
avg(results)
#Plot TPR vs FPR
plot(results, annotate=TRUE)
#plot precision vs recall
plot(results, "prec/rec", annotate=TRUE)

#Compare different algorithms
scheme <- evaluationScheme(ratings_matrix, method="cross", train=0.9, k=4, given=-1, goodRating=4)
algorithms <- list( "random items" = list(name="RANDOM", param=NULL),
   "popular items" = list(name="POPULAR", param=NULL),
   "user-based CF" = list(name="UBCF", param=list(nn=50)),
   "item-based CF" = list(name="IBCF", param=list(k=50)),
   "SVD approximation" = list(name="SVD", param=list(k = 50)))
#Evaluate for top-N
results <- evaluate(scheme, algorithms, type = "topNList",
                     n=c(1, 3, 5, 10, 15, 20))
names(results)
results[["user-based CF"]]
plot(results, annotate=c(1,3), legend="bottomright")
plot(results, "prec/rec", annotate=3, legend="topleft")
x <- as.data.frame(results$`user-based CF`@results[[1]]@cm) #for k1
results$`user-based CF`@results[[1]]@cm[,1]
precision <- results$`user-based CF`@results[[1]]@cm[,5]
recall <- results$`user-based CF`@results[[1]]@cm[,6]
UBCF_EMeasure <- 2*precision*recall/(precision+recall)


#Evaluate for predicting ratings
results <- evaluate(scheme, algorithms, type = "ratings")
plot(results, ylim = c(0,2.5))

