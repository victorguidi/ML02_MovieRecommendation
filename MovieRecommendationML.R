#Movie Recommendation

#Source code: https://data-flair.training/blogs/data-science-r-movie-recommendation/

setwd("C:/Users/vsgui/Documents/RProjects/MovieRecommendation")
getwd()

install.packages("recommenderlab")
install.packages("ggplot2")
install.packages("data.table")
install.packages("reshape2")

library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)

movie_data <- read.csv("IMDB-Dataset/movies.csv", stringsAsFactors = FALSE)
rating_data <- read.csv("IMDB-Dataset/ratings.csv")

#Get the information about the data
str(movie_data)
summary(movie_data)
head(movie_data)

str(rating_data)
summary(rating_data)
head(rating_data)

?data.table

#DATA PRE-PROCESSING  
#Now we have to split the column Genres and transpose into a new Data Frame
#And give columns to each gender

#Individual Data Frame with 1 column with all genres
movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors = FALSE)
View(movie_genre)

#New Data Frame with the genres organized
movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], '[|]', 
                                        type.convert=TRUE), 
                              stringsAsFactors=FALSE) 
colnames(movie_genre2) <- c(1:10)
View(movie_genre2)

#Creating a new Table(Matrix) with with a column for each genre
list_genre <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")
genre_mat1 <- matrix(0,10330,18)
genre_mat1[1,] <- list_genre
colnames(genre_mat1) <- list_genre
View(genre_mat1)

#Populating the table using a for loop (Autor for this loop: DataFlair)
for (index in 1:nrow(movie_genre2)) {
  for (col in 1:ncol(movie_genre2)) {
    gen_col = which(genre_mat1[1,] == movie_genre2[index,col]) 
    genre_mat1[index+1,gen_col] <- 1
  }
}
#Now the new table is populated
View(genre_mat1)

#remove the first row, which was the genre list
genre_mat2 <-as.data.frame(genre_mat1[-1,], stringsAsFactors = FALSE)
View(genre_mat2)

#Last we have to convert from characters to integers, again we use a loop
for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col])
}
str(genre_mat2)
summary(genre_mat2)

#Creating a searching matrix in order to find movies based on their genre
SearchMatrix <- cbind(movie_data[,1:2], genre_mat2[])
View(SearchMatrix)

#What can be perceived is that we have a sparse matrix (A Matrix in which most of the elements are zero)
#But we still have to convert the current Matrix(table) into a sparse matrix
#This is in order to make sense out of the ratings through recommenderlabs

#Convert to a sparse matrix
ratingMatrix <- dcast(rating_data, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingMatrix <- as.matrix(ratingMatrix[,-1])
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
ratingMatrix

#Using the library recommendation lab to get some:
recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)

#Understanding how is the library creating the recommendations:
lapply(recommendation_model, "[[", "description")

#As we can see the best model for what we are looking for is the IBCF or the UBCF("Recommender based on item-based collaborative filtering.")
recommendation_model$IBCF_realRatingMatrix$parameters

#K = denotes the number of items for computing their similarities
#Method : Cosine = Cosine similarity is a metric used to measure how similar the documents are irrespective of their size.
#Normalize : center = data preprocessing step where we adjust the scales of the features to have a standard scale of measure.(Center here)
#Alpha = Alpha is also known as the level of significance. This represents the probability of obtaining your results due to chance. 

#THIRD PART

#Recommending movies is dependent on creating a relationship of similarity between the two users. 

#Exploring way of finding similarities: Cosine, pearson, jaccard

#Similarities with the users by taking four users
#Each cell in the matrix represents the similarity that is shared between two users
similarity_mat <- similarity(ratingMatrix[1:4, ],
                             method = "cosine",
                             which = "users")
as.matrix(similarity_mat)

#Visualizing in a plot:
image(as.matrix(similarity_mat), main = "User's Similarities")

#Similarities with the movies: (Same idea)
movie_similarity <- similarity(ratingMatrix[, 1:4], 
                               method = "cosine",
                               which = "items")
as.matrix(movie_similarity)

#Visualizing in a plot:
image(as.matrix(movie_similarity), main = "Movies Similarity")

#Getting the most unique ratings: (The grades given to the movies by the users, 0 means there is no grade)
rating_values <- as.vector(ratingMatrix@data)
unique(rating_values)

#Now in a table: 
table_of_Ratings <- table(rating_values)
View(table_of_Ratings)

#FOURTH PART
#Visualizing the data so far:
View(genre_mat2)
View(SearchMatrix)
ratingMatrix

#Counting the number of views per movie
movie_views <- colCounts(ratingMatrix)

#Creating a Data Frame for the information above and organizing in descending order
table_views <- data.frame(movie = names(movie_views),
                          views = movie_views)
table_views <- table_views[order(table_views$views,
                           decreasing = TRUE), ]
View(table_views)

#Creating the column with the name of the movies since we only have the ID so far
table_views$title <- NA

#In order to populate the title column we are going to use another for loop
for (index in 1:10325){
  table_views[index, 3] <- as.character(subset(movie_data,
                                               movie_data$movieId ==
                                                 table_views[index, 1])$title)
}
table_views[1:6, ] #The first 6 movies
View(table_views)
tail(table_views)

#Bar plot with the ratings (excluding the ones with 0)
table_of_Ratings_df <- as.data.frame(table_of_Ratings)
table_of_Ratings_df <- table_of_Ratings_df[-1,]
ggplot(table_of_Ratings_df, aes(x= Freq, y = rating_values)) +
  geom_bar(stat="identity", fill = 'cyan') +
  geom_text(aes(label = Freq ), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Total")

#Plotting a bar plot with ggplot to see the number of views for the top movies than for the not so popular
ggplot(table_views[1:6, ], aes(x = title, y = views)) +
  geom_bar(stat="identity", fill ='darkgreen') +
  geom_text(aes(label = views), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Total Views of the Top Films")

#Now the ones with few views (They only have one so nothing really interesting)
ggplot(tail(table_views), aes(x = title, y = views)) +
  geom_bar(stat="identity", fill ='brown') +
  geom_text(aes(label = views), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Total Views of the bottom Films")


#Heatmap with the first 25 rows and 25 columns:
image(ratingMatrix[1:20, 1:25], axes = FALSE, main = "Heatmap of the first 25 rows and 25 columns")

#FIFTH PART - DATA PREPARATION
# 3 steps:
#   Selecting useful data
#   Normalizing data - Organizing it and turning it into a structured form
#   Binarizing the data - is the process of dividing data into two groups and assigning one out. of two values
#                          to all the members of the same group


#Selecting the data  to at least 50 people that rated a movie and to movies that have at least 50 views
movie_ratings <- ratingMatrix[rowCounts(ratingMatrix) > 50,
                              colCounts(ratingMatrix) > 50]
movie_ratings # 420 x 447 rating matrix of class ‘realRatingMatrix’ with 38341 ratings.

#Creating a matrix with relevant user using quantile regression

#Quantile regression models the relationship between a set of predictor (independent) variables and specific percentiles (or "quantiles") 
#of a target (dependent) variable, most often the median - Definition by IBM

minimum_movies <- quantile(rowCounts(movie_ratings), 0.98)
minimum_users <- quantile(colCounts(movie_ratings), 0.98)
image(movie_ratings[rowCounts(movie_ratings) > minimum_movies,
                    colCounts(movie_ratings) > minimum_users],
      main = "Heatmap of the top users and movies")

#Looking at the distribution of the average rating per user using a Histogram
average_ratings <- rowMeans(movie_ratings)
qplot(average_ratings, fill=I("steelblue"), col=I("red")) +
  ggtitle("Distribution of the average rating per user")

#Data Normalization
#Normalization is a data preparation procedure to standardize the numerical values in a column to a common scale value.
#This is done in such a way that there is no distortion in the range of values.
#Normalization transforms the average value of our ratings column to 0.

normalized_ratings <- normalize(movie_ratings)
sum(rowMeans(normalized_ratings) > 0.00001)

image(normalized_ratings[rowCounts(normalized_ratings) > minimum_movies,
                         colCounts(normalized_ratings) > minimum_users],
      main = "Normalized Ratings of the Top User")

#Data Binarization
#Binarizing the data means that we have two discrete values 1 and 0, which will allow our recommendation systems to work more efficiently.
#If the rating is above 3 is equal to 1 if not is equal to 0

binary_minimum_movies <- quantile(rowCounts(movie_ratings), 0.95)
binary_minimum_users <- quantile(colCounts(movie_ratings), 0.95)

good_rated_films <- binarize(movie_ratings, minRating = 3)
image(good_rated_films[rowCounts(movie_ratings) > binary_minimum_movies,
      colCounts(movie_ratings) > binary_minimum_users],
main = "Heatmap of the top users and movies")

#Build the recommendation System

#First we have to divide the data into training and test (0.8 training, 0.2 test)
sampled_data <- sample(x = c(TRUE, FALSE),
                      size = nrow(movie_ratings),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
training_data <- movie_ratings[sampled_data, ]
testing_data <- movie_ratings[!sampled_data]

training_data #332 x 447 rating matrix of class ‘realRatingMatrix’ with 31363 ratings.
testing_data #88 x 447 rating matrix of class ‘realRatingMatrix’ with 6978 ratings.

#Buildin the system
recommendation_system <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
recommendation_model$IBCF_realRatingMatrix$parameters

#Training the model
recommen_model <- Recommender(data = training_data,
                              method = "IBCF",
                              parameter = list(k = 30))
recommen_model
class(recommen_model)

#Creating a matrix that stores the results of the model
model_info <- getModel(recommen_model)
class(model_info$sim)
dim(model_info$sim)
top_items <- 20
image(model_info$sim[1: top_items, 1: top_items],
      main = "Heatmap of the first rows and columns")

#Now we calculate the sum of rows and columns that have similarity above 0 (That is have been rated above 3)
sum_rows <- rowSums(model_info$sim > 0)
table(sum_rows)
sum_cols <- colSums(model_info$sim > 0)
qplot(sum_cols, fill=I("steelblue"), col=I("red"))+ ggtitle("Distribution of the column count")

#Final Part
#Now we determines the top 10 by creating a variable and use the predict() function that will identify similar items and will rank them
#each rating is used as a weight. Each weight is multiplied with related similarities.

top_recommendations <- 10 # the number of items to recommend to each user
predicted_recommendations <- predict(object = recommen_model,
                                     newdata = testing_data,
                                     n = top_recommendations)
predicted_recommendations


#Now we select an user from the testing data and try to recommend the top 10 to make sure it works
user1 <- predicted_recommendations@items[[1]] # recommendation for the first user
movies_user1 <- predicted_recommendations@itemLabels[user1]
movies_user2 <- movies_user1
for (index in 1:10){
  movies_user2[index] <- as.character(subset(movie_data,
                                             movie_data$movieId == movies_user1[index])$title)
}
movies_user2

#Checking how many recommendations we have for each user in the testing data, and adding it to a matrix
recommendation_matrix <- sapply(predicted_recommendations@items,
                                function(x){ as.integer(colnames(movie_ratings)[x]) }) # matrix with the recommendations for each user
View(recommendation_matrix[,1:4])

#Finding the distribution of number of items for IBCF
number_of_items <- factor(table(recommendation_matrix))
chart_title <- "Distribution of the Number of Items for IBCF"
qplot(number_of_items, fill=I("steelblue"), col=I("black")) + ggtitle(chart_title)

#Top 10 movies more recommended:
number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 10)
table_top_10 <- data.frame(as.integer(names(number_of_items_top)),
                        number_of_items_top)
for(i in 1:10) {
  table_top_10[i,1] <- as.character(subset(movie_data,
                                        movie_data$movieId == table_top_10[i,1])$title)
}

colnames(table_top_10) <- c("Movie_Title", "No_of_Items")
View(table_top_10)

#Visualizing the top 10 movies more recommended:
ggplot(table_top_10, aes(x= Movie_Title, y = No_of_Items)) +
  geom_bar(stat="identity", fill = 'darkgreen') +
  geom_text(aes(label = No_of_Items ), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Top 10 movies more recommended")

#Finish
