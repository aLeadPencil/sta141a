library(rio)
library(dplyr)
library(grid)
library(caret)
library(ggplot2)
set.seed(10)

rm(list=ls())
testing_dir <- "D:/School Stuff/Math/Stats 141A/Data Files/Project/Default Files"
new_dir <- "D:/School Stuff/Math/Stats 141A/Data Files/Project/New Files"

###########
#1

label_vec = c("airplane", "automobile", "bird", "cat", "deer", "dog", "frog", "horse", "ship", "truck")

test_batch_function = function(path, test_batch)
{
  test_batch = readBin(con = path, what = "integer", n = 10000 * 3073, size = 1, signed = FALSE)
  test_batch = matrix(test_batch, nrow = 10000, byrow = TRUE)
  test_batch = data.frame(test_batch)
}

load_testing_images = function(input, output)
{
  files = list.files(input, pattern = "test_batch.bin*", full.names = TRUE)
  test_df = test_batch_function(path = files, test_batch)
  dmy_vec = c(rep(0, 10000))
  
  for(i in 1:10000)
  {
    for(j in 1:10)
    {
      if(test_df[i,1] == j - 1)
      {
        dmy_vec[i] = label_vec[j]
      }
    }
  }
  
  dmy_vec = matrix(dmy_vec, nrow = 10000)
  dmy_vec = data.frame(dmy_vec)
  final_test_df = cbind(dmy_vec, test_df)
  
  setwd(new_dir)
  export(final_test_df, "test_batch.rds")
}

load_training_images = function(input, output)
{
  data = list.files(input, pattern = "data_batch*", full.names = TRUE)
  data = data.frame(data)
  data[] = lapply(data, as.character)
  
  for (i in 1:length(data$data)) 
  {
    if(i == 1)
    {
      bin = readBin(con = file(data$data[i], "rb"), what = "integer", n = 3073*10000, size = 1, signed = FALSE)
      bin = matrix(bin, nrow = 10000, byrow = TRUE)
      bin = data.frame(bin)
    }
    else
    {
      bin2 = readBin(con = file(data$data[i], "rb"), what = "integer", n = 3073*10000, size = 1, signed = FALSE)
      bin2 = matrix(bin2, nrow = 10000, byrow = TRUE)
      bin2 = data.frame(bin2)
      bin = rbind(bin, bin2)
    }
  }
  
  bin$X1 = sapply(bin$X1, as.integer)
  dmy_vec = c(rep(0, 50000))
  
  for(i in 1:50000)
  {
    for(j in 1:10)
    {
      if(bin[i,1] == j - 1)
      {
        dmy_vec[i] = label_vec[j]
      }
    }
  }
  
  dmy_vec = matrix(dmy_vec, nrow = 50000)
  dmy_vec = data.frame(dmy_vec)
  final_test_df = cbind(dmy_vec, bin)
  
  setwd(new_dir)
  export(final_test_df, "training_batch.rds")
}

load_testing_images(testing_dir, new_dir)
load_training_images(testing_dir, new_dir)

test_RDS = readRDS("D:/School Stuff/Math/Stats 141A/Data Files/Project/New Files/test_batch.rds")
training_RDS = readRDS("D:/School Stuff/Math/Stats 141A/Data Files/Project/New Files/training_batch.rds")

data_rescale = function(labels, k = 500) sort(as.vector(sapply(unique(labels), function(i) which(labels == i))[1:k,]))
test_RDS = test_RDS[data_rescale(test_RDS[,1], k = 100),]
training_RDS = training_RDS[data_rescale(training_RDS[,1], k = 500),]

#########
#2

view_image = function(RDS, observation)
{
  RGB_only = RDS[observation, -c(1:2)]
  
  red = matrix(RGB_only[1:1024], nrow = 32, byrow = TRUE)
  green = matrix(RGB_only[1025:2048], nrow = 32, byrow = TRUE)
  blue = matrix(RGB_only[2049:3072], nrow = 32, byrow = TRUE)
  
  red = matrix(unlist(red))
  green = matrix(unlist(green))
  blue = matrix(unlist(blue))
  
  RGB_only = rgb(red, green, blue, maxColorValue = 255)
  RGB_only = matrix(RGB_only, nrow = 32, byrow = FALSE)
  print(RDS[observation, 1])
  return(grid.raster(RGB_only, interpolate = FALSE))
}

view_image(training_RDS, 13)

########
#3

random_image = function(df)
{
  index = sample(1:nrow(df), 1)
  view_image(df, index)
}

by_class <- split(test_RDS, test_RDS$dmy_vec)

# Put each image in its own chunk when we do our markdown file:
random_image(by_class[[1]])
random_image(by_class[[2]])
random_image(by_class[[3]])
random_image(by_class[[4]])
random_image(by_class[[5]])
random_image(by_class[[6]])
random_image(by_class[[7]])
random_image(by_class[[8]])
random_image(by_class[[9]])
random_image(by_class[[10]])

names(lapply(by_class, random_image))

most_use <- function(df) {
  index <- c()
  vars <- c()
  for(i in 3:3074) {
    index <- c(index, i-2)
    vars <- c(vars, var(df[,i]))
  }
  ind_var <- data.frame(index, vars)
  most_use <- ind_var %>%
    arrange(vars)
  least_use <- ind_var %>%
    arrange(desc(vars))
  ind_var <- cbind(most_use, least_use)
  names(ind_var) <- c("Best", "Low Var", "Worst", "High Var")
  return(head(ind_var, n = 10))
}

lapply(by_class, most_use)

########
#4

setwd(new_dir)

# Creating + exporting both distance matrices (only has to be done once):
dist_euc <- rbind(test_RDS[ , 3:3074], training_RDS[ , 3:3074])
dist_euc <- as.matrix(dist(dist_euc, method = "euclidean"))
export(dist_euc, "dist_euc.rds")

dist_man <- rbind(test_RDS[ , 3:3074], training_RDS[ , 3:3074])
dist_man <- as.matrix(dist(dist_man, method = "manhattan"))
export(dist_man, "dist_man.rds")

# Get distance matrices from here once they are made:
dist_euc = readRDS("D:/School Stuff/Math/Stats 141A/Data Files/Project/New Files/dist_euc.rds")
dist_man = readRDS("D:/School Stuff/Math/Stats 141A/Data Files/Project/New Files/dist_man.rds")

# Taking upper right rectangle of both distance matrices:
# (Used for predict_knn with test + training) cv = FALSE
dist_euc2 <- dist_euc[1:1000, 1001:6000]
dist_euc2 <- data.frame(dist_euc2)
colnames(dist_euc2) <- 1:5000

dist_man2 <- dist_man[1:1000, 1001:6000]
dist_man2 <- data.frame(dist_man2)
colnames(dist_man2) <- 1:5000

# Taking the lower right square of both distance matrices:
# (Used for CV with shuffled training) cv = TRUE
dist_euc4 <- dist_euc[1001:6000, 1001:6000]
dist_euc4 <- data.frame(dist_euc4)
colnames(dist_euc4) <- 1:5000
rownames(dist_euc4) <- 1:5000
dist_euc4 <- dist_euc4[sample(nrow(dist_euc4)), ]

dist_man4 <- dist_man[1001:6000, 1001:6000]
dist_man4 <- data.frame(dist_man4)
colnames(dist_man4) <- 1:5000
rownames(dist_man4) <- 1:5000
dist_man4 <- dist_man4[sample(nrow(dist_man4)), ]

# Taking upper left recetangle of both distance matrices
# (Used for CV with shuffled testing) cv = TRUE
dist_euc3 = dist_euc[1:1000, 1:1000]
dist_euc3 = data.frame(dist_euc3)
colnames(dist_euc3) <- 1:1000
rownames(dist_euc3) <- 1:1000
dist_euc3 = dist_euc3[sample(nrow(dist_euc3))]

dist_man3 = dist_man[1:1000, 1:1000]
dist_man3 = data.frame(dist_man3)
colnames(dist_man3) <- 1:1000
rownames(dist_man3) <- 1:1000
dist_man3 = dist_man3[sample(nrow(dist_man3))]

predict_knn = function(testing_points = 1:1000, training_points = training_RDS,
                       method, k = 7, cv = FALSE)
{
  if (cv == FALSE) 
  {
    if (method == "euclidean") 
    {
      dist_mat <- dist_euc2
    } 
    
    else if (method == "manhattan") 
    {
      dist_mat <- dist_man2
    }
    
    pred_vec = c()
    for(i in 1:length(testing_points)) 
    {
      indices <- as.integer(head(names(sort(dist_mat[testing_points[i], ])), n = k))
      votes <- training_RDS[indices, 1]
      pred_class <- which.max(table(votes))
      pred_class <- names(pred_class)
      pred_vec = c(pred_vec, pred_class)
    }
  } 
  
  else if (cv == TRUE) 
  {
    if (method == "euclidean") 
    {
      dist_mat <- dist_euc4
    } 
    
    else if (method == "manhattan") 
    {
      dist_mat <- dist_man4
    }
    
    pred_vec = c()
    for(i in 1:length(testing_points)) {
      sorted <- sort(dist_mat[testing_points[i], -testing_points])
      sorted <- sorted[, sorted != 0]
      indices <- as.integer(head(names(sorted), n = k))
      votes <- training_RDS[indices, 1]
      pred_class <- which.max(table(votes))
      pred_class <- names(pred_class)
      pred_vec = c(pred_vec, pred_class)
    }
  }
  return(pred_vec)
}

########
#5

cv_error_knn <- function(method, k) 
{
  if (method == "euclidean") 
  {
    dist_mat <- dist_euc4
  } 
  
  else if (method == "manhattan") 
  {
    dist_mat <- dist_man4
  }
  
  all_rates <- c()
  folds <- seq(1, 4501, 500)
  for (i in folds) 
  {
    testing_points <- i:(i+499)
    pred_vec <- as.character(predict_knn(testing_points = i:(i + 499), method = method,k = k, cv = TRUE))
    indices <- as.integer(rownames(dist_mat[testing_points, ]))
    correct_vec <- as.character(training_RDS[indices, 1])
    compare <- data.frame(pred_vec, correct_vec)
    errors <- 0
    
    for (i in 1:nrow(compare)) 
    {
      if (compare[i, 1] != compare[i, 2]) 
      {
        errors <- errors + 1
      }
    }
    
    error_rate <- errors/nrow(compare)
    all_rates <- c(all_rates, error_rate)
  }
  
  all_rates = mean(all_rates)
  return(all_rates)
}

euc_vector = c(rep(0, 15))
man_vector = c(rep(0, 15))

for(i in 1:15)
{
  euc_vector[i] = cv_error_knn(method = "euclidean", k = i)
}

for(i in 1:15)
{
  man_vector[i] = cv_error_knn(method = "manhattan", k = i)
}

df_euc = data.frame(k = 1:15, euc_vector, method = "Euclidean")
df_man = data.frame(k = 1:15, man_vector, method = "Manhattan")
df_error = cbind(df_euc, df_man)
export(df_error, "df_error.rds")
df_error = readRDS("D:/School Stuff/Math/Stats 141A/Data Files/Project/New Files/df_error.rds")
colnames(df_error) <- c("k", "euc_vector", "euc_method", "k", "man_vector", "man_method")

ggplot() +
  geom_line(aes(x = df_error$k, y = df_error$euc_vector, color = df_error$euc_method)) +
  geom_line(aes(x = df_error$k, y = df_error$man_vector, color = df_error$man_method)) +
  ggtitle("Error Rates of Metric and K Combinations") +
  labs(x = "k", y = "Error Rate") +
  scale_color_discrete(name = "Distance Metric")


#######
#7

cv_knn <- function(method, k) 
{
  if (method == "euclidean") 
  {
    dist_mat <- dist_euc4
  } 
  
  else if (method == "manhattan") 
  {
    dist_mat <- dist_man4
  }
  
  all_rates <- c()
  entire_pred_vec = c()
  entire_correct_vec = c()
  
  folds <- seq(1, 4501, 500)
  for (i in folds) 
  {
    testing_points <- i:(i+499)
    pred_vec <- as.character(predict_knn(testing_points = i:(i + 499), method = method, k = k, cv = TRUE))
    indices <- as.integer(rownames(dist_mat[testing_points, ]))
    correct_vec <- as.character(training_RDS[indices, 1])
    entire_pred_vec = c(entire_pred_vec, pred_vec)
    entire_correct_vec = c(entire_correct_vec, correct_vec)
  }
  return(confusionMatrix(factor(entire_pred_vec),factor(entire_correct_vec)))
}

euc_k13_conf = cv_knn(method = "euclidean", k = 13)
euc_k9_conf = cv_knn(method = "euclidean", k = 9)
euc_k7_conf = cv_knn(method = "euclidean", k = 7)

man_k12_conf = cv_knn(method = "manhattan", k = 12)
man_k11_conf = cv_knn(method = "manhattan", k = 11)
man_k13_conf = cv_knn(method = "manhattan", k = 13)

##########
#8

man_k12_conf = cv_knn(method = "manhattan", k = 12)

##########
#9

predict_knn2 = function(testing_points = 1:1000, training_points = test_RDS,
                        method, k, cv = FALSE)
{
  # browser()
  if (cv == FALSE) 
  {
    if (method == "euclidean") 
    {
      dist_mat <- dist_euc2
    } 
    
    else if (method == "manhattan") 
    {
      dist_mat <- dist_man2
    }
    
    pred_vec = c()
    for(i in 1:length(testing_points)) 
    {
      indices <- as.integer(head(names(sort(dist_mat[testing_points[i], ])), n = k))
      votes <- test_RDS[indices, 1]
      pred_class <- which.max(table(votes))
      pred_class <- names(pred_class)
      pred_vec = c(pred_vec, pred_class)
    }
  } 
  
  else if (cv == TRUE) 
  {
    if (method == "euclidean") 
    {
      dist_mat <- dist_euc3
    } 
    
    else if (method == "manhattan") 
    {
      dist_mat <- dist_man3
    }
    
    pred_vec = c()
    for(i in 1:length(testing_points)) {
      sorted <- sort(dist_mat[testing_points[i], -testing_points])
      sorted <- sorted[, sorted != 0]
      indices <- as.integer(head(names(sorted), n = k))
      votes <- test_RDS[indices, 1]
      pred_class <- which.max(table(votes))
      pred_class <- names(pred_class)
      pred_vec = c(pred_vec, pred_class)
    }
  }
  return(pred_vec)
}

cv_error_knn2 <- function(method, k) 
{
  # browser()
  if (method == "euclidean") 
  {
    dist_mat <- dist_euc3
  } 
  
  else if (method == "manhattan") 
  {
    dist_mat <- dist_man3
  }
  
  all_rates <- c()
  folds <- seq(1, 901, 100)
  for (i in folds) 
  {
    testing_points <- i:(i+99)
    pred_vec <- as.character(predict_knn2(testing_points = i:(i + 99), method = method, k = k, cv = TRUE))
    indices <- as.integer(rownames(dist_mat[testing_points, ]))
    correct_vec <- as.character(test_RDS[indices, 1])
    compare <- data.frame(pred_vec, correct_vec)
    errors <- 0
    
    for (i in 1:nrow(compare)) 
    {
      if (compare[i, 1] != compare[i, 2]) 
      {
        errors <- errors + 1
      }
    }
    
    error_rate <- errors/nrow(compare)
    all_rates <- c(all_rates, error_rate)
  }
  
  all_rates = mean(all_rates)
  return(all_rates)
}

euc_vector2 = c(rep(0, 15))
man_vector2 = c(rep(0, 15))

for(i in 1:15)
{
  euc_vector2[i] = cv_error_knn2(method = "euclidean", k = i)
}

for(i in 1:15)
{
  man_vector2[i] = cv_error_knn2(method = "manhattan", k = i)
}

df_euc2 = data.frame(k = 1:15, euc_vector2, method = "Euclidean")
df_man2 = data.frame(k = 1:15, man_vector2, method = "Manhattan")
df_error2 = cbind(df_euc2, df_man2)
export(df_error2, "df_error2.rds")
df_error2 = readRDS("D:/School Stuff/Math/Stats 141A/Data Files/Project/New Files/df_error2.rds")
colnames(df_error2) <- c("k", "euc_vector", "euc_method", "k", "man_vector", "man_method")

ggplot() +
  geom_line(aes(x = df_error2$k, y = df_error2$euc_vector, color = df_error2$euc_method)) +
  geom_line(aes(x = df_error2$k, y = df_error2$man_vector, color = df_error2$man_method)) +
  ggtitle("Error Rates of Metric and K Combinations") +
  labs(x = "k", y = "Error Rate") +
  scale_color_discrete(name = "Distance Metric")


########
# Sources:

# 1) How to shuffle rows: https://stackoverflow.com/questions/6422273/how-to-randomize-or-permute-a-dataframe-rowwise-and-columnwise


