library(dplyr)
v <- iris
nr <- nrow(v)
nc <- ncol(v)

prob <- .1 ## desired total proportion of NA's

num_na_already_present <- is.na(unlist(v[,2]))
num_na_already_present

na_to_be_added <- floor(prob*nr) - sum(num_na_already_present)  ## number of new NA's
na_to_be_added

num_na_already_present[sample(which(!is.na(num_na_already_present)), na_to_be_added)] <- TRUE  #Sample returns random row numbers which are not NA and makes the num_na_already_present vector true
num_na_already_present

final_arr_with_nas=num_na_already_present

v1 <- matrix(final_arr_with_nas, nr=nr,nc=1L)

# initializing "sepal width"(2) column to NA at random places
v[v1,2]<- NA ## using matrix indexing
v$spec_val <- 0
v <- within(v,spec_val <- factor(Species,labels = c("1","2","3")))
v

#to find out which cols contain NA...Not required!
col_names_containing_na <- colnames(v)[apply(v, 2, anyNA)]
col_names_containing_na

v_drop <- v%>%na.omit()
v_drop
dim(v_drop)

v_test <- v[is.na(v$Sepal.Width),]
v_test
dim(v_test)

# Q1: REPLACING WITH MEAN 
mean_of_non_NA <- apply(v[,(1:4)],
                         2,  #Here 2 means the columns.(1 would have meant the row)
                         mean, #This is the function to be applied!
                         na.rm =  TRUE) #This means the vector can have na values also!
mean_of_non_NA

v_replace <- v %>%
  mutate(Sepal_Width_New = ifelse(is.na(Sepal.Width), mean_of_non_NA[2], Sepal.Width))
v_replace

v_replace[,2] <- v_replace[,7]
v_replace <- v_replace[,1:6]
v_replace

err <- iris[,2]-v_replace[,2]
err <- err*err
class(err)
sum(err)

plot(x = iris$Sepal.Width,y=v_replace$Sepal.Width,
     xlab = "Original Sepal Width",
     ylab = "Sepal Width with mean",
     main = "MEAN")


#REPLACING WITH MEDIAN
median_of_non_NA <- apply(v[,(1:4)],
                         2,
                         median,
                         na.rm =  TRUE)
median_of_non_NA

v_replace1 <- v %>%
  mutate(Sepal_Width_New = ifelse(is.na(Sepal.Width), median_of_non_NA[2], Sepal.Width))
v_replace1

v_replace1[,2] <- v_replace1[,7]
v_replace1 <- v_replace1[,1:6]
v_replace1
err1 <- iris[,2]-v_replace1[,2]
err1 <- err1*err1
class(err1)
sum(err1)

plot(x = iris$Sepal.Width,y=v_replace1$Sepal.Width,
     xlab = "Original Sepal Width",
     ylab = "Sepal Width with median",
     main = "MEDIAN")

#PREDICTING USING LINEAR REGRESSION MODEL
library(stats)
model <- lm(Sepal.Width~Sepal.Length,data = v_drop)
summary(model)
prediction <- predict(model,newdata = v_test)
prediction

v_test[,2] = prediction
v_test

v_new  <- rbind(v_drop,v_test)
v_new

v_pred <- v_new[order(as.numeric(rownames(v_new))),,drop = FALSE]
v_pred

err2 <- iris[,2]-v_pred[,2]
err2 <- err2*err2
sum(err2)
plot(x = iris$Sepal.Width,y=v_pred$Sepal.Width,
     xlab = "Original Sepal Width",
     ylab = "Sepal Width with regression",
     main = "REGRESSION")
