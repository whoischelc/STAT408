data <- read.csv("~/2019-2020/Fall 2019/STAT408/Project/Data/dataset_Facebook.csv", sep=";", header=TRUE)
n <- nrow(data)
# new column N from 500 to 1
data$N <- seq(500, 1, -1)
# reorder so new column N is first 
data <- data[c(20,1:19)]
# sort by N
order.N <- order(data$N)
data <- data[order.N,]
# find difference between total_likes column
total_likes <- c(data[,2])
df <- diff(total_likes, lag=1)
data$delta.likes <- 0
data$delta.likes[2:500] <- df
# reorder so delta_likes is 3rd column
data <- data[c(1:2, 21, 3:20)]
# boxplot of total page likes
boxplot(total_likes, horizontal=TRUE, main = "Distribution of Total Page Likes", xlab = "Total Page Likes")
# range of total page likes
range(data$Page.total.likes)
mean(data$Page.total.likes)
# number of missing data
sum(is.na(data))
# number of each type of post
sum(data$Type=="Photo")
sum(data$Type=="Link")
sum(data$Type=="Status")
# plot total page likes over N
plot(data$Page.total.likes~data$N, main = "Total Page Likes", xlab="N", ylab="Total Page Likes")

plot(data$Lifetime.Post.Total.Reach)

colnames(data)
