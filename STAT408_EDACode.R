FBData <- read.table("/home/tim/Documents/R/STAT408/Project with Chelsea/dataset_Facebook.csv", sep=";", header=TRUE)

    # Preliminary Data Visualizations
    
    par(mfrow=c(3,2))
    hist(FBData$Page.total.likes, main="Histogram of Page Total Likes")
    hist(FBData$Post.Month, breaks=c(0:12), main="Histogram of Month")
    hist(FBData$Lifetime.Engaged.Users, main="Histogram of Lifetime Engaged Users")
    hist(FBData$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post, 
         main="Histogram of Lifetime People who have Liked your Page \n and Engaged with Your Post")
    hist(FBData$like, main = "Histogram of Likes")
    counts <- c(sum(FBData$Type == "Photo"), sum(FBData$Type == "Status"), sum(FBData$Type == "Link"))
    names(counts) = c("Photo", "Status", "Link")
    barplot(counts, main="Type of Post")
    
    # export IMG as .png, width=900, length=1000
    
    # Scatterplots of Some Correlations
    
    par(mfrow=c(2,2))
    
    # Scatterplot (Lifetime Engaged Users)
    
    plot(FBData$Page.total.likes, FBData$Lifetime.Engaged.Users, cex.axis=0.8, pch = 16, cex = 0.9, col = "blue", main = "Page Total Likes versus Lifetime Engaged Users", xlab = "Page Total Likes", ylab = "Lifetime Engaged Users")
    abline(lm(FBData$Lifetime.Engaged.Users ~ FBData$Page.total.likes), col=rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
    
    # Scatterplot (Lifetime People who have Liked Your Page and Engaged with Your Post)
    
    plot(FBData$Page.total.likes, FBData$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post, cex.axis=0.8, pch = 16, cex = 0.9, col = "blue", main = "Page Total Likes versus Lifetime People who have Liked \n Your Page and Engaged with Your Post", xlab = "Page Total Likes", ylab = "LPWHLYPAEWYP")
    abline(lm(FBData$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post ~ FBData$Page.total.likes), col=rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
    
    # Scatterplot (Month)
    
    plot(FBData$Page.total.likes, FBData$Post.Month, cex.axis=0.8, pch = 16, cex = 0.9, col = "blue", main = "Page Total Likes versus Month", xlab = "Page Total Likes", ylab = "Month")
    abline(lm(FBData$Post.Month ~ FBData$Page.total.likes), col=rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
    

    # Scatterplot (Like)
    
    plot(FBData$Page.total.likes, FBData$like, cex.axis=0.8, pch = 16, cex = 0.9, col = "blue", main = "Page Total Likes versus Like", xlab = "Page Total Likes", ylab = "Like")
    abline(lm(FBData$like ~ FBData$Page.total.likes), col=rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
    
    # Export as .png, dimensions width=950, length=650
    
    # Generate a variable Delta Likes 
    # new column N from 500 to 1
    FBData$N <- seq(500, 1, -1)
    # reorder so new column N is first 
    FBData <- FBData[c(20,1:19)]
    # sort by N
    order.N <- order(FBData$N)
    FBData <- FBData[order.N,]
    # find difference between total_likes column
    total_likes <- c(FBData[,2])
    df <- diff(total_likes, lag=1)
    FBData$delta.likes <- 0
    FBData$delta.likes[2:500] <- df
    # reorder so delta_likes is 3rd column
    FBData <- FBData[c(1:2, 21, 3:20)]
    
    par(mfrow=c(1,1))
    # boxplot of total page likes
    boxplot(total_likes, horizontal=TRUE, main = "Distribution of Total Page Likes", xlab = "Total Page Likes")
   
    # boxplot of total page likes
    boxplot(total_likes, horizontal=TRUE, main = "Distribution of Total Page Likes", xlab = "Total Page Likes")
    # range of total page likes
    summary(FBData$Page.total.likes)
    # number of missing data
    sum(is.na(FBData))
    # number of each type of post
    sum(FBData$Type=="Photo")
    sum(FBData$Type=="Link")
    sum(FBData$Type=="Status")
    # plot total page likes over N
    # plots of a few distributions
    par(mfrow=c(2,2))
    plot(FBData$Page.total.likes~FBData$N, main = "Total Page Likes", xlab="N", ylab="Total Page Likes")
    plot(FBData$Total.Interactions~FBData$Post.Hour, main = "Total Interactions Vs. Post Hour")
    plot(FBData$Lifetime.Post.reach.by.people.who.like.your.Page~FBData$Post.Hour, main = "Post Consumptions Vs. Post Hour")
    hist(FBData$Post.Hour)
    # summary of data
    summary(FBData)
    
    
    #################################################
    # Scatterplots of Some Correlations (DELTA LIKES)
    
    par(mfrow=c(2,2))
    
    # Scatterplot (Lifetime Engaged Users)(Delta Likes)
    
    plot(FBData$delta.likes, FBData$Lifetime.Engaged.Users, cex.axis=0.8, pch = 16, cex = 0.9, col = "blue", main = "ΔLikes versus Lifetime Engaged Users", xlab = "ΔLikes", ylab = "Lifetime Engaged Users")
    abline(lm(FBData$Lifetime.Engaged.Users ~ FBData$delta.likes), col=rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
    
    # Scatterplot (Lifetime People who have Liked Your Page and Engaged with Your Post)(Delta Likes)
    
    plot(FBData$delta.likes, FBData$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post, cex.axis=0.8, pch = 16, cex = 0.9, col = "blue", main = "ΔLikes versus Lifetime People who have Liked \n Your Page and Engaged with Your Post", xlab = "ΔLikes", ylab = "LPWHLYPAEWYP")
    abline(lm(FBData$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post ~ FBData$delta.likes), col=rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
    
    # Scatterplot (Month)(Delta Likes)
    
    plot(FBData$delta.likes, FBData$Post.Month, cex.axis=0.8, pch = 16, cex = 0.9, col = "blue", main = "ΔLikes versus Month", xlab = "ΔLikes", ylab = "Month")
    abline(lm(FBData$Post.Month ~ FBData$delta.likes), col=rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
    
    
    # Scatterplot (Like)(Delta Likes)
    
    plot(FBData$delta.likes, FBData$like, cex.axis=0.8, pch = 16, cex = 0.9, col = "blue", main = "ΔLikes versus Like", xlab = "ΔLikes", ylab = "Like")
    abline(lm(FBData$like ~ FBData$delta.likes), col=rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
    