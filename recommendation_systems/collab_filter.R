# x<-d
# recommend.for<-"YW"
# type="user"
# distance="correlation"
# num.neighbors=1
# num.recommendations=1
# user.col="id"
# minkowski.r=NULL
# adjusted.ratings=F


collab_filter<-function(x, recommend.for, type="user", distance="correlation", num.neighbors=1, num.recommendations=1, user.col="id", minkowski.r=NULL, adjusted.ratings=F){
  # x should be a data frame or a named matrix. Users should be in each row, and whatever you are recommending in each column
  # There should be one column with the user IDs (identified by the user.col parameter)
  
  # recommend.for should be a character string, and must be included in the ID column of x
  
  # acceptable values for type and distance are below
  
  # num.neighbors and num.recomendations can be any positive integer. If the number is larger than the length of the data frame
  # R will add NA rows to any relevant data frame. The code below will remove any excess NA values.
  
  # minkowski.r is the power parameter for the minkowski distance function. If using the manhattan distance or eublidean distance
  # parameter, you do not need to provide a minkowski.r parameter. The function will not allow a value of infinity, so to compute 
  # chebyshev distance (maximum distance) put in an arbitrarily and obscenely large number, like 100 trillion. 
  
  # Check parameters
  if(type!="user"&type!="item") stop("Error: type must be user or item.")
  if(type=="item"&distance!="cosine") warning("Cosine distance is recommended for item-based recommendations.") # Page 3-16 to 3-19
  if(type=="item"&adjusted.ratings==F) warning("adjusted.ratings is recommended for item-based recommendations.") # Page 3-16 to 3-19
  if(distance=="cosine") require(proxy)
  
  #rename/store parameters
  x.raw<-x
  r.for<-recommend.for
  
  #Remove id column
  x<-x[,!(names(x) %in% c(user.col))]
  rownames(x)<-x.raw[,c(user.col)]
  
  #transpose data for user-based filtering, fix numeric column names (if id is strictly numeric)
  if(type=="user"){
    x<-t(x)
    x.col.names<-x.raw[,c(user.col)]
    if(is.numeric(x.col.names)){
      x.col.names<-paste("user",x.col.names,sep="")
      r.for<-paste("user",r.for,sep="")
    }
    colnames(x)<-x.col.names
  } else {
    x.temp<-x
  }
  
  #Center ratings
  if(adjusted.ratings==T) x<-x-rowMeans(x,na.rm=T)
  
  #Compute similarity matrix
  if(distance=="correlation"){
    dist.mat<-cor(x, use="pairwise.complete.obs")
    if(type=="user") dist.mat<- dist.mat + 1 # make correlations range from 0-2 instead of -1 to 1, for purposes of weighted.mean() function
  } else if (distance=="manhattan" | distance=="euclidean" | distance=="cosine" | distance=="minkowski"){
    dist.mat<-as.matrix(dist(x=t(x),method=distance,p=minkowski.r))
    if(distance=="manhattan" | distance=="euclidean" | distance=="minkowski") {
      dist.mat<- ( -dist.mat) + max(dist.mat, na.rm=T) # reverse code (make minimum=0; if you want minimum=previous minimum, add +min(dist.mat, na.rm=T))
    } else {
      # distance = cosine; matrix contains 1-cosine, turn back to cosine
      dist.mat<- 1 - dist.mat
      
    }
  } else {
    stop("Error: distance function not found.")
  }
  
  # Convert back to data frame
  dist.mat<-as.data.frame(dist.mat)
  
  if(type=="user"){
    # Sort distances
    nearest.neighbors<-dist.mat[order(-dist.mat[,r.for]),r.for,drop=F]
    # Remove self from neighbors
    nearest.neighbors<-nearest.neighbors[-which(rownames(nearest.neighbors) %in% colnames(nearest.neighbors)), ,drop=F]
    # Get k nearest neighbors, if there are NAs in the neighbors, remove them
    nearest.neighbors<-nearest.neighbors[1:num.neighbors, , drop=F][complete.cases(nearest.neighbors[1:num.neighbors, , drop=F]), , drop=F]
    
    # Calculate weights
    weights<-data.frame(weight=nearest.neighbors/sum(nearest.neighbors))
    
    # Check for negative weights; weighted.mean() rescales weights to sum to 1. If a weight is negative, 
    # all weights will still sum to 1, which causes the rating of the recommended movie  to not be on a
    # 1-7 scale. Instead the sum of the absolute value of the weights should sum to 1. Rather than
    # programming the weighting system by hand, I forced all distances to be positive earlier.
    if(sum( weights < 0)) warning("You have negative neighbor weights. You may want to consider if weighted.mean() implements how you think negative weights should have handled.")
    
    # Get ratings, convert to column
    ratings<-x[,colnames(x) %in% rownames(nearest.neighbors)]
    ratings<-t(ratings)
    ratings<-ratings[match(rownames(weights),rownames(ratings)),]
    
    # Compute expected ratings, sort
    recommendations<-c()
    for (i in colnames(ratings)){
      recommendations[i]<-weighted.mean(ratings[,i,drop=F], t(weights), na.rm=T)
    }    
    recommendations<-as.data.frame(recommendations)
    colnames(recommendations)<-c("rating")
    
    # Remove already seen movies, sort, grab top movies
    recommendations<-recommendations[is.na(x.raw[x.raw[,user.col]==r.for,-1]),,drop=F] # find NAs in raw data (remove the id column)
    recommendations<-recommendations[order(-recommendations$rating),,drop=F][1:num.recommendations,, drop=F]
  } else if(type=="item"){
    #Get normalized ratings, convert to data frame in column form
    rec.ratings<-x.temp[as.character(r.for),]
    rec.ratings<-normalize(rec.ratings, 1, 7) # See pages 3-24 to 3-25, and helper functions at the bottom of the script
    rec.ratings<-data.frame(t(rec.ratings))
    colnames(rec.ratings)<-c("rating")
    
    #Remove movies that have ratings (i.e. get possible recommended movies)
    recommendations<-rec.ratings[is.na(rec.ratings),,drop=F] 
    
    for(i in rownames(recommendations)){
      #Compute ratings
      rating<-dist.mat[i,]*rec.ratings$rating
      sum.exp.rating<-rowSums(rating,na.rm=T);
      sum.rating<-rowSums(abs(dist.mat[i,]),na.rm=T)
      recommendations[i,"rating"]<-sum.exp.rating/sum.rating
    }
    
    #Format recommendations for export (sort, cut down the list, put ratings back in 1-7 scale)
    recommendations<-recommendations[order(-recommendations$rating), ,drop=F]
    recommendations<-recommendations[1:num.recommendations, , drop=F]
    recommendations<-denormalize(recommendations,1,7) # See pages 3-24 to 3-25, and helper functions at the bottom of the script   
  }
  recommendations[complete.cases(recommendations), , drop=F] # return the recommendations, without NAs if there are any
}

normalize<-function(x,min.val=NULL,max.val=NULL){
  if(is.null(min.val)&is.null(max.val)){
    normalized<-(2*(x-min(x,na.rm=T))-(max(x,na.rm=T)-min(x,na.rm=T)))/(max(x,na.rm=T)-min(x,na.rm=T))
  } else if(!is.null(min.val)&!is.null(max.val)){
    normalized<-(2*(x-min.val)-(max.val-min.val))/(max.val-min.val)
  } else {
    stop("Error: Either both or neither min.val and max.val should be set.")
  }
  normalized
}

denormalize<-function(x,min.val,max.val){
  denormalized<- .5*((x+1)*(max.val-min.val))+min.val
  denormalized
}
