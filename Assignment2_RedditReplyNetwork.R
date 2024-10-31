library(RedditExtractoR)

reddit_threads <- find_thread_urls(keywords = "election", sort_by="top",  period="month")
View(reddit_threads)

analytics_reddit_posts <- get_thread_content(reddit_threads$url[1:10]) 

analytics_reddit_threads=as.data.frame(analytics_reddit_posts$threads) 
View(analytics_reddit_threads)

analytics_reddit_comments=as.data.frame(analytics_reddit_posts$comments) 
analytics_reddit_comments$Post=rownames(analytics_reddit_comments) 
View(analytics_reddit_comments)


analytics_reddit_allposts <- merge(analytics_reddit_comments, analytics_reddit_threads, by = 'url')
View(analytics_reddit_allposts)
colnames(analytics_reddit_allposts)[2]
colnames(analytics_reddit_allposts)[12]
View(analytics_reddit_allposts)

analytics_reddit_allposts=subset(analytics_reddit_allposts, analytics_reddit_allposts$Comment_Author!="AutoModerator")

############################################################################################################

library(stringi)

RedditReplyNetwork=data.frame()
UnderscorePosition=data.frame(stri_locate_last(analytics_reddit_allposts$comment_id, fixed = '_'))

# The for loop below generates a data frame with each row representing two users with a reply relationship
# That is, the second user replied to a post by the first user
for (i in 1:nrow(UnderscorePosition)) {  # Loop through the UnderscorePosition vector
  if (is.na(UnderscorePosition[i,]$start)) { # If underscore is not found (i.e., NA)
    PostAuthor=analytics_reddit_allposts[i,]$Thread_Author
    ReplyAuthor=analytics_reddit_allposts[i,]$Comment_Author
    RedditReplyNetwork=rbind(RedditReplyNetwork, c(ReplyAuthor, PostAuthor)) # Add one row with thread author as post author and comment author as reply author
  }
  else {
    
    # Extract the Comment ID of the replied to post before the last underscore
    ReplyToCommentID=substr(analytics_reddit_allposts[i,]$comment_id, 1, UnderscorePosition[i,]$start-1)
    # Extract the URL of the thread
    ThreadURL=analytics_reddit_allposts[i,]$url
    
    # Find the row where the replied to post is located
    ReplyToRow=which((analytics_reddit_allposts$comment_id == ReplyToCommentID) & (analytics_reddit_allposts$url==ThreadURL))
    
    # Extract the authors of the post and its reply
    PostAuthor=analytics_reddit_allposts[ReplyToRow,]$Comment_Author
    ReplyAuthor=analytics_reddit_allposts[i,]$Comment_Author
    
    # Add the pair of authors to data frame of ties/edges for network
    RedditReplyNetwork=rbind(RedditReplyNetwork, c(ReplyAuthor, PostAuthor))
  }
}

colnames(RedditReplyNetwork)=c('ReplyAuthor', 'PostAuthor')

RedditReplyMatrix=as.matrix(RedditReplyNetwork[RedditReplyNetwork$ReplyAuthor != '[deleted]' & RedditReplyNetwork$PostAuthor != '[deleted]',]) 
View(RedditReplyMatrix)
 
######################################################################################################################
#PLOTTING THE REDDIT REPLY NETWORK

library(igraph)

reply_graph = graph.edgelist(RedditReplyMatrix, directed = TRUE)
reply_graph = simplify(reply_graph)
ver_labs = get.vertex.attribute(reply_graph, "name", index=V(reply_graph))
head(ver_labs, 10)
glay = layout.fruchterman.reingold(reply_graph)
par(bg="white", mar=c(1,1,1,1))  

# Plot the graph
edge_list <- as.data.frame(get.edgelist(reply_graph))
colnames(edge_list) <- c("from", "to")

# Count the frequency of each edge (interaction)
edge_weights <- as.data.frame(table(edge_list))
edge_weights <- edge_weights[order(-edge_weights$Freq),] # Order by frequency (descending)

# Select the top N most frequent interactions
top_edges <- head(edge_weights, 50) # Change 50 to desired number of interactions

# Create a subgraph with only the top interactions
top_reply_graph <- graph_from_data_frame(top_edges[, c("from", "to")], directed = TRUE)

# Plot the top interactions graph
plot(top_reply_graph, layout=layout.fruchterman.reingold,
     vertex.color='white', vertex.size=3, vertex.label.cex=0.5, 
     edge.arrow.size=0.3, edge.width=2, edge.color='skyblue')

title("Reddit Reply Network of Election Threads: Top interaction - of 50 frequent interactions",
      cex.main=1, col.main="black") 

RNMetrics=data.frame(cbind(degree(reply_graph, mode=("in"))))   # in-degree
RNMetrics=cbind(RNMetrics, data.frame(cbind(degree(reply_graph, mode=("out")))))   # out-degree
RNMetrics=cbind(RNMetrics, data.frame(cbind(betweenness(reply_graph))))  # betweenness
RNMetrics=cbind(RNMetrics, data.frame(cbind(closeness(reply_graph))))    # closeness
RNMetrics=cbind(RNMetrics, data.frame(cbind(evcent(reply_graph)$vector)))  # eigenvector
colnames(RNMetrics)=c('In-Degree', 'Out-Degree','Betweenness', 'Closeness', 'Eigenvector')  # add column headings
write.csv(RNMetrics, 'ElectionRedditMetrics.csv')

# Identify communities
RN_community <- cluster_walktrap(top_reply_graph)

# Assign colors to each community
community_colors <- rainbow(length(unique(membership(RN_community))))
V(top_reply_graph)$color <- community_colors[membership(RN_community)]

# Plot the graph with communities colored
plot(top_reply_graph, 
     layout=layout_with_fr, # Alternative layout for better spread
     vertex.color=V(top_reply_graph)$color, 
     vertex.size=5, 
     vertex.label=NA, # Remove labels for clarity
     edge.arrow.size=0.2, 
     edge.curved=TRUE,
     main="\nReddit Reply Network of Election Threads: Top Interactions Communities",
     cex.main=1, col.main="black")
