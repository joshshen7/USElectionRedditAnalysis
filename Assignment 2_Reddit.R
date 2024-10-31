install.packages("RedditExtractoR")
library(RedditExtractoR)
library(tm)
library(wordcloud)
library(SnowballC)

reddit_threads <- find_thread_urls(keywords = "election", sort_by = "top", period = "month")
View(reddit_threads)

reddit_posts <- get_thread_content(reddit_threads$url[1:10])
reddit_comments <- as.data.frame(reddit_posts$comments)
reddit_comments$post = rownames(reddit_comments)
View(reddit_comments)
reddit_comments = subset(reddit_comments, reddit_comments$comment != "[deleted]" & reddit_comments$comment != "[removed]")
reddit_comments = subset(reddit_comments, reddit_comments$author != "[deleted]" & reddit_comments$author != "AutoModerator")

Reddit <- data.frame(reddit_comments$post, reddit_comments$comment)
colnames(Reddit) = c("Post", "Comment")
View(Reddit)
Reddit <- head(Reddit, 1000)

for (i in 1:10){
  Reddit = rbind(Reddit, c(as.numeric(Reddit$Post[nrow(Reddit)])+1,
                           paste(reddit_threads$title[i],
                                 '',
                                 reddit_threads$text[i])))
}
View(Reddit)

######################################################################################################################
#Create Word Cloud
text_data <- paste(Reddit$Comment, collapse = " ")

# Create text corpus, DTM, terms to start creating word cloud
reddit_corp <- Corpus(VectorSource(text_data))
reddit_DTM <- DocumentTermMatrix(reddit_corp, control = list(removePunctuation = TRUE, removeNumbers = TRUE, stopwords = TRUE))
reddit_terms <- colSums(as.matrix(reddit_DTM))

reddit_wordcloud <- wordcloud(words = names(reddit_terms),
                              freq = reddit_terms,
                              min.freq = 7,
                              max.words = 100,
                              random.order = FALSE,
                              rot.per = 0.35,
                              scale = c(4, 0.5),
                              colors = brewer.pal(8, "Dark2")
)

#####################################################################################################################
#Named Entity Extraction
install.packages('rJava')
install.packages('NLP')
install.packages('openNLP')
install.packages('openNLPmodels.en', repos='http://datacube.wu.ac.at/', type='source')
install.packages("C:/Users/coach/Downloads/openNLPmodels.en_1.5-1.tar.gz", repos = NULL, type = "source")
install.packages("stringi")

library(stringi)
library(NLP)
library(openNLP)
library(openNLPmodels.en)

person_annotator = Maxent_Entity_Annotator(kind = 'person')
organization_annotator = Maxent_Entity_Annotator(kind = 'organization')
location_annotator = Maxent_Entity_Annotator(kind = 'location')
date_annotator = Maxent_Entity_Annotator(kind = 'date')
money_annotator = Maxent_Entity_Annotator(kind = 'money')
percentage_annotator = Maxent_Entity_Annotator(kind = 'percentage')

RedditEntities=data.frame(Post=numeric(), Type=character(), Entity=character(), Position=numeric(), stringsAsFactors=FALSE)

for (post in 1:nrow(Reddit))  
{
  RedditText=as.String(Reddit[post,2]) 
  RedditText=stri_trim_both(RedditText) 
  if (RedditText=="") {}   
  else  {
    RedditTokens=annotate(RedditText, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator())) # set up annotator
    RedditPersTokens=annotate(RedditText, list(person_annotator), RedditTokens) 
    RedditOrgsTokens=annotate(RedditText, list(organization_annotator), RedditTokens) 
    RedditLocsTokens=annotate(RedditText, list(location_annotator), RedditTokens) 
    RedditDatsTokens=annotate(RedditText, list(date_annotator), RedditTokens)  
    RedditMonsTokens=annotate(RedditText, list(money_annotator), RedditTokens) 
    RedditPctsTokens=annotate(RedditText, list(percentage_annotator), RedditTokens) 
    
    RedditPerson=subset(RedditPersTokens,RedditPersTokens$features=='list(kind = "person")') 
    RedditOrganization=subset(RedditOrgsTokens,RedditOrgsTokens$features=='list(kind = "organization")') 
    RedditLocation=subset(RedditLocsTokens,RedditLocsTokens$features=='list(kind = "location")') 
    RedditDate=subset(RedditDatsTokens,RedditDatsTokens$features=='list(kind = "date")') 
    RedditMoney=subset(RedditMonsTokens,RedditMonsTokens$features=='list(kind = "money")') 
    RedditPercentage=subset(RedditPctsTokens,RedditPctsTokens$features=='list(kind = "percentage")') 
    
    # Add extracted persons to dataframe containing extracted entities
    for (i in 1:nrow(as.data.frame(RedditPerson))) 
    {
      if (nrow(as.data.frame(RedditPerson))>0) {
        RedditEntities=rbind(RedditEntities, cbind(post, 'Person', substr(paste(RedditText, collapse=' '),
                                                                          RedditPerson$start[i],RedditPerson$end[i]),RedditPerson$start[i])) # add post ID, 'Person', name of person extracted, and start position in text into dataframe
      }
    }
    
    # Add extracted organizations to dataframe containing extracted entities
    for (i in 1:nrow(as.data.frame(RedditOrganization)))
    {
      if (nrow(as.data.frame(RedditOrganization))>0) {
        RedditEntities=rbind(RedditEntities, cbind(post, 'Organization', substr(paste(RedditText, collapse=' '),
                                                                                RedditOrganization$start[i],RedditOrganization$end[i]),RedditOrganization$start[i]))
      }
    }
    
    # Add extracted locations to dataframe containing extracted entities
    for (i in 1:nrow(as.data.frame(RedditLocation)))
    {
      if (nrow(as.data.frame(RedditLocation))>0) {
        RedditEntities=rbind(RedditEntities, cbind(post, 'Location', substr(paste(RedditText, collapse=' '),
                                                                            RedditLocation$start[i],RedditLocation$end[i]),RedditLocation$start[i]))
      }
    }
    
    # Add extracted dates to dataframe containing extracted entities
    for (i in 1:nrow(as.data.frame(RedditDate)))
    {
      if (nrow(as.data.frame(RedditDate))>0) {
        RedditEntities=rbind(RedditEntities, cbind(post, 'Date', substr(paste(RedditText, collapse=' '),
                                                                        RedditDate$start[i],RedditDate$end[i]),RedditDate$start[i]))
      }
    }
    
    # Add extracted monies to dataframe containing extracted entities
    for (i in 1:nrow(as.data.frame(RedditMoney)))
    {
      if (nrow(as.data.frame(RedditMoney))>0) {
        RedditEntities=rbind(RedditEntities, cbind(post, 'Money', substr(paste(RedditText, collapse=' '),
                                                                         RedditMoney$start[i],RedditMoney$end[i]),RedditMoney$start[i]))
      }
    }
    
    # Add extracted percentages to dataframe containing extracted entities
    for (i in 1:nrow(as.data.frame(RedditPercentage)))
    {
      if (nrow(as.data.frame(RedditPercentage))>0) {
        RedditEntities=rbind(RedditEntities, cbind(post, 'Percentage', substr(paste(RedditText, collapse=' '),
                                                                              RedditPercentage$start[i],RedditPercentage$end[i]),RedditPercentage$start[i]))
      }
    }
  }
}

#rename columns
colnames(RedditEntities)=c('Post', 'Type', 'Entity', 'Position')

View(RedditEntities)

#merge entity tags with posts
RedditExtratedEntities=merge(RedditEntities, Reddit, by.x='Post', by.y='Post')

View(RedditExtratedEntities)
