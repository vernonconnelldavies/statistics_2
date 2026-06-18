library(rvest)#for web scraping
library(tidyverse)#(includes stringr for cleaning up strings)
library(tidytext)
library(widyr)
library(patchwork)#for pairwise analysis


#we will use first terms speeches of Obama, Biden and Clinton (in that order)
#I have chosen presidents of the democratic party for my analysis
#Barack Obama's speech was made in 2009
#Joe Biden's speech was made in 2021
#Bill Clinton's speech was made in 1993
#Each president faced different challenges of their era and so their speeches reflect that,
#obviously each president also has their own style too

scrape1<-read_html('https://www.presidency.ucsb.edu/documents/inaugural-address-5')#here we scrape the html speech using techniques taught in class
head(scrape1)

#finding the text in the list using <p>
scrape1_nodes<-scrape1 %>%
  html_nodes("p")#here we find the text using techniques taught in class
#check the data
head(scrape1_nodes)
#check how many paragraphs
length(scrape1_nodes)

#creating a tibble of the text
scrape1_text<-tibble(source='obama',text=scrape1 %>% #this is the first speech, Obama's first term inauguration speech
                       html_nodes("p") %>% #straight from Paula's lectures
                       html_text() )

head(scrape1_text)
tail(scrape1_text,20)

n1<-nrow(scrape1_text)
scrape1_text<-scrape1_text[3:(n1-6),] #remove the narration notes at the end of the speech, remove notes at the beginning too
head(scrape1_text)
tail(scrape1_text,20)

obama_speech<- scrape1_text %>%
  unnest_tokens(word,text)

head(obama_speech)
t1<-dim(obama_speech)[1]
head(t1)

scrape2<-read_html('https://www.presidency.ucsb.edu/documents/inaugural-address-53')


scrape2_text<-tibble(source='biden',text=scrape2 %>% #straight from Paula's lectures
                       html_nodes("p") %>%
                       html_text() )

head(scrape2_text)
tail(scrape2_text,20)

n2<-nrow(scrape2_text)
scrape2_text<-scrape2_text[3:(n2-6),]#cleaning up the speech so the only the speech text is included
head(scrape2_text)
tail(scrape2_text,20)

biden_speech<- scrape2_text %>%
  unnest_tokens(word,text)

head(biden_speech)
t2<-dim(biden_speech)[1]
head(t2)

scrape3<-read_html('https://www.presidency.ucsb.edu/documents/inaugural-address-51')
scrape3_text<-tibble(source='clinton',text=scrape3 %>% #straight from Paula's lectures
                     html_nodes("p") %>%
                     html_text() )
head(scrape3_text)
tail(scrape3_text,20)

n3<-nrow(scrape3_text)
scrape3_text<-scrape3_text[3:(n3-6),]#remove some of the notes at the beginning and the notes at the end too (copyright, Twitter tag etc)
head(scrape3_text)
tail(scrape3_text,20)

clinton_speech<- scrape3_text %>%
  unnest_tokens(word,text)

head(clinton_speech)
t3<-dim(clinton_speech)[1]
head(t3)#this counts the number of words in the speech




president_corpus<-rbind(scrape1_text,scrape2_text,scrape3_text)
head(president_corpus)
tail(president_corpus)

#first look at the speech sizes

speech_length<-tibble(President=c("Obama","Biden","Clinton"),word_count=c(t1,t2,t3)) #this compares the speech length for Obama, Biden and Clinton 
head(speech_length)
ggplot(speech_length,aes(x=President,y=word_count))+geom_col()+ggtitle('speech_length')#so we can see Biden has given the speech with the most words, then Obama, Clinton's speech has the least words



#unnest the corpus first

tidy_president<-president_corpus %>%
  unnest_tokens(word,text) #the basic 'unnest_tokens' to turn words into tokens
  

head(tidy_president) #look at the corpus

#remove the stop words

data("stop_words")
tidy_president_without_stop<-tidy_president %>%
  anti_join(stop_words,by="word")#this code is directly from Paula's lectures

#now we will look at the words occurring most frequently in each of the presidents speeches

top_five_words<-tidy_president_without_stop %>% 
  group_by(source) %>%
  count(source,word,sort=TRUE) %>%
  mutate(frequency=n) %>%
  subset(select=-c(n)) #https://stackoverflow.com/questions/4605206/drop-data-frame-columns-by-name
#this is probably a bit clumsy but I wanted to rename the 'n' column to 'frequency' for better clarity for the reader

obama_top_five<-filter(top_five_words,source=="obama")
obama_plot<-ggplot(obama_top_five[1:20,],aes(reorder(word,frequency),frequency))+geom_col(fill='lightblue')+coord_flip()+ggtitle('Obama_top_words')#here I used 'reorder' which appeared in class but I also found online on threads as to how to get the bar chart to order in ascending
obama_plot

biden_top_five<-filter(top_five_words,source=="biden")#this code is a repeat of the Obama ggplot code, but edited slightly for Biden
biden_plot<-ggplot(biden_top_five[1:20,],aes(reorder(word,frequency),frequency))+geom_col(fill='red')+coord_flip()+ggtitle('Biden_top_words')#https://stackoverflow.com/questions/16961921/plot-data-in-descending-order-as-appears-in-data-frame
biden_plot

clinton_top_five<-filter(top_five_words,source=="clinton")#this code is a repeat of the Obama ggplot code, but edited slightly for Clinton
clinton_plot<-ggplot(clinton_top_five[1:20,],aes(reorder(word,frequency),frequency))+geom_col(fill='deeppink1')+coord_flip()+ggtitle('Clinton_top_words')
clinton_plot

#all three presidents draw on patriotism in their speeches, 'America' and 'Americans' being in the top most frequently used words for all three Democrat (party) Presidents
#all three presidents refer to the word 'world' with a lot of frequency, the USA is an exceptionally rich and developed country by global metric, this is possibly a boost about that fact,
#and also a lot of Americans see the USA as a sort of world leader
#both Biden and Clinton both use the word 'democracy' with frequency, most Westerners (and Americans) feel the ability to choose their leader makes their governments more morally righteous than other government types





#now we will look at sentiment analysis
# count the positives/negatives

bing_word_counts<-tidy_president %>%
  inner_join(get_sentiments("bing"),by="word") %>% #this joins the sentiment lists with the text
  count(source,word,sentiment,sort=TRUE) %>% #this looks at the words occurring most frequently from the sentiment lists
  ungroup() 

bing_word_counts %>% #this code was copied from week_6 script, it shows the positive and negative word counts across all 3 speeches combined
  group_by(sentiment) %>%
  slice_max(n,n=10) %>%
  ungroup() %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(n,word,fill=sentiment))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~sentiment,scales="free_y")+
  labs(x="Contribution to Sentiment",y=NULL)+
  scale_fill_brewer(palette = "Dark2")

#we can clearly see 'work' is the most used positive word in the whole corpus, although we can't tell whether this is a reference to employment or 'work' in the verb sense ('we can see the medicine works')
#'unity' and 'peace' are also popular words across the corpus, the USA is a large country and the leaders obviously worrying about its unity and the breacvh of the peace.

head(bing_word_counts,20)

#now lets see which speeches were the most 'optimistic,' where we define 'optimistic' as positive words minus negative words for each of the speeches


president_sentiment_counts<-bing_word_counts %>%
  group_by(source) %>%
  count(positive_or_not=sentiment=='positive') #this code was written by myself, it 
  
president_sentiment_counts<-president_sentiment_counts %>%
  mutate(multiplier = ifelse(positive_or_not == 'FALSE', -1, 1)) %>% #this clumsy piece of code was written by myself in order to prepare for my optimistic factor (number of positive words minus number of negative words)
  mutate(optimistic_factor=multiplier*n) %>%
  summarise(total_optimistic_factor=sum(optimistic_factor))#here I used the 'summarise' function to sum the positive and negative words and create a new dataset

ggplot(president_sentiment_counts,aes(source,total_optimistic_factor))+geom_col(fill='cyan')#here I plotted the optimistic factor of each of the presidents
  
#so we can see Clinton had the most optimistic speech followed by Obama followed by Biden, this is also the chronological order in which they were in office, this would suggest inauguration speeches are becoming less optimistic as times goes on.
  
head(president_sentiment_counts)

#now we will look at where the positive and negative words are placed in each of the speeches in terms of line numbers

tidy_president_with_index<-tidy_president %>% #this piece of code is edited from week_6_sentiment script done in class but edited to exclude the 'chapter' code part
  group_by(source) %>%
  mutate(
    linenumber=row_number(),#here the chapter part was removed as there are no chapters in the President's speeches
  ) 


tidy_president_with_index<-tidy_president_with_index %>% #this piece of code is from the textbook page18,'book' was swapped for 'source' as you would expect.
  inner_join(get_sentiments("bing")) %>%
  count(source,index=linenumber %/% 10,sentiment) %>%
  spread(sentiment,n,fill=0) %>%
  mutate(sentiment=positive-negative)

ggplot(tidy_president_with_index,aes(index,sentiment,fill=source))+geom_col()+facet_wrap(~source)#here we use the facet wrap to dislay all 3 presidents sentiment on 1 page
  
#Clinton's speech is obviously much shorter than the others, Obama seems to have lesser extremities than Biden. Obviously Biden got elected just after the COvid 19 lockdown ended so it was just after a major crisis
#Biden's sentiment is very negative at one point (near the beginning) but also is very positive near the end, the extremities are more pronounced
#all 3 presidents use negative and positive sentiment to effect, to get to where they are (leader of Democratic party) of course requires great oratory skill and charisma




#I hope you enjoyed reading and running my analysis of the Obama,Biden and Clinton's inaugural first term presidential speeches
















