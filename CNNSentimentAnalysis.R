# Load required libraries
library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(stringr)
library(SentimentAnalysis)
library(syuzhet)
library(lubridate)
library(dplyr)
library(tidyr)

# Getting the path of your current open file
current_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
print(getwd())

# Specify the name of your CSV file
csv_file <- "dataset.csv"

# Read the CSV file into a data frame
news_article_data_frame <- read.csv(csv_file)

# Create a word cloud for each category
categories <- unique(news_article_data_frame$Category)

# Additional custom stopwords
custom_stopwords <- c('those', 'on', 'own', 'yourselves', 'ie', 'around', 'between', 'four', 'been', 'alone', 'off', 'am', 'then', 'other', 'can', 
                      'cry', 'hereafter', 'front', 'too', 'wherein', 'everything', 'up', 'onto', 'never', 'either', 'how', 'before', 'anyway', 'since', 
                      'through', 'amount', 'now', 'he', 'cant', 'was', 'con', 'have', 'into', 'because', 'inc', 'not', 'therefore', 'they', 'even', 'whom', 
                      'it', 'see', 'somewhere', 'interest', 'thereupon', 'nothing', 'thick', 'whereas', 'much', 'whenever', 'find', 'seem', 'until', 'whereby', 
                      'at', 'ltd', 'fire', 'also', 'some', 'last', 'than', 'get', 'already', 'our', 'once', 'will', 'noone', 'that', 'what', 'thus', 'no', 
                      'myself', 'out', 'next', 'whatever', 'although', 'though', 'etc', 'which', 'would', 'therein', 'nor', 'somehow', 'whereupon', 'besides', 
                      'whoever', 'thin', 'ourselves', 'few', 'third', 'without', 'anything', 'twelve', 'against', 'while', 'twenty', 'if', 'however', 'found', 
                      'herself', 'when', 'may', 'ours', 'six', 'done', 'seems', 'else', 'call', 'perhaps', 'had', 'nevertheless', 'fill', 'where', 'otherwise', 
                      'still', 'within', 'its', 'for', 'together', 'elsewhere', 'throughout', 'of', 'eg', 'others', 'show', 'sincere', 'anywhere', 'anyhow', 'as', 
                      'are', 'the', 'hence', 'something', 'hereby', 'nowhere', 'de', 'latterly', 'neither', 'his', 'go', 'forty', 'put', 'their', 'by', 'namely', 
                      'could', 'five', 'itself', 'is', 'nine', 'whereafter', 'down', 'bottom', 'thereby', 'such', 'both', 'she', 'become', 'whole', 'who', 'yourself', 
                      'every', 'thru', 'except', 'very', 'several', 'among', 'being', 'be', 'mine', 'further', 'here', 'during', 'why', 'with', 'becomes', 'about', 'a', 
                      'co', 'seeming', 'due', 'wherever', 'beforehand', 'detail', 'fifty', 'becoming', 'might', 'amongst', 'my', 'empty', 'thence', 'thereafter', 'almost', 
                      'least', 'someone', 'often', 'from', 'keep', 'him', 'or', 'top', 'her', 'nobody', 'sometime', 'across', 'hundred', 'only', 'via', 'name', 'eight', 
                      'three', 'back', 'to', 'all', 'became', 'move', 'me', 'we', 'formerly', 'so', 'i', 'whence', 'describe', 'under', 'always', 'himself', 'in', 
                      'herein', 'more', 'after', 'themselves', 'you', 'above', 'sixty', 'them', 'hasnt', 'your', 'made', 'indeed', 'most', 'everywhere', 'fifteen', 
                      'but', 'must', 'along', 'beside', 'hers', 'side', 'former', 'anyone', 'full', 'has', 'yours', 'whose', 'behind', 'please', 'amoungst', 'mill',
                      'ten', 'seemed', 'sometimes', 'should', 'over', 'take', 'each', 'same', 'rather', 'latter', 'and', 'hereupon', 'part', 'per', 'eleven', 'ever',
                      'enough', 'again', 'us', 'yet', 'moreover', 'mostly', 'one', 'meanwhile', 'whither', 'there', 'toward', 'give', 'system', 'do', 'an', 'these', 'everyone', 
                      'towards', 'this', 'bill', 'cannot', 'un', 'afterwards', 'beyond', 'were', 'whether', 'well', 'another', 'below', 'first', 'upon', 'any', 'none', 'many', 
                      'serious', 're', 'two', 'couldnt', 'less', "get","news","new","cnn","says", "said", "the", "and", "is", "it", "in", "to", "for", "that", "with", "on", 
                      "as", "was", "by", "at", "an", "be", "this", "which", "are", "from", "or", "have", "not", "but", "has", "we", "you", "your", "its", "can", 
                      "all", "if", "they", "their", "will", "there", "about", "more", "when", "one", "up", "so", "out", "into", "up", "some", "no", "how", "do", "my", 
                      "am", "me", "his", "her", "him", "our", "we", "were", "been", "than", "just", "like", "who", "what", "where", "when", "why", "which", "also", 
                      "been", "would", "could", "should", "did", "does", "doing", "done", "two", "one", "many", "years", "year", "told", "according", "caption", "day", 
                      "time", "people", "photo", "photos", 'didnt', 'hide', 'dont')

# Create a single PDF with multiple word clouds
pdf_file_path <- "Word_Clouds.pdf"
pdf(pdf_file_path, width = 14, height = 8)

for (category in categories) {
  # Preprocess the text data
  corpus <- Corpus(VectorSource(news_article_data_frame$Article.text[
    news_article_data_frame$Category == category]))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, content_transformer(function(x) str_replace_all(x, "—", " ")))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), custom_stopwords))

  # Create a document-term matrix
  dtm <- DocumentTermMatrix(corpus)

  # Convert the document-term matrix to a matrix
  m <- as.matrix(dtm)

  # Get word frequencies
  word_freqs <- colSums(m)

  # Create a word cloud for each category
  wordcloud(words = names(word_freqs), freq = word_freqs, scale=c(3, 0.5),
            colors=brewer.pal(8, "Dark2"), max.words=150, random.order=FALSE)

  # Add title to the plot
  title(paste("Word Cloud for Category:", category))
}

dev.off()

# Print a message indicating that the plots have been saved
cat("\nPlots have been saved to", pdf_file_path, "\n")

# Create a new PDF for bar plots
pdf_file_path <- "Top_10_Words_Plots.pdf"
pdf(pdf_file_path, width = 14, height = 8)

# Create a bar plot for each category
for (category in categories) {
  # Preprocess the text data
  corpus <- Corpus(VectorSource(news_article_data_frame$Article.text[
    news_article_data_frame$Category == category]))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, content_transformer(function(x) str_replace_all(x, "—", " ")))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), custom_stopwords))

  # Create a document-term matrix
  dtm <- DocumentTermMatrix(corpus)

  # Convert the document-term matrix to a matrix
  m <- as.matrix(dtm)

  # Get word frequencies
  word_freqs <- colSums(m)

  # Get the top 10 words
  top_words <- head(sort(word_freqs, decreasing = TRUE), 10)

  # Create a bar plot
  bar_plot_data <- data.frame(Word = names(top_words), Frequency = top_words)

  # Reorder the factor levels of "word" variable
  bar_plot_data$Word <- reorder(bar_plot_data$Word, -bar_plot_data$Frequency)

  plot <- ggplot(bar_plot_data, aes(x = Word, y = Frequency, fill = Word)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    ggtitle(paste("Top 10 Words for Category:", category))

  # Save the plot to the PDF
  print(plot)
}

dev.off()

# Print a message indicating that the plots have been saved
cat("\nPlots have been saved to", pdf_file_path, "\n")

# Create a new PDF for sentiment analysis plots
pdf_file_path <- "Sentiment_Plots.pdf"
pdf(pdf_file_path, width = 14, height = 8)

for (category in categories) {
  # Extract text for the current category
  category_text <- news_article_data_frame$Article.text[news_article_data_frame$Category == category]

  # Perform sentiment analysis
  sentiment_scores <- get_sentiment(category_text, method = "bing")

  # Categorize sentiment into positive, neutral, and negative
  sentiment_categories <- factor(ifelse(sentiment_scores > 0, "positive", 
                                        ifelse(sentiment_scores == 0, "neutral", "negative")), 
                                        levels = c("positive", "neutral", "negative"))

  # Ensure that all levels are present, even if some are not present in the data
  sentiment_categories <- factor(sentiment_categories, levels = c("positive", "neutral", "negative"))

  # Create a bar plot for sentiment analysis
  sentiment_data <- data.frame(Category = category, Sentiment = sentiment_categories)

  # Set colors for positive, neutral, and negative
  colors <- c("green", "blue", "red")

  plot <- ggplot(sentiment_data, aes(x = Sentiment, fill = Sentiment)) +
    geom_bar(stat = "count") +
    scale_fill_manual(values = colors, drop = FALSE) +  # Ensure all levels are displayed
    theme_minimal() +
    ggtitle(paste("Sentiment Analysis for Category:", category))

  # Save the plot to the PDF
  print(plot)
}

dev.off()

# Print a message indicating that the plots have been saved
cat("\nPlots have been saved to", pdf_file_path, "\n")

# Create a new PDF for sentiment bar plots
pdf_file_path <- "Emotion_Plots.pdf"
pdf(pdf_file_path, width = 14, height = 8)

# Create a bar plot for each category
for (category in unique(news_article_data_frame$Category)) {
  cat_text <- news_article_data_frame$Article.text[news_article_data_frame$Category == category]

  # Perform emotion analysis using the syuzhet library
  emotion_scores <- colSums(get_nrc_sentiment(cat_text))

  # Exclude "positive" and "negative" emotions
  emotion_scores <- emotion_scores[!(names(emotion_scores) %in% c("positive", "negative"))]

  # Create a data frame for plotting
  emotion_data <- data.frame(Emotion = names(emotion_scores), Frequency = emotion_scores)

  # Reorder the levels of the "Emotion" factor based on Frequency
  emotion_data$Emotion <- factor(emotion_data$Emotion, 
                                 levels = rev(names(sort(emotion_scores, decreasing = TRUE))))

  # Create a horizontal bar plot
  plot <- ggplot(emotion_data, aes(x = Frequency, y = Emotion, fill = Emotion)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    ggtitle(paste("Emotion Analysis for Category:", category)) +
    theme(axis.text.y = element_text(hjust = 1))

  # Save the plot to the PDF
  print(plot)
}

dev.off()

# Print a message indicating that the plots have been saved
cat("\nPlots have been saved to", pdf_file_path, "\n")
                                                                                                                                    
extract_authors <- function(author_string) {
  
  # Filter and separate non-author words from the list of author names.
  split_authors <- unlist(str_split(author_string, ",| and |, |and |, |Words by|video by, |By |, | by |
                                    , | By |, | in |, |as told to |, | for, | for |, | France, | with |, | CNN|, |CNN's "))
  
  # Split records that contain multiple authors into a new record
  authors <- trimws(gsub(",.*", "", split_authors))
  authors <- trimws(gsub("&.*", "", split_authors))
  
  # Words to remove
  words_to_remove <- c("Story", "video", "Business", "Analysis", "Opinion", "Review", 
                       "Exclusive", "Beijing", "Paris", "Atlanta", "Photos", "Design", 
                       "Editor", "Graphics", "Produced", "Stories", "translation", "account", 
                       "first", "Senior", "photographs", "photography", "meteorologist", 
                       "meteorologists", "Illustration", "Illustrations", " Equals", ";")
  
  # Filter out records containing specified words
  authors <- authors[!grepl(paste(words_to_remove, collapse = "|"), authors, ignore.case = TRUE)]
  
  # Filter out records empty authors
  authors <- authors[authors != ""]
  authors <- authors[!(grepl("CNN", authors) & !grepl("staff", authors, ignore.case = TRUE))]
  
  return(authors)
}

# Convert 'Date.published' to a Date type with format
news_article_data_frame$Date.published <- as.Date(news_article_data_frame$Date.published, format = "%Y-%m-%d %H:%M:%S")

# Extract year from 'Date.published'
news_article_data_frame$Year <- year(news_article_data_frame$Date.published)

# Change Author data to character type
news_article_data_frame$Author <- as.character(news_article_data_frame$Author)

# Apply the extract_author function to the Author column and unnest
author_list <- news_article_data_frame %>% 
  rowwise() %>%
  mutate(Author = list(extract_authors(Author))) %>% 
  unnest(Author)

# Count the occurrences of each category in the news article data frame
category_counts <- table(news_article_data_frame$Category)

# Create a data frame to store category counts
category_df <- data.frame(Category = names(category_counts), Count = as.numeric(category_counts))

# Create a pie chart for article counts based on categories
plot1 <- ggplot(category_df, aes(x = "", y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Article Counts Based on Categories", x = NULL, y = NULL) +
  theme_minimal() +
  coord_polar(theta = "y") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()) 

# Count the occurrences of each category for each year in the news article data frame
year_category_counts <- table(news_article_data_frame$Year, news_article_data_frame$Category)

# Create a data frame to store year and category counts
year_category_df <- as.data.frame(table(news_article_data_frame$Year, news_article_data_frame$Category))

# Create a stacked bar plot for article count by year and category
plot2 <- ggplot(year_category_df, aes(x = as.factor(Var1), y = Freq, fill = Var2)) +
  geom_bar(stat = "identity") +
  labs(title = "Stacked Bar Graph of Article Count by Year and Category",
       x = "Year Published", y = "Number of Articles",
       fill = "Category") +
  theme_minimal() +
  theme(legend.position = "right")

#Retrieve top 10 Author names and counts of articles written
author_counts <- table(author_list$Author)
author_df <- data.frame(Author = names(author_counts), Count = as.numeric(author_counts))
top_authors <- author_df %>%
  slice_max(order_by = Count, n = 10)

# Retrieve Author names, category and article count
top_authors_category <- as.data.frame(table(author_list$Author, author_list$Category))
names(top_authors_category) <- c("Author", "Category", "Count")

# Get top 10 Authors name, category and counts from authors only found in the top_authors list
merged_data <- inner_join(top_authors, top_authors_category, by = "Author")
merged_data <- select(merged_data, -Count.x)

# Create a Horizontal Stacked bar plot for top 10 Authors by the Number of Articles written and their categories 
plot3 <- ggplot(merged_data, aes(x = as.factor(Author), y = Count.y, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Article Counts Based on Top 10 Authors",
       x = "Authors", y = "Number of Articles",
       fill = "Category") +
  theme_minimal() +
  theme(legend.position = "right") +
  coord_flip()

# Save all plots to a PDF file
pdf_file_path <- "CNN_Plots.pdf"
pdf(pdf_file_path, width = 14, height = 8)
print(plot1)
print(plot2)
print(plot3)
dev.off()

# Print a message indicating that the plots have been saved
cat("\nPlots have been saved to", pdf_file_path, "\n")
