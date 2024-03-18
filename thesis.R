#tesis Paso 1: crear el scraper para descargar las estenográficas
library(rvest)
library(dplyr)
library(stringr)
# Function to scrape individual pages
scrape_page <- function(url) {
  page <- read_html(url)
  content <- page %>% html_nodes(".entry-content p") %>% html_text()
  return(content)
}

# Main scraping script
main_url <- "https://lopezobrador.org.mx/secciones/version-estenografica/"
main_page <- read_html(main_url)

# Extract titles and URLs
titles_xpath <- "/html/body/div[3]/div[2]/div/div[1]/div[1]/article/div[2]/h2/a"
titles <-main_page %>% html_nodes(xpath = titles_xpath) %>% html_text()
urls_xpath <- "/html/body/div[3]/div[2]/div/div[1]/div[1]/article/div[2]/h2/a"
urls <- main_page %>% html_nodes(xpath = urls_xpath) %>% html_attr("href")
print(titles)
print(urls)
# Scrape content from each page
contents <- lapply(urls, scrape_page)
#then use sapply or lapply. 

# Combine data into a dataframe
data <- data.frame(title = titles, urls = urls, content = contents_combined, stringsAsFactors = FALSE)
print(data$content[1])
#---------------------------------------------------------------------------------------------------
#First try with the loop

scrape_page <- function(url) {
  page <- read_html(url)
  content <- page %>% html_nodes(".entry-content p") %>% html_text()
  return(content)
}

# Initialize vectors to store data
all_titles <- c()
all_urls <- c()
all_contents <- c()

# Start from the main page and iterate through subsequent pages
for(i in 140:200) {
  # Construct the URL for each page
  if (i == 1) {
    page_url <- "https://lopezobrador.org.mx/secciones/version-estenografica/"
  } else {
    page_url <- sprintf("https://lopezobrador.org.mx/secciones/version-estenografica/page/%d/", i)
  }
  
  # Read the page
  page <- read_html(page_url)
  
  # Extract titles and URLs
  titles_xpath <- "/html/body/div[3]/div[2]/div/div[1]/div[1]/article/div[2]/h2/a"
  titles <- page %>% html_nodes(xpath = titles_xpath) %>% html_text()
  urls_xpath <- "/html/body/div[3]/div[2]/div/div[1]/div[1]/article/div[2]/h2/a"
  urls <- page %>% html_nodes(xpath = urls_xpath) %>% html_attr("href")
  
  # Scrape content from each page
  contents <- lapply(urls, scrape_page)
  contents_combined <- sapply(contents, function(x) paste(x, collapse = " "))
  
  # Combine the results
  all_titles <- c(all_titles, titles)
  all_urls <- c(all_urls, urls)
  all_contents <- c(all_contents, contents_combined)
  
  # Pause for a few seconds before the next iteration
  Sys.sleep(0.5) # Pause for 1 seconds
}

# Combine data into a dataframe
data3 <- data.frame(title = all_titles, urls = all_urls, content = all_contents, stringsAsFactors = FALSE)
#subset
subs_data3 <- data3[135:184,]
#create new df with merged data for the big data2
new_data2_complete <- data.frame()
new_data2_complete <- rbind(data2, subs_data3)
new_no_duplicates <- new_data2_complete[!duplicated(new_data2_complete[,c("urls")]),]

#now remove the X variable in data (first 450 observations)

data <- data[, -which(names(data) == "X")]

#complete rbind for the entire dataframe

complete_data <- rbind(data,new_no_duplicates)

#remove duplicates
complete_data_no_duplicates <- complete_data[!duplicated(complete_data[,c("urls")]),]

write.csv(data, file="first_450.csv")
data <- read.csv("first_450.csv")
write.csv(data2, file = "full_dataset.csv")
write.csv(data3, file ="last_missing_speeches.csv")
#this is the full csv file without duplicates based on urls
write.csv(complete_data_no_duplicates, file = "full_speeches_mx.csv")

full_data <- read.csv("full_speeches_mx.csv")

full_data <- full_data |> 
  rename(speech_id = X)

#create date variable 
full_data <- full_data |> 
  mutate(date = str_extract(urls, "\\d{4}/\\d{2}/\\d{2}"))
#convert dates so R can understand them 

full_data$date <- as.Date(full_data$date, format = "%Y/%m/%d")
write.csv(full_data, file = "full_speeches_mx.csv")
full_data <- read.csv("full_speeches_mx.csv")

#--------------------------------------------------------------------------------------------------
#Para empezar a hacer topicmodels
library(topicmodels)
library(quanteda)
library(stopwords)
library(spacyr)
library(text)
library(dplyr)
library(stringr)
library(reshape2)
library(LDAvis)
library(ggplot2)
library(tm)
library(lsa)
library(knitr)
library(gridExtra)
library(GGally)
library(grid)
library(tidyverse)
library(tidyr)
library(tidytext)
library(udpipe)

#lemmatize corpus
ud_model <- udpipe_download_model(language = "spanish", model_dir = ".")
ud_model <- udpipe_load_model(file = ud_model$file_model)


# Perform lemmatization
annotated_text <- udpipe_annotate(ud_model, x = full_data$content)
annotated_df <- as.data.frame(annotated_text)

write.csv(annotated_df, file = "annotated_df.csv")
saveRDS(annotated_text, "annotated_text.rds")
annotated_df <- read.csv2("annotated_df.csv")
annotated_text <- readRDS("annotated_text.rds")
full_data$lemmatized_content <- annotated_df$lemma

# Combine lemmatized words back into documents
full_data$lemmatized_content <- with(annotated_df, ave(lemma, doc_id, FUN = paste, collapse = " "))

# Define your custom stopwords. Revisar dos veces si conviene remover "méxico" y "pesos mensuales" para el bigrama. 

custom_stopwords <- c("presidente", "andrés", "manuel", "lópez", "obrador","+","inicia video","finaliza video", "intervención inaudible","méxico","permiso señor","pesos mensuales","pesos","interlocutor","interlocutora","ciento","pregunta","señor","caso","si","entonces","gracias","aquí","va","vamos","ahora","bueno","usted","si","van","centavos","ver","ahí","cómo","pues","ser","así","baja","año","hoy","días")
all_stopwords <- c(stopwords("es"), custom_stopwords)
number_patterns <- c(
  "\\b\\d+\\b",  # Digits
  "\\b(?:cero|uno|dos|tres|cuatro|cinco|seis|siete|ocho|nueve|diez|once|doce|trece|catorce|quince|dieciséis|diecisiete|dieciocho|diecinueve|veinte|treinta|cuarenta|cincuenta|sesenta|setenta|ochenta|noventa|cien|cientos|mil|miles|millón|millones)\\b"  # Written numbers in Spanish
)
full_data$content <- tolower(full_data$content)
pattern_to_remove <- "^\\d{4}: Año de [^.]*\\.?"
full_data$content <- gsub(pattern_to_remove, "", full_data$content)

#tokenize 
tokens_data <- full_data$content %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(pattern = all_stopwords, case_insensitive = TRUE) %>%
  tokens_remove(pattern = number_patterns, valuetype = "regex", case_insensitive = TRUE) %>%
  tokens_ngrams(n = 1:2)


dfm_data <- tokens_data |> 
  dfm() |> 
  dfm_trim(min_termfreq = 15)

#print(dfm_data)

num_topics <- 10  # for example, specifying 10 topics
set.seed(123)
lda_model <- LDA(dfm_data, k = num_topics, method = "Gibbs", control=list(alpha=1))
lda_model2 <- LDA(dfm_data, k = num_topics, method = "Gibbs", control = list(alpha = 5)) 
lda_lemm <- readRDS("lda_lemm.RDS")
#Here alpha =5 because it's a guideline suggested by Griffiths and Steyvers (2004) suggest a value of 50/k for α and 0.1 for δ

#save the model 
saveRDS(lda_model, file = "lda.rds")
lda_model <- readRDS("lda.rds")
# Explore the results
# Get the terms per topic
top_terms <- terms(lda_model, 10)  # Top 10 terms for each topic. Topics are group of words that are the most important to those topics. 
top_terms_df <-  as.data.frame(top_terms)

kable_out <-  kable(top_terms_df, format = "html") 
html_out <- capture.output(kable_out)

p <- grid.table(top_terms_df)

# Saving the table as a JPG file
jpeg("table.jpg", width = 800, height = 600)
grid.draw(p)
dev.off()
#-----------------------------------------------------------------------------------------------------
#FOR LDA LEMM MODEL 
top_terms_lemm <- terms(lda_lemm, 10)  # Top 10 terms for each topic. Topics are group of words that are the most important to those topics. 
top_terms_df_lemm <-  as.data.frame(top_terms_lemm)

kable_out_l <-  kable(top_terms_df_lemm, format = "html") 
html_out_l <- capture.output(kable_out_l)

p_l <- grid.table(top_terms_df_lemm)

# Saving the table as a JPG file
jpeg("table_lemm.jpg", width = 800, height = 600)
grid.draw(p_l)
dev.off()

# Get the topic distribution across documents
topics(lda_model)

#plot the distribution of topics using tidytext 

ap_topics <- tidy(lda_model, matrix = "beta")

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

plot_terms <- ap_top_terms %>%
  mutate(topic = paste("Topic",topic),
    term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_x_continuous(limits = c(0, 0.015))+
  scale_y_reordered()

plot_terms

ggsave("facet_plot.jpeg", plot = plot_terms, width = 12, height = 8, dpi = 300)



#now to create a table and visualize them easily


lda_vis_data <- LDAvis::createJSON(
  phi = posterior(lda_model)$terms,
  theta = posterior(lda_model)$topics,
  doc.length = rowSums(as.matrix(dfm_data)),
  vocab = colnames(as.matrix(dfm_data)),
  term.frequency = colSums(as.matrix(dfm_data))
)
LDAvis::serVis(lda_vis_data, open.browser = TRUE)

#Visualization using ggplot 

top_terms <- terms(lda_model, 10)
term_topic_probs <- posterior(lda_model)$terms
term_topic_freqs <- lda_model@terms
term_topic_matrix <- lda_model@phi
dim(term_topic_matrix)
plot_data <- data.frame(Topic = integer(), Term = character(), Frequency = numeric())
model_terms <- colnames(term_topic_freqs)


# Create a data frame for plotting
for (topic in seq_len(ncol(top_terms))) {
  top_terms_in_topic <- top_terms[, topic]
  
  # Find indices of these terms in the model's term list
  term_indices <- match(top_terms_in_topic, model_terms)
  
  # Extract frequencies using these indices
  frequencies <- term_topic_freqs[term_indices, topic]
  
  # Append to the data frame
  plot_data <- rbind(plot_data, data.frame(Topic = topic, 
                                           Term = top_terms_in_topic, 
                                           Frequency = frequencies))
}

# Plot using ggplot2
ggplot(plot_data, aes(x = reorder(Term, Beta), y = Beta, fill = as.factor(Topic))) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Topic, scales = "free") +
  coord_flip() +
  labs(x = "Terms", y = "Beta", title = "Top Terms in Each Topic") +
  theme_minimal()

#But we can also look at topics and assign them to documents, not only wordclouds. Topics per documents

topic_probabilities <- posterior(lda_model)$topics

# Transforming the data into a matrix format suitable for a heatmap
topic_matrix <- as.matrix(topic_probabilities)

library(pheatmap)
library(plotly)
heatmap(topic_matrix, Rowv = NA, Colv = NA, col = heat.colors(256), scale="column", margins=c(5,10))

pheatmap(topic_matrix, 
         cluster_rows = FALSE, 
         cluster_cols = FALSE, 
         show_rownames = TRUE, 
         show_colnames = TRUE, 
         color = colorRampPalette(c("navyblue", "white", "firebrick3"))(100))


p_ly<- plot_ly(x = colnames(topic_matrix), y = rownames(topic_matrix), z = topic_matrix, 
        type = "heatmap", colorscale = 'Viridis') %>%
  layout(yaxis = list(autorange = "reversed"))

plotly::export(p_ly, file = "heatmap_plotly.png")

#---------------------------------------------------------------------------------------------------
#heatmap with the lemm model 
topic_probabilities_l <- posterior(lda_lemm)$topics
topic_matrix_l <- as.matrix(topic_probabilities_l)

library(pheatmap)
library(plotly)
heatmap(topic_matrix_l, Rowv = NA, Colv = NA, col = heat.colors(256), scale="column", margins=c(5,10))

pheatmap(topic_matrix_l, 
         cluster_rows = FALSE, 
         cluster_cols = FALSE, 
         show_rownames = TRUE, 
         show_colnames = TRUE, 
         color = colorRampPalette(c("navyblue", "white", "firebrick3"))(100))


p_ly<- plot_ly(x = colnames(topic_matrix_l), y = rownames(topic_matrix_l), z = topic_matrix_l, 
               type = "heatmap", colorscale = 'Viridis') %>%
  layout(yaxis = list(autorange = "reversed"))

plotly::export(p_ly, file = "heatmap_plotly_lemm.png")

##new code to plot. BAr plot does not work cuz we have too many speeches (documents)

doc_topic_distr <- posterior(lda_model)$topics
doc_topic_df <- as.data.frame(doc_topic_distr)
colnames(doc_topic_df) <- paste0("Topic", 1:ncol(doc_topic_df))
doc_topic_df$DocumentID <- rownames(doc_topic_df)

terms_per_topic <- terms(lda_model, 10)

library(tidyr)
dt_long <- pivot_longer(doc_topic_df, 
                        cols = -DocumentID, 
                        names_to = "Topic", 
                        values_to = "Probability")

ggplot(dt_long, aes(x = DocumentID, y = Probability, fill = Topic)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Document ID", y = "Topic Proportion", fill = "Topic")


#------------------------------------------------------------------------------------------------- run LDA with spacy
# Initialize spacyr with the Spanish model. Install and use the virtual environment. 
python_exec <- "C:/Users/luisf/Documents/.virtualenvs/r-spacyr/Scripts/python.exe"
system2(python_exec, c('-m', 'pip', 'install', '-U', 'spacy'))
system2(python_exec, c('-m', 'spacy', 'download', 'es_core_news_sm'))
spacy_initialize(model = "es_core_news_sm")

#I'll use a subsample of my data 
set.seed(123)
sample_data <- full_data[sample(nrow(full_data), 100),]

#Lemmatization and tokenization
parsed <- spacy_parse(sample_data$content, lemma = TRUE, pos = TRUE,parser = FALSE, ner = FALSE)

#maybe mantener lemmatized content
# Lemmatize the text content
lemmatized_text <- text::lemmatize_strings(full_data$content, language = "es")
full_data$content <- lemmatized_text

#-------------------------------------------------------------------------- 
#first clean data
text_vector <- tolower(full_data$content)
corpus <- Corpus(VectorSource(text_vector))
# Preprocess the corpus (you can customize this based on your needs)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
corpus <- tm_map(corpus, stripWhitespace)

TDM <- DocumentTermMatrix(corpus)

# Remove sparse terms (optional, adjust the sparsity threshold as needed)
TDM <- removeSparseTerms(TDM, sparse = 0.95)

# View the TDM
inspect(TDM)

dim(TDM)

termcount <-apply(TDM,1,sum)

library(Matrix)
library(tidyr)
non_empty_docs <- rowSums(as.matrix(TDM)) > 0
TDM_non_empty <- TDM[non_empty_docs, ]

# Apply TF-IDF weighting separately
TDM_tfidf <- weightTfIdf(TDM_non_empty)

LSAout <- lsa(TDM_tfidf)#, dims=dimcalc_share())
#dimcalc_share() computes a 'recommended' number of dimensions from the data. You could choose this by hand.  

head(LSAout$dk)  # the document matrix
head(LSAout$tk)  # the term matrix
head(LSAout$sk)  # the diagonal matrix

myDocs <- rownames(LSAout$dk) 
head(myDocs)
myTerms <- rownames(LSAout$tk) 
head(myTerms)

termd = (LSAout$sk * t(LSAout$tk))

cosine_terms <- cosine(LSAout$dk) # this takes some time 
length(cosine_terms)
cosine_terms[1:10,1:10]

docd = (LSAout$sk * t(LSAout$dk))

cosine_docs <- cosine(docd)

d <- dist(cosine_docs) # euclidean distances between the rows
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
fit$points # view results

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric MDS", type="n")
text(x, y, labels = row.names(cosine_docs), cex=1)

#--------------------------------------------------------------------------
##let's try some hierarchical clustering 


corpus <- Corpus(VectorSource(full_data$content)) #this is not filtered
dtm <- DocumentTermMatrix(corpus)

# Perform hierarchical clustering
dist_matrix <- dist(as.matrix(dtm))
hc <- hclust(dist_matrix)

# Cut the dendrogram to create clusters
k <- 10  # Number of clusters (adjust as needed)
clusters <- cutree(hc, k)

# Visualize the dendrogram
plot(hc, main = "Dendrogram of Blame Attribution")

# Visualize the clusters
fviz_dend(hc, k = k, main = "Hierarchical Clustering of Blame Attribution")

#------------------------------------------------------------------------
#Again LSA but with progress bar and more efficient data structures

library(pbapply)
library(text2vec)
library(irlba)

tokens_data <- full_data$content %>%
  pblapply(function(text) {
    text %>%
      tolower() %>%
      gsub(pattern_to_remove, "", .) %>%
      tokens(remove_punct = TRUE) %>%
      tokens_remove(pattern = all_stopwords, case_insensitive = TRUE) %>%
      tokens_remove(pattern = number_patterns, valuetype = "regex", case_insensitive = TRUE) %>%
      tokens_ngrams(n = 1:2)
  })

it <- itoken(full_data$content, progressbar = TRUE)

# Create a vectorizer
vectorizer <- hash_vectorizer()

# Create a DTM
dtm <- create_dtm(it, vectorizer)
tfidf <- TfIdf$new()
dtm_tfidf <- tfidf$fit_transform(dtm)

svd_result <- irlba(dtm_tfidf, nv = 100)
top_terms <- svd_result$v
head(top_terms,10)

dist_matrix <- dist(svd_result$u)  # Distance matrix
hc <- hclust(dist_matrix) 
plot(hc)  # Dendrogram

pca_result <- prcomp(svd_result$u)
ggplot(data.frame(pca_result$x), aes(x = PC1, y = PC2)) + geom_point()

#-----------------------------------------------------------------------------

#Create a network of topics-documents to visualize this relationship presented in the heatmap. 
lda_model <- readRDS("lda.rds")

library(igraph)

topic_probabilities <- posterior(lda_model)$topics

# Step 2: Apply Threshold
binary_relations <- topic_probabilities > 0.15

# Step 3: Create Edges
edges <- which(binary_relations, arr.ind = TRUE)

edges_df <- data.frame(
  from = rownames(topic_probabilities)[edges[, "row"]],
  to = paste("Topic", edges[, "col"])
)

#create nodes 

nodes <- unique(c(edges_df$from, edges_df$to))
nodes_df <- data.frame(name = nodes, type = ifelse(grepl("^Topic", nodes), "Topic", "Document"))


g <- graph_from_data_frame(d=edges_df, vertices = nodes_df)

# Visualize the Network
V(g)$color <- ifelse(V(g)$name %in% edges_df$document, "red", "blue")

layout <- layout_with_dh(g) 

layout2 <- layout_as_bipartite(g)

# Plot with the chosen layout
plot(g, layout = layout, vertex.size = 5, vertex.label = NA, edge.arrow.size = 0.01, vertex.color = V(g)$color)


# Assuming 'g' is your graph

# Step 1: Detect communities
communities <- cluster_louvain(g)

# Step 2 & 3: Extract and visualize each community separately
num_communities <- max(communities$membership)

# Prepare a plotting layout
par(mfrow = c(ceiling(sqrt(num_communities)), ceiling(sqrt(num_communities))), mar = c(1, 1, 2, 1))

for (i in 1:num_communities) {
  # Extract subgraph for community i
  sub_g <- induced_subgraph(g, which(communities$membership == i))
  
  # Calculate layout
  layout <- layout_with_fr(sub_g)
  
  # Plot
  plot(sub_g, layout = layout, vertex.size = 5, vertex.label = NA, edge.arrow.size = 0.5,
       main = paste("Cluster", i), vertex.color = V(sub_g)$color)
}


#---------------------------------------------------------------------
#For visualizing change of topics by date
#I'm missing a vector to name each "topic" as the main idea for that group. 
topic_probabilities <- posterior(lda_lemm)$topics#changed the base model with the lemmatized version

class(full_data$date)
full_data$date <- as.Date(full_data$date)
topic_distributions <- cbind(full_data[, c("date", "title", "urls")], topic_probabilities)
#create dataframe for dates
topic_trends <- topic_distributions |> 
  mutate(year = format(date, "%Y")) |> 
  group_by(year) |> 
  summarise(across(`1`:`10`, mean, na.rm = TRUE))
#renaming columns from 1:10 to "Topic 1",...,"Topic 10"
colnames(topic_probabilities) <- paste("Topic", 1:10)

colnames(topic_trends)[(ncol(topic_trends)-9):ncol(topic_trends)]<- paste("Topic", 1:10) 

#Visualization
topic_trends_long <- tidyr::pivot_longer(topic_trends, -c(year), names_to = "Topic", values_to = "Probability")

ggplot(topic_trends_long, aes(x = year, y = Probability, color = Topic, group=Topic)) +
  geom_line()+
  geom_point() +
  theme_minimal() +
  labs(x = "Year", y = "Average Topic Probability", title = "Trends of Topics Over Years") 
  
#now let's do the same analysis by month. Rename the topics to some meaningful term
full_data<- full_data |> 
  mutate(date = as.Date(date,format = "%Y-%m-%d"),YearMonth=format(date,"%Y-%m"))
write.csv(full_data,file = "full_data_dates.csv")
full_data_dates<- read.csv("full_data_dates.csv")

topic_month_trends <- full_data_dates %>%
  group_by(YearMonth) %>%
  summarise(across(starts_with("Topic"), mean, na.rm = TRUE))
#creating the dataframe to plot
topic_month_trends_long <- topic_month_trends |> 
  pivot_longer(-YearMonth, names_to = "Topic",values_to = "Probability")
#Visualization for the monthly one 

ggplot(topic_month_trends_long, aes(x = YearMonth, y = Probability, group = Topic, color=Topic)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Year-Month", y = "Average Topic Probability", title = "Topic Trends Over Months") +
  coord_flip()+
  theme_minimal()

#Posibles nombres para los tópicos. One idea is to use the 3 most likely terms of each topic to a string that represents a pseudo-name for each topic.

top5termsPerTopic <- terms(lda_lemm, 3)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")

if ((ncol(topic_trends) - 1) == length(topicNames)) {
  # Keep the "Year" column name, and rename the rest with topicNames
  colnames(topic_trends)[-1] <- topicNames
} else {
  warning("The number of topic columns does not match the length of topicNames.")
}
topic_trends_long <- tidyr::pivot_longer(topic_trends, -c(year), names_to = "Topic", values_to = "Probability")

#Plot for yearly 

library(RColorBrewer)
palette <- brewer.pal(min(10, length(unique(topic_trends_long$Topic))), "Set3")
#changed to the lemmatized version by adding the suffix _lemm
yearly_trend_plot_lemm <- ggplot(topic_trends_long, aes(x = year, group=factor(Topic))) +
  geom_area(aes(y = Probability, fill=Topic))+
  scale_fill_manual(values = palette)+
  theme_minimal() +
  labs(x = "Year", y = "Average Topic Probability", title = "Trends of Topics Over Years") 


#plot for the monthly

  topic_month_trends <- full_data_dates %>%
    group_by(YearMonth) %>%
    summarise(across(starts_with("Topic"), mean, na.rm = TRUE))
  #changing names of columns. 
  if ((ncol(topic_month_trends) - 1) == length(topicNames)) {
    # Keep the "Year" column name, and rename the rest with topicNames
    colnames(topic_month_trends)[-1] <- topicNames
  } else {
    warning("The number of topic columns does not match the length of topicNames.")
  }
  #creating the dataframe to plot
  topic_month_trends_long <- topic_month_trends |> 
    pivot_longer(-YearMonth, names_to = "Topic",values_to = "Probability")
  
  #Visualization for the monthly one 
  
monthly_trend_plot<- ggplot(topic_month_trends_long, aes(x = YearMonth, y = Probability, group = Topic, color=Topic)) +
  geom_area(aes(y = Probability, fill=Topic)) +
  scale_fill_manual(values = palette)+
  theme_minimal() +
  coord_flip()+
  labs(x = "Year-Month", y = "Average Topic Probability", title = "Topic Trends Over Months")
ggsave("monthly_trend_plot_lemm.jpeg", plot = monthly_trend_plot, width = 12, height = 8, dpi = 300)
ggsave("yearly_trend_plot_lemm.jpeg", plot = yearly_trend_plot_lemm, width = 12, height = 8, dpi = 300)

#-----------------------------------------------------------------------------------
#Create several models and evaluate them 
k_range <- 5:10
fit_lda_models <- function(dtm, k_range, method = "Gibbs", control = list(seed = 123)) {
  results <- data.frame(k = integer(), perplexity = numeric())
  
  for (k in k_range) {
    set.seed(control$seed)  # For reproducibility
    lda_model <- LDA(dfm_data, k = k, method = method, control = control)
    model_perplexity <- perplexity(lda_model, newdata = dfm_data)  # Calculate in-sample perplexity
    
    results <- rbind(results, data.frame(k = k, perplexity = model_perplexity))
  }
  
  return(results)
}

lda_results <- fit_lda_models(dfm_data, k_range)

# Graph the results to compare model performance
ggplot(lda_results, aes(x = k, y = perplexity)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = k_range) +
  labs(title = "LDA Model In-sample Perplexity by Number of Topics", x = "Number of Topics", y = "Perplexity") +
  theme_minimal()

#-------------------------------------------------------------------------------------------------------------
full_data<- read.csv("full_speeches_mx.csv")
#The idea now is to create a vector with electoral days so I can subset the dastabase and then redo the plots

date_vector <- seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by="day")

full_data$date <- as.Date(full_data$date)
#subset data
data_election_21 <- full_data |>  
  filter(date %in% date_vector)
#save it 
write.csv(data_election_21,file = "data_election.csv")
data_election <- read.csv("data_election.csv")
#----------------------------------------------------------------------------------------------------------------------------
#Rerun the code for this period for the entire year

ud_model <- udpipe_download_model(language = "spanish", model_dir = ".")
ud_model <- udpipe_load_model(file = ud_model$file_model)


# Perform lemmatization
annotated_text_elect <- udpipe_annotate(ud_model, x = data_election_21$content)

annotated_df_elect <- as.data.frame(annotated_text_elect)

#write.csv(annotated_df_elect, file = "annotated_df_elect.csv")
annotated_df_elect <- read.csv("annotated_df_elect.csv")

#saveRDS(annotated_text_elect, "annotated_text_elect.rds")


#annotated_df <- read.csv2("annotated_df.csv")
#annotated_text <- readRDS("annotated_text.rds")
data_election<- data_election_21
annotated_df_elect$doc_id <- gsub("doc", "", annotated_df_elect$doc_id)
lemmatized_by_doc_elect <- annotated_df_elect |> 
  group_by(doc_id) |> 
  summarise(lemmatized_content = paste(lemma, collapse = " ")) |> 
  mutate(doc_id = as.integer(doc_id))
data_election <- data_election |> 
  rename( doc_id=X)
data_election$doc_id <- as.integer(data_election$doc_id)

#data_election$lemmatized_content <- annotated_df_elect$lemma

# Combine lemmatized words back into documents

data_election<- left_join(data_election, lemmatized_by_doc_elect, by = "doc_id")
#write.csv(data_election,file = "data_election.csv")



# Define your custom stopwords. Revisar dos veces si conviene remover "méxico" y "pesos mensuales" para el bigrama. 

custom_stopwords <- c("presidente", "andrés", "manuel", "lópez", "obrador","+","inicia video","finaliza video", "intervención inaudible","méxico","permiso señor","pesos mensuales","pesos","interlocutor","interlocutora","ciento","pregunta","señor","caso","si","entonces","gracias","aquí","va","vamos","ahora","bueno","usted","si","van","centavos","ver","ahí","cómo","pues","ser","así","baja","año","hoy","días")
all_stopwords <- c(stopwords("es"), custom_stopwords)
number_patterns <- c(
  "\\b\\d+\\b",  # Digits
  "\\b(?:cero|uno|dos|tres|cuatro|cinco|seis|siete|ocho|nueve|diez|once|doce|trece|catorce|quince|dieciséis|diecisiete|dieciocho|diecinueve|veinte|treinta|cuarenta|cincuenta|sesenta|setenta|ochenta|noventa|cien|cientos|mil|miles|millón|millones)\\b"  # Written numbers in Spanish
)

data_election$content <- tolower(data_election$content)
pattern_to_remove <- "^\\d{4}: Año de [^.]*\\.?"
data_election$content <- gsub(pattern_to_remove, "", data_election$content)

#tokenize 
tokens_data <- data_election$content %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(pattern = all_stopwords, case_insensitive = TRUE) %>%
  tokens_remove(pattern = number_patterns, valuetype = "regex", case_insensitive = TRUE) %>%
  tokens_ngrams(n = 1:2)


dfm_data <- tokens_data |> 
  dfm() |> 
  dfm_trim(min_termfreq = 15)

#print(dfm_data)

num_topics <- 10  
set.seed(123)
#da_model_elect <- LDA(dfm_data, k = num_topics, method = "Gibbs", control=list(alpha=1))
lda_model_elect2 <- LDA(dfm_data, k = num_topics, method = "Gibbs", control = list(alpha = 5)) 

#Here alpha =5 because it's a guideline suggested by Griffiths and Steyvers (2004) suggest a value of 50/k for α and 0.1 for δ

#save the model 
#saveRDS(lda_model_elect, file = "lda_elect_alpha1.rds")
saveRDS(lda_model_elect2, file = "lda_elect2_alpha5.rds")
#lda_model_elect <- readRDS("lda_elect2_alpha5.rds")

#First table of topics and terms Alpha =1 

top_terms_lemm_elect <- terms(lda_model_elect, 10)  # Top 10 terms for each topic. Topics are group of words that are the most important to those topics. 
top_terms_df_lemm_elect <-  as.data.frame(top_terms_lemm_elect)

kable_out_l_elect <-  kable(top_terms_df_lemm_elect, format = "html") 
html_out_l_elect <- capture.output(kable_out_l_elect)

p_l_elect <- grid.table(top_terms_df_lemm_elect)

# Saving the table as a JPG file
jpeg("table_lemm_elect.jpg", width = 800, height = 600)
grid.draw(p_l_elect)
dev.off()
#table 2 when alpha = 5 
top_terms_lemm_elect2 <- terms(lda_model_elect2, 10)  # Top 10 terms for each topic. Topics are group of words that are the most important to those topics. 
top_terms_df_lemm_elect2 <-  as.data.frame(top_terms_lemm_elect2)

kable_out_l_elect2 <-  kable(top_terms_df_lemm_elect2, format = "html") 
html_out_l_elect2 <- capture.output(kable_out_l_elect2)

p_l_elect2 <- grid.table(top_terms_df_lemm_elect2)

# Saving the table as a JPG file
jpeg("table_lemm_elect2.png", width = 800, height = 600)
grid.draw(p_l_elect2)
dev.off()

#--------------------------------------------------------------------------------------------------
#ANALYSIS BY YEAR, SO USELESS 
#data_election <- read.csv("data_election.csv")

#Visualization by alpha = 5 as recommended

topic_probabilities <- posterior(lda_model_elect)$topics#changed the base model with the lemmatized version

data_election$date <- as.Date(data_election$date)
topic_distributions <- cbind(data_election[, c("date", "title", "urls")], topic_probabilities)
#write.csv(topic_distributions, file ="data_election_topic_distrib.csv")
#create dataframe for dates, but we'll only have one observations since it only one year.

#topic_trends <- topic_distributions |> 
 # mutate(year = format(date, "%Y")) |> 
  #group_by(year) |> 
  #summarise(across(`1`:`10`, mean, na.rm = TRUE))
#renaming columns from 1:10 to "Topic 1",...,"Topic 10"


#colnames(topic_probabilities) <- paste("Topic", 1:10)

#colnames(topic_trends)[(ncol(topic_trends)-9):ncol(topic_trends)]<- paste("Topic", 1:10) 

#Visualization
#topic_trends_long <- tidyr::pivot_longer(topic_trends, -c(year), names_to = "Topic", values_to = "Probability")

#ggplot(topic_trends_long, aes(x = year, y = Probability, color = Topic, group=Topic)) +
 # geom_line()+
  #geom_point() +
  #theme_minimal() +
  #labs(x = "Year", y = "Average Topic Probability", title = "Trends of Topics Over Years") 


#--------------------------------------------------------------------------------------------------------------------------
#Visualization but by month
lda_model_elect <- read_rds("lda_elect2_alpha5.rds")
topic_probabilities <- posterior(lda_model_elect)$topics
colnames(topic_probabilities) <- paste("Topic", 1:10)

topic_distributions <- cbind(data_election[, c("date", "title", "urls")], topic_probabilities)

topic_distributions<- topic_distributions |> 
  mutate(date = as.Date(date,format = "%Y-%m-%d"),YearMonth=format(date,"%Y-%m"))

write.csv(topic_distributions,file = "data_election_dates.csv")
#topic_distributions<- read.csv("data_election_dates.csv")
#Now create dataframe
topic_month_trends <- topic_distributions %>%
  group_by(YearMonth) |> 
  summarise(across(starts_with("Topic"), mean, na.rm = TRUE))
#creating the dataframe to plot
##for the names of each topic 
top5termsPerTopic <- terms(lda_model_elect, 3)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")

if ((ncol(topic_month_trends) - 1) == length(topicNames)) {
  # Keep the "Year" column name, and rename the rest with topicNames
  colnames(topic_month_trends)[-1] <- topicNames
} else {
  warning("The number of topic columns does not match the length of topicNames.")
}

topic_month_trends_long <- topic_month_trends |> 
  pivot_longer(-YearMonth, names_to = "Topic",values_to = "Probability")
#topic_month_trends_long <- topic_month_trends_long |> #remove the "." in Topic.1, Topic.2...Topic.10. 
 # mutate(Topic = gsub("\\."," ",Topic))
  
#Visualization for the monthly one 
library(RColorBrewer)
library(zoo)
palette <- brewer.pal(min(10, length(unique(topic_month_trends_long$Topic))), "Set3")

topic_month_trends_long$YearMonth <- factor(topic_month_trends_long$YearMonth, levels = unique(topic_month_trends_long$YearMonth))

position_of_june <- which(levels(topic_month_trends_long$YearMonth) == "2021-06")

monthly_trend_elect_plot <- ggplot(topic_month_trends_long, aes(x = YearMonth, y = Probability, group = Topic, color = Topic)) +
  geom_area(aes(y=Probability, fill=Topic)) +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  scale_x_discrete(breaks = levels(topic_month_trends_long$YearMonth)) +
  geom_vline(xintercept = position_of_june, colour="black", linetype="dotted", size=2) + # Adding the vertical line
  labs(x = "Year-Month", y = "Average Topic Probability", title = "Topic Trends Over Months Before the Elections")

print(monthly_trend_elect_plot)

ggsave("monthly_trend_elect_plot.jpeg", plot = monthly_trend_elect_plot, width = 12, height = 8, dpi = 300)
