library(readtext)
library(quanteda)
library(tidyverse)
library(textreadr)
library(dplyr)
library(tm)
library(pdftools)
library(stringr)
library(corpus)
library(staplr)
library(BAMMtools)
library(ggplot2)
library(topicmodels)
library(reshape2)
library(pals)
library(data.table)
library(spacyr)
library(udpipe)
library(ldatuning)
library(textstem)
setwd("C:/Users/chris/OneDrive - Georgia State University/20 GRA Lab/Reports")

#New for 4/13------------------------------------------------------------------

#ordering SPRs per country
full_text_english$country_names = str_extract_all(full_text_english$doc_id, pattern = "^.*?(?=[0-9]{2,3})")
full_text_english$country_names
g_text$country_names = str_extract_all(g_text$doc_id, pattern = "^.*?(?=[0-9]{2,3})")
g_text$country_names <- as.character(g_text$country_names)
g_text$trim_docid <- gsub("\\..*", "", g_text$doc_id)
g_text$trim_docid <- gsub("\\[.*?\\]","",g_text$trim_docid)
g_text$trim_docid <- gsub(" Conv$","",g_text$trim_docid)
g_text$trim_docid <- str_trim(g_text$trim_docid, side = "right")
g_text$trim_docid
g_text$doc_id[250:289]
g_text$download_index <- str_extract(g_text$trim_docid, pattern = "[0-9]{1,3}$")
g_text$download_index
g_text <- g_text %>%
  group_by(country_names,download_index) %>%
  mutate(SPR_rank = rank(year,ties.method = "min"))
head(g_text$trim_docid,10)
g_text$SPR_rank

#PROGRAMS
p_text <- data.frame(kwic(full_corpus_english, pattern = c(phrase("new program*"),phrase("novel program*")), window = 15, valuetype = "glob"))
p_text$country_names = str_extract_all(p_text$doc_id, pattern = "^.*?(?=[0-9]{2,3})")
p_text$country_names <- as.character(p_text$country_names)
p_text$trim_docid <- gsub("\\..*", "", p_text$docname)
p_text$trim_docid <- gsub("\\[.*?\\]","",p_text$trim_docid)
p_text$trim_docid <- gsub(" Conv$","",p_text$trim_docid)
p_text$trim_docid <- str_trim(p_text$trim_docid, side = "right")
p_text$trim_docid

p_text$
full_text_english$country_names <- as.character(full_text_english$country_names)
full_text_english<- full_text_english %>%
  group_by(country_names) %>%
  mutate(SPR_Rank = rank(year, ties.method = "min"))
head(full_text_english$SPR_num)

full_text_english$trim_docid = gsub("\\..*","",full_text_english$doc_id)
full_text_english$trim_docid = gsub("\\[.*?\\]","",full_text_english$trim_docid)
full_text_english$trim_docid = gsub(" Conv", "", full_text_english$trim_docid)
full_text_english$trim_docid = str_trim(full_text_english$trim_docid, side = "right")

tbl_p_text <- left_join(as.tbl(p_text), as.tbl(full_text_english), by="trim_docid")
length(tbl_g_text$SPR_Rank)
idx =which(is.na(tbl_g_text$SPR_Rank))
tbl_g_text$trim_docid[idx]


#Expanding word window
aggregate()
BI_kwic <- data.frame(kwic(full_corpus, pattern = phrase("best interest*"), window = 45, valuetype = "glob"))
dim(BI_kwic)

BI_window <- paste(BI_kwic$pre,BI_kwic$keyword,BI_kwic$post, sep = " ")
BI_text <- data.frame(BI_kwic$docname, BI_window)
colnames(BI_text) <- c("doc_id","text")
BI_text$sentences<- as.character(BI_text$sentences)
library(stringr)
#adds a column to text object that can be selected as a text field to create corpus
BI_text$sentences <- str_extract_all(BI_text$text, pattern = "[^.]* Best interest [^.]*\\.|[^.]* best interest [^.]*\\.|[^.]* best interests [^.]*\\.")
#in cases where the sentence beginning/end does not fit within the KWIC window, it will use the limit of the window
BI_text$sentences[42]

BI_corpus<- corpus(BI_text,docid_field = 'doc_id',text_field = "sentences")
g_stopwords <- c(stopwords("en"),"best","interests","interest","child","child's","children","however","children's","take","taken","taking","convention","crc","committee")
BI_dfm <- dfm(BI_corpus, remove_punct=TRUE,remove = g_stopwords) %>%
  dfm_trim(min_docfreq = 0.15, termfreq_type = "quantile")
BI_dfm <- BI_dfm[ntoken(BI_dfm) > 0,]
topic_models_sent_BI <- convert(BI_dfm, to = "topicmodels")
BI_result <- FindTopicsNumber(topic_models_sent_BI, topics = seq(from = 8, to = 32, by =4),
                              metrics = c("CaoJuan2009", "Arun2010", "Deveaud2014", "Griffiths2004"),
                              method = "Gibbs",
                              verbose = TRUE)
FindTopicsNumber_plot(BI_result)
BI_lda_16<-LDA(topic_models_sent_BI,k=16,method="Gibbs")
terms(lemma_lda_19,20)
View(terms(BI_lda_16,20))

library(xlsx)
write.xlsx(terms(BI_lda_16,20),file = "sentence_level_Best_interests_LDA.xlsx")



#LDA on technology
View(technology_kwic)
technology_k


tech_window <- paste(technology_kwic$pre,technology_kwic$keyword,technology_kwic$post, sep = " ")
tech_text <- data.frame(technology_kwic$docname, tech_window)
colnames(tech_text) <- c("doc_id","text")
tech_text$text<- as.character(tech_text$text)

tech_corpus<- corpus(tech_text,docid_field = 'doc_id',text_field = "text")
g_stopwords <- c(stopwords("en"),"best","interests","interest","child","child's","children","however","children's","take","taken","taking","convention","crc","committee")
tech_dfm <- dfm(tech_corpus, remove_punct=TRUE,remove = g_stopwords) %>%
  dfm_trim(min_docfreq = 0.15, termfreq_type = "quantile")
tech_dfm <- tech_dfm[ntoken(tech_dfm) > 0,]
topic_models_tech <- convert(tech_dfm, to = "topicmodels")
tech_result <- FindTopicsNumber(topic_models_tech, topics = seq(from = 4, to = 32, by =4),
                                metrics = c("CaoJuan2009", "Arun2010", "Deveaud2014", "Griffiths2004"),
                                method = "Gibbs",
                                verbose = TRUE)
FindTopicsNumber_plot(tech_result)
tech_lda_10<-LDA(topic_models_tech,k=10,method="Gibbs")
tech_lda_20<-LDA(topic_models_tech,k=20,method="Gibbs")
terms(lemma_lda_19,20)
View(terms(tech_lda_10,20))
write.xlsx(terms(tech_lda_20,20),"Tech_LDA_K20.xlsx")
View(full_text_english)
full_text_english$SPR_num
full_text_english$doc_id[2]
full_text_english[which(full_text_english$country_names=="Albania "),3:4]
pos <- which(full_text_english$"country_names"==country)
table2 <- full_text_english[which(full_text_english$country_names=="Albania "),3:4]
table2[2,3]
order2<- rank(table2$year,ties.method = "min")
order2
pos2 <- which(full_text_english$country_names=="Albania ")

xy2 <- data.frame(pos2,order2)
for (i in range(1:nrow(xy2))){
  x = xy2[i,1]
  y = xy2[i,2]
  full_text_english$SPR_num[x]=y
}
full_text_english$year
full_text_english[which(full_text_english$country_names=="Cyprus "),3:4]
full_text_english$doc_id[10]
xy2
nrow(xy2)


#--------------------------------------------------------------








#Reading Documents ---- Directories are grouped by file type and then sorted alphabetically
docx_convert <- readtext("./docx/*")
summary(docx_convert)
doc_convert <- readtext("./doc/*")
summary(doc_convert)
#My computer does not distinguish between PDF and HTML files
pdf_convert <- readtext("./html/*" )
summary(pdf_convert)
#"Replacement to Corrupted Files" folder on teams
fixed_errors_convert<- readtext("./fixed_errors/*")
summary(fixed_errors_convert)

#Combine all file types: ORDER IS IMPORTANT
full_text <- rbind(doc_convert, docx_convert, pdf_convert, fixed_errors_convert)
summary(full_text)
full_corpus<- corpus(full_text)
backup <- full_corpus
summary(full_corpus)

full_dfm = dfm(full_corpus,tolower = TRUE, stem = TRUE, remove = stopwords("english"), remove_punct = TRUE)

#Filtering out foreign language docs ----
df_dfm <- as.data.frame(full_dfm)
df_dfm$child
#Natural breaks method
partition<-getJenksBreaks(df_of_clan_dfm[,"child"],25)
partition
foreign_lang_filter <-which(df_of_clan_dfm$child<partition[3])
#check results of filter
test_results <- df_dfm$document[foreign_lang_filter]
test_results

#Documents that were flagged by filter but need to be kept, ordered by file type
#doc files: brazil 602, hk 606(annex), italy 36
#docx files: bosnia 620, cambodia 636, canada 659, greece 649 (text annex), japan 41 (text annex), singapore 569 (text annex),
#pdf/html files (my computer does not distinguish between the two): burkina faso 545 (text annex), costa rica 553 (addendum), cyprus 359, dom republic 428, paraguay 525, portugal 644, rwanda 563, saint kitts 469, samoa 582, sao tome 560, tajikistan 436,Sri Lanka 79 (text annex)
#"Replacement to Corrupted Files" folder on teams: Guinea 20 Nov 1996 467[2].pdf","Spain 10 Aug 1993 540 [2].pdf"


english_keep <- c(3,7,14,36,40,43,52,55,80,83,97,109,112,113,128,132,135,136,138,139,142,148,149)
to_remove<- foreign_lang_filter[-english_keep]

#Minty fresh corpus
full_text_english <- full_text[-to_remove,]
summary(full_text_english)
full_corpus_english2<- corpus(full_text_english)
full_corpus_english<- full_corpus[-to_remove,]
new_dfm <- dfm(full_corpus_english)
new_df <- as.data.frame(new_dfm)
head(new_df,10)
texts(full_corpus_english)[1]

length(full_text_english$doc_id)
#Apply Dates----
for (i in 1:length(full_text_english$doc_id)){
  if(length(unlist(str_extract_all(full_text_english$doc_id[i],"[[:digit:]]{4}")))!=0){
    full_text_english$year[i] = unlist(str_extract_all(full_text_english$doc_id[i],"[[:digit:]]{4}"))
  }
  else {
    full_text_english$year[i]=0
  }
}
metacorpus()
full_corpus_english <- corpus(full_text_english,meta=list(full_text_english$year))
CRC

#Sentence Analysis on Gowtham's----
g_kwic <- read.csv("best_interest.csv",header=TRUE )
head(g_kwic,10)
colnames(g_kwic)
g_kwic <- g_kwic[,]
g_window <- paste(g_kwic$pre,g_kwic$keyword,g_kwic$post, sep = " ")
g_text <- data.frame(g_kwic$docname, g_window)
colnames(g_text) <- c("doc_id","text")
g_text$text<- as.character(g_text$text)
g_corpus<- corpus(g_text,)
g_stopwords <- c(stopwords("en"),"best","interests","interest","child","child's","children","however","children's","take","taken","taking","convention","crc","committee")
g_dfm <- dfm(g_corpus, remove_punct=TRUE,remove = g_stopwords) %>%
  dfm_trim(min_docfreq = 0.10, termfreq_type = "quantile")
g_dfm <- g_dfm[ntoken(g_dfm) > 0,]
topic_models_g <- convert(g_dfm, to = "topicmodels")
result <- FindTopicsNumber(topic_models_g, topics = seq(from = 5, to = 20, by =2),
                           metrics = c("CaoJuan2009", "Arun2010", "Deveaud2014", "Griffiths2004"),
                           method = "Gibbs",
                           verbose = TRUE)
FindTopicsNumber_plot(result)
g_lda<-LDA(topic_models_g,k=8,method="Gibbs")
g_lda_17<-LDA(topic_models_g,k=17,method="Gibbs")
terms(g_lda,20)
View(terms(g_lda_17,20))

for (i in 1:length(docnames(g_corpus))){
  if(length(unlist(str_extract_all(g_text$doc_id[i],"[[:digit:]]{4}")))!=0){
    g_text$year[i] = unlist(str_extract_all(g_text$doc_id[i],"[[:digit:]]{4}"))
  }
  else {
    g_text$year[i]=0
  }
}
setDT(g_text)

#Export top 20 LDA Terms
termsdf<-as.data.frame(terms(g_lda_17,20))
colnames(termsdf) <-topicNames[2:18]
write.xlsx(termsdf,file= "Top20.xlsx")

tmResult <- posterior(g_lda_17)
theta <- tmResult$topics
theta
beta <- tmResult$terms
topicNames <- c("Year","Definition of a Child","Legal Structures","Basic Social Programs","Referencing General Principles","Human Rights","Legal Deliberations","Custody/Guardianship","Adoption","Youth Programs","(Non-)discrimination","Juvenile Court","Foster Care","Community","Refugees/Unaccompanied Minors","Terms of Art","National Efforts","Unfit Parents")
unknown_years$year
topic_proportion_by_year <- aggregate(theta, by = list(year), mean)
dim(topic_proportion_by_year)
length(theta)-length(unknown_years$year)

#Topic proportions BY SPR RANK
topic_proportion_by_SPRnum <- aggregate(theta, by = list(SPRnum = tbl_g_text$SPR_Rank),mean)
length(theta)
topic_proportion_by_SPRnum
colnames(topic_proportion_by_SPRnum) <- topicNames
vizDF <- melt(topic_proportion_by_SPRnum, id.vars = "Year")
colnames(vizDF) <- c("SPR_Submission_Number", "variable", "value")
ggplot(vizDF, aes(x=SPR_Submission_Number, y= value, fill = variable)) + geom_bar(stat = "identity") + ggtitle("Proportions of Topics by SPR Iteration") + ylab("proportion") + scale_fill_manual(values = paste0(alphabet(20),"FF"), name = "Topics") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(tbl_g_text, aes(x = factor(SPR_Rank))) + stat_summary(fun.y= "mean", geom = "bar")

vizDF <- melt(topic_proportion_by_SPRnum, id.vars = "Year")
n <- df.aree$name

# transpose all but the first column (name)
df.aree <- as.data.frame(t(df.aree[,-1]))
colnames(df.aree) <- n
df.aree$myfactor <- factor(row.names(df.aree))

str(df.aree)
domainsdf <- data.frame(Domain=c("Substantive Rights", "Legal Principles", "Rules of Procedure", "Legal Principles","Substantive Rights","Rules of Procedure","Substantive Rights", "Rules of Procedure", "Rules of Procedure","Substantive Rights", "Legal Principles", "Rules of Procedure","Substantive Rights","Legal Principles","Rules of Procedure", "Legal Principles","Rules of Procedure"),
                        topics = pt_df$Topics)
View(domains)
vizDF2 <- left_join(as.tbl(vizDF),as.tbl(domainsdf), by = c("variable"="topics"), keep = TRUE)
View(vizDF2)
colnames(vizDF) <- c("SPR_Submission_Number", "variable", "value")
nb=3
nm=17
install.packages("hcl")
library(hcl)
library(graphics)
hsv(.5,.5,.5)
colors = apply(expand.grid(seq(70,40,length=nm), 100, seq(15,375, length = nb+1)[1:nb]), 1, function(x))
ggplot(vizDF2, aes(x=SPR_Submission_Number, y= value, fill = variable, color = Domain)) + geom_bar(stat = "identity") + ggtitle("Proportions of Topics by SPR Iteration") + ylab("proportion") + scale_fill_manual(values = paste0(alphabet(20),"FF"), name = "Topics") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(tbl_g_text, aes(x = factor(SPR_Rank))) + stat_summary(fun.y= "mean", geom = "bar")

# set topic names to aggregated columns
colnames(topic_proportion_by_year) <- topicNames
vizDataFrame <- melt(topic_proportion_by_year, id.vars = "Year")
colnames(topic_proportion_by_year)
cleanedViz <- vizDataFrame[vizDataFrame$Year!=0,]
View(cleanedViz)
ggplot(cleanedViz, aes(x=Year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + ggtitle("Context of Best Interest Over Time") +
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "topics") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
pt_df<- as.data.frame(countsOfPrimaryTopics[primaryTopic]+1)
pt_df
pt_df<- setDT(pt_df, keep.rownames = TRUE)[]
colnames(pt_df)<-c("Topics","Count")
vdf2 = melt(tbl_g_text,)
ggplot(tbl_g_text, aes(x=SPR_Rank)) + geom_bar(stat = "count")
#Count and Plot the Top Topics
countsOfPrimaryTopics <- rep(0, 17)
names(countsOfPrimaryTopics) <- topicNames[2:18]
for (i in 1:nrow(topic_models_g)) {
  topicsPerDoc <- theta[i, ] # select topic distribution for document i
  # get first element position from ordered list
  primaryTopic <- order(topicsPerDoc, decreasing = TRUE)[1] 
  countsOfPrimaryTopics[primaryTopic] <- countsOfPrimaryTopics[primaryTopic] + 1
}
domains <- data. ("Substantive Rights", "Legal Principles", "Rules of Procedure", "Legal Principles","Substantive Rights","Rules of Procedure","Substantive Rights", "Rules of Procedure", "Rules of Procedure","Substantive Rights", "Legal Principles", "Rules of Procedure","Substantive Rights","Legal Principles","Rules of Procedure", "Legal Principles","Rules of Procedure")
countsOfPrimaryTopics
pt_df<- as.data.frame(countsOfPrimaryTopics)
pt_df$Domain <- domains
pt_tbl <- arrange(as.tbl(pt_df),Domain)
pt_tbl
pt_df$Topics
colnames(pt_df)[1]<-c("Count")
topic_models_g
idx <- grep(pattern = "\\d+{1,3}$",x = g_text$trim_docid, invert = TRUE)
g_text$trim_docid[idx]
head(full_text_english$trim_docid)
pt_df$Topics <- row.names(pt_df)
pt_df
topicNames[2:18]
library(forcats)
pt_tbl
pt_tbl$Domain <- as.factor(pt_tbl$domain)
fct_reorder(Domain,pt_tbl)
pt_tbl$Topics <- factor(pt_tbl$Topics, levels = pt_tbl$Topics[order(pt_tbl$Domain)])
ggplot(pt_tbl, aes(x=Topics, y=Count, fill= Domain)) + coord_flip()+
  geom_bar(stat = "identity",width=0.5) + ylab("Count") + ggtitle("Frequency of Each Topic") +
  theme(axis.text.x = element_text( hjust = 1)) 
warnings()
pt_df2 %>% mutate()
str(pt_tbl)

#Lemmatization ----

#Sentence Analysis on Gowtham's----
g_kwic <- read.csv("best_interest.csv",header=TRUE )
head(g_kwic,10)
colnames(g_kwic)
g_kwic <- g_kwic[,]
g_window <- paste(tolower(g_kwic$pre),tolower(g_kwic$keyword),tolower(g_kwic$post), sep = " ")
g_text <- data.frame(g_kwic$docname, g_window)
colnames(g_text) <- c("doc_id","text")
g_text$text<- as.character(g_text$text)
g_mat <- as.matrix(g_text)
View(g_mat)
g_lemma <- as.data.frame(t(apply(g_mat,1,lemmatize_strings)))
colnames(g_lemma) <- c("doc_id","text")
g_lemma$text<- as.character(g_lemma$text)

textstat_frequency()
lemma_corpus<- corpus(g_lemma,docid_field = 'doc_id',text_field = "text")
g_stopwords <- c(stopwords("en"),"best","interests","interest","child","child's","children","however","children's","take","taken","taking","convention","crc","committee")
lemma_dfm <- dfm(lemma_corpus, remove_punct=TRUE,remove = g_stopwords) %>%
  dfm_trim(min_docfreq = 0.15, termfreq_type = "quantile")
lemma_dfm <- lemma_dfm[ntoken(lemma_dfm) > 0,]
topic_models_lemma <- convert(lemma_dfm, to = "topicmodels")
lemma_result <- FindTopicsNumber(topic_models_lemma, topics = seq(from = 5, to = 20, by =2),
                           metrics = c("CaoJuan2009", "Arun2010", "Deveaud2014", "Griffiths2004"),
                           method = "Gibbs",
                           verbose = TRUE)
FindTopicsNumber_plot(lemma_result)
lemma_lda_19<-LDA(topic_models_lemma,k=19,method="Gibbs")
terms(lemma_lda_19,20)
View(terms(lemma_lda_19,20))

lemma_termsdf<-as.data.frame(terms(lemma_lda_19,20))
library(xlsx)
write.xlsx(lemma_termsdf,file= "Top20_lemmatized.xlsx")

#Trimming to sentence level KWIC----

#takes a pasted KWIC output and then modifies for conversion into corpus
g_window <- paste(g_kwic$pre,g_kwic$keyword,g_kwic$post, sep = " ")
g_text <- data.frame(g_kwic$docname, g_window)
colnames(g_text) <- c("doc_id","text")
g_text$text<- as.character(g_text$text)


library(stringr)
#adds a column to text object that can be selected as a text field to create corpus
g_text$sentences <- str_extract_all(g_text$text, pattern = "[^.]* Best interest [^.]*\\.|[^.]* best interest [^.]*\\.|[^.]* best interests [^.]*\\.")
#in cases where the sentence beginning/end does not fit within the KWIC window, it will use the limit of the window
g_text$sentences[42]


#textstats on 60-word window
sixtyword_freqs <- textstat_frequency(g_dfm,n = 25)
write.xlsx(sixtyword_freqs,file = "60Word BI Window Most Frequent Terms.xlsx")



full_text_english$country_names <- as.character(full_text_english$country_names)
full_text_english<- full_text_english %>%
  group_by(country_names) %>%
  mutate(SPR_Rank = rank(year, ties.method = "min"))


all_topics <- g_lda_17@gamma
colnames(all_topics) <- topicNames[2:18]
head(all_topics,10)
test <- sort(all_topics[1,], decreasing = TRUE)[1:2]
colnames(test)[1]
toptwo<- apply(all_topics,2,sort, decreasing= TRUE)
toptwo
all_topics <- data.frame(all_topics)
all_topics$top1 <- NA
all_topics$top2 <- NA
f1 <- function(x){
  return(sort(x[[1]], decreasing = TRUE)[1:2])
}
all_topics$top <- NA
j <- apply(all_topics, 1, f1)
all_topics$top1
j[1]
typeof(j)
head(j)
for (i in range(1:nrow(all_topics))){
  all_topics$top1[i] <- colnames(rank(all_topics[i,]))[1]
  all_topics$top2[i] <- colnames(rank(all_topics[i,]))[2]
}
nrow(all_topics)
order(all_topics[1,])
