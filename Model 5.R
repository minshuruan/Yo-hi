install.packages("RedditExtractoR")
library(RedditExtractoR)
#
#
#
AA_threads = find_thread_urls(keywords = "Anthony Albanese",
                              sort_by = "comments",
                              period = "year")
#
SL_threads = find_thread_urls(keywords = "Sussan Ley",
                              sort_by = "comments",
                              period = "year")
#
LW_threads = find_thread_urls(keywords = "Larissa Waters",
                              sort_by = "comments",
                              period = "year")
#
AA_title_texts = AA_threads$title
SL_title_texts = SL_threads$title
LW_title_texts = LW_threads$title
#
#
all_title_texts = c(AA_title_texts,
                    SL_title_texts,
                    LW_title_texts)
#
#
#
install.packages('SnowballC')
install.packages("tm")
library(NLP)
library(tm)

#create a corpus from Reddit title text
corpus = Corpus(VectorSource(all_title_texts))
#convert characters to ASCII
corpus = tm_map(corpus, function(x) iconv(x, to = 'ASCII'))
#Section (if enable will remove names of party leaders)
#from anylysis
#(reason: they may be highly frequent, we know who they are, optional)
corpus = tm_map(corpus,
                removeWords,
                c(stopwords(), c("Anthony",
                                 "albanese",
                                 "Sussan",
                                 "Ley",
                                 "Larissa",
                                 "Waters"))
)
#create doccument term matrix applying some transformations
tdm = TermDocumentMatrix(corpus,
                         control = list(removePunctuation = TRUE,
                                        stopwords = TRUE,
                                        removeNumbers = TRUE,
                                        tolwer = TRUE,
                                        stemming = TRUE))
#remove empty doccument (after cleaning)
#cleaning may cause total removal of all words from a text 
#not useful as we want words (i.e worldclould)
empties = which(colSums(as.matrix(tdm)) ==0)
#


tdm = tdm[,-empties]
#covert to a standard R matrix
M = as.matrix(tdm)
#WORLDCLOULD =====================
#side by side word cloulds prep
par(mfrow=c(1,2))
#load library
install.packages('wordcloud')   
install.packages('RColorBrewer')
installed.packages('SnowballC')
library(wordcloud)
#create worldclould (add freq. of all terms across all documents)
freqs = rowSums(M)


#remove anywords that have count "NA".
freqs = freqs[!is.na(freqs)]
#use scale, max.words, min.freqs to adjust
#clould to look fluffy round
wordcloud(names(freqs),
          freqs,
          random.order = FALSE,
          min.freq = 6,
          max.words = 200,
          colors = brewer.pal(8, "Dark2"),
          scale=c(2, .5)) # play with scale to make clould round
#Add a title to wordcloud
title("un-weighted M (TDM)") 
#
tdmw = weightTfIdf(tdm)
#
T = as.matrix(tdmw)
#
freqsw = rowSums(T)
#
wordcloud(names(freqsw),
          freqsw,
          random.order = FALSE,
          min.freq = 6,
          max.words = 200,
          colors = brewer.pal(8, "Dark2"),
          scale=c(-1, .5)) # play with scale to make clould round
#
title("TF-IDF weighted T(TDM)") 
#
#
#
#
#
#
colours = c(rep("red", length(AA_title_texts)),
            rep("blue", length(SL_title_texts)),
            rep("green", length(LW_threads_texts)))
#
#
colours = colours[-empties]
colours
#
pcaT = prcomp(t(T))
#
plot(pcaT$x[,1], pcaT$x[,2], col = colours, pch = 16,
     main = "PCA: PC1 &PC2 TFIDF weighted T")
#
plot(pcaT$x[,1], pcaT$x[,3], col = colours, pch = 16,
     main = "PCA: PC1 & PC3 TFIDF weighted T")
#
PcaM = prcomp(t(sqrt(M)))
#
plot(PcaM$x[,1], PcaM$x[,2], col = colours, pch = 16,
     main = "PCA: PG1 & PG2 SQRT un-weighted M")
#
plot(PcaM$x[,1], PcaM$x[,3], col = colours, pch = 16,
     main = "PCA: PC1 & PC3 SQRT un-weighted M")
#
#
summary(PcaM)$importance[,1:5]
#
#
par(mfrow=c(2,3))
#
#
D = dist(t(T))
dim(as.matrix(D))
#
mdsT = cmdscale(D, k=2)
#
plot(mdsT[,1], mdsT[,2], col = colours, pch=16,
     main="MDS: weighted T Euclidean distance
     \nSame as weight T PCA!")
#
#
#
#
#
#
CM = M %*% diag(1/sqrt(colSums(M^2)))
#
#
D = dist(t(CM), method = "euclidean")^2/2 #
#
mdsM = cmdscale(D, k = 2)
#
plot(mdsM[,1], mdsM[,2], col = colours, pch=16,
     main = "MDS: unweight M Cosine Distance")
#
D = dist(t(M), method = "binary")
 mdsM = cmdscale(D, k = 2)
 #
 plot(mdsM[,1], mdsM[,2], col=colours, pch =16,
      main = "MDS:unweighted M Binary Distance")
 #
 #
 #
 CT = T%*% diag(1/sqrt(colSums(T^2)))
#
 D = dist(t(CT), method = "euclidean")^2/2
 #
 mdsT = cmdscale(D, k=2)
 #
 plot(mdsT[,1], mdsT[,2], col = colours, pch = 16,
      main = "MDS: TFIDF weighted T Cosine Distance")
 #
 #
 D = dist(t(T), method = "binary")
 #
 mdsT = cmdscale(D, k =2)
#
 plot(mdsT[,1], mdsT[,2], col = colours, pch=16,
      main = " MDS TFIDF weighted T Binary Distance")
 
 
 
 
 

