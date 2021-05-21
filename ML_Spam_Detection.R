##1st part of project---------------------------------------------------
# Load libraries
library('tm')
library('ggplot2')

dir<-getwd()
setwd(dir)

# Set the global paths
spam.path <- file.path("data", "spam")
spam2.path <- file.path("data", "spam_2")
easyham.path <- file.path("data", "easy_ham")
easyham2.path <- file.path("data", "easy_ham_2")
hardham.path <- file.path("data", "hard_ham")
hardham2.path <- file.path("data", "hard_ham_2")


get.msg <- function(path)
{
  con <- file(path, open="rt")
  text <- readLines(con)
  # The message always begins after the first full line break
  msg <- text[seq(which(text == "")[1] + 1, length(text), 1)]
  close(con)
  return(paste(msg, collapse = "\n"))
}

spam.docs <- dir(spam.path)
spam.docs <- spam.docs[which(spam.docs != "cmds")]
all.spam <- sapply(spam.docs,
                   function(p)get.msg(file.path(spam.path,p,sep="")))

# Create a TermDocumentMatrix (TDM) from the corpus of SPAM email.
# The TDM control can be modified, and the sparsity level can be 
# altered.  This TDM is used to create the feature set used to do 
# train our classifier.
get.tdm <- function(doc.vec)
{
  control <- list(stopwords = TRUE,
                  removePunctuation = TRUE,
                  removeNumbers = TRUE,
                  minDocFreq = 2)

  doc.corpus <- Corpus(VectorSource(doc.vec))
  doc.corpus <- tm_map(doc.corpus, function(x) iconv(x, to='UTF-8', sub='byte'))
  doc.dtm <- TermDocumentMatrix(doc.corpus, control)
  return(doc.dtm)
}

# Create a DocumentTermMatrix from that vector
spam.tdm <- get.tdm(all.spam)

# Create a data frame that provides the feature set from the training SPAM data
spam.matrix <- as.matrix(spam.tdm)
spam.counts <- rowSums(spam.matrix)

spam.df <- data.frame(cbind(names(spam.counts),
                            as.numeric(spam.counts)),
                      stringsAsFactors = FALSE)                      
names(spam.df) <- c("term", "frequency")
spam.df$frequency <- as.numeric(spam.df$frequency)

spam.occurrence<-sapply(1:nrow(spam.matrix),function(i)
                          {
                            length(which(spam.matrix[i, ] > 0)) / ncol(spam.matrix)
                          })

spam.density <- spam.df$frequency/ sum(spam.df$frequency)

# Add the term density and occurrence rate
spam.df <- transform(spam.df,
                     density = spam.density,
                     occurrence = spam.occurrence)

# Now do the same for the EASY HAM email
easyham.docs <- dir(easyham.path)
easyham.docs <- easyham.docs[which(easyham.docs != "cmds")]
all.easyham <- sapply(easyham.docs,
                      function(p) get.msg(file.path(easyham.path, p)))

easyham.tdm <- get.tdm(all.easyham)
easyham.matrix <- as.matrix(easyham.tdm)
easyham.counts <- rowSums(easyham.matrix)
easyham.df <- data.frame(cbind(names(easyham.counts),
                               as.numeric(easyham.counts)),
                         stringsAsFactors = FALSE)
names(easyham.df) <- c("term", "frequency")
easyham.df$frequency <- as.numeric(easyham.df$frequency)
easyham.occurrence <- sapply(1:nrow(easyham.matrix),
                            function(i)
                            {
                              length(which(easyham.matrix[i, ] > 0)) / ncol(easyham.matrix)
                            })
easyham.density <- easyham.df$frequency / sum(easyham.df$frequency)

easyham.df <- transform(easyham.df,
                        density = easyham.density,
                        occurrence = easyham.occurrence)

# Save the training data for use in Chapter 4
write.csv(spam.df, file.path("data", "spam_df.csv"), row.names = FALSE)
write.csv(easyham.df, file.path("data", "easyham_df.csv"), row.names = FALSE)
##-------------------------------------------------------------------

dirspamcsv <- dir + "/data/spam_df.csv"
direasyhamcsv <- dir + "/data/spam_df.csv"
spam.df<-read.table(dirspamcsv,header=T,row.names=NULL,sep=",",dec=".")
easyham.df<-read.table(direasyhamcsv ,header=T,row.names=NULL,sep=",",dec=".")

# Plot 10 words with highest occurrences
spam.ddff<-spam.df[with(spam.df,order(-occurrence)),]
dim(spam.ddff)
View(spamtop10)
spamtop10<-spam.ddff[1:10,]
spamtop10<-data.frame(spamtop10)

plot.spamtop10<- ggplot(spamtop10,aes(x=spamtop10$term,y=spamtop10$occurrence)) + geom_bar(stat = "identity") + labs(title="Frequency bar chart")  # Y axis derived from counts of X item
plot.spamtop10

easyham.ddff<-(easyham.df[with(easyham.df,order(-occurrence)),])
dim(easyham.ddff)
View(easyhamtop10)
easyhamtop10<-easyham.ddff[1:10,]

plot.easyhamtop10<- ggplot(easyhamtop10,aes(x=easyhamtop10$term,y=easyhamtop10$occurrence)) + geom_bar(stat = "identity") + labs(title="Frequency bar chart")  # Y axis derived from counts of X item
plot.easyhamtop10

# Find words in top-10 words appearing in both spam and easy ham and do not consider them 
intersect10<-intersect(spamtop10$term,easyhamtop10$term)

spamvariable<-spamtop10[-c(1,2,4,9),]
spamvariable$term
compare<-rbind(easyham.df[which(easyham.df$term=="html"),],easyham.df[which(easyham.df$term=="please"),],
easyham.df[which(easyham.df$term=="email"),],easyham.df[which(easyham.df$term=="click"),],
easyham.df[which(easyham.df$term=="href"),],easyham.df[which(easyham.df$term=="free"),])

ggplot(spamvariable,aes(x=term,y=occurrence)) + geom_bar(stat = "identity") + ylim(c(0,1))+labs(title="Frequency bar chart")  # Y axis derived from counts of X item
ggplot(compare, aes(x=term, y=occurrence)) + geom_bar(stat = "identity") + ylim(c(0,1)) + labs(title="Frequency bar chart")

easyhamvariable<-easyhamtop10[-c(1,2,3,4),]
easyhamvariable$term
compare1<-rbind(spam.df[which(spam.df$term=="can"),],spam.df[which(spam.df$term=="listinfo"),],
spam.df[which(spam.df$term=="net"),],spam.df[which(spam.df$term=="date"),],
spam.df[which(spam.df$term=="just"),],spam.df[which(spam.df$term=="wrote"),])

ggplot(easyhamvariable,aes(x=easyhamvariable$term,y=easyhamvariable$occurrence)) +ylim(c(0,1))+ geom_bar(stat = "identity") + labs(title="Frequency bar chart")  # Y axis derived from counts of X item
ggplot(compare1, aes(x=term, y=occurrence)) + geom_bar(stat = "identity") + ylim(c(0,1)) + labs(title="Frequency bar chart")


## 2nd part of project----------------------------------------------------
install.packages("class")
library(class)
library(MASS)

# 1.get the data for logistic lda qda---------
# This function takes a file path to an email file and a string, 
# the term parameter, and returns the count of that term in 
# the email body.

count.word <- function(path, term)
{
  msg <- get.msg(path)
  msg.corpus <- Corpus(VectorSource(msg))
  msg.corpus <- tm_map(msg.corpus, function(x) iconv(x, to='UTF-8', sub='byte'))

  # Hard-coded TDM control
  control <- list(stopwords = TRUE,
                  removePunctuation = TRUE,
                  removeNumbers = TRUE)
  msg.tdm <- TermDocumentMatrix(msg.corpus, control)

  word.freq <- rowSums(as.matrix(msg.tdm))
  term.freq <- word.freq[which(names(word.freq) == term)]
  # We use ifelse here because term.freq = NA if nothing is found
  return(ifelse(length(term.freq) > 0, term.freq, 0))
}

# Find counts of six distinctive words identifed in many SPAMS in all SPAM and EASYHAM docs, and create figure
html.spam <- sapply(spam.docs,
                    function(p) count.word(file.path(spam.path, p), "html"))
please.spam <- sapply(spam.docs,
                     function(p) count.word(file.path(spam.path, p), "please"))
email.spam <- sapply(spam.docs,
                     function(p) count.word(file.path(spam.path, p), "email"))
click.spam <- sapply(spam.docs,
                     function(p) count.word(file.path(spam.path, p), "click"))
href.spam <- sapply(spam.docs,
                     function(p) count.word(file.path(spam.path, p), "href"))
free.spam <- sapply(spam.docs,
                     function(p) count.word(file.path(spam.path, p), "free"))
spam.init <- cbind(html.spam, please.spam,email.spam,click.spam,href.spam,free.spam, "SPAM")

html.ham <- sapply(easyham.docs,
                    function(p) count.word(file.path(easyham.path, p), "html"))
please.ham <- sapply(easyham.docs,
                     function(p) count.word(file.path(easyham.path, p), "please"))
email.ham <- sapply(easyham.docs,
                     function(p) count.word(file.path(easyham.path, p), "email"))
click.ham <- sapply(easyham.docs,
                     function(p) count.word(file.path(easyham.path, p), "click"))
href.ham <- sapply(easyham.docs,
                     function(p) count.word(file.path(easyham.path, p), "href"))
free.ham <- sapply(easyham.docs,
                     function(p) count.word(file.path(easyham.path, p), "free"))
easyham.init <- cbind(html.ham, please.ham,email.ham,click.ham,href.ham,free.ham, "HAM")

# Examine data
head(easyham.init)
dim(easyham.init)

tail(spam.init)
dim(spam.init)

init.df <- data.frame(rbind(spam.init, easyham.init),
                      stringsAsFactors = FALSE)
names(init.df) <- c("html","please","email","click","href","free","type")
cols = c(1:6)    
init.df[,cols] = apply(init.df[,cols], 2, function(x) as.numeric(as.character(x)))
init.df$type <- as.factor(init.df$type)
class(init.df[,7])

init.df<-data.frame(init.df)
row.names(init.df)<-1:3000
init.df1<-init.df[1:1000,]

# Examine data
head(init.df1)
View(rbind(head(init.df1),tail(init.df1)))

## Prepare all testing dataset---------------------------------------------
hardham.docs <- dir(hardham.path)
hardham.docs <- hardham.docs[which(hardham.docs != "cmds")]

hardham2.docs <- dir(hardham2.path)
hardham2.docs <- hardham2.docs[which(hardham2.docs != "cmds")]

easyham2.docs <- dir(easyham2.path)
easyham2.docs <- easyham2.docs[which(easyham2.docs != "cmds")]

spam2.docs <- dir(spam2.path)
spam2.docs <- spam2.docs[which(spam2.docs != "cmds")]

html.spam2 <- sapply(spam2.docs,
                    function(p) count.word(file.path(spam2.path, p), "html"))
please.spam2<- sapply(spam2.docs,
                     function(p) count.word(file.path(spam2.path, p), "please"))
email.spam2<- sapply(spam2.docs,
                     function(p) count.word(file.path(spam2.path, p), "email"))
click.spam2<- sapply(spam2.docs,
                     function(p) count.word(file.path(spam2.path, p), "click"))
href.spam2 <- sapply(spam2.docs,
                     function(p) count.word(file.path(spam2.path, p), "href"))
free.spam2 <- sapply(spam2.docs,
                     function(p) count.word(file.path(spam2.path, p), "free"))
spam2.init <- cbind(html.spam2, please.spam2,email.spam2,click.spam2,href.spam2,free.spam2)
names(spam2.init) <- c("html","please","email","click","href","free")
cols = c(1:6)    
spam2.init[,cols] = apply(spam2.init[,cols], 2, function(x) as.numeric(as.character(x)))
spam2.init<-data.frame(spam2.init)
row.names(spam2.init)=1:1397

tail(spam2.init)
dim(spam2.init)
View(head(spam2.init))

html.ham2 <- sapply(easyham2.docs,
                    function(p) count.word(file.path(easyham2.path, p), "html"))
please.ham2 <- sapply(easyham2.docs,
                     function(p) count.word(file.path(easyham2.path, p), "please"))
email.ham2 <- sapply(easyham2.docs,
                     function(p) count.word(file.path(easyham2.path, p), "email"))
click.ham2 <- sapply(easyham2.docs,
                     function(p) count.word(file.path(easyham2.path, p), "click"))
href.ham2 <- sapply(easyham2.docs,
                     function(p) count.word(file.path(easyham2.path, p), "href"))
free.ham2 <- sapply(easyham2.docs,
                     function(p) count.word(file.path(easyham2.path, p), "free"))
easyham2.init <- cbind(html.ham2, please.ham2,email.ham2,click.ham2,href.ham2,free.ham2)
names(easyham2.init) <- c("html","please","email","click","href","free")
cols = c(1:6)    
easyham2.init[,cols] = apply(easyham2.init[,cols], 2, function(x) as.numeric(as.character(x)))
easyham2.init<-data.frame(easyham2.init)
row.names(easyham2.init)=1:1400

tail(easyham2.init)
dim(easyham2.init)
View(head(easyham2.init))

html.hardham <- sapply(hardham.docs,
                    function(p) count.word(file.path(hardham.path, p), "html"))
please.hardham <- sapply(hardham.docs,
                     function(p) count.word(file.path(hardham.path, p), "please"))
email.hardham <- sapply(hardham.docs,
                     function(p) count.word(file.path(hardham.path, p), "email"))
click.hardham <- sapply(hardham.docs,
                     function(p) count.word(file.path(hardham.path, p), "click"))
href.hardham <- sapply(hardham.docs,
                     function(p) count.word(file.path(hardham.path, p), "href"))
free.hardham<- sapply(hardham.docs,
                     function(p) count.word(file.path(hardham.path, p), "free"))
hardham.init <- cbind(html.hardham, please.hardham,email.hardham,click.hardham,href.hardham,free.hardham)
names(hardham.init) <- c("html","please","email","click","href","free")
cols = c(1:6)    
hardham.init[,cols] = apply(hardham.init[,cols], 2, function(x) as.numeric(as.character(x)))
hardham.init<-data.frame(hardham.init)
row.names(hardham.init)=1:249

dim(hardham.init)
tail(hardham.init)
View(head(hardham.init))

html.hardham2 <- sapply(hardham2.docs,
                    function(p) count.word(file.path(hardham2.path, p), "html"))
please.hardham2 <- sapply(hardham2.docs,
                     function(p) count.word(file.path(hardham2.path, p), "please"))
email.hardham2 <- sapply(hardham2.docs,
                     function(p) count.word(file.path(hardham2.path, p), "email"))
click.hardham2 <- sapply(hardham2.docs,
                     function(p) count.word(file.path(hardham2.path, p), "click"))
href.hardham2 <- sapply(hardham2.docs,
                     function(p) count.word(file.path(hardham2.path, p), "href"))
free.hardham2<- sapply(hardham2.docs,
                     function(p) count.word(file.path(hardham2.path, p), "free"))
hardham2.init <- cbind(html.hardham2, please.hardham2,email.hardham2,click.hardham2,href.hardham2,free.hardham2)
names(hardham2.init) <- c("html","please","email","click","href","free")
cols = c(1:6)    
hardham2.init[,cols] = apply(hardham2.init[,cols], 2, function(x) as.numeric(as.character(x)))
hardham2.init<-data.frame(hardham2.init)
row.names(hardham2.init)=1:248

tail(hardham2.init)
dim(hardham2.init)
View(head(hardham2.init))


##1 Logistic--------------------------
logistic.fit<-glm(type~html+please+email+click+href+free,data=init.df1,family=binomial)
summary(logistic.fit)
logistic.fit<-glm(type~html+please+click+href+free,data=init.df,family=binomial)
summary(logistic.fit)
logistic.fit<-glm(type~please+click+href+free,data=init.df,family=binomial)
summary(logistic.fit)

logistic.probs=predict(logistic.fit,hardham.init,type="response")
logistic.pred=rep("HAM",249)
logistic.pred[logistic.probs>1/2]="SPAM"
HAM<-rep("HAM",249)
table(logistic.pred,HAM)
mean(logistic.pred==HAM)

logistic.probs=predict(logistic.fit,hardham2.init,type="response")
dim(hardham2.init)
logistic.pred=rep("HAM",248)
logistic.pred[logistic.probs>1/2]="SPAM"
HAM<-rep("HAM",248)
table(logistic.pred,HAM)
mean(logistic.pred==HAM)

logistic.probs=predict(logistic.fit,easyham2.init,type="response")
dim(easyham2.init)
logistic.pred=rep("HAM",1400)
logistic.pred[logistic.probs>1/2]="SPAM"
HAM<-rep("HAM",1400)
table(logistic.pred,HAM)
mean(logistic.pred==HAM)

logistic.probs=predict(logistic.fit,spam2.init,type="response")
dim(spam2.init)
logistic.pred=rep("HAM",1397)
logistic.pred[logistic.probs>1/2]="SPAM"
SPAM<-rep("SPAM",1397)
table(logistic.pred,SPAM)
mean(logistic.pred==SPAM)

##2 3 LDA QDA --------------------------
lda.fit<-lda(type~html+please+email+click+href+free,data=init.df1)
lda.fit
plot(lda.fit)

lda.pred=predict(lda.fit,hardham.init)
lda.class<-lda.pred$class
HAM<-rep("HAM",249)
table(lda.class,HAM)
sum(lda.pred$posterior[,1]>=1/2)
mean(lda.class==HAM)

lda.pred=predict(lda.fit,hardham2.init)
lda.class<-lda.pred$class
HAM<-rep("HAM",248)
table(lda.class,HAM)
sum(lda.pred$posterior[,1]>1/2)
mean(lda.class==HAM)

lda.pred=predict(lda.fit,easyham2.init)
lda.class<-lda.pred$class
HAM<-rep("HAM",1400)
table(lda.class,HAM)
sum(lda.pred$posterior[,1]>=1/2)
mean(lda.class==HAM)

lda.pred=predict(lda.fit,spam2.init)
lda.class<-lda.pred$class
SPAM<-rep("SPAM",1397)
table(lda.class,SPAM)
sum(lda.pred$posterior[,1]>=1/2)
mean(lda.class==SPAM)

qda.fit<-qda(type~html+please+email+click+href+free,data=init.df1)
qda.fit

qda.class=predict(qda.fit,hardham.init)$class
HAM<-rep("HAM",249)
table(qda.class,HAM)
mean(qda.class==HAM)

qda.class=predict(qda.fit,hardham2.init)$class
HAM<-rep("HAM",248)
table(qda.class,HAM)
mean(qda.class==HAM)

qda.class=predict(qda.fit,easyham2.init)$class
HAM<-rep("HAM",1400)
table(qda.class,HAM)
mean(qda.class==HAM)

qda.class=predict(qda.fit,spam2.init)$class
SPAM<-rep("SPAM",1397)
table(qda.class,SPAM)
mean(qda.class==SPAM)

##4 KNN---------------------------------

init.dftrainx<-init.df[1:1000,c(1,2)]
init.dftrainx<-scale(init.dftrainx)
init.dftrainy<-init.df[1:1000,7]

set.seed(1)
hardham.inits<-scale(hardham.init)
init.testx<-hardham.inits[,c(1,2)]
knn.pred<-knn(init.dftrainx,init.testx,init.dftrainy)
HAM<-rep("HAM",249)
table(knn.pred,HAM)
mean(knn.pred=="HAM")

set.seed(1)
hardham2.inits<-scale(hardham2.init)
init.testx<-hardham2.inits[,c(1,2)]
knn.pred<-knn(init.dftrainx,init.testx,init.dftrainy)
HAM<-rep("HAM",248)
table(knn.pred,HAM)
mean(knn.pred=="HAM")

set.seed(1)
easyham2.inits<-scale(easyham2.init)
init.testx<-easyham2.inits[,c(1,2)]
knn.pred<-knn(init.dftrainx,init.testx,init.dftrainy)
HAM<-rep("HAM",1400)
table(knn.pred,HAM)
mean(knn.pred=="HAM")

set.seed(1)
spam2.inits<-scale(spam2.init)
init.testx<-spam2.inits[,c(1,2)]
knn.pred<-knn(init.dftrainx,init.testx,init.dftrainy)
SPAM<-rep("SPAM",1397)
table(knn.pred,SPAM)
mean(knn.pred=="SPAM")

##.---end

