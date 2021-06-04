# Spam-Detection-2017
Homework for the Machine Learning course  - use Logistic, LDA, QDA, KNN to filter spam

## Introduction

The analysis processes and results are explained in ML_Spam_Detection_Report.pdf.   
There are Annotations in Codes to make it more understandable .

Data is on [SpamAssassin public corpus](https://spamassassin.apache.org/old/publiccorpus/).   
We only use a portion of it.

## Structure
1. data directory  
  - Contains eash ham/ hard ham/ spam folders  
    - Each type are splitted in two halves, the name containing 2 is used for testing.
    - Each folder contains original unstructed mails. Each mail is a document.

2. ML_Spam_Detection.R   
  - Create a TermDocumentMatrix (TDM) from the corpus of each type's mails.
 
  - Compute each word's occurence & density in easy ham and spam 
      - Occurrence (Number of documents contain the word)
      - Density (Propotion of frequency of the word to the sum of frequencies of all words)
     
  - Find 10 words with highest occurrences in spam and easy ham.
  - Find words in top-10 words appearing in both spam and easy ham.
    If words appear in both criterions, then do not consider them
  - Use remain six words as differentiate features

## Notice
This is the first hw of the first Machine Learning course I took.  
I tried using recently learned classification ML models to detect spam,
and reached effective results.

