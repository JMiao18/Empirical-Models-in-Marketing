# -*- coding: utf-8 -*-


"""
Spyder Editor
Assignment for Empirical Models in Marketing
Programmer: Jin Miao (JMiao18)
Date: 04/10/2018
"""

import matplotlib	
import pandas as pd
import nltk

data = pd.read_csv("yelp_homework.csv", index_col = 0) #read data
for ix, name in enumerate(data.columns):
    print (ix, name)

## Remove the User Link
data = data.drop(data.columns[[12]],axis =1) #remove userlink in order to keep reviewers anonymous
data['date'] = pd.to_datetime(data['date'])
data.head()

## Collect all the reviews
review = data.review.iloc[:]
rw = review.to_string(index=False)
print(rw)

## Tokenize the Reviews
from nltk.tokenize import word_tokenize
tokens = word_tokenize(rw)

## Use Regular Expression to get rid of pnuctuation signs
import re
token = re.findall(r'[a-zA-Z]{3,}', rw)

## Remove Stop Words
stop = pd.read_csv("yelp.stop", names = ["words"])
stop = stop['words'].values.tolist()

updatedreview = [element for element in token if element.lower() not in stop]
" ".join(updatedreview)

## Ad-hoc Check of Performance: Nice
from collections import Counter
count = Counter()
for text in updatedreview:
    token_list = Counter(re.findall(r"[a-zA-Z]+",text.lower()))
    for token_ in token_list:
        count[token_] +=1
        
count.most_common(30)

## Stemming is skipped

from sklearn.feature_extraction.text import CountVectorizer
vec = CountVectorizer(decode_error = 'ignore', token_pattern = "[a-zA-Z]{3,}", lowercase= True, stop_words = stop, max_features =800)

sparse_matrix = vec.fit_transform(data.review)
sparse_matrix.toarray()

Term_document = pd.DataFrame(sparse_matrix.toarray(), columns = vec.get_feature_names())
Term_document

## TF-IDF
from sklearn.feature_extraction.text import TfidfVectorizer
vec = TfidfVectorizer(decode_error = 'ignore', token_pattern = "[a-zA-Z]{3,}",lowercase= True,stop_words = stop, max_features =800)
sparse_matrix = vec.fit_transform(data.review)
pd.DataFrame(sparse_matrix.toarray(), columns = vec.get_feature_names())

## Latent Dirichlet Allication
from sklearn.decomposition import LatentDirichletAllocation
from sklearn.model_selection import train_test_split
vec = CountVectorizer( token_pattern= "[a-zA-Z']{3,}",decode_error ='ignore', lowercase=True, stop_words = stop, max_features = 800)
sparse_matrice = vec.fit_transform(data.review)
X_train, X_test = train_test_split(sparse_matrice, test_size=0.20, random_state=33) 

lda = LatentDirichletAllocation(n_components = 15, max_iter=450, topic_word_prior= 0.01, doc_topic_prior=1.0, learning_method='batch', n_jobs= 1, random_state=275)    
topic_components = lda.fit_transform(X_train)

## Document Topic Distribution
topic_components[:20]

def print_top_words(model, feature_names, n_top_words):
    for topic_idx, topic in enumerate(model.components_):
        print("Topic #%d:" % topic_idx)
        print(" ".join([feature_names[i]
                        for i in topic.argsort()[:-n_top_words - 1:-1]]))
    print()
    
print_top_words(lda,vec.get_feature_names(), 10)

## Finding the oprimal number of topics
lda.perplexity(X_test)

