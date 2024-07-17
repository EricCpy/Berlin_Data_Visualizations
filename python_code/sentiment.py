from germansentiment import SentimentModel
import pandas as pd

df_airbnb = pd.read_csv('../data/airbnb/March_2024/reviews_de.csv')
df_airbnb.info()

model = SentimentModel()

def predict_sentiments(row):
    if 'language' not in row or row['language'] =='de':
        classes, probabilities = model.predict_sentiment([row['comments']], output_probabilities = True)
        return pd.Series({
            'sentiment': classes[0], 
            'positive_sentiment': probabilities[0][0][1], 
            'negative_sentiment': probabilities[0][1][1], 
            'neutral_sentiment': probabilities[0][2][1]
        })
    else:
        sentiments = sia.polarity_scores(row['comments'])
        sentiment_label = max(sentiments, key=lambda x: sentiments[x] if x in ['pos', 'neg', 'neu'] else -1)
        sentiment_map = {'pos': 'positive', 'neg': 'negative', 'neu': 'neutral'}

        return pd.Series({
            'sentiment': sentiment_map[sentiment_label], 
            'positive_sentiment': sentiments['pos'], 
            'negative_sentiment': sentiments['neg'], 
            'neutral_sentiment': sentiments['neu']
        }) 
    
df_airbnb[['sentiment', 'positive_sentiment', 'negative_sentiment', 'neutral_sentiment']] = df_airbnb['comments'].apply(predict_sentiments)
df_airbnb[['id', 'sentiment', 'positive_sentiment', 'negative_sentiment', 'neutral_sentiment']].to_csv("../data/airbnb/March_2024/sentiments_de.csv", index=False)
