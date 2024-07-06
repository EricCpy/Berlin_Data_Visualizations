from transformers import MarianMTModel, MarianTokenizer
import pandas as pd

df_airbnb = pd.read_csv('../data/airbnb/March_2024/review_comments_fr_2024.csv')
df_airbnb.info()

def translate(text, source_language, target_language):
    model_name = f'Helsinki-NLP/opus-mt-{source_language}-{target_language}'
    tokenizer = MarianTokenizer.from_pretrained(model_name)
    model = MarianMTModel.from_pretrained(model_name)

    inputs = tokenizer.encode(text, return_tensors="pt")
    outputs = model.generate(inputs, num_beams=4, max_length=50, early_stopping=True)
    translated_text = tokenizer.decode(outputs[0], skip_special_tokens=True)

    return translated_text
  
# input_text = "Hello, how are you?"
# source_language = 'en'
# target_language = 'es'
# translated_text = translate(input_text, source_language, target_language)
# print(translated_text)

df_airbnb[['german_text']] = df_airbnb['text'].apply(translate, source_language = 'fr', target_language = 'de')
df_airbnb[['text', 'german_text']].to_csv("../data/airbnb/March_2024/translated_reviews_fr_2024.csv", index=False)
