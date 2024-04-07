# Import libraries
import pandas as pd
import openai
import numpy as np
import openpyxl
import chardet

# Get OpenAI secret key
open_ai_key = open('openai_key.txt','r').read()
openai.api_key = open_ai_key

# Get dataset with correct encoding
#with open('tweets_filtered_BR_PT.csv', 'rb') as f:
#    result = chardet.detect(f.read())

df = pd.read_csv("tweets_filtered_BR_PT.csv", encoding='latin-1').reset_index().iloc[:,2:]
df = pd.read_csv("tweets_filtered_BR_PT_gpt4turbo.csv", encoding='latin-1').reset_index().iloc[:,0:]

print(df.info())

print(len(df))

# Getting prompts 
prompt_1 = "Qual é a atitude do autor em relação às vacinas expresso pelo usuário do texto do tweet delimitado por crases triplos? \
Use uma das seguintes palavras: neutro, negativo, positivo. Inclua uma explicação para a seleção do sentimento. \
```{TEXTO_DO_TWEET_AQUI}```"

prompt_2 = "Qual é a atitude do autor em relação às vacinas expresso pelo usuário do texto do tweet delimitado por crases triplos? \
Use uma das seguintes palavras: neutro, negativo, positivo. Inclua uma explicação para a seleção do sentimento. \
\
Exemplos de sentimento positivo: \
* Por favor não se esqueçam de tomar a vacina! \
* Vacina contra o câncer da pele é testada com sucesso. \
\
Examplos de sentimento neutro: \
* Fui tomar vacina ontem. \
* A nova campanha de vacinação contra a gripe começou hoje. \
\
Examplos de sentimento negativo: \
* Tomei vacina contra a gripe e meu braço ta doendo. \
* Estudo mostra efeitos colaterais sérios da nova vacina contra a gripe. \
\
```{TEXTO_DO_TWEET_AQUI}```"

prompt_2b = "Qual é a atitude do autor em relação às vacinas expresso pelo usuário do texto do tweets delimitados por crases triplos? Existem vários tweets separados por por três asteriscos.  \
Para cada um desses tweets, use apenas uma das seguintes palavras: neutro, negativo, positivo.  \
\
Exemplos de sentimento positivo: \
* Por favor não se esqueçam de tomar a vacina! \
* Vacina contra o câncer da pele é testada com sucesso. \
\
Examplos de sentimento neutro: \
* Fui tomar vacina ontem. \
* A nova campanha de vacinação contra a gripe começou hoje. \
\
Examplos de sentimento negativo: \
* Tomei vacina contra a gripe e meu braço ta doendo. \
* Estudo mostra efeitos colaterais sérios da nova vacina contra a gripe. \
\
```{TEXTO_DO_TWEET_AQUI}``` "

prompt_3 = "Give me the sentiment regarding vaccination expressed by the user of the tweet text delimited by triple backticks. \
Use one of the following words: neutral, negative, positive. Include an explanation for the selection of the sentiment. \
```{TWEET_TEXT_HERE}```"


num_rows = 10
n_tweets = []
combined_text = []
for i in range(0, len(df), num_rows):
    n_tweets = "***".join(df['text'].iloc[i:i+10])
    combined_text.append(n_tweets)

combined_text_df = pd.DataFrame(combined_text)

# Loop for combined texts df tmux 0
id_gpt = []
sent_gpt = []
text = []

for i in range(73743, 80000):
    prompt_i = prompt_2b.replace('TEXTO_DO_TWEET_AQUI', combined_text_df.iloc[i,0])
    response = openai.ChatCompletion.create(
            model = "gpt-4-0125-preview",
            messages = [{"role": "user", "content": prompt_i}],
            temperature = 0.8,
            top_p = 1,
            frequency_penalty = 0,
            #timeout = 300.0,
            presence_penalty = 0
            )
    text.append(combined_text_df.iloc[i,0])
    sent_gpt.append(response.choices[0].message.content)
    #id_gpt.append(combined_text_df.iloc[i,1])
    print(i, len(combined_text_df)-i) # Check how many tweets are left

df_gpt = pd.DataFrame(list(zip(text, sent_gpt)),
        columns = ['text', 'sentiment_gpt'])

df_gpt.to_csv('gpt_sentiment_prompt2b_all_tweets_0_79999.csv')  

# Loop for combined texts df tmux 1 (running)
id_gpt = []
sent_gpt = []
text = []

for i in range(161935, 170000):
    prompt_i = prompt_2b.replace('TEXTO_DO_TWEET_AQUI', combined_text_df.iloc[i,0])
    response = openai.ChatCompletion.create(
            model = "gpt-4-0125-preview",
            messages = [{"role": "user", "content": prompt_i}],
            temperature = 0.8,
            top_p = 1,
            frequency_penalty = 0,
            #timeout = 300.0,
            presence_penalty = 0
            )
    text.append(combined_text_df.iloc[i,0])
    sent_gpt.append(response.choices[0].message.content)
    #id_gpt.append(combined_text_df.iloc[i,1])
    print(i, len(combined_text_df)-i) # Check how many tweets are left

df_gpt = pd.DataFrame(list(zip(text, sent_gpt)),
        columns = ['text', 'sentiment_gpt'])

df_gpt.to_csv('gpt_sentiment_prompt2b_all_tweets_100000_161934.csv')  

# Loop for tmux 2 (completed)
combined_text_df = pd.read_csv("tweets_filtered_BR_PT_gpt4turbo_combined.csv").reset_index()
combined_text_df = combined_text_df[['0']]  

id_gpt = []
sent_gpt = []
text = []

for i in range(212471, 216881):
    prompt_i = prompt_2b.replace('TEXTO_DO_TWEET_AQUI', combined_text_df.iloc[i,0])
    response = openai.ChatCompletion.create(
            model = "gpt-4-0125-preview",
            messages = [{"role": "user", "content": prompt_i}],
            temperature = 0.8,
            top_p = 1,
            frequency_penalty = 0,
            #timeout = 300.0,
            presence_penalty = 0
            )
    text.append(combined_text_df.iloc[i,0])
    sent_gpt.append(response.choices[0].message.content)
    #id_gpt.append(combined_text_df.iloc[i,1])
    print(i, len(combined_text_df)-i) # Check how many tweets are left

df_gpt = pd.DataFrame(list(zip(text, sent_gpt)),
        columns = ['text', 'sentiment_gpt'])

df_gpt.to_csv('gpt_sentiment_prompt2b_all_tweets_200000_216880.csv')  

# Loop for tmux 3 (completed)
combined_text_df = pd.read_csv("tweets_filtered_BR_PT_gpt4turbo_combined.csv").reset_index()
combined_text_df = combined_text_df[['0']]  

id_gpt = []
sent_gpt = []
text = []

for i in range(80426, 100000):
    prompt_i = prompt_2b.replace('TEXTO_DO_TWEET_AQUI', combined_text_df.iloc[i,0])
    response = openai.ChatCompletion.create(
            model = "gpt-4-0125-preview",
            messages = [{"role": "user", "content": prompt_i}],
            temperature = 0.8,
            top_p = 1,
            frequency_penalty = 0,
            #timeout = 300.0,
            presence_penalty = 0
            )
    text.append(combined_text_df.iloc[i,0])
    sent_gpt.append(response.choices[0].message.content)
    #id_gpt.append(combined_text_df.iloc[i,1])
    print(i, len(combined_text_df)-i) # Check how many tweets are left

df_gpt = pd.DataFrame(list(zip(text, sent_gpt)),
        columns = ['text', 'sentiment_gpt'])

df_gpt.to_csv('gpt_sentiment_prompt2b_all_tweets_80000_99999.csv')  


# Prompt 2   
# Setting up the columns for the database with the sentiment
id_gpt = []
sent_gpt = []
text = []

## Loop to get the sentiment from mixtral
# tmux 0 
for i in range(1009000, 1012000):
    prompt_i = prompt_2.replace('TEXTO_DO_TWEET_AQUI', df.iloc[i,0])
    response = openai.ChatCompletion.create(
            model = "gpt-4",
            messages = [{"role": "user", "content": prompt_i}],
            temperature = 0.8,
            top_p = 1,
            frequency_penalty = 0,
            presence_penalty = 0
            )
    text.append(df.iloc[i,0])
    sent_gpt.append(response.choices[0].message.content)
    id_gpt.append(df.iloc[i,1])
    print(i, "prompt2") # Check how many tweets are left

## Save the sentiment in a file and export it
df_gpt_2 = pd.DataFrame(list(zip(id_gpt, text, sent_gpt)),
        columns = ['id', 'text', 'sentiment_gpt'])

#df_gpt_2['prompt'] = 2

print(df_gpt_2.info())
print(df_gpt_2.head())
print(len(df_gpt_2))

df_gpt_2.to_csv('gpt_sentiment_prompt2_all_tweets_1000000_1011999.csv')     

# tmux 1
for i in range(106000, 109000):
    prompt_i = prompt_2.replace('TEXTO_DO_TWEET_AQUI', df.iloc[i,0])
    response = openai.ChatCompletion.create(
            model = "gpt-4",
            messages = [{"role": "user", "content": prompt_i}],
            temperature = 0.8,
            top_p = 1,
            frequency_penalty = 0,
            presence_penalty = 0
            )
    text.append(df.iloc[i,0])
    sent_gpt.append(response.choices[0].message.content)
    id_gpt.append(df.iloc[i,1])
    print(i, "prompt2") # Check how many tweets are left

## Save the sentiment in a file and export it
df_gpt_2 = pd.DataFrame(list(zip(id_gpt, text, sent_gpt)),
        columns = ['id', 'text', 'sentiment_gpt'])

#df_gpt_2['prompt'] = 2

print(df_gpt_2.info())
print(df_gpt_2.head())
print(len(df_gpt_2))

df_gpt_2.to_csv('gpt_sentiment_prompt2_all_tweets_100000_108999.csv')     

# tmux 2
for i in range(6000, 9000):
    prompt_i = prompt_2.replace('TEXTO_DO_TWEET_AQUI', df.iloc[i,0])
    response = openai.ChatCompletion.create(
            model = "gpt-4",
            messages = [{"role": "user", "content": prompt_i}],
            temperature = 0.8,
            top_p = 1,
            frequency_penalty = 0,
            presence_penalty = 0
            )
    text.append(df.iloc[i,0])
    sent_gpt.append(response.choices[0].message.content)
    id_gpt.append(df.iloc[i,1])
    print(i, "prompt2") # Check how many tweets are left

## Save the sentiment in a file and export it
df_gpt_2 = pd.DataFrame(list(zip(id_gpt, text, sent_gpt)),
        columns = ['id', 'text', 'sentiment_gpt'])

#df_gpt_2['prompt'] = 2

print(df_gpt_2.info())
print(df_gpt_2.head())
print(len(df_gpt_2))

df_gpt_2.to_csv('gpt_sentiment_prompt2_all_tweets_0_8999.csv')     

# tmux 3
for i in range(2006000, 2009000):
    prompt_i = prompt_2.replace('TEXTO_DO_TWEET_AQUI', df.iloc[i,0])
    response = openai.ChatCompletion.create(
            model = "gpt-4",
            messages = [{"role": "user", "content": prompt_i}],
            temperature = 0.8,
            top_p = 1,
            frequency_penalty = 0,
            presence_penalty = 0
            )
    text.append(df.iloc[i,0])
    sent_gpt.append(response.choices[0].message.content)
    id_gpt.append(df.iloc[i,1])
    print(i, "prompt2") # Check how many tweets are left

## Save the sentiment in a file and export it
df_gpt_2 = pd.DataFrame(list(zip(id_gpt, text, sent_gpt)),
        columns = ['id', 'text', 'sentiment_gpt'])

#df_gpt_2['prompt'] = 2

print(df_gpt_2.info())
print(df_gpt_2.head())
print(len(df_gpt_2))

df_gpt_2.to_csv('gpt_sentiment_prompt2_all_tweets_2000000_2008999.csv')     
