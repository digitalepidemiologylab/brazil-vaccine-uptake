# Import libraries
import pandas as pd
from llama_index.llms.ollama import Ollama

# Getting mistral and dataset
llm = Ollama(
        base_url = "http://iccluster133.iccluster.epfl.ch:12923",
        model="mixtral", 
        temperature = 0.8,
        request_timeout = 300.0
        )

df = pd.read_csv("paho_tweets_filtered.csv", encoding='latin-1').reset_index()
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

prompt_3 = "Give me the sentiment regarding vaccination expressed by the user of the tweet text delimited by triple backticks. \
Use one of the following words: neutral, negative, positive. Include an explanation for the selection of the sentiment. \
```{TWEET_TEXT_HERE}```"

# Prompt 1     
# Setting up the columns for the database with the sentiment
id_mixtral = []
sent_mixtral = []
text = []

## Loop to get the sentiment from mixtral

for i in range(0, len(df)):
    prompt_i = prompt_1.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_mixtral.append(response)
    id_mixtral.append(df.iloc[i,1])
    print(i, "prompt1") # Check how many tweets are left

## Save the sentiment in a file and export it
df_mixtral_1 = pd.DataFrame(list(zip(id_mixtral, text, sent_mixtral)),
        columns = ['id', 'text', 'sentiment_mixtral'])

df_mixtral_1['prompt'] = 1

print(df_mixtral_1.info())
print(df_mixtral_1.head())
print(len(df_mixtral_1))

df_mixtral_1.to_csv('mixtral_sentiment_paho_prompt1.csv')

# Prompt 2   
# Setting up the columns for the database with the sentiment
id_mixtral = []
sent_mixtral = []
text = []

## Loop to get the sentiment from mixtral

for i in range(0, len(df)):
    prompt_i = prompt_2.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_mixtral.append(response)
    id_mixtral.append(df.iloc[i,1])
    print(i, "prompt2") # Check how many tweets are left

## Save the sentiment in a file and export it
df_mixtral_2 = pd.DataFrame(list(zip(id_mixtral, text, sent_mixtral)),
        columns = ['id', 'text', 'sentiment_mixtral'])

df_mixtral_2['prompt'] = 2

print(df_mixtral_2.info())
print(df_mixtral_2.head())
print(len(df_mixtral_2))

df_mixtral_2.to_csv('mixtral_sentiment_prompt2.csv')     

# Prompt 3   
# Setting up the columns for the database with the sentiment
id_mixtral = []
sent_mixtral = []
text = []

## Loop to get the sentiment from mixtral

for i in range(0, len(df)):
    prompt_i = prompt_3.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_mixtral.append(response)
    id_mixtral.append(df.iloc[i,1])
    print(i, "prompt3") # Check how many tweets are left

## Save the sentiment in a file and export it
df_mixtral_3 = pd.DataFrame(list(zip(id_mixtral, text, sent_mixtral)),
        columns = ['id', 'text', 'sentiment_mixtral'])

df_mixtral_3['prompt'] = 3

print(df_mixtral_3.info())
print(df_mixtral_3.head())
print(len(df_mixtral_3))

df_mixtral_3.to_csv('mixtral_sentiment_prompt3.csv')     