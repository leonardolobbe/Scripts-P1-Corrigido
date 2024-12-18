require(data.table)
require(tidyverse)

dados <- read.csv("C:\\Users\\levan\\Documents\\Aulas em R\\chocolate.csv.gz")

paises_produz <- dados %>% 
  summarise(num_paises = n_distinct(origem_cacau))

paises_produz

tres_ingredientes <- dados %>% 
  filter(as.numeric(str_extract(ingredientes, "^\\d+")) >= 3) %>% 
  summarise(num_chocolates = n())

tres_ingredientes


cinco_ingredientes <- dados %>% 
  filter(as.numeric(str_extract(ingredientes, "^\\d+")) == 5) %>% 
  summarise(num_ingredientes = n())

cinco_ingredientes


quatro_caracteristicas <- dados %>% 
  mutate(num_caracteristicas = str_count(caracteristicas, ",") + 1) %>% 
  filter(num_caracteristicas >= 4) %>% 
  summarise(num_chocolates = n())


quatro_caracteristicas


sal_composicao <- dados %>% 
  filter(str_detect(ingredientes, "Sa")) %>% 
  summarise(num_sal = n())

sal_composicao 


baunilha_composicao <- dados %>% 
  filter(str_detect(ingredientes, "V")) %>% 
  summarise(num_baunilha = n())

baunilha_composicao


banilha_lecitina <- dados %>% 
  filter(str_detect(ingredientes, "V") & str_detect(ingredientes, "L")) %>% 
  summarise(num_composicao = n())

banilha_lecitina




art <- fread("C:\\Users\\levan\\Documents\\Aulas em R\\Art.csv.gz")
moma <- fread("C:\\Users\\levan\\Documents\\Aulas em R\\Art_Moma.csv.gz")


moma_media_ano <- moma %>% 
  group_by(year) %>% 
  summarise(media_ano = mean(moma_count_to_year)) %>% 
  arrange(desc(media_ano))

print(moma_media_ano, n = 22)

whitney_media_ano <- moma %>% 
  group_by(year) %>% 
  summarise(media_ano = mean(whitney_count_to_year)) %>% 
  arrange(desc(media_ano))

print(whitney_media_ano, n = 22)


juncao <- moma %>%
  inner_join(art %>% select(artist_unique_id, artist_race_nwi, artist_name),
             by = c("artist_unique_id" = "artist_unique_id"))


moma_media_ano_nwi <- juncao %>% 
  filter(artist_race_nwi == "Non-White") %>% 
  group_by(year) %>% 
  summarise(media_ano = mean(moma_count_to_year)) %>% 
  arrange(desc(media_ano))

print(moma_media_ano_nwi, n = 22)


whitney_media_ano_nwi <- juncao %>% 
  filter(artist_race_nwi == "Non-White") %>% 
  group_by(year) %>% 
  summarise(media_ano = mean(whitney_count_to_year)) %>% 
  arrange(desc(media_ano))

print(whitney_media_ano_nwi)



quatro_artistas <- juncao %>%
  group_by(artist_name) %>% 
  summarise(mais_apresenta = sum(moma_count_to_year)) %>% 
  arrange(desc(mais_apresenta)) %>% 
  slice_head(n = 4)


quatro_artistas



genero_artistas <- art %>% 
  group_by(artist_gender) %>% 
  summarise(genero = n()) 

genero_artistas



nacionalidades_top <- art %>% 
  group_by(artist_nationality) %>% 
  summarise(nacionalidades = n()) %>% 
  arrange(desc(nacionalidades)) %>% 
  slice_head(n = 5)

nacionalidades_top


cada_livro_moma <- juncao %>% 
  filter(moma_count_to_year > 0) %>% 
  group_by(book) %>% 
  summarise(apresenta_moma = n_distinct(artist_name)) %>% 
  arrange(desc(apresenta_moma))

cada_livro_moma


cada_livro_whitney <- juncao %>% 
  filter(whitney_count_to_year > 0) %>% 
  group_by(book) %>% 
  summarise(apresenta_whitney = n_distinct(artist_name)) %>%
  arrange(desc(apresenta_whitney))

cada_livro_whitney              



media_espaco <- juncao %>% 
  group_by(artist_name) %>% 
  summarise(media_espaco = mean(space_ratio_per_page_total)) %>% 
  arrange(desc(media_espaco))

media_espaco






refugiados_pais <- fread("C:\\Users\\levan\\Documents\\Aulas em R\\refugiados_pais.csv.gz")
refugiados <- fread("C:\\Users\\levan\\Documents\\Aulas em R\\refugiados.csv.gz")


# Realizar o left join
tb4 <- refugiados %>%
  left_join(refugiados_pais, by = c("id_origem" = "id")) %>%
  left_join(refugiados_pais, by = c("id_destino" = "id"), suffix = c("_origem", "_destino"))

# Exibir a planilha
print(tb4)



# Filtrar para o ano de 2006
tb4_2006 <- tb4 %>% filter(ano == 2006)

# Criar a matriz de migração [origem, destino]
matriz_migracao_2006 <- tb4_2006 %>%
  group_by(regiao_origem, regiao_destino) %>%
  summarise(total_migrantes = sum(refugiados)) %>%
  pivot_wider(names_from = regiao_destino, values_from = total_migrantes, values_fill = 0)




# Exibir a matriz de migração no formato desejado
print(matriz_migracao_2006)



# Especificar o nome_origem, nome_destino e ano desejados
ano_desejado <- 1972

q4b <- tb4 %>%
  group_by(nome_origem, nome_destino, ano) %>%
  summarise(refugiados = sum(refugiados), .groups = "drop") 

resultado <- q4b %>%
  filter(ano >= ano_desejado) %>%
  group_by(nome_origem, nome_destino) %>%
  summarise(total_refugiados = sum(refugiados))


refugiados_af_can <- resultado %>%
  filter(nome_origem == "Afghanistan", nome_destino == "Canada")
print(refugiados_af_can)


refugiados_paq_can <- resultado %>%
  filter(nome_origem == "Pakistan", nome_destino == "Canada")
print(refugiados_paq_can)





ano_c <- 1965

q4c <- tb4 %>%
  group_by(nome_origem, subregiao_origem, ano) %>%
  summarise(refugiados = sum(refugiados), .groups = "drop") 

q4c <- q4c %>%
  filter(ano == ano_c) %>%
  group_by(nome_origem, subregiao_origem) %>%
  summarise(total_refugiados = sum(refugiados)) %>%
  arrange(desc(total_refugiados))



ano_d <- 1982

q4d <- tb4 %>%
  group_by(nome_destino, ano) %>%
  summarise(refugiados = sum(refugiados), .groups = "drop") %>%
  na.omit()

q4d <- q4d %>%
  filter(ano >= ano_d) %>%
  group_by(nome_destino) %>%
  summarise(total_refugiados = sum(refugiados)) %>%
  arrange(desc(total_refugiados))

q4d






n_refugiados <- 5382652

q4e<- tb4 %>%
  group_by(nome_destino) %>%
  summarise(refugiados = sum(refugiados), .groups = "drop") %>%
  na.omit()

q4e <- q4e %>%
  filter(refugiados >= n_refugiados) %>%
  arrange(desc(refugiados))

cat('Existem', nrow(q4e), 'países que receberam pelo menos', n_refugiados, 'refugiados.\n')



require(tidyverse)
require(readr)
require(dplyr)
require(magrittr)
require(lubridate)
require(DescTools)
require(tidytuesdayR)


Idol <- read_csv("American_Idol.csv")

FastFood <- read_csv('Datafiniti_Fast_Food_Restaurants.csv')


```

```{r}

##1)

Instrumentos <- c(38,56,97,94,38,34,46,99,81,89)
names(Instrumentos) <- c('Violino','Viola','Violoncelo','Contrabaixo','Flauta','Clarinete','Oboé','Trompete','Trombone','Percussão')

LimiteVolume <- c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
names(LimiteVolume) <- names(Instrumentos)

## -----------------------------------------------

raiz_da_media <- Instrumentos %>% ### Verifica a raiz da média dos volumes dos instrumentos
  mean() %>%
  sqrt()

raiz_da_media

## -----------------------------------------------

mediana_limite <- Instrumentos[LimiteVolume] %>%   ### Verifica a mediana dos instrumentos que alcançaram o limite
  median()

mediana_limite

## -----------------------------------------------

maximo_nao_limite <- Instrumentos[!LimiteVolume] %>% ### Verifica o máximo dos instrumentos que não atingiram o limite
  max()

maximo_nao_limite

## -----------------------------------------------

quantia_ajustes <- Instrumentos[Instrumentos<69 & Instrumentos>40] %>% ### Confere quais instrumentos tem nota inferior a 69 E superior a 40
  length() ### Verifica o comprimento para saber quantos se tem.

quantia_ajustes

## -----------------------------------------------

desvio_limite <- Instrumentos[LimiteVolume] %>% ### Verifica o desvio padrão dos instrumentos que atingiram o limite
  sd()

desvio_limite

```
```{r}

##2)


valores <- c(2.3,-4.8,-0.8,-0.9,3.0,4.6,-3.8,4.5,-3.2,0.6,
             -0.3,-0.9,2.1,4.8,1.8,2.4,0.5,1.1,0.9,-0.9,
             3.6,3.9,-2.8,-3.8,-1.5,1.7,-3.5,-3.3,-0.7,-1.1,
             -2.6,-0.4,-2.4,-2.3,1.9,-2.3,-4.9,-4.1,-4.4 ,0.2,
             -0.1,-4.4,2.6,-2.9,-1.5,-0.4,2.9,4.3,1.2,-3.4) ## Armazena os valores pré mudança


ativos <- c('Ativo1','Ativo2','Ativo3','Ativo4','Ativo5','Ativo6','Ativo7','Ativo8','Ativo9','Ativo10') ## Nomes dos ativos

mesnomes <- c('mes1','mes1','mes1','mes1','mes1','mes1','mes1','mes1','mes1','mes1',
              'mes2','mes2','mes2','mes2','mes2','mes2','mes2','mes2','mes2','mes2',
              'mes3','mes3','mes3','mes3','mes3','mes3','mes3','mes3','mes3','mes3',
              'mes4','mes4','mes4','mes4','mes4','mes4','mes4','mes4','mes4','mes4',
              'mes5','mes5','mes5','mes5','mes5','mes5','mes5','mes5','mes5','mes5') ## Nomes dos meses

dados <- data.frame(mesnomes, ativos, valores) 



X <- dados  %>%
  pivot_wider(names_from = ativos, values_from = valores)


valores2 <- c(4.2,4.7,-0.3,0.6,4.5,0.5,0.2 ,-3.3,-3.1,4.3,
              -3.9,1.6,-1.3,4.8,3.9,3.6,0.3,1.3,-1.6,-2.9,
              3.3,4.6,1.0,4.2,-4.3,2.9,4.4,-4.8,0.7,1.9,
              1.2,2.0,0.8,0.1,-3.3,-3.7,-1.7,-0.5,1.2,-0.2,
              1.3,0.5,-2.5,4.4,-3.1,0.7,-4.0,3.9,0.4,-0.9) ##Armazena os valores pós mudança
dados2 <- data.frame(mesnomes, ativos, valores2)         



Y <- dados2 %>%
  pivot_wider(names_from = ativos,values_from = valores2) ##Formata para pivot_wider

## Retira a coluna string

valoresativos <- X 
valoresativos$mesnomes <- NULL

valoresativos2 <- Y
valoresativos2$mesnomes <- NULL

## -----------------------------------------------


maior_media <- valoresativos %>% ## Calcula a maior média das colunas
  colMeans() %>%
  sort(decreasing = TRUE) %>%
  head(1)

maior_media

## -----------------------------------------------

matrizY <- as.matrix(valoresativos2) ## Transforma em matriz

matrizY

matriz_ativos <- matrizY %*% t(matrizY) ## Produto 

soma_diagonais <- matriz_ativos %>% ## Faz a soma das diagonais
  diag() %>%
  sum()

soma_diagonais


## -----------------------------------------------


matrizX <- as.matrix(valoresativos) ##Armazena em matriz


matriz_mudança <- abs(matrizX - matrizY) ## Mudança absoluta


maior_media_dif <- matriz_mudança %>% 
  colMeans() %>%
  sort(decreasing = TRUE) %>%
  head(1)

maior_media_dif

## -----------------------------------------------

Maiores <- matriz_mudança %>% ## Pega os 5 maiores ativos
  colMeans() %>%
  sort(decreasing = TRUE) %>%
  head(5) %>%
  names()

deteccao <- (colnames(matriz_mudança) %in% Maiores)  ## Detecta quais colunas serão necessárias

valores_matriz_final <- matriz_mudança[deteccao] ## Valores para a matriz

MatrizFinal <- matrix( ## Formando a matriz
  valores_matriz_final,
  nrow = 5,
  ncol = length(Maiores)
)

determinante <- det(MatrizFinal) 

determinante


```

```{r}

FastFood <- FastFood %>%
  mutate(name = tolower(name))%>%
  mutate(name = str_trim(name, side = 'both')) %>%
  mutate(name = str_squish(name)) %>%
  mutate(name = str_replace(name, '\'s', 's'))%>%
  mutate(name = str_replace(name, '\'S','S')) %>%
  mutate(name = str_replace(name, 'JR.','JR')) %>%
  mutate(name = str_replace(name, '-',' '))

## -----------------------------------------------

Quantia_provincias <- FastFood %>%
  filter(province == 'MI' | province == 'OR' | province == 'TX' | province == 'WA') %>%
  count()

Quantia_provincias

## -----------------------------------------------

Valor_maximo <- FastFood %>%
  filter(province == 'MI' | province == 'OR' | province == 'TX' | province == 'WA') %>%
  group_by(province) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(1)

Valor_maximo

## -----------------------------------------------


Estado_consumidor <- FastFood %>%
  filter(name %like% 'charleys grilled subs') %>%
  group_by(province) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(1)

Estado_consumidor

## -----------------------------------------------



MN_2016 <- FastFood %>%
  mutate(ano = format(as.Date(dateAdded), "%Y")) %>%
  filter(ano == 2016 & province == 'MN') %>%
  count()

MN_2016


## -----------------------------------------------


rede_mais_expandida <- FastFood %>%
  mutate(ano = format(as.Date(dateAdded), "%Y")) %>%
  filter(ano == 2015)%>%
  group_by(name) %>%
  summarise(n = n()) %>%
  arrange(desc(n))  %>%
  head(1)

rede_mais_expandida

```

```{r}



tuesdata <- tidytuesdayR::tt_load(2021, week = 48)



escritores <- tuesdata$writers
diretores <- tuesdata$directors 
episodios <- tuesdata$episodes
imdb <- tuesdata$imdb

## -----------------------------------------------

dire_escrit <- inner_join(escritores, diretores, by = 'story_number')

quantia_dupla <- dire_escrit %>%
  filter(director == 'Adam Smith' & writer == 'Steven Moffat')%>%
  count()

quantia_dupla

## -----------------------------------------------


escritor_ep <- inner_join(escritores, episodios, by = 'story_number')

quantia_chris_2020 <- escritor_ep %>%
  mutate(ano = format(as.Date(first_aired), "%Y")) %>%
  filter(ano == 2020 & writer == 'Chris Chibnall') %>%
  count()

quantia_chris_2020

## -----------------------------------------------

Gareth_roberts_anos <- escritor_ep %>%
  mutate(ano = format(as.Date(first_aired), '%Y')) %>%
  filter(writer == 'Gareth Roberts') %>%
  group_by(ano) %>%
  summarise(n = n()) %>%
  count()

Gareth_roberts_anos

## -----------------------------------------------

Quantia_Gareth <- escritores %>%
  filter(writer %like% 'Gareth Roberts') %>%
  count()

Quantia_Gareth

## -----------------------------------------------

diretor_ep <- inner_join(diretores, episodios, by = 'story_number')

Paul_Duracao <- diretor_ep %>%
  filter(director %like% 'Paul Murphy')%>%
  pull(duration)%>%
  mean()

Paul_duracao
```



```{r}

Idol %>%
  filter(season == 'Season_18') %>%
  filter(str_detect(song, 'Love')) %>%
  summarise(n = n_distinct(artist))

Idol %>%
  filter(str_detect(artist, '^M')) %>%
  group_by(artist) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(1)


Idol


## -----------------------------------------------

nao_eliminados <- Idol %>%
  filter(season == 'Season_11' & order > 2 & result != 'Eliminated') %>%
  count() ## Conta quantos existem dadas as condições descritas.

nao_eliminados 


## -----------------------------------------------

quantia_total <- Idol %>% ## Quantia total na temporada 15
  filter(season == 'Season_15') %>%
  count()


quantia_eliminada <- Idol %>%  ## Quantia eliminada
  filter(season == 'Season_15' & (artist == 'Bee Gees' | artist == 'Elton John') & result == 'Eliminated') %>%
  count()

proporcao <- quantia_eliminada/quantia_total

proporcao


## -----------------------------------------------


Musica_famosa <- Idol %>%  ## Filtra para nomes sem a letra E, maiuscula ou minúscula
  filter( ! str_detect(contestant, '(E|e)')) %>%
  group_by(song) %>%
  summarise(quantia = n_distinct(contestant)) %>% ## Conta competidores distintos
  arrange(desc(quantia)) %>%
  head(1)

Musica_famosa