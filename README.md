library(readxl)
library (tidyverse)
library(clipr)
library(dplyr)
library (tibble)
library (sf) ### necessário para o GEOBR
library (geobr)
library(readr)
library (openxlsx)
install.packages("openxlsx")

### Os arquivos do CONARE, pelo menos para o período 2010 a 2022 estão no formato UTF-8. Isso precisou
### ser mantido para garantir a correta acentuação e uso de caracteres especiais. Para confirmar essa informação,
### eu abrir o .csv no NotePad++ (tive que fazer o download), conferir que é UTF-8, salvar como .csv de novo e rodar
### aqui

##Downloado do microdado através do site do OBMigra

Conare2010a2024 <- read.csv ("D:/Users/sophi/Documents/PosDoc/CONARE/CONARE_Div_2010_2024.csv",
                             sep = ";",fileEncoding = "Windows-1252")



## Não precisa rodar o código abaixo
Conare2023 %>% select(ANO_PROCESSO, ANO_DECISAO) %>%
  distinct() %>%
  arrange(ANO_PROCESSO, ANO_DECISAO)  ## comando para ver os registros da coluna ano_processo e ano_decisão

### Conare 2010 a 2022 registro mais antigo 1994

### Conare 2023 registro mais antigo 1900, 1919


## APENAS PARA CONARE 2023 QUE TEM REGISTROS MUITO ANTIGOS

# Inspecionar os valores únicos na coluna ANO_processo
unique(Conare2023$ANO_PROCESSO)

# Limpar a coluna ANO_processo
Conare2023 <- Conare2023 %>%
  mutate(
    ANO_PROCESSO = trimws(ANO_PROCESSO), # Remove espaços extras no início e no fim
    ANO_PROCESSO = ifelse(ANO_PROCESSO == "", NA, ANO_PROCESSO) # Substitui células vazias por NA
  )

# Contar os registros NO FRAME 2023 QUE SÃO MUITO VELHOS

agrupamento1 <- Conare2023 %>%
  mutate(
    condicao = case_when(
      is.na(ANO_PROCESSO) ~ "Vazio (NA)",               # Sem preenchimento (NA)         
      ANO_PROCESSO == "#VALOR!" ~ "#VALOR!",            # Preenchido com #VALOR!
      suppressWarnings(as.numeric(ANO_PROCESSO)) < 1960 ~ "Menor que 1960", # Data menor que 1960
      TRUE ~ "Outros"                                   # Caso contrário
    )
  ) %>%
  group_by(condicao) %>%
  summarise(quantidade = n(), .groups = "drop")

# Filtrar e excluir registros com as condições específicas
# Filtrar e excluir registros com as condições específicas
Conare2023_filtrado2 <- Conare2023 %>%
  filter(
    # Manter os registros que:
    is.na(ANO_PROCESSO) |                        # São NA (ausentes)
      !(ANO_PROCESSO == "#VALOR!" |                # Não são "#VALOR!"
          suppressWarnings(as.numeric(ANO_PROCESSO)) < 1960) # E não são menores que 1960
  )
# Contar os registros no dataframe filtrado
registros_restantes <- nrow(Conare2023_filtrado1)


print(nrow(Conare2023))         # Total de registros originais
print(nrow(Conare2023_filtrado2)) # Total de registros após o filtro

# Exibir os resultados
print(agrupamento1)         # Contagem inicial agrupada
print(registros_restantes) # Quantidade de registros após filtrar



#### OUTRA COISA- NÃO PRECISA RODAR
valores_unicos <- unique (Conare2023$TIPO_PROCESSO) # pra ver quantos registros tem na variável

print(valores_unicos)

frequencia_tipos <- table (Conare2023$TIPO_PROCESSO)

print (frequencia_tipos)

total_registros <- sum (frequencia_tipos)

print (total_registros)

### Arrumar a coluna sexo para ficar apenas letras e não 01 M. E também na fundamentação e sexo deixar o NE como
### NÃO ESPECIFICADO

Conare2010a2024_filtro <- Conare2010a2024 %>% mutate(FUNDAMENTACAO = ifelse
                            (FUNDAMENTACAO == "NE", "Não Especificado", FUNDAMENTACAO))
                             
Conare2010a2024_filtro <- Conare2010a2024_filtro %>% mutate (SEXO = ifelse 
                                     (SEXO == "03", "Não Especificado", SEXO)) 

Conare2010a2024_filtro <- Conare2010a2024_filtro %>%  mutate(SEXO = gsub("^[0-9]+\\s", "", SEXO))

Conare2010a2024_filtro <- Conare2010a2024_filtro %>% mutate (CIDADE_edit= ifelse
            (CIDADE_edit == "NE", "Não Especificado", CIDADE_edit))


Conare2010a2024_filtro <- Conare2010a2024_filtro %>%
  mutate(SEXO = case_when(
    SEXO == 1 ~ "Masculino",
    SEXO == 2 ~ "Feminino",
    SEXO == 3 ~ "Não especificado",
    TRUE ~ NA_character_  # Para tratar valores fora do esperado
  ))



### Fazendo o left_join com o código dos estados Brasileiros


codigoUF <- read_xls ("D:/Users/sophi/Documents/PosDoc/CodigosMunicipiosUF_IBGE/Codigo_UF.xls")

Conare2010a2024_filtro <- Conare2010a2024_filtro %>% left_join(codigoUF %>% select (UFs,Códigos),
              by = c ("UF"= "Códigos")) ## isso pq o nome das colunas que tem os códigos,


#tipo_processos <- table (Conare2010a2022$TIPO_PROCESSO)

#print (tipo_processos)
                          
Deferidas <- Conare2010a2024_filtro

##### Selecionando e exportando APENAS os processos deferidos

Deferidas <- subset(Deferidas,TIPO_PROCESSO %in% c("DEFERIDO","EXTENSÃO DEFERIDA","REASSENTAMENTO"))

#frequencia_tipos <- table (Deferidas$TIPO_PROCESSO)
#print (frequencia_tipos)


# Transformar a coluna ANO_PROCESSO para numeric
Deferidas$ANO_PROCESSO <- as.numeric(Deferidas$ANO_PROCESSO)

# Criar a nova coluna Tempo_Ava_Dec com a diferença entre ANO_DECISAO e ANO_PROCESSO
Deferidas$Tempo_AVALIACAO <- Deferidas$ANO_DECISAO - Deferidas$ANO_PROCESSO



### No banco Conare2010_2022 transformar a coluna ANO_processo em numeric e depois criar a coluna
### tempo avaliação

Conare2010a2024_filtro$ANO_PROCESSO <- as.numeric(Conare2010a2024_filtro$ANO_PROCESSO)

Conare2010a2024_filtro$Tempo_AVALIACAO <- Conare2010a2024_filtro$ANO_DECISAO - Conare2010a2024_filtro$ANO_PROCESSO


#Exportando

write.xlsx(Deferidas, "D:/Users/sophi/Documents/PosDoc/CONARE/FINAIS/Deferidos2010a2024.xlsx")

write.xlsx(Conare2010a2024_filtro, "D:/Users/sophi/Documents/PosDoc/CONARE/FINAIS/Processos_Avaliados2010a2024.xlsx")



### Processando os dados de 2024 que estão separados por meses

# Definindo o diretório onde os arquivos .csv estão localizados 

diretorio <- "D:/Users/sophi/Documents/PosDoc/CONARE/CONARE_2024" 

# Listando todos os arquivos .csv no diretório 

arquivos <- list.files(diretorio, pattern = "\\.csv$", full.names = TRUE) 

# Lendo e combinando todos os arquivos .csv 

dados_combinados <- do.call(rbind, lapply(arquivos, read.csv, sep = ";", fileEncoding = "UTF-8")) 



## Não precisa rodar o código abaixo
dados_combinados %>% select(ANO_PROCESSO, ANO_DECISAO) %>%
  distinct() %>%
  arrange(ANO_PROCESSO, ANO_DECISAO)  ## comando para ver os registros da coluna ano_processo e ano_decisão

## Registro mais antigo é 1976. Nesse ano tem também 1983,1984,1985,1986,1990,1992,1994,1995.1997, 2000 ....e  mais

### Arrumando o preenchimento das colunas fundamentaçao e sexo

dados_combinados <- dados_combinados %>% mutate(FUNDAMENTACAO = ifelse
                                              (FUNDAMENTACAO == "NE", "Não Especificado", FUNDAMENTACAO))

dados_combinados <- dados_combinados %>% mutate (SEXO = ifelse 
                                               (SEXO == "03 NE", "Não Especificado", SEXO))  

dados_combinados <- dados_combinados %>% mutate (CIDADE_edit= ifelse
                                                 (CIDADE_edit == "NE", "Não Especificado", CIDADE_edit))

dados_combinados <- dados_combinados %>%  mutate(SEXO = gsub("^[0-9]+\\s", "", SEXO))


### Fazendo o left_join com o código dos estados Brasileiros


codigoUF <- read_xls ("D:/Users/sophi/Documents/PosDoc/CodigosMunicipiosUF_IBGE/Codigo_UF.xls")

dados_combinados <- dados_combinados %>% left_join( codigoUF %>% select (UFs,Códigos),
                                                  by = c ("UF"= "Códigos")) 


processo_tipo <- table (dados_combinados$TIPO_PROCESSO)
print (processo_tipo)

###Processando os dados

##### Selecionando e exportando APENAS os processos deferidos

Deferidas <-dados_combinados

Deferidas <- subset(Deferidas,TIPO_PROCESSO %in% c ("DEFERIDO","EXTENSÃO DEFERIDA","REASSENTAMENTO"))

tipo_processo <- table (Deferidas$TIPO_PROCESSO)
print (tipo_processo)

# Criar a nova coluna Tempo_Ava_Dec com a diferença entre ANO_DECISAO e ANO_PROCESSO
Deferidas$Tempo_AVALIACAO <- Deferidas$ANO_DECISAO - Deferidas$ANO_PROCESSO

## agora para o banco todo
dados_combinados$Tempo_AVALIACAO <- dados_combinados$ANO_DECISAO - dados_combinados$ANO_PROCESSO

#Exportando

write.xlsx(Deferidas, "D:/Users/sophi/Documents/PosDoc/CONARE/FINAIS/Deferidos2024.xlsx")

write.xlsx(dados_combinados, "D:/Users/sophi/Documents/PosDoc/CONARE/FINAIS/Processos_Avaliados2024.xlsx")
