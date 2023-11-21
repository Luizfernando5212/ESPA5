library(ggplot2)
library(dplyr)
library(readr)
library(vcd)

fa_casoshumanos_1994_2021 <- read_delim("fa_casoshumanos_1994-2021.csv", 
                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(fa_casoshumanos_1994_2021)

summary(fa_casoshumanos_1994_2021)

# Excluindo linhas que podem prejudicar a análise
fa_casoshumanos_1994_2021 <- na.omit(fa_casoshumanos_1994_2021)

#Convertendo coluna idade para numérico
fa_casoshumanos_1994_2021 <- transform(fa_casoshumanos_1994_2021,
          IDADE = as.numeric(IDADE))

hist(fa_casoshumanos_1994_2021$IDADE, main = 'Distribuição de idades', xlab = 'Idade', ylab = 'Frequência')

# Separando casos por estado
casos_x_estado <- count(fa_casoshumanos_1994_2021, fa_casoshumanos_1994_2021$UF_LPI, sort = TRUE)

# Adicionando frequência relativa
casos_x_estado <- casos_x_estado %>%
  mutate(frequencia_relativa = n/sum(n))

names(casos_x_estado) <- c('UF', 'NUM_OCORRENCIAS', 'FREQUENCIA_RELATIVA')

ggplot(casos_x_estado, aes(x = reorder(UF, -NUM_OCORRENCIAS), y = NUM_OCORRENCIAS, fill = FREQUENCIA_RELATIVA)) +
  geom_bar(stat = 'identity') +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Ocorrências por Estado",
       x = "Estado",
       y = "Número de Ocorrências") +
  theme_minimal()

# Mesma coisa com casos por sexo
casos_x_sexo <- count(fa_casoshumanos_1994_2021, fa_casoshumanos_1994_2021$SEXO)

casos_x_sexo <- casos_x_sexo %>%
  mutate(frequencia_relativa = n/sum(n))

names(casos_x_sexo) <- c('SEXO', 'NUM_OCORRENCIAS', 'FREQUENCIA_RELATIVA')


ggplot(casos_x_sexo, aes(x = reorder(SEXO, -NUM_OCORRENCIAS), y = NUM_OCORRENCIAS, fill = FREQUENCIA_RELATIVA)) +
  geom_bar(stat = 'identity') +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Ocorrências por Sexo",
       x = "Sexo",
       y = "Número de Ocorrências") +
  theme_minimal()
# É possível notar que o número de incidências é muito maior em homens

# Vendo se há uma diferênça significativa entre a idade dos homes e mulheres
t.test(IDADE ~ SEXO, data = fa_casoshumanos_1994_2021)

#O valor p de 0.1593 é maior que o nível de significância comum de 0.05. Portanto, não há evidência estatística suficiente para rejeitar a hipótese nula.

# Separando faixas etárias
cat <- nclass.Sturges(fa_casoshumanos_1994_2021$IDADE)

fa_casoshumanos_1994_2021$faixa_etaria <- cut(fa_casoshumanos_1994_2021$IDADE, seq(0, 93, l = cat + 1), include.lowest = TRUE)

# Transformando variáveis categóricas
fa_casoshumanos_1994_2021$obito_n <- as.numeric(factor(fa_casoshumanos_1994_2021$OBITO, levels = c("SIM", "N?O", 'IGN')))
fa_casoshumanos_1994_2021$obito_n <- ifelse(is.na(fa_casoshumanos_1994_2021$obito_n), 2, fa_casoshumanos_1994_2021$obito_n)

#Estados para numeros
estados <- tibble(unique(fa_casoshumanos_1994_2021$UF_LPI), c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)) 
names(estados) <- c('UF', 'NUM')
fa_casoshumanos_1994_2021$estados_n <- as.numeric(factor(fa_casoshumanos_1994_2021$UF_LPI, levels = estados$UF))

fa_casoshumanos_1994_2021$SEXO_n <- as.numeric(factor(fa_casoshumanos_1994_2021$SEXO, levels = c ('M', 'F')))

# Tabela de contingência para a variável OBITO em relação à SEXO
table(fa_casoshumanos_1994_2021$OBITO, fa_casoshumanos_1994_2021$SEXO)

assoc_measure <- assocstats(table(fa_casoshumanos_1994_2021$OBITO, fa_casoshumanos_1994_2021$SEXO))
assoc_measure$chisq

# ESTADO
table(fa_casoshumanos_1994_2021$OBITO, fa_casoshumanos_1994_2021$UF_LPI)

assoc_measure <- assocstats(table(fa_casoshumanos_1994_2021$OBITO, fa_casoshumanos_1994_2021$UF_LPI))
assoc_measure$chisq

# Faixa_etaria
table(fa_casoshumanos_1994_2021$OBITO, fa_casoshumanos_1994_2021$faixa_etaria)

assoc_measure <- assocstats(table(fa_casoshumanos_1994_2021$OBITO, fa_casoshumanos_1994_2021$faixa_etaria))
assoc_measure$chisq

