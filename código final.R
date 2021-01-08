library(tidyverse)
library(readxl)
library(plotly)
library(factoextra)
library(FactoMineR)
library(cluster)
library(clValid)
library(fpc)
library(NbClust)
library(data.table)
library(DT)
options(scipen=999)
set.seed(1234)


### Forecast de receita e novos clientes
forecast <- read_excel('forecast.xlsx', col_types = c('date', 
                                                      'text', 'numeric', 'numeric', 'numeric', 
                                                      'numeric', 'numeric', 'numeric', 'numeric'))

forecastplot <-
  forecast %>% 
  ggplot() +
  aes(x = `Data Apuração`, y = `Base ativa`, colour = Canal) +
  geom_line(size = 1L) +
  scale_color_brewer(palette = 'Paired') +
  labs(x = 'Mês de referência', y = 'Lucro(R$)', title = 'Forecast de crescimento da base ativa por canal', caption = 'O canal escolhido para novos cliente foi o 3') +
  theme_minimal()
ggplotly(forecastplot)


forecastplot <-
  forecast %>% 
  ggplot() +
  aes(x = `Data Apuração`, y = Lucro, colour = Canal) +
  geom_line(size = 1L) +
  scale_color_brewer(palette = 'Paired') +
  labs(x = 'Mês de referência', y = 'Lucro(R$)', title = 'Forecast de receita por canal', caption = 'O canal escolhido para novos cliente foi o 3 e para ações de rentabilização da base, 1') +
  theme_minimal()
ggplotly(forecastplot)

#### Importação do data set  

data <- read_excel('CASE_RentabilidadeAnalitica.xlsx', skip = 2,
                   col_names = c(
                     'stonecode',	'MCC',	'Data_Apuracao',
                     'Data_de_Entrada_do_Cliente', 'Canal', 'Cidade',	
                     'Volume_Transacionado', 'Quantidade_de_Transacoes', 'Net_MDR',
                     'Aluguel', 'Pre_pagamento_receita', 'Total_receita',
                     'Pre_pagamento_custos', 'Equipamento_Logistica', 'Comissoes',
                     'Transacionais', 'Outros',	'Total_custos','Margem_final'
                   ),
                   col_types = c(
                     'text', 'numeric', 'date', 
                     'date', 'text', 'text', 'numeric', 
                     'numeric', 'numeric', 'numeric', 
                     'numeric', 'numeric', 'numeric', 
                     'numeric', 'numeric', 'numeric', 
                     'numeric', 'numeric', 'numeric'
                   )
)

#### Agregação

#Por termos os dados de uma série temporal (5 meses) é preciso agregar os dados de alguma forma a fim de condensar as informações  
#de cada cliente numa única entrada. Para simplificar, aqui estou utilizado os totais do período.   
#Aproveito e já deixo apenas as colunas númericas, passando o stonecode para row name (lá na frente vamos precisar para associar cluster e Cliente). 

#agregação 
data_totals <-
  data %>% 
  select(-'MCC',	-'Data_Apuracao',
         -'Data_de_Entrada_do_Cliente', -'Canal', -'Cidade') %>% 
  group_by(stonecode) %>% 
  summarise(
    sum_Volume_Transacionado = sum(Volume_Transacionado), 
    sum_Quantidade_de_Transacoes = sum(Quantidade_de_Transacoes), 
    sum_Net_MDR = sum(Net_MDR),
    sum_Aluguel = sum(Aluguel), 
    sum_Pre_pagamento_receita = sum(Pre_pagamento_receita), 
    sum_Total_receita = sum(Total_receita),
    sum_Pre_pagamento_custos = sum(Pre_pagamento_custos), 
    sum_Equipamento_Logistica = sum(Equipamento_Logistica), 
    sum_Comissoes = sum(Comissoes),
    sum_Transacionais = sum(Transacionais), 
    sum_Outros = sum(Outros),	
    sum_Total_custos = sum(Total_custos),
    sum_Margem_final = sum(Margem_final)
    
  ) 
row.names(data_totals) <- data_totals$stonecode
data_totals_final <- data_totals %>% 
  select(- stonecode)


#### Normalização dos dados  
#Como temos valores de ordens de grandeza diferentes os dados precisam ser normalizados.

#normalizando os dados
totals_scaled <- scale(data_totals_final)
summary(totals_scaled)
```


### Verificando o melhor algoritimo de clusterização   

# melhor algoritimo
clmethods <- c('hirerchical', 'kmeans', 'clara')

intern <- clValid(totals_scaled, nClust = 2:8, clMethods = clmethods, maxitems = 4000, validation = 'internal')
summary(intern)

stab <- clValid(totals_scaled, nClust = 2:8, clMethods = clmethods, maxitems = 4000, validation = 'stability')
optimalScores(stab)

### Quantidade de clusters  
#Temos que kmeans, em geral, vai ser o melhor algoritimo para clusterização desta base.  
#Para escolher a quantidade ótima de clusters vamos ver a quantidade que nos dá uma boa compactação.

# elbow

fviz_nbclust(totals_scaled, kmeans, method = 'wss')

#A maior queda na soma dos quadrados intra-cluster (o "cotovelo") está em dois clusters, mas com essa quantidade pequena é provável que percamos algumas nuances das caracteríscas dos clientes. Vemos pelo gráfico, que há um segundo cotovelo em 5 clusters, que será a quantidade adotada.

### Cluster plot  
#clusterizando
km <- kmeans(totals_scaled, 5, nstart = 1000)
grp_km <- as.factor(km$cluster)

plot.km <- fviz_cluster(km, totals_scaled, ellipse.type = 'euclid',
                        ellipse.level = .9, ellipse.alpha = .1, pointsize = 1,
                        labelsize = .5, ggtheme = theme_minimal(),
                        outlier.color = 'black', palette = 'Paired')+
  coord_cartesian(xlim = c(-2,50), ylim = c(-25, 20))

ggplotly(plot.km)

# levando os cluster para o data frame original
mcc <- read_excel('MCC.xlsx') %>% 
  select(MCC, DescricaoGrupo)

clust <- data.frame(list(km$cluster))

clust <- setDT(clust, keep.rownames = TRUE)[] %>% 
  rename(stonecode = rn,
         cluster = c..100100026313922....5L...100100082815567....5L...100100087809648....5L..
  )			

data_clusters <- 
  inner_join(clust, data, by = 'stonecode') %>% 
  inner_join(mcc, by = 'MCC') %>% 
  select(-'MCC') %>% 
  mutate(ativado = case_when(
    Quantidade_de_Transacoes == 0 ~ 'Inativo',
    Quantidade_de_Transacoes > 0 ~ 'Ativo'
  ),
  cluster = as.factor(cluster)
  )

### Quantidade de Clientes por cluster  
#Temos uma grande concentração dos Clientes no cluster 5 e uma quantidade pequena nos clusters 3 e 4.


# qtd
qtd_cluster <-
  data_clusters %>%
  select(stonecode, cluster) %>% 
  distinct() %>%     
  group_by(cluster) %>%
  summarise(total = n())%>% 
  ggplot() +
  aes(x = cluster, fill = cluster, y = total) +
  geom_col() +
  scale_fill_brewer(palette = 'Paired') +
  labs(x = 'Cluster', y = 'Total de Clientes', title = 'Quantidade de Clientes por cluster') +
  theme_minimal() 
ggplotly(qtd_cluster)

### Receita, custo e lucro por mês  

# receita, custo e lucro por mês
fin_cluster <-
  data_clusters %>%
  select(cluster, Total_receita, Total_custos, Margem_final, Data_Apuracao
  ) %>% 
  group_by(cluster, Data_Apuracao) %>% 
  summarise(
    Receita = sum(Total_receita),
    Custos = sum(Total_custos),
    Resultado = sum(Margem_final),
  ) %>% 
  mutate(
    receita_total = Receita/sum(data_clusters$Total_receita),
    custos_total = Custos/sum(data_clusters$Total_custos),
    margem_total = Resultado/sum(data_clusters$Margem_final)
  ) %>% 
  gather(metrica, valor, 3:8) %>% 
  #filter(!(cluster %in% c('4', '3'))) %>%  
  filter(metrica %in% c('Receita', 'Custos', 'Resultado')) %>%
  ggplot() +
  aes(x = Data_Apuracao, y = valor, colour = cluster) +
  geom_line(size = 1L) +
  scale_color_brewer(palette = 'Paired') +
  labs(x = 'Mês de refêrencia', y = 'Valor (R$)', title = 'Resultado financeiro por cluster') +
  theme_minimal() +
  facet_wrap(vars(metrica))
ggplotly(fin_cluster)


### Percentual de receita, custo e lucro  

# percentual de receita, custo e lucro
pct_cluster <- 
  data_clusters %>%
  select(cluster, Total_receita, Total_custos, Margem_final
  ) %>% 
  group_by(cluster) %>% 
  summarise(
    Receita = sum(Total_receita),
    Custos = sum(Total_custos),
    Resultado = sum(Margem_final),
  ) %>% 
  mutate(
    Receita_pct = 100*Receita/sum(data_clusters$Total_receita),
    Custos_pct = 100*Custos/sum(data_clusters$Total_custos),
    Resultado_pct = 100*Resultado/sum(data_clusters$Margem_final)
  ) %>% 
  gather(metrica, valor, 2:7) %>%
  filter(metrica %in% c('Receita_pct', 'Custos_pct', 'Resultado_pct')) %>%
  ggplot() +
  aes(x = cluster, fill = cluster, weight = valor) +
  geom_bar() +
  scale_fill_brewer(palette = 'Paired') +
  labs(x = 'Cluster', y = '(%)', title = 'Participação (%) no resultado por cluster') +
  theme_minimal() +
  facet_wrap(vars(metrica))
ggplotly(pct_cluster)

### Taxa de churn mensal por cluster  

#Taxa de churn mensal por cluster
clusters_churn <-
  
  data_clusters %>%
  select(stonecode, cluster, Data_Apuracao, ativado) %>% 
  spread(Data_Apuracao, ativado) %>% 
  mutate(baseativaNov = case_when(`2016-10-01` == 'Ativo' ~ 1, T~0),
         baseativaDez = case_when(`2016-11-01` == 'Ativo' ~ 1, T~0),
         baseativaJan = case_when(`2016-12-01` == 'Ativo' ~ 1, T~0),
         baseativaFev = case_when(`2017-01-01` == 'Ativo' ~ 1, T~0),
         
         basechurnNov = paste(`2016-10-01`,`2016-11-01`, sep = ''),
         basechurnDez = paste(`2016-11-01`,`2016-12-01`, sep = ''),
         basechurnJan = paste(`2016-12-01`,`2017-01-01`, sep = ''),
         basechurnFev = paste(`2017-01-01`,`2017-02-01`, sep = '')
  ) %>% 
  
  mutate(basechurnNov = case_when(basechurnNov == 'AtivoInativo' ~ 1, T ~ 0),
         basechurnDez = case_when(basechurnDez == 'AtivoInativo' ~ 1, T ~ 0),
         basechurnJan = case_when(basechurnJan == 'AtivoInativo' ~ 1, T ~ 0),
         basechurnFev = case_when(basechurnFev == 'AtivoInativo' ~ 1, T ~ 0)
  ) %>% 
  select(-`2016-10-01`, -`2016-11-01`, -`2016-12-01`, -`2017-01-01`, -`2017-02-01`, -stonecode) %>% 
  group_by(cluster) %>% 
  summarise(
    baseativaNov = sum(baseativaNov),
    baseativaDez = sum(baseativaDez),
    baseativaJan = sum(baseativaJan),
    baseativaFev = sum(baseativaFev),
    basechurnNov = sum(basechurnNov),
    basechurnDez = sum(basechurnDez),
    basechurnJan = sum(basechurnJan),
    basechurnFev = sum(basechurnFev)
  ) %>% 
  transmute(
    cluster,
    '2016-11-01'= round((100*basechurnNov/baseativaNov),2),
    '2016-12-01' = round((100*basechurnDez/baseativaDez),2),
    '2017-01-01'= round((100*basechurnJan/baseativaJan),2),
    '2017-02-01' = round((100*basechurnFev/baseativaFev),2)
  ) %>% 
  gather(mes, churn,2:5) %>% 
  mutate(mes = as.Date(mes)) 

churn_mes_plot <- clusters_churn %>% 
  ggplot() +
  aes(x = mes, y = churn, colour = cluster) +
  geom_line(size = 1L) +
  scale_color_brewer(palette = 'Paired') +
  labs(x = 'Mês', y = 'Taxa de churn (%)', title = 'Taxa de churn mensal por cluster') +
  theme_minimal()
ggplotly(churn_mes_plot)  

### Taxa Média de churn por cluster

clusters_churn  %>%
  group_by(cluster) %>% 
  summarise(churn = mean(churn))

### TVM por cluster  
tvm_cluster <- 
  data_clusters %>%
  select(stonecode, cluster, Data_Apuracao, ativado) %>% 
  spread(Data_Apuracao, ativado) %>% 
  mutate(baseativaNov = case_when(`2016-10-01` == 'Ativo' ~ 1, T~0),
         baseativaDez = case_when(`2016-11-01` == 'Ativo' ~ 1, T~0),
         baseativaJan = case_when(`2016-12-01` == 'Ativo' ~ 1, T~0),
         baseativaFev = case_when(`2017-01-01` == 'Ativo' ~ 1, T~0),
         
         basechurnNov = paste(`2016-10-01`,`2016-11-01`, sep = ''),
         basechurnDez = paste(`2016-11-01`,`2016-12-01`, sep = ''),
         basechurnJan = paste(`2016-12-01`,`2017-01-01`, sep = ''),
         basechurnFev = paste(`2017-01-01`,`2017-02-01`, sep = '')
  ) %>% 
  
  mutate(basechurnNov = case_when(basechurnNov == 'AtivoInativo' ~ 1, T ~ 0),
         basechurnDez = case_when(basechurnDez == 'AtivoInativo' ~ 1, T ~ 0),
         basechurnJan = case_when(basechurnJan == 'AtivoInativo' ~ 1, T ~ 0),
         basechurnFev = case_when(basechurnFev == 'AtivoInativo' ~ 1, T ~ 0)
  ) %>% 
  select(-`2016-10-01`, -`2016-11-01`, -`2016-12-01`, -`2017-01-01`, -`2017-02-01`, -stonecode) %>% 
  group_by(cluster) %>% 
  summarise(
    baseativaNov = sum(baseativaNov),
    baseativaDez = sum(baseativaDez),
    baseativaJan = sum(baseativaJan),
    baseativaFev = sum(baseativaFev),
    basechurnNov = sum(basechurnNov),
    basechurnDez = sum(basechurnDez),
    basechurnJan = sum(basechurnJan),
    basechurnFev = sum(basechurnFev)
  ) %>% 
  transmute(
    cluster,
    '2016-11-01'= round(1/(basechurnNov/baseativaNov),2),
    '2016-12-01'= round(1/(basechurnDez/baseativaDez),2),
    '2017-01-01'= round(1/(basechurnJan/baseativaJan),2),
    '2017-02-01'= round(1/(basechurnFev/baseativaFev),2)
  ) %>% 
  gather(mes, TVM,2:5) %>% 
  mutate(mes = as.Date(mes)) %>% 
  filter(TVM != Inf ) %>% 
  group_by(cluster) %>% 
  summarise(TVM = mean(TVM)) %>% 
  ggplot() +
  aes(x = cluster, fill = cluster, weight = TVM) +
  geom_bar() +
  scale_fill_brewer(palette = 'Paired') +
  labs(x = 'Cluster', y = 'Tempo de vida médio (meses)', title = 'Tempo de vida médio por cluster') +
  theme_minimal()
ggplotly(tvm_cluster)

# lucro medio
cluster_lucromedio <- 
  data_clusters %>%
  select(stonecode, cluster, Data_Apuracao, ativado, Margem_final) %>%
  mutate(ativado = case_when(ativado == 'Ativo' ~ 1, T~0)) %>% 
  select(-stonecode) %>% 
  filter(ativado== 1) %>% 
  group_by(cluster, Data_Apuracao) %>% 
  summarise(ativos = sum(ativado),
            lucro = sum(Margem_final),
            lucro_medio = lucro/ativos
  ) %>% 
  group_by(cluster) %>% 
  summarise(lucro_medio = mean(lucro_medio)) %>% 
  ggplot() +
  aes(x = cluster, fill = cluster, weight = lucro_medio) +
  geom_bar() +
  scale_fill_brewer(palette = 'Paired') +
  labs(x = 'Cluster', y = 'Lucro Médio (R$)', title = 'Lucro médio por Cliente') +
  theme_minimal()
ggplotly(cluster_lucromedio)

# lucro medio por cliente ativo
cluster_lucromed <- 
  data_clusters %>%
  select(stonecode, cluster, Data_Apuracao, ativado, Margem_final) %>%
  mutate(ativado = case_when(ativado == 'Ativo' ~ 1, T~0)) %>% 
  select(-stonecode) %>% 
  filter(ativado== 1) %>% 
  group_by(cluster, Data_Apuracao) %>% 
  summarise(ativos = sum(ativado),
            lucro = sum(Margem_final),
            lucro_medio = lucro/ativos
  ) %>% 
  ggplot() +
  aes(x = Data_Apuracao, y = lucro_medio, colour = cluster) +
  geom_line(size = 1L) +
  scale_color_brewer(palette = 'Paired') +
  labs(x = 'Mês de referência', y = 'Lucro médio (R$)', title = 'Lucro médio por Cliente ativo') +
  theme_minimal()
ggplotly(cluster_lucromed)

# Principais atividades
clust_act <-
  data_clusters %>%
  select(stonecode, cluster, DescricaoGrupo) %>% 
  distinct() %>% 
  group_by(cluster, DescricaoGrupo) %>% 
  summarise(quantidade = n()) %>% 
  mutate(totalcluster = sum(quantidade)) %>% 
  mutate(pct = 100*quantidade/totalcluster) %>% 
  arrange(desc(pct)) %>% 
  filter(pct >= 5L ) %>%
  ggplot() +
  aes(x = DescricaoGrupo, fill = cluster, y = pct) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = 'Paired') +
  labs(x = '', y = 'Quantidade', title = 'Atividades mais comuns por cluster', 
       caption = 'Foram consideradas apenas atividades que representam 5% ou mais do cluster') +
  theme_minimal() +
  facet_wrap(vars(cluster), ncol = 1, scales = 'free_y') +
  geom_text(aes(label = round(pct,2)),angle = 90)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplotly(clust_act)


# canal preferido

cluster_canal <-
  data_clusters %>%
  select(stonecode, cluster, Canal) %>% 
  distinct() %>% 
  group_by(cluster, Canal) %>% 
  summarise(quantidade = n()) %>% 
  mutate(totalcluster = sum(quantidade)) %>% 
  mutate(pct = 100*quantidade/totalcluster) %>% 
  arrange(desc(pct)) %>% 
  ggplot() +
  aes(x = Canal, fill = cluster, y = pct) +
  geom_col() +
  scale_fill_brewer(palette = 'Paired') +
  coord_flip() +
  theme_minimal() +
  facet_wrap(vars(cluster))+
  labs(x = '', y = 'Quantidade', title = '% de Clientes por canal em cada cluster') +
  theme_minimal() +
  facet_wrap(vars(cluster), ncol = 1, scales = 'free_x') +
  geom_text(aes(label = round(pct,2)),angle =0)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggplotly(cluster_canal)  


# metricas financeiras

clusters_metricas<-
  data_clusters %>% 
  select(cluster, Volume_Transacionado, Quantidade_de_Transacoes, Net_MDR, Margem_final, Total_custos, Total_receita) %>% 
  group_by(cluster) %>% 
  summarise(
    
    transacoes_total = round(sum(Quantidade_de_Transacoes),2),
    tpv_total = round(sum(Volume_Transacionado),2),
    ticket_medio = round(tpv_total/transacoes_total,2),
    Net_MDR_total = round(sum(Net_MDR),2),
    Receita_total = round(sum(Total_receita),2),
    Custo_total = round(sum(Total_custos),2),
    Margem_final_total = round(sum(Margem_final),2),
    
    transacoes_media = round(mean(Quantidade_de_Transacoes),2),
    tpv_medio = round(mean(Volume_Transacionado),2),
    Net_MDR_medio =round( mean(Net_MDR),2),
    Receita_media = round(mean(Total_receita),2),
    Custo_medio = round(mean(Total_custos),2),
    Margem_final_media = round(mean(Margem_final),2)
  ) %>% 
  mutate(
    transacoes_absoluto = round(sum(transacoes_total),2),
    tpv_absoluto = round(sum(tpv_total),2),
    Net_MDR_Absoluto = round(sum(Net_MDR_total),2),
    Receita_absoluta = round(sum(Receita_total),2),
    Custo_absoluto = round(sum(Custo_total),2), 
    Margem_absoluta = round(sum(Margem_final_total),2)
  ) %>% 
  mutate(
    transacoes_pct = round(100*transacoes_total/transacoes_absoluto,2),
    tpv_pct = round(100*tpv_total/tpv_absoluto,2),
    Net_MDR_pct = round(100*Net_MDR_total/Net_MDR_Absoluto,2),
    Receita_pct = round(100*Receita_total/Receita_absoluta,2),
    Custo_pct = round(100*Custo_total/Custo_absoluto,2), 
    Margem_pct = round(100*Margem_final_total/Margem_absoluta,2)
  ) %>% 
  inner_join(data_clusters %>%
               select(stonecode, cluster) %>% 
               distinct() %>%     
               group_by(cluster) %>%
               summarise(total = n()), by = 'cluster')

datatable(clusters_metricas, filter = 'top')

