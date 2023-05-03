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

options(scipen=999)

#importando os dados - mudei os nomes de algumas colunas para facilitar meu manuseio
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
                   ) %>% 
  mutate(ticket_medio = case_when(
                                  Quantidade_de_Transacoes == 0 ~ 0,
                                  Quantidade_de_Transacoes > 0 ~ Volume_Transacionado/Quantidade_de_Transacoes
                                  )
        )

# descrição de grupos de MCC para usar no final
mcc <- read_excel('MCC.xlsx') %>% 
  select(MCC, DescricaoGrupo)

apply(is.na(data), 2, which) 

data_totals <-
  data %>% 
  select(	-'Data_Apuracao',
         -'Data_de_Entrada_do_Cliente', -'Cidade') %>% 
  group_by(stonecode, MCC, Canal) %>% 
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
    sum_Margem_final = sum(Margem_final), 
    #avg_ticket_medio = mean(ticket_medio)

  ) 

row.names(data_totals) <- data_totals$stonecode

data_totals_final <- data_totals %>% 
  select(- stonecode)

totals_scaled <- scale(data_totals_final)

# verificando o melhor algoritimo de clusterização
clmethods <- c('hirerchical', 'kmeans', 'clara')

intern <- clValid(totals_scaled, nClust = 2:8, clMethods = clmethods, maxitems = 3194, validation = 'internal')
summary(intern)

stab <- clValid(totals_scaled, nClust = 2:8, clMethods = clmethods, validation = 'stability')

summary(stab)
optimalScores(stab)


# Kmeans

set.seed(1234)

fviz_nbclust(totals_scaled, kmeans, method = 'wss')

km <- kmeans(totals_scaled, 5, nstart = 1000)
grp_km <- as.factor(km$cluster)

plot.km <- fviz_cluster(km, totals_scaled, ellipse.type = 'euclid',
                        ellipse.level = .9, ellipse.alpha = .2, pointsize = 1,
                        labelsize = .5, ggtheme = theme_minimal(), palette = 'Dark2',
                        outlier.color = 'black')+
  #scale_color_manual(values = c('#006666', '#d91f00','#003300','#000066','#000099' ))+
  coord_cartesian(xlim = c(-2,50), ylim = c(-25, 20))

ggplotly(plot.km)

#fviz_silhouette(eclust(totals_scaled, 'kmeans', k = 5, nstart = 1000, graph  =F), palette= 'jco', ggtheme = theme_classic())

table(km$cluster)

clust <- data.frame(list(km$cluster))

clust <- setDT(clust, keep.rownames = TRUE)[] %>% 
  rename(stonecode = rn,
         cluster = 	
           c..100100026313922....4L...100100082815567....4L...100100087809648....4L..
      
  )	

data_clusters <- 
  inner_join(clusters, data, by = 'stonecode') %>% 
  inner_join(mcc, by = 'MCC') %>% 
  select(-'MCC') %>% 
  mutate(ticket_medio = case_when(
         Quantidade_de_Transacoes == 0 ~ 0,
         Quantidade_de_Transacoes > 0 ~ Volume_Transacionado/Quantidade_de_Transacoes
                                  ),
          ativado = case_when(
          Quantidade_de_Transacoes == 0 ~ 'Inativo',
          Quantidade_de_Transacoes > 0 ~ 'Ativo'
                                  ),
        cluster = as.factor(cluster)
         )


clientes_ativos <- data.frame(table(data_clusters$Data_Apuracao, data_clusters$ativado, data_clusters$cluster))

# canal de aquisição

clusters_channel <- 
  data_clusters %>% 
  select(stonecode, Canal, cluster) %>% 
  distinct() %>% 
  group_by(Canal, cluster) %>% 
  summarise(total = n()) %>% 
  ggplot() +
  aes(x = cluster, weight = total, fill = cluster) +
  geom_bar() +
  scale_fill_brewer(palette = 'Dark2') +
  theme_minimal() +
  facet_wrap(vars(Canal))

  ggplotly(clusters_channel)

# atividade
clusters_mcc <-  
  data_clusters %>%
  select(stonecode, cluster, DescricaoGrupo) %>% 
  distinct() %>% 
  group_by(cluster, DescricaoGrupo) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total)) %>%
  filter(total >0, cluster %in% c(1,2,5) ) %>%
  ggplot() +
  aes(x = DescricaoGrupo, fill = cluster, y = total) +
  geom_col(stat='identity') +
  scale_fill_brewer(palette = 'Dark2') +
  theme_minimal() +
 # geom_text(aes(label = DescricaoGrupo),angle = 90, label.size = 0.25 )+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(vars(cluster), ncol = 1)

ggplotly(clusters_mcc)

esquisser()

clusters_pct_act <-
data_clusters %>%
  select(stonecode, cluster, DescricaoGrupo, ticket_medio) %>% 
  distinct() %>% 
  group_by(cluster) %>% 
  mutate(total_cluster = n()) %>% 
  select(-stonecode) %>% 
  group_by(cluster, DescricaoGrupo) %>% 
  mutate(total = n(), ticket_medio = round(mean(ticket_medio),2)) %>% 
  distinct() %>% 
  mutate(actividade_pct = round(100*(total/total_cluster),2)) %>% 
  arrange(desc(actividade_pct)) 
  datatable(clusters_pct_act, filter = 'top')
  
  
  qtd_cluster <-
  data_clusters %>%
    select(stonecode, cluster) %>% 
    distinct() %>%     
    group_by(cluster) %>%
    summarise(total = n())%>% 
  ggplot() +
    aes(x = cluster, fill = cluster, y = total) +
    geom_col() +
    scale_fill_brewer(palette = 'Dark2') +
    labs(x = 'Cluster', y = 'Total de Clientes', title = 'Quantidade de Clientes por cluster') +
    theme_minimal() 
    ggplotly(qtd_cluster)
  
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
           Receita_pct = Receita/sum(data_clusters$Total_receita),
           Custos_pct = Custos/sum(data_clusters$Total_custos),
           Resultado_pct = Resultado/sum(data_clusters$Margem_final)
          ) %>% 
    gather(metrica, valor, 3:8) %>% 
    filter(metrica %in% c('Receita', 'Custos', 'Resultado')) %>%
    ggplot() +
    aes(x = Data_Apuracao, y = valor, colour = cluster) +
    geom_line(size = 1L) +
    scale_color_brewer(palette = 'Dark2') +
    labs(x = 'Mês de refêrencia', y = 'Valor (R$)', title = 'Resultado financeiro por cluster') +
    theme_minimal() +
    facet_wrap(vars(metrica))
  ggplotly(fin_cluster)
  
  
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
    scale_fill_brewer(palette = 'Dark2') +
    labs(x = 'Cluster', y = '(%)', title = 'Particicipação (%) no resultado por cluster') +
    theme_minimal() +
    facet_wrap(vars(metrica))
  ggplotly(pct_cluster)
  
  data_clusters %>%
    select(stonecode, cluster) %>% 
    distinct() %>%     
    group_by(cluster) %>%
    summarise(total = n())%>% 
    esquisser()
  
  
  # distribuições
cluster_distr <-
  data_clusters %>%
    select( -Data_Apuracao, -Data_de_Entrada_do_Cliente, 
           -Canal, -Cidade, -DescricaoGrupo, - ativado) %>% 
    group_by(stonecode, cluster) %>% 
  
  summarise(total_transacoes = sum(Quantidade_de_Transacoes),
            avg_transacoes = mean(Quantidade_de_Transacoes),
            total_tpv = sum(Volume_Transacionado),
            avg_tpv = mean(Volume_Transacionado),
            total_Net = sum(Net_MDR),
            avg_net = mean(Net_MDR),
            total_aluguel = sum(Aluguel),
            avg_aluguel = mean(Aluguel),
            total_prePag_receita = sum(Pre_pagamento_receita),
            avg_prePag_receita = mean(Pre_pagamento_receita),
            total_receita = sum(Total_receita),
            avg_receita = mean(Total_receita),
            total_prePag_custos = sum(Pre_pagamento_custos),
            avg_prePag_custos = mean(Pre_pagamento_custos),
            total_equiplog = sum(Equipamento_Logistica),
            avg_equiplog = mean(Equipamento_Logistica),
            total_transacionais = sum(Transacionais),
            avg_transacionais = mean(Transacionais),
            
            total_outros = sum(Outros),
            avg_outros = mean(Outros),
            
            total_custos = sum(Total_custos),
            avg_custos = mean(total_custos),
            
            total_margem_final = sum(Margem_final),
            avg_margem_final = mean(Margem_final),
  )

cluster_distr %>%
  filter(!(cluster %in% c('4', '3'))) %>%
  filter(total_custos >= mean(cluster_distr$total_custos)-2*(sd(cluster_distr$total_custos))) %>%
  ggplot() +
  aes(x = cluster, y = total_custos, fill = cluster) +
  geom_boxplot() +
  scale_fill_brewer(palette = 'Dark2') +
  theme_minimal() +
  facet_wrap(vars(cluster))
  
cluster_distr %>%
  filter(!(cluster %in% c('4', '3'))) %>%
  filter(total_custos >= mean(cluster_distr$total_margem_final)-2*(sd(cluster_distr$total_margem_final))) %>%
  ggplot() +
  aes(x = cluster, y = total_margem_final, fill = cluster) +
  geom_boxplot() +
  scale_fill_brewer(palette = 'Dark2') +
  theme_minimal()
  
# medidas
  
data_clusters %>%
  select(-stonecode, -Data_Apuracao, -Data_de_Entrada_do_Cliente, 
         -Canal, -Cidade, -DescricaoGrupo, - ativado) %>% 
  group_by(cluster) %>% 
  summarise(total_transacoes = sum(Quantidade_de_Transacoes),
            avg_transacoes = mean(Quantidade_de_Transacoes),
            total_tpv = sum(Volume_Transacionado),
            avg_tpv = mean(Volume_Transacionado),
            total_Net = sum(Net_MDR),
            avg_net = mean(Net_MDR),
            total_aluguel = sum(Aluguel),
            avg_aluguel = mean(Aluguel),
            total_prePag_receita = sum(Pre_pagamento_receita),
            avg_prePag_receita = mean(Pre_pagamento_receita),
            total_receita = sum(Total_receita),
            avg_receita = mean(Total_receita),
            total_prePag_custos = sum(Pre_pagamento_custos),
            avg_prePag_custos = mean(Pre_pagamento_custos),
            total_equiplog = sum(Equipamento_logistica),
            avg_equiplog = mean(Equipamento_logistica),
            total_transacionais = sum(Transacionais),
            avg_transacionais = mean(Transacionais),
            
            total_outros = sum(Outros),
            avg_outros = mean(Outros),
            
            total_custos = sum(Total_custos),
            avg_custos = mean(total_custos),
            
            total_margem_final = sum(Margem_final),
            avg_margem_final = mean(Margem_final),
            )
  



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
  mutate(mes = as.Date(mes)) %>% 
  filter(!(cluster %in% c('3', '4'))) %>%
  ggplot() +
  aes(x = mes, y = churn, colour = cluster) +
  geom_line(size = 1L) +
  scale_color_brewer(palette = 'Dark2') +
  labs(x = 'Mês', y = 'Taxa de churn (%)', title = 'Taxa de churn mensal por cluster') +
  theme_minimal()
ggplotly(clusters_churn)  
esquisser()




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
  mutate(mes = as.Date(mes)) %>% 
  #filter(!(cluster %in% c('3', '4'))) %>% 
  group_by(cluster) %>% 
  summarise(churn_medio = mean(churn))





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
  filter(TVM != Inf) %>% 
  mutate(mes = as.Date(mes)) %>% 
  #filter(!(cluster %in% c('3', '4')), TVM != Inf ) %>% 
  group_by(cluster) %>% 
  #mutate(TVM = case_when(TVM == Inf ~ 0, T ~ TVM)) %>% 
  summarise(TVM = mean(TVM)) %>% 
  ggplot() +
  aes(x = cluster, fill = cluster, weight = TVM) +
  geom_bar() +
  scale_fill_brewer(palette = 'Dark2') +
  labs(x = 'Cluster', y = 'Tempo de vida médio (meses)', title = 'Tempo de vida médio por cluster') +
  theme_minimal()
ggplotly(tvm_cluster)

esquisser()

clust_act <-
  data_clusters %>%
  select(stonecode, cluster, DescricaoGrupo) %>% 
  distinct() %>% 
  group_by(cluster, DescricaoGrupo) %>% 
  summarise(quantidade = n()) %>% 
  mutate(totalcluster = sum(quantidade)) %>% 
  mutate(pct = 100*quantidade/totalcluster) %>% 
  arrange(desc(pct)) %>% 
 # filter(!(cluster %in% c('4', '3'))) %>%
  filter(pct >= 5L ) %>%
  ggplot() +
  aes(x = DescricaoGrupo, fill = cluster, y = pct) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = 'Dark2') +
  labs(x = '', y = 'Qauntidade', title = 'Atividades mais comuns por cluster', 
       caption = 'Foram consideradas apenas atividades que representam 5% ou mais do cluster') +
  theme_minimal() +
  facet_wrap(vars(cluster), ncol = 1, scales = "free_y") +
  geom_text(aes(label = round(pct,2)),angle = 90)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplotly(clust_act)


cluster_canal <-
data_clusters %>%
  select(stonecode, cluster, Canal) %>% 
  distinct() %>% 
  group_by(cluster, Canal) %>% 
  summarise(quantidade = n()) %>% 
  mutate(totalcluster = sum(quantidade)) %>% 
  mutate(pct = 100*quantidade/totalcluster) %>% 
  arrange(desc(pct)) %>% 
 # filter(!(cluster %in% c('4', '3'))) %>%
  ggplot() +
  aes(x = Canal, fill = cluster, y = pct) +
  geom_col() +
  scale_fill_brewer(palette = "Dark2") +
  coord_flip() +
  theme_minimal() +
  facet_wrap(vars(cluster))+
  labs(x = '', y = 'Quantidade', title = '% de Clientes por canal em cada cluster') +
  theme_minimal() +
  facet_wrap(vars(cluster), ncol = 1, scales = "free_y") +
  geom_text(aes(label = round(pct,2)),angle =0)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggplotly(cluster_canal)  



cluster_ticket <-
  data_clusters %>%
  select(stonecode, cluster, ticket_medio) %>% 
  group_by(cluster) %>% 
  summarise(ticket_medio = mean(ticket_medio)) %>% 
  mutate(totalcluster = sum(quantidade)) %>% 
  mutate(pct = 100*quantidade/totalcluster) %>% 
  arrange(desc(pct)) %>% 
  # filter(!(cluster %in% c('4', '3'))) %>%
  ggplot() +
  aes(x = Canal, fill = cluster, y = pct) +
  geom_col() +
  scale_fill_brewer(palette = "Dark2") +
  coord_flip() +
  theme_minimal() +
  facet_wrap(vars(cluster))+
  labs(x = '', y = 'Quantidade', title = '% de Clientes por canal em cada cluster') +
  theme_minimal() +
  facet_wrap(vars(cluster), ncol = 1, scales = "free_y") +
  geom_text(aes(label = round(pct,2)),angle =0)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggplotly(cluster_canal)  

clusters_metricas<-
data_clusters %>% 
  select(cluster, Volume_Transacionado, Quantidade_de_Transacoes, Net_MDR, Margem_final, Total_custos, Total_receita) %>% 
  group_by(cluster) %>% 
  summarise(

            transacoes_total = sum(Quantidade_de_Transacoes),
            tpv_total = sum(Volume_Transacionado),
            ticket_medio = tpv_total/transacoes_total,
            Net_MDR_total = sum(Net_MDR),
            Receita_total = sum(Total_receita),
            Custo_total = sum(Total_custos),
            Margem_final_total = sum(Margem_final),
            
            transacoes_media = mean(Quantidade_de_Transacoes),
            tpv_medio = mean(Volume_Transacionado),
            Net_MDR_medio = mean(Net_MDR),
            Receita_media = mean(Total_receita),
            Custo_medio = mean(Total_custos),
            Margem_final_media = mean(Margem_final)
            )

 table(data_clusters$cluster)
  
 gather(medida, valor, 2:14) %>% 
  esquisser()




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
  filter(!(cluster %in% c('4', '3'))) %>%
  filter(metrica %in% c("Receita_pct", "Custos_pct", "Resultado_pct")) %>%
  ggplot() +
  aes(x = cluster, fill = cluster, weight = valor) +
  geom_bar() +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Cluster", y = "(%)", title = "Participação (%) no resultado por cluster") +
  theme_minimal() +
  
  facet_wrap(vars(metrica))



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
  filter(!(cluster %in% c("3", "4"))) %>%
  ggplot() +
  aes(x = Data_Apuracao, y = lucro_medio, colour = cluster) +
  geom_line(size = 1L) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Mês de referência", y = "Lucro médio (R$)", title = "Lucro médio por Cliente ativo") +
  theme_minimal()

lucromedio <-
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
  #filter(!(cluster %in% c("3", "4"))) %>%
  group_by(cluster) %>% 
  summarise(lucro_medio = mean(lucro_medio)) %>% 
  ggplot() +
  aes(x = cluster, fill = cluster, weight = lucro_medio) +
  geom_bar() +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Cluster", y = "Lucro Médio (R$)", title = "Lucro médio por Cliente") +
  theme_minimal()

ggplotly(lucromedio)

  esquisser()
  
  spread(Data_Apuracao, ativado) %>% 
  mutate(baseativaNov = case_when(`2016-10-01` == 'Ativo' ~ 1, T~0),
         baseativaDez = case_when(`2016-11-01` == 'Ativo' ~ 1, T~0),
         baseativaJan = case_when(`2016-12-01` == 'Ativo' ~ 1, T~0),
         baseativaFev = case_when(`2017-01-01` == 'Ativo' ~ 1, T~0),
         
         basechurnNov = paste(`2016-10-01`,`2016-11-01`, sep = ''),
         basechurnDez = paste(`2016-11-01`,`2016-12-01`, sep = ''),
         basechurnJan = paste(`2016-12-01`,`2017-01-01`, sep = ''),
         basechurnFev = paste(`2017-01-01`,`2017-02-01`, sep = '')
  )





esquisser()
  
