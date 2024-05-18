### Grupo
# Juliana Louro Muniz Carneiro da Costa
# Milena da Silva Santos Pacheco
# Carlla Amara Nogueira Alves
# Victória Evellyn Costa Moraes Sousa
# Pedro Maurício Ximenez da Silva

require(tidyverse)
require(readxl)
require(xlsx)
require(stringr)
require(Hmisc)

round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

cb_cartoes = read.csv("campeonato-brasileiro-cartoes.csv",sep = ',')
cb_estat = read.csv("campeonato-brasileiro-estatisticas-full.csv",sep = ',')
cb_full = read.csv("campeonato-brasileiro-full.csv",sep = ',')
cb_gols = read.csv("campeonato-brasileiro-gols.csv",sep = ',')

head(cb_cartoes)
head(cb_estat)
head(cb_full)
head(cb_gols)

colnames(cb_full)[1] = "partida_id"

### Tratamento das bases

# Cartões

cb_cartoes2 = full_join(cb_cartoes,cb_full)
cb_cartoes2 = cb_cartoes2 %>% select(colnames(cb_cartoes),data)
cb_cartoes2$ano = str_sub(cb_cartoes2$data,start = 7)

cb_cartoes_Clubes = cb_cartoes2 %>% group_by(ano,clube,cartao) %>% summarise(total_cartoes = n())
cb_cartoes_Clubes = subset(cb_cartoes_Clubes, !is.na(clube))

# Estatísticas

cb_estat2 = full_join(cb_estat,cb_full)
cb_estat2 = cb_estat2 %>% select(colnames(cb_estat),data)
cb_estat2$ano = str_sub(cb_estat2$data,start = 7)

cb_estat_Clubes = cb_estat2 %>% group_by(ano,clube) %>% summarise(total_faltas = sum(faltas),
                                                             total_imp = sum(impedimentos),
                                                             total_esc = sum(escanteios),
                                                             total_chutes = sum(chutes),
                                                             total_chutescertos = sum(chutes_no_alvo)) %>% filter(total_faltas != 0)
cb_estat_Clubes$perc = cb_estat_Clubes$total_chutescertos/cb_estat_Clubes$total_chutes
cb_estat_Clubes$perc2 = paste0(str_trim(str_replace_all(format(round2(as.numeric(cb_estat_Clubes$perc*100),2),nsmall = 2),"\\.",",")),"%")

# Full
cb_full2 = cb_full %>% select(data,mandante,visitante,vencedor)
cb_full2$ano = str_sub(cb_full2$data,start = 7)

cb_total = NULL
for(i in 1:length(unique(cb_full2$ano))){
  cb_full_venc = cb_full2 %>% dplyr::filter(ano == unique(cb_full2$ano)[i] & vencedor != "-")
  cb_full_emp = cb_full2 %>% dplyr::filter(ano == unique(cb_full2$ano)[i] & vencedor == "-")
  
  cb_full_venc$pontos = 3
  cb_full_emp$pontos = 1
  
  cb_full_emp_mand = cb_full_emp %>% group_by(ano,mandante) %>% summarise(pts_empM = sum(pontos))
  cb_full_emp_vist = cb_full_emp %>% group_by(ano,visitante) %>% summarise(pts_empV = sum(pontos))
  cb_full_venc_pts = cb_full_venc %>% group_by(ano,vencedor) %>% summarise(pts_ven = sum(pontos))
  
  colnames(cb_full_emp_mand)[2] = "clube"
  colnames(cb_full_emp_vist)[2] = "clube"
  colnames(cb_full_venc_pts)[2] = "clube"
  
  cb_full_total = full_join(cb_full_venc_pts,cb_full_emp_mand)
  cb_full_total = full_join(cb_full_total,cb_full_emp_vist)
  cb_full_total$finalpts = cb_full_total$pts_ven + cb_full_total$pts_empM + cb_full_total$pts_empV
  cb_full_total2 = cb_full_total %>% select(ano, clube, finalpts)
  
  cb_total = rbind(cb_total,cb_full_total2)
}

# Gols

cb_gols2 = full_join(cb_gols,cb_full)
cb_gols2 = cb_gols2 %>% select(colnames(cb_gols),data)
cb_gols2$ano = str_sub(cb_gols2$data,start = 7)

cb_gols_Clubes = cb_gols2 %>% group_by(ano,clube,tipo_de_gol) %>% summarise(total_gols = n())
cb_gols_Clubes = subset(cb_gols_Clubes, !is.na(clube))
cb_gols_Clubes$tipo_de_gol[which(cb_gols_Clubes$tipo_de_gol == "")] <- "Gols"

# Salvamento das bases

cb_cartoes_Clubes = as.data.frame(cb_cartoes_Clubes)
cb_estat_Clubes = as.data.frame(cb_estat_Clubes)
cb_total = as.data.frame(cb_total)
cb_gols_Clubes = as.data.frame(cb_gols_Clubes)

saveRDS(cb_cartoes_Clubes,
        "./Dash_CB/data/cb_cartoes_Clubes.rds")
saveRDS(cb_estat_Clubes,
        "./Dash_CB/data/cb_estat_Clubes.rds")
saveRDS(cb_total,
        "./Dash_CB/data/cb_total.rds")
saveRDS(cb_gols_Clubes,
        "./Dash_CB/data/cb_gols_Clubes.rds")
