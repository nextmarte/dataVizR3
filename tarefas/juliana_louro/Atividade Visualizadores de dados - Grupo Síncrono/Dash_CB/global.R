### Grupo
# Juliana Louro Muniz Carneiro da Costa
# Milena da Silva Santos Pacheco
# Carlla Amara Nogueira Alves
# Victória Evellyn Costa Moraes Sousa
# Pedro Maurício Ximenez da Silva

require(tidyverse)
require(readxl)
require(xlsx)
require(shiny)
require(plotly)
require(stringr)
require(DT)
require(htmltools)

cb_cartoes_Clubes = readRDS("data/cb_cartoes_Clubes.rds")
cb_estat_Clubes = readRDS("data/cb_estat_Clubes.rds")
cb_total        = readRDS("data/cb_total.rds")
cb_gols_Clubes = readRDS("data/cb_gols_Clubes.rds")
