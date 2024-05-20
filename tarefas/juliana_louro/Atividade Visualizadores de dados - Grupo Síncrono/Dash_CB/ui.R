ui <- fluidPage(
  navbarPage(title="",
             theme = "custom.css",
             windowTitle = "Análise Brasileirão",
             hr(),
             div("Análise de dados - Brasileirão Série A",
                 style = "font-size:18pt; text-align:center; font-weight:bold"),
             hr(),
             tabsetPanel(id="setri",
                         tabPanel("Pontuação",
                                  br(),
                                  selectInput("ano",label = "Ano",
                                              choices = unique(cb_total$ano),
                                              selected = 2023),
                                  DTOutput(outputId = "tabpontos")),
                         tabPanel("Gols",
                                  br(),
                                  selectInput("ano1",label = "Ano",
                                              choices = unique(cb_gols_Clubes$ano),
                                              selected = 2023),
                                  DTOutput(outputId = "tabgols")),
                         tabPanel("Estatisticas",
                                  br(),
                                  selectInput("ano2",label = "Ano",
                                              choices = unique(cb_estat_Clubes$ano),
                                              selected = 2023),
                                  DTOutput(outputId = "tabestat")),
                         tabPanel("Cartões",
                                  br(),
                                  selectInput("ano3",label = "Ano",
                                              choices = unique(cb_cartoes_Clubes$ano),
                                              selected = 2023),
                                  DTOutput(outputId = "tabcartoes")),
             )
             
             
             
  ))

