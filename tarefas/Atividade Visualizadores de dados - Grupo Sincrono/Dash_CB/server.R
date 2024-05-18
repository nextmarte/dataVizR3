server = function(input,output,session){
  
  ################################################################
  
  # Classificação
  output$tabpontos = renderDT({
    tabpontos2 = cb_total %>% filter(ano == input$ano)
    tabpontos2 = tabpontos2[order(tabpontos2$finalpts,decreasing = c(TRUE)),]
    tabpontos2 = tabpontos2 %>% select(-ano)
    n200 = length(unique(tabpontos2$clube))
    tabpontos2$Posicao = c(1:n200)
    tabpontos2 = tabpontos2 %>% select(Posicao,clube,finalpts)
    if(n200 == 20){
      listacor = c('#002D4D','#002D4D','#002D4D','#002D4D',"#647AA5","#647AA5",
                   "#D7D9DC","#D7D9DC","#D7D9DC","#D7D9DC","#D7D9DC","#D7D9DC",
                   "#FFF","#FFF","#FFF","#FFF","#FF2525","#FF2525",
                   "#FF2525","#FF2525")
      listacor2 = c("white","white","white","white","black","black",
                    "black","black","black","black","black","black",
                    "black","black","black","black","black","black",
                    "black","black")
    }else if(n200==22){
      listacor = c('#002D4D','#002D4D','#002D4D','#002D4D',"#647AA5","#647AA5",
                   "#D7D9DC","#D7D9DC","#D7D9DC","#D7D9DC","#D7D9DC","#D7D9DC",
                   "#FFF","#FFF","#FFF","#FFF","#FFF","#FFF",
                   "#FF2525","#FF2525","#FF2525","#FF2525")
      listacor2 = c("white","white","white","white","black","black",
                    "black","black","black","black","black","black",
                    "black","black","black","black","black","black",
                    "black","black","black","black")
    }else{
      listacor = c('#002D4D','#002D4D','#002D4D','#002D4D',"#647AA5","#647AA5",
                   "#D7D9DC","#D7D9DC","#D7D9DC","#D7D9DC","#D7D9DC","#D7D9DC",
                   "#FFF","#FFF","#FFF","#FFF","#FFF","#FFF",
                   "#FFF","#FFF","#FF2525","#FF2525","#FF2525","#FF2525")
      listacor2 = c("white","white","white","white","black","black",
                    "black","black","black","black","black","black",
                    "black","black","black","black","black","black",
                    "black","black","black","black","black","black")
    }
    colnames(tabpontos2) = c("Posição","Clubes","Classificação")
    datatable(tabpontos2,
              rownames = FALSE,
              options = list(
                pageLength = n200,
                dom = 'ti',
                columnDefs = list(list(targets = c(0),className = 'dt-center',orderable = FALSE),
                                  list(targets = c(1),className = 'dt-center',orderable = FALSE),
                                  list(targets = c(2),className = 'dt-center',orderable = FALSE)),
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#fff'});",
                  "}"))) %>% formatStyle("Posição", target = 'row',
                                         backgroundColor = styleEqual(c(1:length(tabpontos2$Clubes)),
                                                                      c(listacor)),
                                         fontWeight = styleEqual(c(1),
                                                                 c("bold")),
                                         color = styleEqual(c(1:length(tabpontos2$Clubes)),
                                                            c(listacor2)),
                                         "font-size"=styleEqual(1,"15px"))
  })
  
  # Gols
  output$tabgols = renderDT({
    tabpontos = cb_gols_Clubes %>% filter(ano == input$ano1)
    tabpontos2 = reshape2::dcast(tabpontos,clube~tipo_de_gol,fun.aggregate = sum,value.var = 'total_gols')
    tabpontos2 = tabpontos2[order(tabpontos2$Gols,decreasing = c(TRUE)),]
    colnames(tabpontos2)[1] = c("Clubes")
    datatable(tabpontos2,
              rownames = FALSE,
              options = list(
                pageLength = 20,
                dom = 'ti',
                columnDefs = list(list(targets = c(0),className = 'dt-left',orderable = FALSE),
                                  list(targets = c(1),className = 'dt-center',orderable = FALSE),
                                  list(targets = c(2),className = 'dt-center',orderable = FALSE),
                                  list(targets = c(3),className = 'dt-center',orderable = FALSE)),
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#1F3F78', 'color': '#fff'});",
                  "}")))
  })
  
  # Estatísticas
  output$tabestat = renderDT({
    tabpontos = cb_estat_Clubes %>% filter(ano == input$ano2)
    tabpontos2 = tabpontos %>% select(-c(perc,ano))
    colnames(tabpontos2) = c('Clube','Total de Faltas',"Total de Impedimentos",
                             'Total de Chutes',"Total de Escanteios","Total de Chutes Certos",
                             "Percentual de chutes certos")
    datatable(tabpontos2,
              rownames = FALSE,
              options = list(
                pageLength = 20,
                dom = 'ti',
                columnDefs = list(list(targets = c(0),className = 'dt-left',orderable = FALSE),
                                  list(targets = c(1),className = 'dt-center',orderable = FALSE),
                                  list(targets = c(2),className = 'dt-center',orderable = FALSE),
                                  list(targets = c(3),className = 'dt-center',orderable = FALSE),
                                  list(targets = c(4),className = 'dt-center',orderable = FALSE),
                                  list(targets = c(5),className = 'dt-center',orderable = FALSE),
                                  list(targets = c(6),className = 'dt-center',orderable = FALSE)),
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#1F3F78', 'color': '#fff'});",
                  "}")))
  })
  
  # Cartões
  output$tabcartoes = renderDT({
    tabpontos = cb_cartoes_Clubes %>% filter(ano == input$ano3)
    tabpontos2 = reshape2::dcast(tabpontos,clube~cartao,fun.aggregate = sum,value.var = 'total_cartoes')
    colnames(tabpontos2)[1] = c('Clube')
    datatable(tabpontos2,
              rownames = FALSE,
              options = list(
                pageLength = 20,
                dom = 'ti',
                columnDefs = list(list(targets = c(0),className = 'dt-left',orderable = FALSE),
                                  list(targets = c(1),className = 'dt-center',orderable = FALSE),
                                  list(targets = c(2),className = 'dt-center',orderable = FALSE)),
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#1F3F78', 'color': '#fff'});",
                  "}")))
  })
  
  }


  
  