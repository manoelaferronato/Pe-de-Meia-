# Carregar pacotes necessários
library(PNADcIBGE)
library(survey)
library(purrr)
library(dplyr)
library(xtable)
library(openxlsx)

################### Caracterização do Público Alvo - Programa Pé de Meia ##########################
# Objetivo: criar uma função para carregar um intervalo de dados anuais da PNADc e realizar análises demográficas e socioeconômicas do público alvo


# Definindo diretório
setwd("C:/Users/manuf/Desktop/EcoChallenge")

# Função para carregar e processar os dados de um ano específico
processar_ano <- function(ano) {
  
  pnadc_anual_visita <- get_pnadc(year=ano, 
                                  interview=1, defyear=2023, 
                                  labels=TRUE, deflator=TRUE, 
                                  design=FALSE)
  
  ########### Cálculo da estimativa RDPC - Fonte: https://github.com/Gabriel-Assuncao/PNADcIBGE-RDPC ##########################
  pnadc_anual_visita <- transform(pnadc_anual_visita, ID_DOMICILIO=paste0(UPA,V1008,V1014))
  pnadc_anual_visita <- transform(pnadc_anual_visita, Pais=as.factor("Brasil"))
  pnadc_anual_visita$Pais <- factor(x=pnadc_anual_visita$Pais, levels=c("Brasil"))
  pnadc_anual_visita <- transform(pnadc_anual_visita, GR=as.factor(ifelse(substr(UPA, start=1, stop=1)=="1","Norte",
                                                                          ifelse(substr(UPA, start=1, stop=1)=="2","Nordeste",
                                                                                 ifelse(substr(UPA, start=1, stop=1)=="3","Sudeste",
                                                                                        ifelse(substr(UPA, start=1, stop=1)=="4","Sul",
                                                                                               ifelse(substr(UPA, start=1, stop=1)=="5","Centro-Oeste",
                                                                                                      NA)))))))
  
  pnadc_anual_visita$GR <- factor(x=pnadc_anual_visita$GR, levels=c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste"))
  
  # Realizando processo de obtenção da estimativa do rendimento domiciliar real
  pnadc_anual_visita <- transform(pnadc_anual_visita, V2001_rendimento=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,1))
  pnadc_anual_visita <- transform(pnadc_anual_visita, VD4019real_proprioano=ifelse(is.na(VD4019) | is.na(V2001_rendimento),NA,VD4019*CO1))
  pnadc_anual_visita <- transform(pnadc_anual_visita, VD4048real_proprioano=ifelse(is.na(VD4048) | is.na(V2001_rendimento),NA,VD4048*CO1e))
  pnadc_anual_visita <- transform(pnadc_anual_visita, VD4019real_ultimoano=ifelse(is.na(VD4019) | is.na(V2001_rendimento),NA,VD4019*CO2))
  pnadc_anual_visita <- transform(pnadc_anual_visita, VD4048real_ultimoano=ifelse(is.na(VD4048) | is.na(V2001_rendimento),NA,VD4048*CO2e))
  
  pnadc_anual_visita_rendimento <- pnadc_anual_visita %>% 
    dplyr::group_by(ID_DOMICILIO) %>% 
    dplyr::summarise(moradores_rendimento=sum(V2001_rendimento, na.rm=TRUE),
                     rendimento_todos_trabalhos_proprioano=sum(VD4019real_proprioano, na.rm=TRUE),
                     rendimento_outras_fontes_proprioano=sum(VD4048real_proprioano, na.rm=TRUE),
                     rendimento_todos_trabalhos_ultimoano=sum(VD4019real_ultimoano, na.rm=TRUE),
                     rendimento_outras_fontes_ultimoano=sum(VD4048real_ultimoano, na.rm=TRUE))
  
  # Rendimento domiciliar real
  pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5007real_proprioano=rendimento_todos_trabalhos_proprioano+rendimento_outras_fontes_proprioano)
  pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5007real_ultimoano=rendimento_todos_trabalhos_ultimoano+rendimento_outras_fontes_ultimoano)
  
  # Rendimento domiciliar per capita real
  pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5008real_proprioano=VD5007real_proprioano/moradores_rendimento)
  pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5008real_ultimoano=VD5007real_ultimoano/moradores_rendimento)
  
  # Remvendo colunas da base principal que não vamos mais usar
  pnadc_anual_visita <- pnadc_anual_visita[,!(names(pnadc_anual_visita) %in% c("V2001_rendimento","VD4019real_proprioano","VD4048real_proprioano","VD4019real_ultimoano","VD4048real_ultimoano"))]
  
  # Remvendo colunas da base de rendimento que não vamos mais usar
  pnadc_anual_visita_rendimento <- pnadc_anual_visita_rendimento[,!(names(pnadc_anual_visita_rendimento) %in% c("moradores_rendimento","rendimento_todos_trabalhos_proprioano","rendimento_outras_fontes_proprioano","rendimento_todos_trabalhos_ultimoano","rendimento_outras_fontes_ultimoano"))]
  
  # Combinando as bases 
  pnadc_anual_visita <- merge(x=pnadc_anual_visita, y=pnadc_anual_visita_rendimento, by.x="ID_DOMICILIO", by.y="ID_DOMICILIO", all.x=TRUE, all.y=FALSE)
  
  rm(pnadc_anual_visita_rendimento)

  # Retirando variáveis de rendimento domiciliar para casos em que o morador não é da família
  pnadc_anual_visita <- transform(pnadc_anual_visita, VD5007real_proprioano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5007real_proprioano))
  pnadc_anual_visita <- transform(pnadc_anual_visita, VD5008real_proprioano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5008real_proprioano))
  pnadc_anual_visita <- transform(pnadc_anual_visita, VD5007real_ultimoano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5007real_ultimoano))
  pnadc_anual_visita <- transform(pnadc_anual_visita, VD5008real_ultimoano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5008real_ultimoano))
  
  salariominimo_ultimoano <- 1320
  
# Educação dos Pais (Máximo entre pai e mãe)
  # Educação da mãe
  pnadc_anual_visita <- pnadc_anual_visita %>%
    # Passo 1: Identificar a mãe em cada família
    mutate(is_mae = (as.numeric(V2007) == 2 & (as.numeric(VD2002) == 1 | as.numeric(VD2002) == 2 | as.numeric(VD2002) == 6)
    )) %>%
    
    # Passo 2: Agrupar pela família e criar a coluna educacao_mae
    group_by(ID_DOMICILIO) %>%
    mutate(educacao_mae = ifelse(any(is_mae), VD3005[is_mae][1], NA)) %>%
    
    # Passo 3: Remover a coluna auxiliar is_mae e desagrupar
    select(-is_mae) %>%
    ungroup()
  
  # Father Educ and Maximimum between Mother and Father
  pnadc_anual_visita <- pnadc_anual_visita %>%
    # Passo 1: Identificar a mãe em cada família
    mutate(is_pai = (as.numeric(V2007) == 1 & (as.numeric(VD2002) == 1 | as.numeric(VD2002) == 2 | as.numeric(VD2002) == 6)
    )) %>%
    
    # Passo 2: Agrupar pela família e criar a coluna educacao_pai
    group_by(ID_DOMICILIO) %>%
    mutate(educacao_pai = ifelse(any(is_pai), VD3005[is_pai][1], NA)) %>%
    
    # Passo 3: Criar a coluna max_educacao_pais com o valor máximo entre educacao_mae e educacao_pai
    mutate(max_educacao_pais = pmax(educacao_mae, educacao_pai, na.rm = TRUE)) %>%
    
    # Passo 4: Remover a coluna auxiliar is_pai e desagrupar
    select(-is_pai) %>%
    ungroup()
  
  
  # Realizando processo de incorporação do desenho amostral nos microdados
  pnadc_anual_visita <- tibble::as_tibble(x=pnadc_anual_visita)
  pnadc_anual_visita <- PNADcIBGE::pnadc_design(data_pnadc=pnadc_anual_visita)


# Definindo subset do público-alvo do programa Pé de Meia
pnadc_pdm <- subset(pnadc_anual_visita, V2009 >= 14 & V2009 <= 24 
                      & V3003A == "Regular do ensino médio"
                      & V3002A == "Rede pública"
                      & (VD5008real_ultimoano<=salariominimo_ultimoano/2 | V5001A=="Sim"|V5002A=="Sim"|V5003A=="Sim")
                      & VD2004 != "Unipessoal")

pnadc_pdm <- transform(pnadc_pdm, contagem=1)

# Definindo subset do público-alvo potencial do programa Pé de Meia
pnadc_pdmpotencial <- subset(pnadc_anual_visita, V2009 >= 14 & V2009 <= 24 
                             & (VD5008real_ultimoano<=salariominimo_ultimoano/2 | V5001A=="Sim"|V5002A=="Sim"|V5003A=="Sim")
                             & VD3004 == "Fundamental completo ou equivalente" | VD3004 == "Médio incompleto ou equivalente")

pnadc_pdmpotencial <- transform(pnadc_pdmpotencial, contagem=1)

# Definindo subset de pessoas na rede pública de ensino 
pnadc_redepublica <- subset(pnadc_anual_visita, V3002A == "Rede pública")
                             
                             

pnadc_redepublica <- transform(pnadc_redepublica, contagem=1)


# Cálculo das estatísticas baseado na condição de ano
stats <- list()

# Para os totais (totalpubalvo, totalpubalvopot e totalredepub) 
if (ano %in% c(2018, 2019, 2022, 2023)) {
  stats$totalpubalvo <- svytotal(~contagem, pnadc_pdm, na.rm = TRUE)
  stats$totalpubalvopot <- svytotal(~contagem, pnadc_pdmpotencial, na.rm = TRUE)
  stats$totalredepub <- svytotal(~contagem, pnadc_redepublica, na.rm = TRUE)

  stats$totalempot = svymean(~interaction(V3002, V3003A), pnadc_pdmpotencial, na.rm = TRUE)
  stats$totalempublicopot = svymean(~interaction(V3002, V3003A, V3002A), pnadc_pdmpotencial, na.rm = TRUE)
  stats$totaltipodomiciliopot = svymean(~VD2004, pnadc_pdmpotencial, na.rm = TRUE)
}

# Para o restante das estatísticas (somente 2018, 2019 e 2022)
if (ano %in% c(2018, 2019, 2022)) {
  stats$totalsexo <- svymean(~V2007, pnadc_pdm, na.rm = TRUE)
  stats$totalraca <- svymean(~V2010, pnadc_pdm, na.rm = TRUE)
  stats$mediarendapercapita <- svymean(~VD5008real_ultimoano, pnadc_pdm, na.rm = TRUE)
  stats$totalGR <- svymean(~GR, pnadc_pdm, na.rm = TRUE)
  stats$totalarea <- svymean(~V1022, pnadc_pdm, na.rm = TRUE)
  stats$carro <- svymean(~S010311, pnadc_pdm, na.rm = TRUE)
  stats$notebook <- svymean(~S01028, pnadc_pdm, na.rm = TRUE)
  stats$maqlav <- svymean(~S01024, pnadc_pdm, na.rm = TRUE)
  stats$totalmaterialcasa <- svymean(~S01002, pnadc_pdm, na.rm = TRUE)
  stats$totalmaterialtelhado <- svymean(~S01003, pnadc_pdm, na.rm = TRUE)
  stats$totalmaterialpiso <- svymean(~S01004, pnadc_pdm, na.rm = TRUE)
  stats$totalabastagua <- svymean(~S01007, pnadc_pdm, na.rm = TRUE)
  stats$totalaguacanal <- svymean(~S01010, pnadc_pdm, na.rm = TRUE)
  stats$totaldomicilio <- svymean(~S01017, pnadc_pdm, na.rm = TRUE)
  stats$totaltrabalha <- svymean(~V4003, pnadc_pdm, na.rm = TRUE)
  stats$totalmaxeducpais <- svymean(~max_educacao_pais, pnadc_pdm, na.rm = TRUE)
  
  stats$totalpubalvopot = svytotal(~contagem, pnadc_pdmpotencial, na.rm = TRUE)
  stats$totalsexopot = svymean(~V2007, pnadc_pdmpotencial, na.rm = TRUE)
  stats$totalracapot = svymean(~V2010, pnadc_pdmpotencial, na.rm = TRUE)
  stats$mediarendapercapitapot = svymean(~VD5008real_ultimoano, pnadc_pdmpotencial, na.rm = TRUE)
  stats$totalGRpot = svymean(~GR, pnadc_pdmpotencial, na.rm = TRUE)
  stats$totalareapot = svymean(~V1022, pnadc_pdmpotencial, na.rm = TRUE)
  stats$totalmaxeducpais <- svymean(x=~max_educacao_pais, pnadc_pdmpotencial, na.rm=TRUE)
  
  stats$totalredepub = svytotal(~contagem, pnadc_redepublica, na.rm = TRUE)
  stats$totalsexoredepub = svymean(~V2007, pnadc_redepublica, na.rm = TRUE)
  stats$totalracaredepub = svymean(~V2010, pnadc_redepublica, na.rm = TRUE)
  stats$mediarendapercapitaredepub = svymean(~VD5008real_ultimoano, pnadc_redepublica, na.rm = TRUE)
  stats$totalGRredepub = svymean(~GR, pnadc_redepublica, na.rm = TRUE)
  stats$totalarearedepub = svymean(~V1022, pnadc_redepublica, na.rm = TRUE)
  stats$carroredepub = svymean(~S010311, pnadc_redepublica, na.rm = TRUE)
  stats$notebookredepub = svymean(~S01028, pnadc_redepublica, na.rm = TRUE)
  stats$maqlavredepub = svymean(~S01024, pnadc_redepublica, na.rm = TRUE)
  stats$totalmaterialcasaredepub <- svymean(x=~S01002, pnadc_redepublica, na.rm=TRUE)
  stats$totalmaterialtelhadoredepub <- svymean(x=~S01003, pnadc_redepublica, na.rm=TRUE)
  stats$totalmaterialpisoredepub <- svymean(x=~S01004, pnadc_redepublica, na.rm=TRUE)
  stats$totalabastaguaredepub <- svymean(x=~S01007, pnadc_redepublica, na.rm=TRUE)
  stats$totalaguacanalredepub <- svymean(x=~S01010, pnadc_redepublica, na.rm=TRUE)
  stats$totaldomicilioredepub <- svymean(x=~S01017, pnadc_redepublica, na.rm=TRUE)
  stats$totaltrabalharedepub <- svymean(x=~V4003, pnadc_redepublica, na.rm=TRUE)
  stats$totalmaxeducpaisredepub <- svymean(x=~max_educacao_pais, pnadc_redepublica, na.rm=TRUE)
}

# Expandir estatísticas em um data.frame
expandir_estatisticas <- function(estat) {
  if (is.atomic(estat)) {
    return(data.frame(Categoria = names(estat), Valor = as.numeric(estat)))
  } else {
    return(data.frame(Categoria = names(estat), Valor = as.numeric(estat), row.names = NULL))
  }
}

# Transformar cada estatística em um data.frame 
resultado <- lapply(stats, function(x) {
  estat_exp <- expandir_estatisticas(x)
  estat_exp$Ano <- ano
  return(estat_exp)
})

return(resultado)
}

# Loop para calcular as estatísticas para cada ano e consolidar os resultados
anos <- c(2018, 2019, 2022, 2023)
resultados_por_ano <- lapply(anos, processar_ano)

      # Consolidar os resultados por estatística
estatisticas <- names(resultados_por_ano[[1]])
resultados_por_estatistica <- lapply(estatisticas, function(estat) {
  do.call(rbind, lapply(resultados_por_ano, `[[`, estat))
})
names(resultados_por_estatistica) <- estatisticas

# Exportar resultados para um arquivo xlsx, com uma aba para cada estatística
wb <- createWorkbook()
for (estat in estatisticas) {
  addWorksheet(wb, estat)
  writeData(wb, estat, resultados_por_estatistica[[estat]])
}
saveWorkbook(wb, "resultados_consolidado.xlsx", overwrite = TRUE)

