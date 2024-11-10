# Carregar pacotes necessários
library(tidyverse)
library(dplyr)
library(readxl)
library(xtable)
library(openxlsx)
library(tibble)

################### Caracterização do Público Alvo - Programa Pé de Meia ##########################
# Objetivo: gerar análises em Latex ou Gráficos para diferentes subamostras e anos

# Definindo diretório
setwd("C:/Users/manuf/Desktop/EcoChallenge")

###########################################################################################################

# 1. Tabela Latex com Total de Participantes (Público Alvo, Potencial e Rede Pública)

# Define a função que processa e consolida as abas em colunas para uma única tabela
tab_latex_por_subamostra <- function(caminho_arquivo, abas) {
  # Inicializa uma lista para armazenar os dados de cada aba
  lista_dados <- list()
  
  # Loop para carregar e processar cada aba, armazenando na lista com o nome da aba
  for (aba in abas) {
    dados <- read.xlsx(caminho_arquivo, sheet = aba) %>%
             mutate(Ano = as.integer(Ano))

    
    # Renomeia a coluna "Valor" com o nome da aba para identificar a subamostra
    names(dados)[names(dados) == "Valor"] <- ifelse(aba == "totalpubalvo", "Público Alvo", ifelse(aba=="totalpubalvopot", "Público Potencial", "Rede Pública"))
    
    # Armazena a tabela processada na lista
    lista_dados[[aba]] <- dados
  }
  
  # Consolida os dados em uma tabela única, fazendo a junção das abas por "Categoria"
  dados_consolidados <- Reduce(function(x, y) merge(x, y, by = c("Categoria", "Ano"), all = TRUE), lista_dados)
  
  # Converte os dados consolidados para o formato LaTeX
  tabela_latex <- xtable(dados_consolidados)
  
  # Define o nome do arquivo de saída
  nome_arquivo <- "tabela_total_por_subamostra.txt"
  
  # Salva o código LaTeX no arquivo .txt
  print(tabela_latex, type = "latex", file = nome_arquivo, include.rownames = FALSE)
  
  # Retorna uma mensagem confirmando a criação do arquivo
  message("Arquivo criado: ", nome_arquivo)
}

# Lista das abas que você quer processar
abas <- c("totalpubalvo", "totalpubalvopot", "totalredepub")

# Caminho do arquivo Excel
caminho_arquivo <- "C:/Users/manuf/Desktop/EcoChallenge/resultados_consolidado.xlsx"

# Executa a função para gerar a tabela consolidada
tab_latex_por_subamostra(caminho_arquivo, abas)

###########################################################################################################

# 2. Gráfico distribuição por região (Público Alvo, Público Potencial e Rede Pública)

# Carregar o shapefile
shp_reg <- sf::read_sf("C:/Users/manuf/Desktop/EcoChallenge/regioes_2010/regioes_2010.shp") %>%
  rename("Categoria" = "nome")

# Lista de abas para as quais você deseja criar os gráficos
abas <- c("totalGR", "totalGRpot", "totalGRredepub")  

# Loop para iterar sobre as abas e salvar os gráficos
for (aba in abas) {
  # Carregar os dados da aba
  p <- read.xlsx("C:/Users/manuf/Desktop/EcoChallenge/resultados_consolidado.xlsx", aba) %>%
    filter(Ano == 2023) %>%
    mutate(Categoria = substring(Categoria, 3)) %>%
    left_join(shp_reg, by = "Categoria")
  
  # Gerar o gráfico para cada aba
  grafico <- p %>%
    ggplot(aes(fill = Valor, geometry = geometry)) +
    geom_sf(color = "grey90") +  
    geom_sf_text(aes(label = Categoria), size = 4, color = "black", fontface = "bold", alpha = 0.7, nudge_y = 1) +
    geom_sf_text(aes(label = paste0(round(Valor * 100), "%")), size = 4, color = "black", fontface = "plain", alpha = 0.7, nudge_y = -1) +
    scale_fill_gradient(low = "white", high = "black", limits = c(0, 1), guide = "none") +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),  
      plot.caption = element_text(size = 10, hjust = 0.5, color = "grey40")  
    )
  
  # Salvar o gráfico em um arquivo
  ggsave(paste0("grafico_", aba, "_2023.png"), plot = grafico, width = 10, height = 8, dpi = 300)
}

###########################################################################################################

# 3. Tabela características demográficas (Público Alvo, Potencial e Rede Pública)

# Define a função para processar e consolidar as abas em uma tabela única
tab_latex_por_categoria <- function(caminho_arquivo, pares_abas) {
  # Inicializa uma lista para armazenar os dados de cada par de abas
  lista_dados <- list()
  
  # Loop para carregar e processar cada par de abas
  for (par in names(pares_abas)) {
    abas <- pares_abas[[par]]
    
    # Carrega as abas da categoria para "Público Alvo", Potencial" e Rede Pública"
    dados_publico_alvo <- read.xlsx(caminho_arquivo, sheet = abas[1]) %>%
      filter(Ano == 2022) %>%
      rename(`Público Alvo` = Valor) %>%
      select(Categoria, `Público Alvo`)
    
    dados_publico_potencial <- read.xlsx(caminho_arquivo, sheet = abas[2]) %>%
      filter(Ano == 2022) %>%
      rename(`Público Potencial` = Valor) %>%
      select(Categoria, `Público Potencial`)
    
    dados_rede_publica <- read.xlsx(caminho_arquivo, sheet = abas[3]) %>%
      filter(Ano == 2022) %>%
      rename(`Rede Pública` = Valor) %>%
      select(Categoria, `Rede Pública`)
    
    # Condição para realizar transformações nas outras categorias, exceto "Renda Média per capita"
    if (par != "Renda Média per capita") {
      dados_publico_alvo <- dados_publico_alvo %>%
        mutate(`Público Alvo` = round(`Público Alvo` * 100, 2),
               `Público Alvo` = paste0(`Público Alvo`, "%")) 
      
      dados_publico_potencial <- dados_publico_potencial %>%
        mutate(`Público Potencial` = round(`Público Potencial` * 100, 2),
               `Público Potencial` = paste0(`Público Potencial`, "%")) 
      
      dados_rede_publica <- dados_rede_publica %>%
        mutate(`Rede Pública` = round(`Rede Pública` * 100, 2),
               `Rede Pública` = paste0(`Rede Pública`, "%")) 
    }
    
    dados_categoria <- merge(dados_publico_alvo, dados_publico_potencial, by = "Categoria", all = TRUE)
    dados_categoria <- merge(dados_categoria, dados_rede_publica, by = "Categoria", all = TRUE)
    
    # Adiciona o nome da categoria como uma linha de título para formatação
    dados_categoria <- dados_categoria %>%
      add_row(Categoria = par, `Público Alvo` = NA, `Público Potencial` = NA, `Rede Pública` = NA, .before = 1)
    
    # Transforma todas as colunas para 'character' 
    dados_categoria <- dados_categoria %>%
      mutate_all(as.character)
    
    # Armazena a tabela formatada na lista
    lista_dados[[par]] <- dados_categoria
  }
  
  # Consolida as tabelas de todas as categorias em uma única tabela
  dados_consolidados <- bind_rows(lista_dados)
  
  # Gera a tabela LaTeX 
  tabela_latex <- xtable(dados_consolidados, type = "latex")
  
  # Salva o código LaTeX no arquivo
  print(tabela_latex, include.rownames = FALSE, hline.after = c(-1, 0, nrow(dados_consolidados)), file = "tabela_demo_por_subamostra.txt")
  
  # Mensagem de confirmação
  message("Arquivo criado: tabela_demo_por_subamostra.txt")
}

# Lista de pares de abas para cada categoria
pares_abas <- list(
  "Sexo" = c("totalsexo", "totalsexopot","totalsexoredepub"),
  "Raça"= c("totalraca", "totalracapot", "totalracaredepub"),
  "Localização" = c("totalarea", "totalareapot", "totalarearedepub"),
  "Renda Média per capita" = c("mediarendapercapita", "mediarendapercapitapot", "mediarendapercapitaredepub")
)


# Caminho do arquivo Excel
caminho_arquivo <- "C:/Users/manuf/Desktop/EcoChallenge/resultados_consolidado.xlsx"

# Executa a função para gerar a tabela consolidada
tab_latex_por_categoria(caminho_arquivo, pares_abas)

###########################################################################################################

# 4. Tabela de características socioeconômicas (Público Alvo x Rede Pública)

# Define a função para processar e consolidar as abas em uma tabela única
tab_latex_por_categoria <- function(caminho_arquivo, pares_abas) {
  # Inicializa uma lista para armazenar os dados de cada par de abas
  lista_dados <- list()
  
  # Loop para carregar e processar cada par de abas
  for (par in names(pares_abas)) {
    abas <- pares_abas[[par]]
    
    # Carrega as abas da categoria para "Público Alvo" e "Rede Pública"
    dados_publico_alvo <- read.xlsx(caminho_arquivo, sheet = abas[1]) %>%
      filter(Ano == 2022) %>%
      rename(`Público Alvo` = Valor) %>%
      select(Categoria, `Público Alvo`) %>%
      mutate(`Público Alvo` = round(`Público Alvo` * 100, 2),
             `Público Alvo` = paste0(`Público Alvo`, "%"))
    
    dados_rede_publica <- read.xlsx(caminho_arquivo, sheet = abas[2]) %>%
      filter(Ano == 2022) %>%
      rename(`Rede Pública` = Valor) %>%
      select(Categoria, `Rede Pública`) %>%
      mutate(`Rede Pública` = round(`Rede Pública` * 100, 2),
             `Rede Pública` = paste0(`Rede Pública`, "%"))
    
    dados_categoria <- merge(dados_publico_alvo, dados_rede_publica, by = "Categoria", all = TRUE)
    
    # Adiciona o nome da categoria como uma linha de título para formatação
    dados_categoria <- dados_categoria %>%
      add_row(Categoria = par, `Público Alvo` = NA, `Rede Pública` = NA, .before = 1)
    
    # Armazena a tabela formatada na lista
    lista_dados[[par]] <- dados_categoria
  }
  
  # Consolida as tabelas de todas as categorias em uma única tabela
  dados_consolidados <- bind_rows(lista_dados)
  
  # Gera a tabela LaTeX usando o pacote xtable
  tabela_latex <- xtable(dados_consolidados, type = "latex")
  
  # Salva o código LaTeX no arquivo
  print(tabela_latex, include.rownames = FALSE, hline.after = c(-1, 0, nrow(dados_consolidados)), file = "tabela_socio_por_subamostra.txt")
  
  # Mensagem de confirmação
  message("Arquivo criado: tabela_socio_por_subamostra.txt")
}

# Lista de pares de abas para cada categoria
pares_abas <- list(
  
  "Carro" = c("carro", "carroredepub"),
  "Material da Casa" = c("totalmaterialcasa", "totalmaterialcasaredepub"),
  "Material do Telhado" = c("totalmaterialtelhado", "totalmaterialtelhadoredepub"),
  "Material do Piso" = c("totalmaterialpiso", "totalmaterialpisoredepub"),
  "Abastecimento de Água" = c("totalabastagua", "totalabastaguaredepub"),
  "Água Canalizada" = c("totalaguacanal", "totalaguacanalredepub"),
  "Tipo do Domicílio" = c("totaldomicilio", "totaldomicilioredepub"),
  "Média de anos de escolaridade (Máx. Educação Pai e Mãe)" = c("totalmaxeducpais", "totalmaxeducpaisredepub")
)

# Caminho do arquivo Excel
caminho_arquivo <- "C:/Users/manuf/Desktop/EcoChallenge/resultados_consolidado.xlsx"

# Executa a função para gerar a tabela consolidada
tab_latex_por_categoria(caminho_arquivo, pares_abas)

###########################################################################################################

# 5. Quantos atualmente frequentam EM? Quantos frequentam EM público?  (Público Potencial)

# Define a função que processa e consolida as abas em colunas para uma única tabela
tab_latex_em <- function(caminho_arquivo, abas) {
  # Inicializa uma lista para armazenar os dados de cada aba
  lista_dados <- list()
  
  # Loop para carregar e processar cada aba, armazenando na lista com o nome da aba
  for (aba in abas) {
    dados <- read.xlsx(caminho_arquivo, sheet = aba) %>%
      filter(Ano == 2023 & Valor > 0) %>%
      mutate(Valor = round(Valor * 100, 2),
        Valor = paste0(Valor, "%")) %>%
        select(Categoria, Valor)
        
    
    if (aba == "totalempot") {
      dados <- dados %>%
        mutate(Categoria = substr(Categoria, 27, nchar(Categoria)))
    }
    
    if (aba == "totalempublicopot") {
      dados <- dados %>%
       mutate(Categoria = substr(Categoria, 35, nchar(Categoria)))
    }
      
    # Adiciona o nome da categoria como uma linha de título para formatação
    dados <- dados %>%
      add_row(Categoria = aba, Valor = NA, .before = 1)
    
    names(dados)[names(dados) == "Valor"] <- "Público Potencial"
    
    # Armazena a tabela processada na lista
    lista_dados[[aba]] <- dados
  }
  
  # Consolida as tabelas de todas as categorias em uma única tabela
  dados_consolidados <- bind_rows(lista_dados)
  
  # Converte os dados consolidados para LaTeX
  tabela_latex <- xtable(dados_consolidados)
  
  # Define o nome do arquivo de saída
  nome_arquivo <- "tabela_pubpot_EM.txt"
  
  # Salva o código LaTeX no arquivo .txt
  print(tabela_latex, type = "latex", file = nome_arquivo, include.rownames = FALSE)
  
  # Retorna uma mensagem confirmando a criação do arquivo
  message("Arquivo criado: ", nome_arquivo)
}

# Lista das abas que você quer processar
abas <- c("totalempot", "totalempublicopot")

# Caminho do arquivo Excel
caminho_arquivo <- "C:/Users/manuf/Desktop/EcoChallenge/resultados_consolidado.xlsx"

# Executa a função para gerar a tabela consolidada
tab_latex_em(caminho_arquivo, abas)


###########################################################################################################

# 6. Tabela de distribuição tipo de domicílio - Unipessoal, etc. (Público Potencial)

# Define a função que processa e consolida as abas 
tab_latex_dom <- function(caminho_arquivo, abas) {
  
  # Loop para carregar e processar cada aba, armazenando na lista com o nome da aba
  for (aba in abas) {
    dados <- read.xlsx(caminho_arquivo, sheet = aba) %>%
      filter(Ano == 2023) %>%
      mutate(Valor = round(Valor * 100, 2),
             Valor = paste0(Valor, "%"),
             Categoria = substr(Categoria, 7, nchar(Categoria))) %>%
      select(Categoria, Valor)
    
    
    # Renomeia a coluna "Valor" para identificar a subamostra
    names(dados)[names(dados) == "Valor"] <- "Público Potencial"
    
  }
  
  # Converte os dados para LaTeX
  tabela_latex <- xtable(dados)
  
  # Define o nome do arquivo de saída
  nome_arquivo <- "tabela_pubpot_tipodom.txt"
  
  # Salva o código LaTeX no arquivo .txt
  print(tabela_latex, type = "latex", file = nome_arquivo, include.rownames = FALSE)
  
  # Retorna uma mensagem confirmando a criação do arquivo
  message("Arquivo criado: ", nome_arquivo)
}

# Lista das abas que você quer processar
abas <- c("totaltipodomiciliopot")

# Caminho do arquivo Excel
caminho_arquivo <- "C:/Users/manuf/Desktop/EcoChallenge/resultados_consolidado.xlsx"

# Executa a função para gerar a tabela consolidada
tab_latex_dom(caminho_arquivo, abas)

