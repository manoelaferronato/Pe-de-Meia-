# Carregando pacotes necessários
library(PNADcIBGE)
library(survey)
library(tidyverse)
library(dplyr)
library(openxlsx)


# Definindo diretório
setwd("C:/Users/manuf/Desktop/EcoChallenge")


pnadc_1tri <- get_pnadc(year=2023, quarter=1, defyear=2023, 
                         labels=TRUE, deflator=TRUE, 
                         design=FALSE) %>%
              mutate(id_individuo = paste0(UPA, "_", V1008, "_", V1014, "_", V2003, "_", V2008, "_", V20081, "_", V20082)) %>%
              filter(V20082 != 9999) %>% # Removendo observações onde V20082 é igual a 9999
              filter(V1016 == "1")

pnadc_5tri <- get_pnadc(year=2024, quarter=1, defyear=2023, 
                        labels=TRUE, deflator=TRUE, 
                        design=FALSE) %>%
  mutate(id_individuo = paste0(UPA, "_", V1008, "_", V1014, "_", V2003, "_", V2008, "_", V20081, "_", V20082)) %>%
  filter(V20082 != 9999) #Removendo observações onde V20082 é igual a 9999


########### Cálculo da estimativa RDPC somente para PNADc trime- Fonte: https://github.com/Gabriel-Assuncao/PNADcIBGE-RDPC ##########################
pnadc_1tri <- transform(pnadc_1tri, ID_DOMICILIO=paste0(UPA,V1008,V1014))
pnadc_1tri <- transform(pnadc_1tri, Pais=as.factor("Brasil"))
pnadc_1tri$Pais <- factor(x=pnadc_1tri$Pais, levels=c("Brasil"))
pnadc_1tri <- transform(pnadc_1tri, GR=as.factor(ifelse(substr(UPA, start=1, stop=1)=="1","Norte",
                                                        ifelse(substr(UPA, start=1, stop=1)=="2","Nordeste",
                                                               ifelse(substr(UPA, start=1, stop=1)=="3","Sudeste",
                                                                      ifelse(substr(UPA, start=1, stop=1)=="4","Sul",
                                                                             ifelse(substr(UPA, start=1, stop=1)=="5","Centro-Oeste",
                                                                                    NA)))))))

pnadc_1tri$GR <- factor(x=pnadc_1tri$GR, levels=c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste"))

# Realizando processo de obtenção da estimativa do rendimento domiciliar real
pnadc_1tri <- transform(pnadc_1tri, V2001_rendimento=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,1))
pnadc_1tri <- transform(pnadc_1tri, VD4020real_proprioano=ifelse(is.na(VD4020) | is.na(V2001_rendimento),NA,VD4020*Efetivo))
pnadc_1tri <- transform(pnadc_1tri, VD4020real_ultimoano=ifelse(is.na(VD4020) | is.na(V2001_rendimento),NA,VD4020*Efetivo))


pnadc_1tri_rendimento <- pnadc_1tri %>% 
  dplyr::group_by(ID_DOMICILIO) %>% 
  dplyr::summarise(moradores_rendimento=sum(V2001_rendimento, na.rm=TRUE),
                   rendimento_todos_trabalhos_proprioano=sum(VD4020real_proprioano, na.rm=TRUE),
                   rendimento_todos_trabalhos_ultimoano=sum(VD4020real_ultimoano, na.rm=TRUE))

# Rendimento domiciliar real
pnadc_1tri_rendimento <- transform(pnadc_1tri_rendimento, VD5007real_proprioano=rendimento_todos_trabalhos_proprioano)
pnadc_1tri_rendimento <- transform(pnadc_1tri_rendimento, VD5007real_ultimoano=rendimento_todos_trabalhos_ultimoano)

# Rendimento domiciliar per capita real
pnadc_1tri_rendimento <- transform(pnadc_1tri_rendimento, VD5008real_proprioano=VD5007real_proprioano/moradores_rendimento)
pnadc_1tri_rendimento <- transform(pnadc_1tri_rendimento, VD5008real_ultimoano=VD5007real_ultimoano/moradores_rendimento)

# Remvendo colunas da base principal que não vamos mais usar
pnadc_1tri <- pnadc_1tri[,!(names(pnadc_1tri) %in% c("V2001_rendimento","VD4020real_proprioano","VD4020real_ultimoano"))]

# Remvendo colunas da base de rendimento que não vamos mais usar
pnadc_1tri_rendimento <- pnadc_1tri_rendimento[,!(names(pnadc_1tri_rendimento) %in% c("moradores_rendimento","rendimento_todos_trabalhos_proprioano","rendimento_todos_trabalhos_ultimoano"))]

# Combinando as bases 
pnadc_1tri <- merge(x=pnadc_1tri, y=pnadc_1tri_rendimento, by.x="ID_DOMICILIO", by.y="ID_DOMICILIO", all.x=TRUE, all.y=FALSE)

rm(pnadc_1tri_rendimento)

# Retirando variáveis de rendimento domiciliar para casos em que o morador não é da família
pnadc_1tri <- transform(pnadc_1tri, VD5007real_proprioano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5007real_proprioano))
pnadc_1tri <- transform(pnadc_1tri, VD5008real_proprioano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5008real_proprioano))
pnadc_1tri <- transform(pnadc_1tri, VD5007real_ultimoano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5007real_ultimoano))
pnadc_1tri <- transform(pnadc_1tri, VD5008real_ultimoano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5008real_ultimoano))

salariominimo_ultimoano <- 1320

########### Educação dos Pais (Máximo entre pai e mãe)
# Educação da mãe
pnadc_1tri <- pnadc_1tri %>%
  # Passo 1: Identificar a mãe em cada família
  mutate(is_mae = (as.numeric(V2007) == 2 & (as.numeric(VD2002) == 1 | as.numeric(VD2002) == 2 | as.numeric(VD2002) == 6)
  )) %>%
  
  # Passo 2: Agrupar pela família e criar a coluna educacao_mae
  group_by(ID_DOMICILIO) %>%
  mutate(educacao_mae = ifelse(any(is_mae), VD3005[is_mae][1], NA)) %>%
  
  # Passo 3: Remover a coluna auxiliar is_mae e desagrupar
  select(-is_mae) %>%
  ungroup()

# Educação do pai e máximo entre mãe e pai
pnadc_1tri <- pnadc_1tri %>%
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

############################################################################################################

# Definindo subset de estudantes do EM da Rede Pública
pnadc_1tri_em <- pnadc_1tri %>%
  filter(V3003A == "Regular do ensino médio" & V3002A == "Rede pública") %>%
  filter(V3006 == "Primeira (o)" | V3006 == "Segunda (o)" | V3006 == "Terceira (o)")

pnadc_5tri_em <- pnadc_5tri %>%
  filter(V3006 == "Primeira (o)" | V3006 == "Segunda (o)" | V3006 == "Terceira (o)" | is.na(V3006))


# Identificando evasão 
evasao <- merge(pnadc_1tri_em, pnadc_5tri_em, by = "id_individuo") %>%
  mutate(evasao = case_when(
    V3002.x == "Sim" & V3002.y == "Não" ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(evasao = case_when(
    V3002.x == "Sim" & V3002.y == "Não" & V3006.x == "Terceira (o)" & is.na(V3006.y) ~ 0,
    TRUE ~ evasao
  )) %>%
  group_by(id_individuo, evasao) %>%
  summarise(id_individuo, evasao)

# Trazendo colunas pnadc_1tri para a base de evasao
df_combinada <- merge(evasao, pnadc_1tri_em, by = "id_individuo") %>%
  mutate(V2010 = case_when(
    V2010 == "Branca" ~ "Branca",
    V2010 == "Preta" ~ "Preta ou Parda",
    V2010 == "Amarela" ~ "Amarela",
    V2010 == "Parda" ~ "Preta ou Parda", 
    V2010 == "Indígena" ~ "Indígena", 
    V2010 == "Ignorado" ~ "Ignorado"),
    V2010 = factor(V2010, levels = c("Branca", "Preta ou Parda", "Amarela", "Indígena", "Ignorado"))) %>%
  rename("area" = "V1022",
         "raca" = "V2010",
         "sexo" = "V2007",
         "idade" = "V2009",
         "n_de_moradores" = "V2001",
         "serie" = "V3006",
         "rend_domiciliar_pc" = "VD5008real_ultimoano")

# Realizando processo de incorporação do desenho amostral nos microdados
pnadc_combinada <- tibble::as_tibble(x=df_combinada)
pnadc_combinada <- PNADcIBGE::pnadc_design(data_pnadc=pnadc_combinada)

# Definindo subset para o Público Alvo
pnadc_pdm <- subset(pnadc_combinada, (idade >= 14 & idade <= 24) 
                    & V3003A == "Regular do ensino médio"
                    & V3002A == "Rede pública"
                    & (!is.na(rend_domiciliar_pc) & rend_domiciliar_pc<=salariominimo_ultimoano/2)
                    & VD2004 != "Unipessoal")

# Definindo subset do público potencial do programa Pé de Meia
pnadc_pdmpotencial <- subset(pnadc_combinada, (idade >= 14 & idade <= 24) 
                             & ((!is.na(rend_domiciliar_pc) & rend_domiciliar_pc<=salariominimo_ultimoano/2))
                             & (VD3004 == "Fundamental completo ou equivalente" | VD3004 == "Médio incompleto ou equivalente"))



probit0 <- svyglm(formula=evasao~sexo+raca+idade+area+GR+n_de_moradores+rend_domiciliar_pc+max_educacao_pais, design=pnadc_combinada, family = binomial(link = "probit"))
summary(probit0) 
probit1 <- svyglm(formula=evasao~sexo+raca+idade+area+GR+n_de_moradores+rend_domiciliar_pc+max_educacao_pais, design=pnadc_pdm, family = binomial(link = "probit"))
summary(probit1) 
probit2 <- svyglm(formula=evasao~sexo+raca+idade+area+GR+n_de_moradores+rend_domiciliar_pc+max_educacao_pais, design=pnadc_pdmpotencial, family = binomial(link = "probit"))
summary(probit2) 

# Gerar tabelas LaTeX
tab_probit0 <- xtable(summary(probit0)$coefficients, caption = "Probit - Estudantes de E.M. da Rede Pública")
tab_probit1 <- xtable(summary(probit1)$coefficients, caption = "Probit - Público-Alvo")
tab_probit2 <- xtable(summary(probit2)$coefficients, caption = "Probit - Público Potencial")

# Salvar em um arquivo .tex
latex_file <- "tabelas_modelos_probit.tex"
sink(latex_file)

cat("\\documentclass{article}\n")
cat("\\usepackage{booktabs}\n")
cat("\\usepackage[margin=1in]{geometry}\n")
cat("\\begin{document}\n")

cat("\\section*{Probit}\n")


print(tab_probit0, include.rownames = TRUE, floating = TRUE, tabular.environment = "tabular", hline.after = NULL)
cat("\\vspace{1cm}\n")

print(tab_probit1, include.rownames = TRUE, floating = TRUE, tabular.environment = "tabular", hline.after = NULL)
cat("\\vspace{1cm}\n")

print(tab_probit2, include.rownames = TRUE, floating = TRUE, tabular.environment = "tabular", hline.after = NULL)
cat("\\end{document}\n")

sink()

# Compilar para PDF (se tiver o pdflatex instalado)
system("pdflatex tabelas_modelos_probit.tex")



