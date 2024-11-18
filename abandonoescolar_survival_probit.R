# Análise de Sobrevivência sobre o abandono escolar do público alvo 
# plotando curva de Kaplan-Meier por série e 
# estimando a regressão de Cox (Proportional Hazards) multivariada 
# para analisar os efeitos de covariadas no risco de abandono. 

# Carregando pacotes necessários
library(PNADcIBGE)
library(survey)
library(tidyverse)
library(dplyr)
library(openxlsx)
library(survival)
library(ggfortify)
library(gridExtra)
library(xtable)

# Definindo diretório
setwd("C:/Users/manuf/Desktop/EcoChallenge")

pnadc_1tri <-  get_pnadc(year=2023, quarter=1, defyear=2023, 
                        labels=TRUE, deflator=TRUE, 
                        design=FALSE) 
  
pnadc_2tri <-  get_pnadc(year=2023, quarter=2, defyear=2023, 
                         labels=TRUE, deflator=TRUE, 
                         design=FALSE) 

pnadc_3tri <-  get_pnadc(year=2023, quarter=3, defyear=2023, 
                         labels=TRUE, deflator=TRUE, 
                         design=FALSE)  

pnadc_4tri <-  get_pnadc(year=2023, quarter=4, defyear=2023, 
                         labels=TRUE, deflator=TRUE, 
                         design=FALSE) 

pnadc_5tri <- get_pnadc(year=2023, quarter=5, defyear=2023, 
                        labels=TRUE, deflator=TRUE, 
                        design=FALSE) 


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

# Criando variável de id_individuo para todos os trimestres
pnadc_1tri <- pnadc_1tri %>%
  mutate(id_individuo = paste0(UPA, "_", V1008, "_", V1014, "_", V2003, "_", V2008, "_", V20081, "_", V20082)) %>%
  filter(V20082 != 9999)  # Removendo observações onde V20082 é igual a 9999

pnadc_2tri <- pnadc_2tri %>%
  mutate(id_individuo = paste0(UPA, "_", V1008, "_", V1014, "_", V2003, "_", V2008, "_", V20081, "_", V20082)) %>%
  filter(V20082 != 9999)  # Removendo observações onde V20082 é igual a 9999

pnadc_3tri <- pnadc_3tri %>%
  mutate(id_individuo = paste0(UPA, "_", V1008, "_", V1014, "_", V2003, "_", V2008, "_", V20081, "_", V20082)) %>%
  filter(V20082 != 9999)  # Removendo observações onde V20082 é igual a 9999

pnadc_4tri <- pnadc_4tri %>%
  mutate(id_individuo = paste0(UPA, "_", V1008, "_", V1014, "_", V2003, "_", V2008, "_", V20081, "_", V20082)) %>%
  filter(V20082 != 9999)  # Removendo observações onde V20082 é igual a 9999

# Filtrando para pegar somente individuos que tem Visita = 1 no primeiro trimestre e frequentam escola
pnadc_1tri_1visita <- pnadc_1tri %>%
  filter(V1016 == "1") %>%
  filter(V3002 == "Sim" & V3002A == "Rede pública" & V3003A == "Regular do ensino médio") 


# Combinar bases trimestres 1 e 2
pnadc_12 <- merge(pnadc_1tri_1visita, pnadc_2tri, by = "id_individuo") %>%
         mutate(status = as.numeric(case_when(
              V3002.y == "Não" ~ 1,
              TRUE ~ 0)),
                tempo = as.numeric(case_when(
              V3002.y == "Não" ~ 2,
              TRUE ~ 0))) %>%
                select("id_individuo", "status", "tempo")

  # Identificar quem abandona no segundo trimestre 
  abandono2 <- pnadc_12 %>%
  filter(status == 1) %>%
    select("id_individuo", "status", "tempo")
   
  # Identificar quem NÃO abandona e segue  
  nao_abandono2 <- pnadc_12 %>%
    filter(status == 0) %>%
    select("id_individuo")
  
  # Identificar quem aparece em 1 mas não aparece em 2 
  anti_12 <- anti_join(pnadc_1tri_1visita, pnadc_2tri, by = "id_individuo") %>%
    mutate(status = as.numeric(0),
      tempo = as.numeric(1)) %>%
    select("id_individuo", "status", "tempo")

  
# Combinar bases trimestres 1+2 e 3
pnadc_123 <- merge(nao_abandono2, pnadc_3tri, by = "id_individuo") %>%
  mutate(status = as.numeric(case_when(
    V3002 == "Não" ~ 1,
    TRUE ~ 0)),
    tempo = as.numeric(case_when(
      V3002 == "Não" ~ 3,
      TRUE ~ 0))) %>%
  select("id_individuo", "status", "tempo")

  # Identificar quem abandona no terceiro trimestre
  abandono3 <- pnadc_123 %>%
    filter(status == 1) %>%
    select("id_individuo", "status", "tempo")
  
  # Identificar quem NÃO abandona e segue
  nao_abandono3 <- pnadc_123 %>%
    filter(status == 0) %>%
    select("id_individuo")
  
  # Identificar quem parece em 1 e 2 mas não em 3
  anti_123 <- anti_join(nao_abandono2, pnadc_3tri, by = "id_individuo") %>%
    mutate(status = as.numeric(0),
      tempo = as.numeric(2)) %>%
    select("id_individuo", "status", "tempo")

# Combinar bases trimestres 1+2+3 e 4
pnadc_1234 <- merge(nao_abandono3, pnadc_4tri, by = "id_individuo") %>%
  mutate(status = as.numeric(case_when(
    V3002 == "Não" ~ 1,
    TRUE ~ 0)),
    tempo = as.numeric(case_when(
    V3002 == "Não" ~ 4,
      TRUE ~ 0))) %>%
  select("id_individuo", "status", "tempo")

  # Identificar quem abandona no quarto trimestre
  abandono4 <- pnadc_1234 %>%
    filter(status == 1) %>%
    select("id_individuo", "status", "tempo")
  
  # Identificar quem NÃO abandona e segue
  nao_abandono4 <- pnadc_1234 %>%
    filter(status == 0) %>%
    mutate(tempo = 4)
  
  # Identificar quem aparece no 1,2,3 mas não no 4
  anti_1234 <- anti_join(nao_abandono3, pnadc_4tri, by = "id_individuo") %>%
    mutate(status = as.numeric(0),
      tempo = as.numeric(3)) %>%
    select("id_individuo", "status", "tempo")

  
# Combinar todas as bases
df_surv <- bind_rows(abandono2, abandono3, abandono4, anti_12, anti_123, anti_1234, nao_abandono4) %>%
  mutate(tempo=as.integer(tempo),
         status=as.integer(status))
  
# Combinar com os dados da PNADc do primeiro trimestre para pegar infos socioeconômicas
pnadc_surv <- merge(df_surv, pnadc_1tri_1visita, by = "id_individuo") %>%
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

# Definindo subset para o Público Alvo
pnadc_pdm <- subset(pnadc_surv, (idade >= 14 & idade <= 24) 
                            & V3003A == "Regular do ensino médio"
                            & V3002A == "Rede pública"
                            & (!is.na(rend_domiciliar_pc) & rend_domiciliar_pc<=salariominimo_ultimoano/2)
                            & VD2004 != "Unipessoal")

# Definindo subset do público potencial do programa Pé de Meia
pnadc_pdmpotencial <- subset(pnadc_surv, (idade >= 14 & idade <= 24) 
                                      & ((!is.na(rend_domiciliar_pc) & rend_domiciliar_pc<=salariominimo_ultimoano/2))
                                      & (VD3004 == "Fundamental completo ou equivalente" | VD3004 == "Médio incompleto ou equivalente"))


####### !!!! Não consegui fazer funcionar incoporando os pesos amostrais

############### Análise de Sobrevivência ################################################

# Kaplan-Meier Survival Analysis

s0<-survfit(Surv(tempo, status) ~ 1, data=pnadc_surv)
s1<-survfit(Surv(tempo, status) ~ V3006, data=pnadc_surv)
s2<-survfit(Surv(tempo, status) ~ V3006, data=pnadc_pdm)
s3<-survfit(Surv(tempo, status) ~ V3006, data=pnadc_pdmpotencial)

autoplot(s1, 
         conf.int = FALSE,
         censor.shape = '*', 
         censor.size = 5, 
         facets = TRUE,
         ylab = "Probabilidade de sobrevivência",
         xlab = "Trimestre",
         ncol = 2) +
  coord_cartesian(ylim = c(0.6, 1)) +
  labs(color = "Série", title = "Curva de Kaplan-Meier para estudantes do E.M. da Rede Pública")

autoplot(s2, 
         conf.int = FALSE,
         censor.shape = '*', 
         censor.size = 5, 
         facets = TRUE,
         ylab = "Probabilidade de sobrevivência",
         xlab = "Trimestre",
         ncol = 2) +
  coord_cartesian(ylim = c(0.6, 1)) +
  labs(color = "Série", title = "Curva de Kaplan-Meier para Público Alvo do Pé-de-Meia")

autoplot(s3, 
         conf.int = FALSE,
         censor.shape = '*', 
         censor.size = 5, 
         facets = TRUE,
         ylab = "Probabilidade de sobrevivência",
         xlab = "Trimestre",
         ncol = 2) +
  coord_cartesian(ylim = c(0.6, 1)) +
  labs(color = "Série", title = "Curva de Kaplan-Meier para Público Potencial do Pé-de-Meia")

# Cox Proportional Hazards Model

cox0 <- coxph(Surv(tempo, status) ~  sexo + raca + idade + max_educacao_pais + area + GR + rend_domiciliar_pc + n_de_moradores + serie, data = pnadc_surv)
summary(cox0)

cox1 <- coxph(Surv(tempo, status) ~  sexo + raca + idade + max_educacao_pais + area + GR + rend_domiciliar_pc + n_de_moradores + serie, data = pnadc_pdm)
summary(cox1)

cox2 <- coxph(Surv(tempo, status) ~  sexo + raca + idade + max_educacao_pais + area + GR + rend_domiciliar_pc + n_de_moradores + serie, data = pnadc_pdmpotencial)
summary(cox2)

# Carregar pacotes
library(survival)
library(xtable)

# Criar os modelos Cox
cox0 <- coxph(Surv(tempo, status) ~ sexo + raca + idade + max_educacao_pais + 
                area + GR + rend_domiciliar_pc + n_de_moradores + serie, 
              data = pnadc_surv)

cox1 <- coxph(Surv(tempo, status) ~ sexo + raca + idade + max_educacao_pais + 
                area + GR + rend_domiciliar_pc + n_de_moradores + serie, 
              data = pnadc_pdm)

cox2 <- coxph(Surv(tempo, status) ~ sexo + raca + idade + max_educacao_pais + 
                area + GR + rend_domiciliar_pc + n_de_moradores + serie, 
              data = pnadc_pdmpotencial)

# Gerar tabelas LaTeX
tab_cox0 <- xtable(summary(cox0)$coefficients, caption = "Modelo Cox - Base Completa")
tab_cox1 <- xtable(summary(cox1)$coefficients, caption = "Modelo Cox - Público-Alvo")
tab_cox2 <- xtable(summary(cox2)$coefficients, caption = "Modelo Cox - Público Potencial")

# Salvar em um arquivo .tex
latex_file <- "tabelas_modelos_cox.tex"
sink(latex_file)

cat("\\documentclass{article}\n")
cat("\\usepackage{booktabs}\n")
cat("\\usepackage[margin=1in]{geometry}\n")
cat("\\begin{document}\n")

cat("\\section*{Análise de Modelos de Cox}\n")


print(tab_cox0, include.rownames = TRUE, floating = TRUE, tabular.environment = "tabular", hline.after = NULL)
cat("\\vspace{1cm}\n")

print(tab_cox1, include.rownames = TRUE, floating = TRUE, tabular.environment = "tabular", hline.after = NULL)
cat("\\vspace{1cm}\n")

print(tab_cox2, include.rownames = TRUE, floating = TRUE, tabular.environment = "tabular", hline.after = NULL)

cat("\\end{document}\n")

sink()

# Mensagem de conclusão
cat("Arquivo LaTeX gerado: tabelas_modelos_cox.tex\n")

# Compilar para PDF (se tiver o pdflatex instalado)
system("pdflatex tabelas_modelos_cox.tex")

############ Probit Abandono ###################################################

# Realizando processo de incorporação do desenho amostral nos microdados
pnadc_probit <- tibble::as_tibble(x=pnadc_surv)
pnadc_probit <- PNADcIBGE::pnadc_design(data_pnadc=pnadc_probit) 

# Definindo subset para o Público Alvo
pnadc_pdm <- subset(pnadc_probit, (idade >= 14 & idade <= 24) 
                    & V3003A == "Regular do ensino médio"
                    & V3002A == "Rede pública"
                    & (!is.na(rend_domiciliar_pc) & rend_domiciliar_pc<=salariominimo_ultimoano/2)
                    & VD2004 != "Unipessoal")

# Definindo subset do público potencial do programa Pé de Meia
pnadc_pdmpotencial <- subset(pnadc_probit, (idade >= 14 & idade <= 24) 
                             & ((!is.na(rend_domiciliar_pc) & rend_domiciliar_pc<=salariominimo_ultimoano/2))
                             & (VD3004 == "Fundamental completo ou equivalente" | VD3004 == "Médio incompleto ou equivalente"))


probit0 <- svyglm(formula=status~sexo+raca+idade+area+GR+n_de_moradores+rend_domiciliar_pc+max_educacao_pais, design=pnadc_probit, family = binomial(link = "probit"))
summary(probit0) 
probit1 <- svyglm(formula=status~sexo+raca+idade+area+GR+n_de_moradores+rend_domiciliar_pc+max_educacao_pais, design=pnadc_pdm, family = binomial(link = "probit"))
summary(probit1) 
probit2 <- svyglm(formula=status~sexo+raca+idade+area+GR+n_de_moradores+rend_domiciliar_pc+max_educacao_pais, design=pnadc_pdmpotencial, family = binomial(link = "probit"))
summary(probit2) 

# Gerar tabelas LaTeX
tab_probit0 <- xtable(summary(probit0)$coefficients, caption = "Probit - Estudantes de E.M. da Rede Pública")
tab_probit1 <- xtable(summary(probit1)$coefficients, caption = "Probit - Público-Alvo")
tab_probit2 <- xtable(summary(probit2)$coefficients, caption = "Probit - Público Potencial")

# Salvar em um arquivo .tex
latex_file <- "tabelas_modelos_probit_abandono.tex"
sink(latex_file)

cat("\\documentclass{article}\n")
cat("\\usepackage{booktabs}\n")
cat("\\usepackage[margin=1in]{geometry}\n")
cat("\\begin{document}\n")

cat("\\section*{Probit Abandono (2023)}\n")


print(tab_probit0, include.rownames = TRUE, floating = TRUE, tabular.environment = "tabular", hline.after = NULL)
cat("\\vspace{1cm}\n")

print(tab_probit1, include.rownames = TRUE, floating = TRUE, tabular.environment = "tabular", hline.after = NULL)
cat("\\vspace{1cm}\n")

print(tab_probit2, include.rownames = TRUE, floating = TRUE, tabular.environment = "tabular", hline.after = NULL)
cat("\\end{document}\n")

sink()

# Compilar para PDF (se tiver o pdflatex instalado)
system("pdflatex tabelas_modelos_probit_abandono.tex")


