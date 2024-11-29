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
library(stargazer)

# Definindo diretório
setwd("C:/Users/manuf/Desktop/EcoChallenge")

pnadc_2023 <-  get_pnadc(year=2023, interview = 1, defyear=2023, 
                         labels=TRUE, deflator=TRUE, 
                         design=FALSE) 


########### Cálculo da estimativa RDPC somente para PNADc trime- Fonte: https://github.com/Gabriel-Assuncao/PNADcIBGE-RDPC ##########################
pnadc_2023 <- transform(pnadc_2023, ID_DOMICILIO=paste0(UPA,V1008,V1014))
pnadc_2023 <- transform(pnadc_2023, Pais=as.factor("Brasil"))
pnadc_2023$Pais <- factor(x=pnadc_2023$Pais, levels=c("Brasil"))
pnadc_2023 <- transform(pnadc_2023, GR=as.factor(ifelse(substr(UPA, start=1, stop=1)=="1","Norte",
                                                        ifelse(substr(UPA, start=1, stop=1)=="2","Nordeste",
                                                               ifelse(substr(UPA, start=1, stop=1)=="3","Sudeste",
                                                                      ifelse(substr(UPA, start=1, stop=1)=="4","Sul",
                                                                             ifelse(substr(UPA, start=1, stop=1)=="5","Centro-Oeste",
                                                                                    NA)))))))

pnadc_2023$GR <- factor(x=pnadc_2023$GR, levels=c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste"))

# Realizando processo de obtenção da estimativa do rendimento domiciliar real
pnadc_2023 <- transform(pnadc_2023, V2001_rendimento=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,1))
pnadc_2023 <- transform(pnadc_2023, VD4019real_proprioano=ifelse(is.na(VD4019) | is.na(V2001_rendimento),NA,VD4019*CO1))
pnadc_2023 <- transform(pnadc_2023, VD4048real_proprioano=ifelse(is.na(VD4048) | is.na(V2001_rendimento),NA,VD4048*CO1e))
pnadc_2023 <- transform(pnadc_2023, VD4019real_ultimoano=ifelse(is.na(VD4019) | is.na(V2001_rendimento),NA,VD4019*CO2))
pnadc_2023 <- transform(pnadc_2023, VD4048real_ultimoano=ifelse(is.na(VD4048) | is.na(V2001_rendimento),NA,VD4048*CO2e))

pnadc_2023_rendimento <- pnadc_2023 %>% 
  dplyr::group_by(ID_DOMICILIO) %>% 
  dplyr::summarise(moradores_rendimento=sum(V2001_rendimento, na.rm=TRUE),
                   rendimento_todos_trabalhos_proprioano=sum(VD4019real_proprioano, na.rm=TRUE),
                   rendimento_outras_fontes_proprioano=sum(VD4048real_proprioano, na.rm=TRUE),
                   rendimento_todos_trabalhos_ultimoano=sum(VD4019real_ultimoano, na.rm=TRUE),
                   rendimento_outras_fontes_ultimoano=sum(VD4048real_ultimoano, na.rm=TRUE))

# Rendimento domiciliar real
pnadc_2023_rendimento <- transform(pnadc_2023_rendimento, VD5007real_proprioano=rendimento_todos_trabalhos_proprioano+rendimento_outras_fontes_proprioano)
pnadc_2023_rendimento <- transform(pnadc_2023_rendimento, VD5007real_ultimoano=rendimento_todos_trabalhos_ultimoano+rendimento_outras_fontes_ultimoano)


# Rendimento domiciliar per capita real
pnadc_2023_rendimento <- transform(pnadc_2023_rendimento, VD5008real_proprioano=VD5007real_proprioano/moradores_rendimento)
pnadc_2023_rendimento <- transform(pnadc_2023_rendimento, VD5008real_ultimoano=VD5007real_ultimoano/moradores_rendimento)

# Remvendo colunas da base principal que não vamos mais usar
pnadc_2023 <- pnadc_2023[,!(names(pnadc_2023) %in% c("V2001_rendimento","VD4019real_proprioano","VD4048real_proprioano","VD4019real_ultimoano","VD4048real_ultimoano"))]

# Remvendo colunas da base de rendimento que não vamos mais usar
pnadc_2023_rendimento <- pnadc_2023_rendimento[,!(names(pnadc_2023_rendimento) %in% c("moradores_rendimento","rendimento_todos_trabalhos_proprioano","rendimento_outras_fontes_proprioano","rendimento_todos_trabalhos_ultimoano","rendimento_outras_fontes_ultimoano"))]

# Combinando as bases 
pnadc_2023 <- merge(x=pnadc_2023, y=pnadc_2023_rendimento, by.x="ID_DOMICILIO", by.y="ID_DOMICILIO", all.x=TRUE, all.y=FALSE)

rm(pnadc_2023_rendimento)

# Retirando variáveis de rendimento domiciliar para casos em que o morador não é da família
pnadc_2023 <- transform(pnadc_2023, VD5007real_proprioano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5007real_proprioano))
pnadc_2023 <- transform(pnadc_2023, VD5008real_proprioano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5008real_proprioano))
pnadc_2023 <- transform(pnadc_2023, VD5007real_ultimoano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5007real_ultimoano))
pnadc_2023 <- transform(pnadc_2023, VD5008real_ultimoano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5008real_ultimoano))

salariominimo_ultimoano <- 1320

########### Educação dos Pais (Máximo entre pai e mãe)
# Educação da mãe
pnadc_2023 <- pnadc_2023 %>%
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
pnadc_2023 <- pnadc_2023 %>%
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

# Criando colunas de sal/hora, log sal, etc
pnadc_2023 <-  pnadc_2023 %>%
  mutate(log_VD4017 = ifelse(VD4017==0, VD4017, log(VD4017)),
         sal_hora = ifelse(VD4017/(V4039*4.3)==0, VD4017, VD4017/(V4039*4.3)),
         log_sal_hora = ifelse(sal_hora==0, 0, log(sal_hora))) %>%
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
         "n_de_moradores" = "V2001")

# Incorporação do design amostral
design_2023 <- tibble::as_tibble(x=pnadc_2023)
design_2023 <- PNADcIBGE::pnadc_design(data_pnadc=design_2023) 

# ****************************************************************************
#                           Equação de Mincer
# ****************************************************************************

# Subset para pessoas de 24 a 40 anos que não são funcionários públicos
pnadc_mincer <- subset(design_2023, (idade >= 25 & idade <= 40) 
                    & VD4008 != "Empregado no setor público (inclusive servidor estatutário e militar)")

pnadc_mincer <- update(pnadc_mincer, log_sal_hora = ifelse(is.na(log_sal_hora), 0, log_sal_hora))                     

# Estimando modelo linear 
mod_mincer <- svyglm(formula=log_sal_hora ~ VD3005 + sexo + raca + area + n_de_moradores + GR, design=pnadc_mincer)
summary(mod_mincer)

summary(pnadc_mincer$variables$VD3005)
# Definindo subset para o público potencial 
pnadc_pdmpotencial <- subset(design_2023, (idade >= 14 & idade <= 24) 
                             & ((!is.na(VD5008real_ultimoano) & VD5008real_ultimoano<=salariominimo_ultimoano/2))
                             & (VD3004 == "Fundamental completo ou equivalente" | VD3004 == "Médio incompleto ou equivalente"))

# Resgatando resíduos da eq de Mincer
residuos <- residuals(mod_mincer, type = "response")

# Amostra aleatória dos resíudos com reposição
residuos_amostra <- sample(residuos, size = nrow(pnadc_pdmpotencial), replace = TRUE)

# Criando um data.frame a partir do design de pnadc_pdmpotencial
pnadc_pdmpotencial_df <- as.data.frame(pnadc_pdmpotencial$variables)

# Criando a matriz de dummies para a variável VD3005
variaveis_explicativas <- model.matrix(~ VD3005 + sexo + raca + area + GR - 1, 
                                       data = pnadc_pdmpotencial_df)
variaveis_explicativas <- variaveis_explicativas[, -1]
dim(variaveis_explicativas)

# Recuperar coeficientes da eq de Mincer
coeficientes <- coef(mod_mincer)
coeficientes_exc <- coeficientes[-1]  
coeficientes_exc <- coeficientes_exc[-24] 

# Calcular log_sal_hora usando coeficientes e resíduos para Publico Potencial
pnadc_pdmpotencial_df <- pnadc_pdmpotencial_df %>%
  mutate(log_sal_hora_imputado = coeficientes[1] + 
           as.vector(variaveis_explicativas %*% coeficientes_exc) + coeficientes[24] +
           residuos_amostra,  
         sal_hora_imputado = exp(log_sal_hora_imputado))

# Agora, atualizando o design amostral com as novas variáveis imputadas
pnadc_pdmpotencial <- update(pnadc_pdmpotencial, 
                             log_sal_hora_imputado = pnadc_pdmpotencial_df$log_sal_hora_imputado,
                             sal_hora_imputado = pnadc_pdmpotencial_df$sal_hora_imputado)

# Estimando eq de Mincer para Público Potencial
mod_mincer_pubpot <- svyglm(formula=log_sal_hora_imputado ~ VD3005 + sexo + raca + area + n_de_moradores + GR, design=pnadc_pdmpotencial)
summary(mod_mincer_pubpot)

# Gerar tabela final usando stargazer
stargazer(
  mod_mincer, mod_mincer_pubpot,
  type = "latex",
  title = "Equação de Mincer",
  dep.var.labels.include = FALSE,
  column.labels = c("ln(sal/hora) - Público 25-40 anos","ln(sal/hora) - Público Potencial PdM"),
  omit.stat = c("aic", "bic", "adj.rsq", "f", "ser", "ll"),
  star.cutoffs = c(0.05, 0.01, 0.001),
  digits = 3,
  notes = "Standard errors in parentheses. * p<0.05, ** p<0.01, *** p<0.001",
  out = "AverageEffects.tex"
)

# ****************************************************************************
#                           Heckit
# ****************************************************************************

# 1 Estágio
# Definir subset
pnadc_heckit <- subset(design_2023, (idade >= 25 & idade <= 40) 
                       & (is.na(VD4008) | VD4008 != "Empregado no setor público (inclusive servidor estatutário e militar)"))


# Criar uma variável indicadora para estar empregado
pnadc_heckit <- update(pnadc_heckit, empregado = ifelse(!is.na(VD4017) & VD4017 > 0, 1, 0))


# Rodar probit da função controle
probit_selecao <- svyglm(empregado ~ idade + sexo + raca + V2005,
                design   = pnadc_heckit,
                family = binomial(link = 'probit'))

summary(probit_selecao)

# Resgatar as probabilidades de seleção para cada observação
probit_p <- predict(probit_selecao)

# Índice de Mills
pnadc_heckit <- update(pnadc_heckit , mills0 = dnorm(probit_p)/pnorm(probit_p))

# 2 Estágio
# Estimar linear novamente usando mills0
mod_heckit <- 
  svyglm(
    log_sal_hora ~ VD3005 + idade + I(idade^2) + sexo + raca + area + n_de_moradores + GR + mills0, 
    design = subset(pnadc_heckit, empregado) 
  )

summary(mod_heckit)

# Gerar tabela final usando stargazer
stargazer(
  probit_selecao, mod_heckit,
  type = "text",
  title = "Heckit",
  dep.var.labels.include = FALSE,
  column.labels = c("Eq. de Seleção (probit)","ln(sal/hora)"),
  omit.stat = c("aic", "bic", "adj.rsq", "f", "ser", "ll"),
  star.cutoffs = c(0.05, 0.01, 0.001),
  digits = 3,
  notes = "Standard errors in parentheses. * p<0.05, ** p<0.01, *** p<0.001",
  out = "AverageEffects.tex"
)

