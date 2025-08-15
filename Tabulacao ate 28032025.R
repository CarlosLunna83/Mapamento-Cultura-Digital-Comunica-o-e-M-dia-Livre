# Tabulação das respostas ARForms até 28/03/2025

library(tidyverse)
library(janitor)
library(readODS)

# Respostas validadas no geral até 28/03/2025:

validadas_geral <- read_ods("Tabulacao/Ate 28032025/tabela_para_tabulacao_28032025.ods") %>%
  clean_names()

colnames(validadas_geral)

##########################################################################################
# Sombreados de azul claro na planilha: Inscrições que ainda não estavam na base de dados do MinC

novas_inscricoes <- read_ods("Tabulacao/Ate 28032025/ARForms_mapeamento-temtico-da-cultura-digital-opcional_ at 28032025 - 1731455268_0 novas inscricoes.ods") %>%
  clean_names()

view(novas_inscricoes)

# Total 868

##########################################################################################

# Sombreados de verde na planilha: Pontos de Cultura já Inscritos na Base do MinC Mapas Culturais

ja_inscritxs <- read_ods("Tabulacao/Ate 28032025/ARForms_mapeamento-temtico-da-cultura-digital-opcional_ at 28032025 - 1731455268_0 ja inscritxs.ods") %>%
  clean_names()

view(ja_inscritxs)

# Total 383

##########################################################################################
# Questão 1: Quantidade de Agentes Cultura Viva que já utilizaram:

agentes_usando <- unique(validadas_geral$x1_nome_do_agente_cultura_viva)

# Total 181 agentes cultura viva diferentes já responderam ao mapeamento utilizando o formulário do
# Colaborativas.net

##########################################################################################
# Questão 2: Quantidade de pontões utilizando o formulário:

pontoes_usando_form <-unique(validadas_geral$x2_nome_da_instituicao_proponente_do_pontao_de_cultura)

View(pontoes_usando_form)

# Total 19 Pontões

#########################################################################################

# Questão 4: É uma Entidade (com CNPJ) ou Coletivo (sem CNPJ)

entidades <- validadas_geral %>%
  filter(str_detect(x4_e_uma_entidade_com_cnpj_ou_coletivo_sem_cnpj, "Entidade"))

# 593 dos Pontos de cultura ou coletivos são Entidades com CNPJ

coletivos <- validadas_geral %>%
  filter(str_detect(x4_e_uma_entidade_com_cnpj_ou_coletivo_sem_cnpj, "Coletivo"))

# 658 foram respondidos por Coletivos não formalizados

##########################################################################################

# Questão 6: Tipo de sede

tipo_de_sede <- validadas_geral %>%
  select(x6_possui_sede_se_sim_indique_qual_a_condicao, quantidade) %>%
  filter(!is.na(x6_possui_sede_se_sim_indique_qual_a_condicao)) %>%
  group_by(x6_possui_sede_se_sim_indique_qual_a_condicao) %>%
  summarise(sede = sum(quantidade)) %>%
  arrange(desc(sede))

tipo_de_sede

#1 Própria 412
#2 Cedida  293
#3 Não possui sede fixa 209
#4 Alugada 203
#5 Equipamento público (escola, centro cultural, teatro etc)    95
#6 Outros 39

##########################################################################################
# Questão 9: Quantidade de municípios citados

qtd_mun <- unique(validadas_geral$x9_municipio)

# Total de 450 municípios citados

#########################################################################################
# Questão 11: Respostas por Unidade Federativa

respostas_por_estado <-validadas_geral %>%
  select(x11_estado, quantidade) %>%
  group_by(x11_estado) %>%
  summarise(por_estado = sum(quantidade)) %>%
  arrange(desc(por_estado))

view(respostas_por_estado)

#1 Sergipe 268
#2 Espírito Santo 232
#3 Bahia 218
#4 Rio Grande do Norte 65
#5 Rio Grande do Sul 57
#6 Santa Catarina 49
#7 São Paulo 46
#8 Minas Gerais 45
#9 Rio de Janeiro 44
#10 Goiás 38
#11 Ceará 32
#12 Alagoas 29
#13 Amazonas 20
#14 Maranhão 19
#15 Paraná 14
#16 Pernambuco 14
#17 Mato Grosso do Sul 12
#18 Piauí 11
#19 Pará 9
#20 Acre 7
#21 Distrito Federal 5
#22 Paraíba 5
#23 Rondônia 5
#24 Amapá 2
#25 Mato Grosso 1
#26 Roraima 1
#27 Tocantins 1

#########################################################################################

# Questão 17: Está em território de povos e comunidades tradicionais?

territorio_comunidades_tradicionais <- validadas_geral %>%
  select(x17_esta_em_territorio_de_povos_e_comunidades_tradicionais, quantidade) %>%
  filter(!is.na(x17_esta_em_territorio_de_povos_e_comunidades_tradicionais)) %>%
  group_by(x17_esta_em_territorio_de_povos_e_comunidades_tradicionais) %>%
  summarise(territorio_tradicional = sum(quantidade)) %>%
  arrange(desc(territorio_tradicional))

view(territorio_comunidades_tradicionais)

# 1 Não está em território de Comunidades tradicionais 690
# 2 Quilombolas 134
# 3 Sertanejos 114
# 4 Comunidades de Terreiro/Povos e Comunidades de Matriz Africana 95
# 5 Povos Indígenas 54
# 6 Pescadores Artesanais 44
# 7 Ribeirinhos 32
# 8 Açorianos 28
# 9 Caatingueiros 12
# 10 Extrativistas Costeiros e Marinhos 9
# 11 Praieiros 6
# 12 Marisqueiras 5
# 13 Raizeiros 5
# 14 Campeiros 3
# 15 Seringueiros 3
# 16 Benzendeiros 2
# 17 Catadores de Mangaba 2
# 18 Ciganos 2
# 19 Ilhéus 2
# 20 Apanhadores de Flores Sempre-Vivas 1
# 21 Caboclos 1
# 22 Castanheiros 1
# 23 Extrativistas 1
# 24 Fundo e Fecho de Pasto 1
# 25 Jangadeiros 1
# 26 Quebradeiras de coco-de-babaçu 1

#########################################################################################
# Questão 18: Conhece a Política Nacional Cultura Viva?

conhece_cultura_viva <- validadas_geral %>%
  select(x18_conhece_a_politica_nacional_cultura_viva, quantidade) %>%
  group_by(x18_conhece_a_politica_nacional_cultura_viva) %>%
  summarise(conhece_ou_nao = sum(quantidade)) %>%
  arrange(desc(conhece_ou_nao))

conhece_cultura_viva

# Sim 933
# Não 318

########################################################################################
# Questão 19: Conhece os Pontos de Cultura?

conhece_pontos_cultura <- validadas_geral %>%
  select(x19_conhece_os_pontos_de_cultura, quantidade) %>%
  group_by(x19_conhece_os_pontos_de_cultura) %>%
  summarise(conhecepc_ou_nao = sum(quantidade)) %>%
  arrange(desc(conhecepc_ou_nao))

conhece_pontos_cultura

# Sim 1013
# Não 238

#########################################################################################

# Questão 20: Pretende se inscrever em editais da Política Nacional Aldir Blanc?

pretende_aldir_blanc <- validadas_geral %>%
  select(x20_pretendem_se_inscrever_em_editais_da_politica_nacional_aldir_blanc, quantidade) %>%
  group_by(x20_pretendem_se_inscrever_em_editais_da_politica_nacional_aldir_blanc) %>%
  summarise(pretende_ou_nao = sum(quantidade)) %>%
  arrange(desc(pretende_ou_nao))

pretende_aldir_blanc

# Sim 1208
# Não   43

#########################################################################################
# Questão 22
# Pontos de Cultura com Atuação prioritária Cultura Digital, Comunicação e Mídia Livre

questao_22 <- validadas_geral %>%
  filter(str_detect(x22_atuacao_tematica_prioritaria, "Cultura Digital, Comunicação e Mídia Livre"))

# Total 60 respostas

colnames(questao_22)

view(questao_22)

#########################################################################################

# Questão 23
# Pontos de Cultura com Atuação secundária Cultura Digital, Comunicação e Mídia Livre

questao_23 <- validadas_geral %>%
  filter(str_detect(x23_atuacao_tematica_secundaria, "Cultura Digital, Comunicação e Mídia Livre"))

# Total 80 respostas

colnames(questao_23)

view(questao_23)

#######################################################################################

cultura_digital_22_23_juntas <- read_ods("Tabulacao/Ate 28032025/ARForms_mapeamento-temtico-da-cultura-digital-opcional_ at 28032025 - 1731455268_0 questoes 22 e 23 juntas cultura digital.ods") %>%
  clean_names()

view(cultura_digital_22_23_juntas)

# Total 137 respostas

#########################################################################################
# Questão 24: A entidade/coletivo cultural realiza atividades e atendimento ao público 
# em sua sede?

atende_publico_na_sede <- validadas_geral %>%
  select(x24_a_entidade_coletivo_cultural_realiza_atividades_e_atendimento_ao_publico_em_sua_sede, quantidade) %>%
  group_by(x24_a_entidade_coletivo_cultural_realiza_atividades_e_atendimento_ao_publico_em_sua_sede) %>%
  summarise(atende_ou_nao = sum(quantidade)) %>%
  arrange(desc(atende_ou_nao))

atende_publico_na_sede

# Sim 1040
# Não  211

#########################################################################################
# Questão 25: Acesso à internet:

acesso_internet <- validadas_geral %>%
  select(x25_tem_acesso_a_internet, quantidade) %>%
    group_by(x25_tem_acesso_a_internet) %>%
  summarise(acesso_ou_nao = sum(quantidade)) %>%
  arrange(desc(acesso_ou_nao))

acesso_internet

# 1 Na entidade/Coletivo Cultural                                 573
# 2 Em casa                                                       321
# 3 Na entidade/Coletivo Cultural, Em casa                        135
# 4 Outro local                                                    75
# 5 Não possui acesso à internet                                   64
# 6 Na entidade/Coletivo Cultural, Em casa, Outro local            53
# 7 Na entidade/Coletivo Cultural, Outro local                     15
# 8 Em casa, Outro local                                           14
# 9 Em casa, Não possui acesso à internet                           1

#########################################################################################

