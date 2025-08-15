# Tabulação Mapeamento da Cultura Digital

library(tidyverse)
library(janitor)
library(readODS)

# Carregar tabulação geral: 

validadas_geral <- read_ods("Tabulacao/Ate 12052025/tabela_para_tabulacao_12052025.ods") %>%
  clean_names()

colnames(validadas_geral)

#############################################################################################

# Selecionar apenas area de atuação Cultura Digital, Comunicação e Mídia Livre:


# Pontos de Cultura que responderam Cultura Digital Comunicação e Mídia Livre como
# Atividade Prioritária:

cultura_digital_prioritaria <- validadas_geral %>%
  select(id, x3_nome_da_entidade_coletivo_cultural_de_base_comunitaria_mapeado,
         x22_atuacao_tematica_prioritaria, x23_atuacao_tematica_secundaria,
         x1_qual_internet_o_seu_coletivo_ou_ponto_de_cultura_usa,
         se_respondeu_outros_na_questao_acima_diga_qual,
         x2_qual_sistema_operacional_o_seu_coletivo_ou_ponto_de_cultura_utiliza,
         x3_voce_conhece_ou_ja_teve_experiencia_com_outro_sistema_operacional_alem_do_que_voce_usa,
         se_respondeu_sim_na_pergunta_acima_qual,
         x4_quais_dispositivos_o_seu_coletivo_ou_ponto_de_cultura_utiliza_para_acessar_a_internet,
         x5_na_sua_producao_o_seu_coletivo_ou_ponto_de_cultura_utiliza_quais_equipamentos,
         se_respondeu_outros_na_questao_acima_responda_quais_39,
         x6_liste_os_principais_softwares_que_o_seu_coletivo_ou_ponto_de_cultura_utiliza,
         x7_qual_e_a_atividade_do_seu_coletivo_ou_ponto_de_cultura_na_cultura_digital,
         se_respondeu_outros_na_questao_acima_responda_quais_42,
         x8_voce_sabe_o_que_e_fediverso, x9_o_seu_coletivo_ou_ponto_de_cultura_utiliza_uma_dessas_redes_federadas,
         se_respondeu_outros_na_questao_acima_responda_quais_45,
         x10_quantas_pessoas_se_sustentam_do_coletivo_ou_ponto_de_cultura,
         x11_como_coletivo_ou_ponto_de_cultura_se_sustenta,
         se_marcou_outros_quais,
         x12_como_foi_o_seu_letramento_digital,
         x13_em_quais_areas_da_cultura_digital_voce_gostaria_de_fazer_formacoes,
         se_respondeu_outros_na_questao_acima_responda_quais_51,
         x14_o_seu_coletivo_ou_ponto_de_cultura_utiliza_algum_tipo_de_inteligencia_artificial_em_sua_atividades,
         x15_se_respondeu_sim_na_resposta_anterior_para_qual_finalidade,
         se_respondeu_outros_na_questao_anterior_diga_quais,
         x16_tem_alguma_questao_sobre_cultura_digital_que_voce_gostaria_de_responder_e_nao_viu_nesse_formulario,
         quantidade) %>%
  filter(str_detect(x22_atuacao_tematica_prioritaria, "Cultura Digital, Comunicação e Mídia Livre")) %>%
  group_by(x3_nome_da_entidade_coletivo_cultural_de_base_comunitaria_mapeado)

write_ods(cultura_digital_prioritaria, "Somente Cultura Digital Como Atividade Prioritaria at 12052025.ods")


# Pontos de Cultura que responderam Cultura Digital Comunicação e Mídia Livre como
# Atividade Secundaria:

cultura_digital_secundaria <- validadas_geral %>%
  select(id, x3_nome_da_entidade_coletivo_cultural_de_base_comunitaria_mapeado,
         x22_atuacao_tematica_prioritaria, x23_atuacao_tematica_secundaria,
         x1_qual_internet_o_seu_coletivo_ou_ponto_de_cultura_usa,
         se_respondeu_outros_na_questao_acima_diga_qual,
         x2_qual_sistema_operacional_o_seu_coletivo_ou_ponto_de_cultura_utiliza,
         x3_voce_conhece_ou_ja_teve_experiencia_com_outro_sistema_operacional_alem_do_que_voce_usa,
         se_respondeu_sim_na_pergunta_acima_qual,
         x4_quais_dispositivos_o_seu_coletivo_ou_ponto_de_cultura_utiliza_para_acessar_a_internet,
         x5_na_sua_producao_o_seu_coletivo_ou_ponto_de_cultura_utiliza_quais_equipamentos,
         se_respondeu_outros_na_questao_acima_responda_quais_39,
         x6_liste_os_principais_softwares_que_o_seu_coletivo_ou_ponto_de_cultura_utiliza,
         x7_qual_e_a_atividade_do_seu_coletivo_ou_ponto_de_cultura_na_cultura_digital,
         se_respondeu_outros_na_questao_acima_responda_quais_42,
         x8_voce_sabe_o_que_e_fediverso, x9_o_seu_coletivo_ou_ponto_de_cultura_utiliza_uma_dessas_redes_federadas,
         se_respondeu_outros_na_questao_acima_responda_quais_45,
         x10_quantas_pessoas_se_sustentam_do_coletivo_ou_ponto_de_cultura,
         x11_como_coletivo_ou_ponto_de_cultura_se_sustenta,
         se_marcou_outros_quais,
         x12_como_foi_o_seu_letramento_digital,
         x13_em_quais_areas_da_cultura_digital_voce_gostaria_de_fazer_formacoes,
         se_respondeu_outros_na_questao_acima_responda_quais_51,
         x14_o_seu_coletivo_ou_ponto_de_cultura_utiliza_algum_tipo_de_inteligencia_artificial_em_sua_atividades,
         x15_se_respondeu_sim_na_resposta_anterior_para_qual_finalidade,
         se_respondeu_outros_na_questao_anterior_diga_quais,
         x16_tem_alguma_questao_sobre_cultura_digital_que_voce_gostaria_de_responder_e_nao_viu_nesse_formulario,
         quantidade) %>%
  filter(str_detect(x23_atuacao_tematica_secundaria, "Cultura Digital, Comunicação e Mídia Livre")) %>%
  group_by(x3_nome_da_entidade_coletivo_cultural_de_base_comunitaria_mapeado)

write_ods(cultura_digital_secundaria, "Somente Cultura Digital Como Atividade Secundaria at 12052025.ods")

# Unindo as duas tabelas e ficando com uma tabela que inclua os Pontos de Cultura
# que colocaram Cultura Digital, Comunicação e Mídia Livre em uma tabela só:

cultura_digital_22_23_juntas <- rbind(cultura_digital_prioritaria, 
                                     cultura_digital_secundaria)

# Removendo as repetições dos Pontos de Cultura que tem ambas as atividades prioritaria e
# secundaria, como Cultura Digital, Comunicação e Mídia Livre:

cultura_digital_sem_duplicatas <- distinct(cultura_digital_22_23_juntas)

write_ods(cultura_digital_sem_duplicatas, "Somente Cultura Digital Geral at 12052025.ods")

colnames(cultura_digital_sem_duplicatas)

# 1) Qual internet o seu coletivo ou ponto de cultura usa?

tipo_internet <- cultura_digital_sem_duplicatas %>%
  select(x1_qual_internet_o_seu_coletivo_ou_ponto_de_cultura_usa, quantidade) %>%
  filter(!is.na(x1_qual_internet_o_seu_coletivo_ou_ponto_de_cultura_usa)) %>%
  group_by(x1_qual_internet_o_seu_coletivo_ou_ponto_de_cultura_usa) %>%
  summarise(qual_internet = sum(quantidade)) %>%
  arrange(desc(qual_internet))

tipo_internet

# 2) Qual sistema operacional o seu coletivo ou ponto de cultura utiliza?

sistema_operacional <- cultura_digital_sem_duplicatas %>%
  select(x2_qual_sistema_operacional_o_seu_coletivo_ou_ponto_de_cultura_utiliza, quantidade) %>%
  filter(!is.na(x2_qual_sistema_operacional_o_seu_coletivo_ou_ponto_de_cultura_utiliza)) %>%
  group_by(x2_qual_sistema_operacional_o_seu_coletivo_ou_ponto_de_cultura_utiliza) %>%
  summarise(qual_so = sum(quantidade)) %>%
  arrange(desc(qual_so))

# 3) Você conhece ou já teve experiência com outro sistema operacional além do que você usa?

outro_sistema_operacional <- cultura_digital_sem_duplicatas %>%
  select(x3_voce_conhece_ou_ja_teve_experiencia_com_outro_sistema_operacional_alem_do_que_voce_usa, quantidade) %>%
  filter(!is.na(x3_voce_conhece_ou_ja_teve_experiencia_com_outro_sistema_operacional_alem_do_que_voce_usa)) %>%
  group_by(x3_voce_conhece_ou_ja_teve_experiencia_com_outro_sistema_operacional_alem_do_que_voce_usa) %>%
  summarise(outro_so = sum(quantidade)) %>%
  arrange(desc(outro_so))

# Se respondeu sim na pergunta acima, qual?

qual_outro_sistema_operacional_experimentou <- cultura_digital_sem_duplicatas %>%
  select(se_respondeu_sim_na_pergunta_acima_qual, quantidade) %>%
  filter(!is.na(se_respondeu_sim_na_pergunta_acima_qual)) %>%
  group_by(se_respondeu_sim_na_pergunta_acima_qual) %>%
  summarise(qual_outro_so = sum(quantidade)) %>%
  arrange(desc(qual_outro_so))

# 4) Quais dispositivos o seu coletivo ou ponto de cultura utiliza para acessar a internet?

dispositivos_para_internet <- cultura_digital_sem_duplicatas %>%
  select(x4_quais_dispositivos_o_seu_coletivo_ou_ponto_de_cultura_utiliza_para_acessar_a_internet, quantidade) %>%
  filter(!is.na(x4_quais_dispositivos_o_seu_coletivo_ou_ponto_de_cultura_utiliza_para_acessar_a_internet)) %>%
  group_by(x4_quais_dispositivos_o_seu_coletivo_ou_ponto_de_cultura_utiliza_para_acessar_a_internet) %>%
  summarise(dispositivos = sum(quantidade)) %>%
  arrange(desc(dispositivos))

# 5) Na sua produção o seu coletivo ou ponto de cultura utiliza quais equipamentos:

uso_equipamentos <-cultura_digital_sem_duplicatas %>%
  select(x5_na_sua_producao_o_seu_coletivo_ou_ponto_de_cultura_utiliza_quais_equipamentos, quantidade) %>%
  filter(!is.na(x5_na_sua_producao_o_seu_coletivo_ou_ponto_de_cultura_utiliza_quais_equipamentos)) %>%
  group_by(x5_na_sua_producao_o_seu_coletivo_ou_ponto_de_cultura_utiliza_quais_equipamentos) %>%
  summarise(quais_equipamentos = sum(quantidade)) %>%
  arrange(desc(quais_equipamentos))

# Se respondeu Outros na questão acima, responda quais?

outros_equipamentos <- cultura_digital_sem_duplicatas %>%
  select(se_respondeu_outros_na_questao_acima_responda_quais_39, quantidade) %>%
  filter(!is.na(se_respondeu_outros_na_questao_acima_responda_quais_39)) %>%
  group_by(se_respondeu_outros_na_questao_acima_responda_quais_39) %>%
  summarise(outros_equip = sum(quantidade)) %>%
  arrange(desc(outros_equip))

# 6) Liste os principais softwares que o seu coletivo ou ponto de cultura utiliza:

principais_softwares <- cultura_digital_sem_duplicatas %>%
  select(x6_liste_os_principais_softwares_que_o_seu_coletivo_ou_ponto_de_cultura_utiliza, quantidade) %>%
  filter(!is.na(x6_liste_os_principais_softwares_que_o_seu_coletivo_ou_ponto_de_cultura_utiliza)) %>%
  group_by(x6_liste_os_principais_softwares_que_o_seu_coletivo_ou_ponto_de_cultura_utiliza) %>%
  summarise(quais_softwares = sum(quantidade)) %>%
  arrange(desc(quais_softwares))

view(principais_softwares)

# 7) Qual é a atividade do seu coletivo ou ponto de cultura na cultura digital?

atividade_cultura_digital <- cultura_digital_sem_duplicatas %>%
  select(x7_qual_e_a_atividade_do_seu_coletivo_ou_ponto_de_cultura_na_cultura_digital, quantidade) %>%
  filter(!is.na(x7_qual_e_a_atividade_do_seu_coletivo_ou_ponto_de_cultura_na_cultura_digital)) %>%
  group_by(x7_qual_e_a_atividade_do_seu_coletivo_ou_ponto_de_cultura_na_cultura_digital) %>%
  summarise(qual_atividade = sum(quantidade)) %>%
  arrange(desc(qual_atividade))

# Se respondeu Outros na questão acima, responda quais?

outras_atividades_cultura_digital <- cultura_digital_sem_duplicatas %>%
  select(se_respondeu_outros_na_questao_acima_responda_quais_42, quantidade) %>%
  filter(!is.na(se_respondeu_outros_na_questao_acima_responda_quais_42)) %>%
  group_by(se_respondeu_outros_na_questao_acima_responda_quais_42) %>%
  summarise(outras_atividades = sum(quantidade)) %>%
  arrange(desc(outras_atividades))

# 8) Você sabe o que é Fediverso?

conhece_fediverso <- cultura_digital_sem_duplicatas %>%
  select(x8_voce_sabe_o_que_e_fediverso, quantidade) %>%
  filter(!is.na(x8_voce_sabe_o_que_e_fediverso)) %>%
  group_by(x8_voce_sabe_o_que_e_fediverso) %>%
  summarise(conhece_fed = sum(quantidade)) %>%
  arrange(desc(conhece_fed))

# 9) O seu coletivo ou ponto de cultura utiliza uma dessas redes federadas?

usa_redes_federadas <- cultura_digital_sem_duplicatas %>%
  select(x9_o_seu_coletivo_ou_ponto_de_cultura_utiliza_uma_dessas_redes_federadas, quantidade) %>%
  filter(!is.na(x9_o_seu_coletivo_ou_ponto_de_cultura_utiliza_uma_dessas_redes_federadas)) %>%
  group_by(x9_o_seu_coletivo_ou_ponto_de_cultura_utiliza_uma_dessas_redes_federadas) %>%
  summarise(usa_fed = sum(quantidade)) %>%
  arrange(desc(usa_fed))

# Se respondeu Outros na questão acima, responda quais?

usa_outras_redes_federadas <- cultura_digital_sem_duplicatas %>%
  select(se_respondeu_outros_na_questao_acima_responda_quais_45, quantidade) %>%
  filter(!is.na(se_respondeu_outros_na_questao_acima_responda_quais_45)) %>%
  group_by(se_respondeu_outros_na_questao_acima_responda_quais_45) %>%
  summarise(outras_fed = sum(quantidade)) %>%
  arrange(desc(outras_fed))

# 10) Quantas pessoas se sustentam do coletivo ou ponto de cultura?

quantos_se_sustentam_do_coletivo <- cultura_digital_sem_duplicatas %>%
  select(x10_quantas_pessoas_se_sustentam_do_coletivo_ou_ponto_de_cultura, quantidade) %>%
  filter(!is.na(x10_quantas_pessoas_se_sustentam_do_coletivo_ou_ponto_de_cultura)) %>%
  group_by(x10_quantas_pessoas_se_sustentam_do_coletivo_ou_ponto_de_cultura) %>%
  summarise(sustentacao = sum(quantidade)) %>%
  arrange(desc(sustentacao))

# 11) Como coletivo ou ponto de cultura se sustenta:

como_coletivo_se_sustenta <- cultura_digital_sem_duplicatas %>%
  select(x11_como_coletivo_ou_ponto_de_cultura_se_sustenta, quantidade) %>%
  filter(!is.na(x11_como_coletivo_ou_ponto_de_cultura_se_sustenta)) %>%
  group_by(x11_como_coletivo_ou_ponto_de_cultura_se_sustenta) %>%
  summarise(como_sustenta = sum(quantidade)) %>%
  arrange(desc(como_sustenta))

# Se marcou outros, quais?

outras_formas_sustento <- cultura_digital_sem_duplicatas %>%
  select(se_marcou_outros_quais, quantidade) %>%
  filter(!is.na(se_marcou_outros_quais)) %>%
  group_by(se_marcou_outros_quais) %>%
  summarise(outro_sustento = sum(quantidade)) %>%
  arrange(desc(outro_sustento))

# 12) Como foi o seu letramento digital?

letramento_digital <-cultura_digital_sem_duplicatas %>%
  select(x12_como_foi_o_seu_letramento_digital, quantidade) %>%
  filter(!is.na(x12_como_foi_o_seu_letramento_digital)) %>%
  group_by(x12_como_foi_o_seu_letramento_digital) %>%
  summarise(tipo_letramento = sum(quantidade)) %>%
  arrange(desc(tipo_letramento))

# 13) Em quais áreas da cultura digital você gostaria de fazer formações?

que_formacoes_gostaria <- cultura_digital_sem_duplicatas %>%
  select(x13_em_quais_areas_da_cultura_digital_voce_gostaria_de_fazer_formacoes, quantidade) %>%
  filter(!is.na(x13_em_quais_areas_da_cultura_digital_voce_gostaria_de_fazer_formacoes)) %>%
  group_by(x13_em_quais_areas_da_cultura_digital_voce_gostaria_de_fazer_formacoes) %>%
  summarise(formacoes_desejadas = sum(quantidade)) %>%
  arrange(desc(formacoes_desejadas))

# Se respondeu Outros na questão acima, responda quais?

outras_formacoes <- cultura_digital_sem_duplicatas %>%
  select(se_respondeu_outros_na_questao_acima_responda_quais_51, quantidade) %>%
  filter(!is.na(se_respondeu_outros_na_questao_acima_responda_quais_51)) %>%
  group_by(se_respondeu_outros_na_questao_acima_responda_quais_51) %>%
  summarise(formacoes_desejadas_2 = sum(quantidade)) %>%
  arrange(desc(formacoes_desejadas_2))

# 14) O seu coletivo ou ponto de cultura utiliza 
# algum tipo de Inteligência Artificial em sua atividades?

usa_inteligencia_artificial <- cultura_digital_sem_duplicatas %>%
  select(x14_o_seu_coletivo_ou_ponto_de_cultura_utiliza_algum_tipo_de_inteligencia_artificial_em_sua_atividades, quantidade) %>%
  filter(!is.na(x14_o_seu_coletivo_ou_ponto_de_cultura_utiliza_algum_tipo_de_inteligencia_artificial_em_sua_atividades)) %>%
  group_by(x14_o_seu_coletivo_ou_ponto_de_cultura_utiliza_algum_tipo_de_inteligencia_artificial_em_sua_atividades) %>%
  summarise(uso_de_ia = sum(quantidade)) %>%
  arrange(desc(uso_de_ia))

# 15) Se respondeu sim na resposta anterior, para qual finalidade?

para_que_inteligencia_artificial <- cultura_digital_sem_duplicatas %>%
  select(x15_se_respondeu_sim_na_resposta_anterior_para_qual_finalidade, quantidade) %>%
  filter(!is.na(x15_se_respondeu_sim_na_resposta_anterior_para_qual_finalidade)) %>%
  group_by(x15_se_respondeu_sim_na_resposta_anterior_para_qual_finalidade) %>%
  summarise(para_q_ia = sum(quantidade)) %>%
  arrange(desc(para_q_ia))

# Se respondeu /outros na questão anterior diga quais?

outros_usos_inteligencia_artificial <- cultura_digital_sem_duplicatas %>%
  select(se_respondeu_outros_na_questao_anterior_diga_quais, quantidade) %>%
  filter(!is.na(se_respondeu_outros_na_questao_anterior_diga_quais)) %>%
  group_by(se_respondeu_outros_na_questao_anterior_diga_quais) %>%
  summarise(outros_usos_ia = sum(quantidade)) %>%
  arrange(desc(outros_usos_ia))
