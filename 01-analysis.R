library("tidyverse")
library("gt")
library("stargazer")
library("broom")
library("marginaleffects")
library("ggrepel")
library("sjmisc")

ches_ppepe <- read_csv("CHES2019_ppepe.csv") |> 
  rename(rightwing = `right-wing`,
         leftwing = `left-wing`) |> 
  mutate(party_type = case_when(
    valence == 1 ~ "Valence populist",
    rightwing == 1 ~ "Right-wing populist",
    leftwing == 1 ~ "Left-wing populist",
    TRUE ~ "Non-populist"
  )) |> 
  select(-starts_with("eu_"))

ches_experts_raw <- read_csv("CHES2019_experts.csv")

ches_ppepe_vc <- ches_ppepe |> 
  filter(country_name %in% unique(ches_ppepe$country_name[ches_ppepe$valence == 1])) 

ches_ppepe |> 
  ggplot(aes(lrecon_blur, galtan_blur, colour = factor(valence))) +
  geom_label_repel(data = filter(ches_ppepe, valence == 1), aes(label = party)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  theme(legend.position = "none") + 
  scale_colour_manual(values = c("grey", "black")) +
  scale_x_continuous(breaks = 0:10, limits = c(0, 10)) +
  scale_y_continuous(breaks = 0:10, limits = c(0, 10)) +
  labs(x = "Blurry position on economic issues",
       y = "Blurry position on socio-cultural issues")

ggsave("fig_02.png", width = 6, height = 6, bg = "white")
ggsave("fig_02.pdf", width = 6, height = 6)

ches_ppepe_vc |> 
  ggplot(aes(lrecon_blur, galtan_blur, colour = factor(valence))) +
  geom_label_repel(data = filter(ches_ppepe, valence == 1), aes(label = party)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  theme(legend.position = "none") + 
  scale_colour_manual(values = c("grey", "black")) +
  scale_x_continuous(breaks = 0:10, limits = c(0, 10)) +
  scale_y_continuous(breaks = 0:10, limits = c(0, 10)) +
  labs(x = "Blurry position on economic issues",
       y = "Blurry position on socio-cultural issues")

ggsave("fig_02_vc.png", width = 6, height = 6, bg = "white")

ches_ppepe |> 
  ggplot(aes(lrecon, galtan, colour = factor(valence))) +
  geom_label_repel(data = filter(ches_ppepe, valence == 1), aes(label = party)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  theme(legend.position = "none") + 
  scale_colour_manual(values = c("grey", "black")) +
  scale_x_continuous(breaks = 0:10, limits = c(0, 10)) +
  scale_y_continuous(breaks = 0:10, limits = c(0, 10)) +
  labs(x = "Position on economic issues",
       y = "Position on socio-cultural issues")

ggsave("fig_01.png", width = 6, height = 6, bg = "white")
ggsave("fig_01.pdf", width = 6, height = 6)

ches_ppepe_vc |> 
  ggplot(aes(lrecon, galtan, colour = factor(valence))) +
  geom_label_repel(data = filter(ches_ppepe, valence == 1), aes(label = party)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  theme(legend.position = "none") + 
  scale_colour_manual(values = c("grey", "black")) +
  scale_x_continuous(breaks = 0:10, limits = c(0, 10)) +
  scale_y_continuous(breaks = 0:10, limits = c(0, 10)) +
  labs(x = "Position on economic issues",
       y = "Position on socio-cultural issues")

ggsave("fig_01_vc.png", width = 6, height = 6, bg = "white")

ches_ppepe |> 
  filter(populist == 1) |> 
  mutate(party = gsub("/.*", "", party)) |> 
  mutate(party = paste0(party, " (", country_name, ")")) |> 
  mutate(party = fct_reorder(party, corrupt_salience)) |> 
  ggplot(aes(party, corrupt_salience, fill = factor(valence))) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") + 
  scale_fill_manual(values = c("grey", "black")) +
  scale_y_continuous(breaks = seq(0, 10, 1), labels = c("", "Not important\nat all", rep("", 7), "Extremely\nimportant", "")) +
  labs(y = "Salience of political corruption",
       x = NULL) +
  theme(axis.title.x = element_text(size = 10))

ggsave("fig_05.png", width = 6, height = 8, bg = "white")
ggsave("fig_05.pdf", width = 6, height = 8)

ches_ppepe_vc |> 
  filter(populist == 1) |> 
  mutate(party = gsub("/.*", "", party)) |> 
  mutate(party = paste0(party, " (", country_name, ")")) |> 
  mutate(party = fct_reorder(party, corrupt_salience)) |> 
  ggplot(aes(party, corrupt_salience, fill = factor(valence))) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") + 
  scale_fill_manual(values = c("grey", "black")) +
  scale_y_continuous(breaks = seq(0, 10, 1), labels = c("", "Not important\nat all", rep("", 7), "Extremely\nimportant", "")) +
  labs(y = "Salience of political corruption",
       x = NULL) +
  theme(axis.title.x = element_text(size = 10))

ggsave("fig_05_vc.png", width = 6, height = 8, bg = "white")

ches_ppepe |> 
  drop_na(lrecon_blur, galtan_blur) |> 
  mutate(party_type = case_when(
    valence == 1 ~ "Valence populist",
    rightwing == 1 ~ "Right-wing populist",
    leftwing == 1 ~ "Left-wing populist",
    TRUE ~ "Non-populist"
  )) |> 
  group_by(party_type) |> 
  summarise(across(c(contains("_blur"), contains("salience")), 
                   list(mean = mean, 
                        sd = sd), 
                   .names = "{col}_{fn}"),
            n = n()) |> 
  transmute(party_type, 
            `Blurry: economy` = paste0(round(lrecon_blur_mean, 2), " (", round(lrecon_blur_sd, 2), ")"), 
            `Blurry: socio-cultural` = paste0(round(galtan_blur_mean, 2), " (", round(galtan_blur_sd, 2), ")"),
            `Salience: economy` = paste0(round(lrecon_salience_mean, 2), " (", round(lrecon_salience_sd, 2), ")"),
            `Salience: socio-cultural` = paste0(round(galtan_salience_mean, 2), " (", round(galtan_salience_sd, 2), ")"),
            `Salience: corruption` = paste0(round(corrupt_salience_mean, 2), " (", round(corrupt_salience_sd, 2), ")"),
            Observations = n
            ) |> 
  rotate_df(cn = TRUE) |> 
  rownames_to_column() |> 
  gt() |> 
  gtsave("table_02.docx")

reg_lrecon_blur <- lm(lrecon_blur ~ valence + rightwing + leftwing, data = ches_ppepe)
reg_galtan_blur <- lm(galtan_blur ~ valence + rightwing + leftwing, data = ches_ppepe)

reg_lrecon_salience <- lm(lrecon_salience ~ valence + rightwing + leftwing, data = ches_ppepe)
reg_galtan_salience <- lm(galtan_salience ~ valence + rightwing + leftwing, data = ches_ppepe)
reg_corrupt_salience <- lm(corrupt_salience ~ valence + rightwing + leftwing, data = ches_ppepe)

stargazer(reg_lrecon_blur, reg_galtan_blur, type = "text", 
          keep.stat = c("n", "rsq"),
          covariate.labels = c("Valence populist", "Right-wing populist", "Left-wing populist"),
          column.labels = c("Economy", "Social"),
          out = "table_03.htm")

stargazer(reg_lrecon_salience, reg_galtan_salience, reg_corrupt_salience, 
          type = "text", keep.stat = c("n", "rsq"),
          covariate.labels = c("Valence populist", "Right-wing populist", "Left-wing populist"),
          column.labels = c("Economy", "Social", "Corruption"),
          out = "table_04.htm")


me_lrecon_blur <- reg_lrecon_blur |> 
  marginaleffects() |> 
  tidy() |> 
  mutate(outcome = "lrecon_blur")

me_galtan_blur <- reg_galtan_blur |> 
  marginaleffects() |> 
  tidy() |> 
  mutate(outcome = "galtan_blur")

me_lrecon_salience <- reg_lrecon_salience |> 
  marginaleffects() |> 
  tidy() |> 
  mutate(outcome = "lrecon_salience")

me_galtan_salience <- reg_galtan_salience |> 
  marginaleffects() |> 
  tidy() |> 
  mutate(outcome = "galtan_salience")

me_corrupt_salience <- reg_corrupt_salience |> 
  marginaleffects() |> 
  tidy() |> 
  mutate(outcome = "corrupt_salience")

me_df <- bind_rows(me_lrecon_blur, me_galtan_blur,
                   me_lrecon_salience, me_galtan_salience,
                   me_corrupt_salience)

me_df |> 
  filter(str_detect(outcome, "_blur")) |> 
  mutate(term = case_when(
    term == "valence" ~ "Valence",
    term == "rightwing" ~ "Right-wing",
    term == "leftwing" ~ "Left-wing"
  ),
  outcome = case_when(
    outcome == "galtan_blur" ~ "Socio-cultural",
    outcome == "lrecon_blur" ~ "Economic"
  )) |> 
  ggplot(aes(x = term, y = estimate,
             ymin = estimate - 1.96 * std.error,
             ymax = estimate + 1.96 * std.error)) +
  geom_hline(yintercept = 0, colour = "gray") +
  geom_point() +
  geom_errorbar(width = 0) +
  theme_minimal() +
  facet_wrap(~ outcome) +
  coord_flip() +
  labs(x = NULL,
       y = "Coefficient (w. 95% CI)") +
  theme(axis.title.x = element_text(size = 8))

ggsave("fig_03.png", width = 6, height = 2, bg = "white")
ggsave("fig_03.pdf", width = 6, height = 2)

me_df |> 
  filter(str_detect(outcome, "_salience")) |> 
  mutate(term = case_when(
    term == "valence" ~ "Valence",
    term == "rightwing" ~ "Right-wing",
    term == "leftwing" ~ "Left-wing"
  ),
  outcome = case_when(
    outcome == "galtan_salience" ~ "2) Socio-cultural",
    outcome == "lrecon_salience" ~ "1) Economic",
    outcome == "corrupt_salience" ~ "3) Corruption"
  )) |> 
  ggplot(aes(x = term, y = estimate,
             ymin = estimate - 1.96 * std.error,
             ymax = estimate + 1.96 * std.error)) +
  geom_hline(yintercept = 0, colour = "gray") +
  geom_point() +
  geom_errorbar(width = 0) +
  theme_minimal() +
  facet_wrap(~ outcome) +
  coord_flip() +
  labs(x = NULL,
       y = "Coefficient (w. 95% CI)") +
  theme(axis.title.x = element_text(size = 8))

ggsave("fig_04.png", width = 8, height = 2, bg = "white")
ggsave("fig_04.pdf", width = 8, height = 2)

reg_lrecon_blur_vc <- lm(lrecon_blur ~ valence + rightwing + leftwing, data = ches_ppepe_vc)
reg_galtan_blur_vc <- lm(galtan_blur ~ valence + rightwing + leftwing, data = ches_ppepe_vc)

reg_lrecon_salience_vc <- lm(lrecon_salience ~ valence + rightwing + leftwing, data = ches_ppepe_vc)
reg_galtan_salience_vc <- lm(galtan_salience ~ valence + rightwing + leftwing, data = ches_ppepe_vc)
reg_corrupt_salience_vc <- lm(corrupt_salience ~ valence + rightwing + leftwing, data = ches_ppepe_vc)

stargazer(reg_lrecon_blur_vc, reg_galtan_blur_vc, type = "text", 
          keep.stat = c("n", "rsq"),
          covariate.labels = c("Valence populist", "Right-wing populist", "Left-wing populist"),
          column.labels = c("Economy", "Social"),
          out = "table_03_vc.htm")

stargazer(reg_lrecon_salience_vc, reg_galtan_salience_vc, reg_corrupt_salience_vc, 
          type = "text", keep.stat = c("n", "rsq"),
          covariate.labels = c("Valence populist", "Right-wing populist", "Left-wing populist"),
          column.labels = c("Economy", "Social", "Corruption"),
          out = "table_04_vc.htm")


me_lrecon_blur_vc <- reg_lrecon_blur_vc |> 
  marginaleffects() |> 
  tidy() |> 
  mutate(outcome = "lrecon_blur")

me_galtan_blur_vc <- reg_galtan_blur_vc |> 
  marginaleffects() |> 
  tidy() |> 
  mutate(outcome = "galtan_blur")

me_lrecon_salience_vc <- reg_lrecon_salience_vc |> 
  marginaleffects() |> 
  tidy() |> 
  mutate(outcome = "lrecon_salience")

me_galtan_salience_vc <- reg_galtan_salience_vc |> 
  marginaleffects() |> 
  tidy() |> 
  mutate(outcome = "galtan_salience")

me_corrupt_salience_vc <- reg_corrupt_salience_vc |> 
  marginaleffects() |> 
  tidy() |> 
  mutate(outcome = "corrupt_salience")

me_df_vc <- bind_rows(me_lrecon_blur_vc, me_galtan_blur_vc,
                   me_lrecon_salience_vc, me_galtan_salience_vc,
                   me_corrupt_salience_vc)

me_df_vc |> 
  filter(str_detect(outcome, "_blur")) |> 
  mutate(term = case_when(
    term == "valence" ~ "Valence",
    term == "rightwing" ~ "Right-wing",
    term == "leftwing" ~ "Left-wing"
  ),
  outcome = case_when(
    outcome == "galtan_blur" ~ "Socio-cultural",
    outcome == "lrecon_blur" ~ "Economic"
  )) |> 
  ggplot(aes(x = term, y = estimate,
             ymin = estimate - 1.96 * std.error,
             ymax = estimate + 1.96 * std.error)) +
  geom_hline(yintercept = 0, colour = "gray") +
  geom_point() +
  geom_errorbar(width = 0) +
  theme_minimal() +
  facet_wrap(~ outcome) +
  coord_flip() +
  labs(x = NULL,
       y = "Coefficient (w. 95% CI)") +
  theme(axis.title.x = element_text(size = 8))

ggsave("fig_03_vc.png", width = 6, height = 2, bg = "white")

me_df_vc |> 
  filter(str_detect(outcome, "_salience")) |> 
  mutate(term = case_when(
    term == "valence" ~ "Valence",
    term == "rightwing" ~ "Right-wing",
    term == "leftwing" ~ "Left-wing"
  ),
  outcome = case_when(
    outcome == "galtan_salience" ~ "2) Socio-cultural",
    outcome == "lrecon_salience" ~ "1) Economic",
    outcome == "corrupt_salience" ~ "3) Corruption"
  )) |> 
  ggplot(aes(x = term, y = estimate,
             ymin = estimate - 1.96 * std.error,
             ymax = estimate + 1.96 * std.error)) +
  geom_hline(yintercept = 0, colour = "gray") +
  geom_point() +
  geom_errorbar(width = 0) +
  theme_minimal() +
  facet_wrap(~ outcome) +
  coord_flip() +
  labs(x = NULL,
       y = "Coefficient (w. 95% CI)") +
  theme(axis.title.x = element_text(size = 8))

ggsave("fig_04_vc.png", width = 8, height = 2, bg = "white")


# Run models with country FE
reg_lrecon_blur <- lm(lrecon_blur ~ factor(country) + valence + rightwing + leftwing, data = ches_ppepe)
reg_galtan_blur <- lm(galtan_blur ~ factor(country) + valence + rightwing + leftwing, data = ches_ppepe)

stargazer(reg_lrecon_blur, reg_galtan_blur, type = "text")

reg_lrecon_salience <- lm(lrecon_salience ~ factor(country) + valence + rightwing + leftwing, data = ches_ppepe)
reg_galtan_salience <- lm(galtan_salience ~ factor(country) + valence + rightwing + leftwing, data = ches_ppepe)
reg_corrupt_salience <- lm(corrupt_salience ~ factor(country) + valence + rightwing + leftwing, data = ches_ppepe)

stargazer(reg_lrecon_salience, reg_galtan_salience, reg_corrupt_salience, type = "text")

ches_experts <- left_join(ches_experts_raw,
                          select(ches_ppepe, party_id, populist, rightwing, valence, leftwing, party_type),
                          by = "party_id") |> 
  drop_na(party_type) |> 
  mutate(lrgen_na = ifelse(is.na(lrgen), 1, 0),
         lrecon_na = ifelse(is.na(lrecon), 1, 0),
         galtan_na = ifelse(is.na(galtan), 1, 0)
         )

ches_experts |> 
  group_by(party_type) |> 
  summarise(lrgen_na = mean(lrgen_na))

ches_experts |> 
  group_by(party_type) |> 
  summarise(lrecon_na = mean(lrecon_na))

ches_experts |> 
  group_by(party_type) |> 
  summarise(galtan_na = mean(galtan_na))

reg_lrgen_na <- lm(lrgen_na ~ valence + rightwing + leftwing, data = ches_experts)
reg_lrecon_na <- lm(lrecon_na ~ valence + rightwing + leftwing, data = ches_experts)
reg_galtan_na <- lm(galtan_na ~ valence + rightwing + leftwing, data = ches_experts)

stargazer(reg_lrgen_na, reg_lrecon_na, reg_galtan_na,
          type = "text", keep.stat = c("n", "rsq"),
          covariate.labels = c("Valence populist", "Right-wing populist", "Left-wing populist"),
          column.labels = c("General left-right", "Economic issues", "Socio-cultural issues"),
          out = "table_experts_na.htm")

sink("sessionInfo.txt") 
cat("\nThe results are produced with 01-analysis.R with this session in R:\n\n")
sessionInfo()
sink() 
