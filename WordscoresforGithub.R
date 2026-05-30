library(dplyr)
library(tidyr)
library(quanteda)
library(quanteda.textmodels)
library(irlba)
library(readr)
library(stringr)
library(e1071)
library(ggplot2)
library(readxl)

setwd("~/Desktop/Political Messaging")
df2 <- read_csv("dataverse_files/candidate_platform")

#get docs
years_keep <- c(2020, 2022)

subset_platform_all <- df2 %>%
  filter(
    year %in% years_keep,
    cand_party %in% c("Democrat", "Republican")
  ) %>%
  mutate(
    candidate_id = paste(
      candidate_webname,
      state_postal,
      cd,
      year,
      cand_party,
      sep = "__"
    )
  )

#CampaignView papers says to get candidate-year-issue documents for full training corpus
candidate_docs_issue_all <- subset_platform_all %>%
  arrange(candidate_webname, state_postal, cd, year, cand_party, statement_id) %>%
  mutate(
    issue_text_clean = issue_text %>%
      str_squish() %>%
      str_to_lower()
  ) %>%
  group_by(candidate_id, policy_code, cand_party) %>%
  summarise(
    all_text = paste(issue_text_clean, collapse = " "),
    .groups = "drop"
  ) %>%
  mutate(
    doc_id = paste(candidate_id, policy_code, sep = "__ISSUE__")
  )

#pooled model

corp_all <- corpus(candidate_docs_issue_all, text_field = "all_text")
docnames(corp_all) <- candidate_docs_issue_all$doc_id

toks_all <- tokens(
  corp_all,
  remove_punct = TRUE,
  remove_symbols = TRUE
) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_wordstem()

dfm_all <- dfm(toks_all)
dfm_all <- dfm_all[rowSums(dfm_all) > 0, ]

ref_data_all <- candidate_docs_issue_all %>%
  filter(doc_id %in% docnames(dfm_all)) %>%
  mutate(
    score = case_when(
      cand_party == "Democrat" ~ -1,
      cand_party == "Republican" ~ 1,
      TRUE ~ NA_real_
    )
  )

reference_scores <- ref_data_all$score[
  match(docnames(dfm_all), ref_data_all$doc_id)
]

ws_model <- textmodel_wordscores(dfm_all, y = reference_scores)

#score full corpus

ws_pred_all <- predict(ws_model, newdata = dfm_all, se.fit = TRUE)

candidate_scores_all <- tibble(
  doc_id = docnames(dfm_all),
  wordscores = ws_pred_all$fit,
  wordscores_se = ws_pred_all$se.fit
) %>%
  separate(
    doc_id,
    into = c("candidate_id", "policy_code"),
    sep = "__ISSUE__",
    remove = FALSE
  ) %>%
  left_join(
    candidate_docs_issue_all %>%
      select(candidate_id, policy_code, cand_party) %>%
      distinct(),
    by = c("candidate_id", "policy_code")
  )

#keep those withbioguide id 

member_lookup <- subset_platform_all %>%
  filter(!is.na(BIOGUIDE_id)) %>%
  mutate(
    candidate_id = paste(
      candidate_webname,
      state_postal,
      cd,
      year,
      cand_party,
      sep = "__"
    )
  ) %>%
  select(candidate_id, bioguide_id = BIOGUIDE_id, year) %>%
  distinct()

H117 <- read_csv("H117_members.csv")
H118 <- read_csv("H118_members.csv")

members_house <- bind_rows(H117, H118) %>%
  distinct(bioguide_id, .keep_all = TRUE) %>%
  select(bioguide_id, bioname, party_code)

member_scores <- candidate_scores_all %>%
  inner_join(member_lookup, by = "candidate_id") %>%
  left_join(
    members_house %>% select(bioguide_id, bioname, party_code),
    by = "bioguide_id"
  ) %>%
  select(
    bioguide_id,
    year,
    policy_code,
    wordscores,
    wordscores_se,
    bioname,
    party_code,
    cand_party
  )

#get platform text
plat_raw <- read_excel("PartyPlatformText.xlsx")

plat_docs <- plat_raw %>%
  filter(!is.na(text) & text != "NA" & text != "") %>%
  group_by(party, issue_code) %>%
  summarise(
    text = paste(text, collapse = " "),
    .groups = "drop"
  ) %>%
  mutate(
    doc_id = paste(party, issue_code, sep = "_")
  )

corp_plat <- corpus(
  plat_docs,
  text_field = "text",
  docid_field = "doc_id"
)

toks_plat <- tokens(
  corp_plat,
  remove_punct = TRUE,
  remove_symbols = TRUE
) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_wordstem()

dfm_plat <- dfm(toks_plat)
dfm_plat <- dfm_plat[rowSums(dfm_plat) > 0, ]

#match platform with full dfm
dfm_plat_matched <- dfm_match(
  dfm_plat,
  features = featnames(dfm_all)
)

ws_pred_plat_on_campaign_scale <- predict(
  ws_model,
  newdata = dfm_plat_matched,
  se.fit = TRUE
)

platform_wordscores <- tibble(
  doc_id = docnames(dfm_plat_matched),
  wordscores = ws_pred_plat_on_campaign_scale$fit,
  wordscores_se = ws_pred_plat_on_campaign_scale$se.fit
) %>%
  separate(
    doc_id,
    into = c("party", "issue_code"),
    sep = "_",
    remove = FALSE
  ) %>%
  mutate(
    issue_code = as.character(issue_code)
  )

#members for plots

members_plot <- member_scores %>%
  filter(party_code %in% c(100, 200)) %>%
  mutate(
    Party = if_else(party_code == 100, "Democrat", "Republican"),
    issue_code = as.character(policy_code),
    policy_code = as.character(policy_code)
  )

issue_xwalk <- tribble(
  ~issue_code,       ~policy_code,
  "agriculture",     "Agriculture",
  "civil",           "Civil Rights, Liberties, and Minority Issues",
  "crime",           "Crime",
  "defense",         "Defense",
  "econ",            "Economics and Commerce",
  "education",       "Education",
  "energy",          "Energy and Environment",
  "gov",             "Government Operations",
  "healthcare",      "Healthcare",
  "immigration",     "Immigration",
  "international",   "International Affairs",
  "social",          "Social Welfare",
  "transportation",  "Transportation and Infrastructure"
)

plat_lines <- platform_wordscores %>%
  mutate(
    Party = case_when(
      party == "D" ~ "Democrat",
      party == "R" ~ "Republican",
      TRUE ~ party
    ),
    issue_code = as.character(issue_code)
  )

plat_lines2 <- plat_lines %>%
  left_join(issue_xwalk, by = "issue_code") %>%
  filter(!is.na(policy_code))

members_plot2 <- members_plot %>%
  mutate(policy_code = as.character(policy_code))

#main plot

main_plot <- ggplot(members_plot2, aes(x = wordscores, fill = Party)) +
  geom_density(alpha = 0.35, color = "black", linewidth = 0.4) +
  geom_vline(
    data = plat_lines2,
    aes(xintercept = wordscores, color = Party),
    linewidth = 1.2,
    show.legend = FALSE
  ) +
  facet_wrap(~ policy_code, scales = "fixed") +
  labs(
    title = "Campaign text Wordscores vs Party Platform section Wordscores",
    x = "Wordscores",
    y = "Density",
    fill = "Party"
  ) +
  scale_fill_manual(
    values = c("Democrat" = "steelblue", "Republican" = "firebrick")
  ) +
  scale_color_manual(
    values = c("Democrat" = "steelblue", "Republican" = "firebrick")
  ) +
  theme_minimal()

ggsave(
  "~/Downloads/main_plot.pdf",
  plot = main_plot
)

#plot thats comparable to the issueIRT plot
selected_issues <- c(
  "Civil Rights",
  "Crime",
  "Economics/Commerce",
  "Defense",
  "Energy/Environment",
  "Government Operations",
  "Healthcare",
  "Immigration",
  "International Affairs"
)

members_plot_selected <- members_plot2 %>%
  mutate(
    policy_code = recode(
      policy_code,
      "Energy and Environment" = "Energy/Environment",
      "Economics and Commerce" = "Economics/Commerce",
      "Civil Rights, Liberties, and Minority Issues" = "Civil Rights"
    )
  ) %>%
  filter(policy_code %in% selected_issues) %>%
  mutate(
    policy_code = factor(
      policy_code,
      levels = selected_issues
    )
  )

plat_lines_selected <- plat_lines2 %>%
  mutate(
    policy_code = recode(
      policy_code,
      "Energy and Environment" = "Energy/Environment",
      "Economics and Commerce" = "Economics/Commerce",
      "Civil Rights, Liberties, and Minority Issues" = "Civil Rights"
    )
  ) %>%
  filter(policy_code %in% selected_issues) %>%
  mutate(
    policy_code = factor(
      policy_code,
      levels = selected_issues
    )
  )

ggplot(
  members_plot_selected,
  aes(x = wordscores, fill = Party)
) +
  geom_density(
    alpha = 0.35,
    color = "black",
    linewidth = 0.4
  ) +
  geom_vline(
    data = plat_lines_selected,
    aes(xintercept = wordscores, color = Party),
    linewidth = 1.2,
    show.legend = FALSE
  ) +
  facet_wrap(
    ~ policy_code,
    ncol = 3,
    scales = "fixed"
  ) +
  labs(
    title = "Issue-Specific Campaign Text Wordscores",
    subtitle = "Scale trained on all 2020/2022 major-party campaign text; dashed lines show party platform positions",
    x = "Wordscores",
    y = "Density",
    fill = "Party"
  ) +
  scale_fill_manual(
    values = c(
      "Democrat" = "steelblue",
      "Republican" = "firebrick"
    )
  ) +
  scale_color_manual(
    values = c(
      "Democrat" = "steelblue",
      "Republican" = "firebrick"
    )
  ) +
  theme_minimal()

main_plot_3x3

ggsave(
  "~/Downloads/main_plot_3x3.pdf",
  plot = main_plot_3x3
)

## three issue plots 

keep_issues <- c(
  "Government Operations",
  "Civil Rights, Liberties, and Minority Issues",
  "International Affairs"
)

members_plot2_sub <- members_plot2 %>%
  filter(policy_code %in% keep_issues) %>%
  mutate(policy_code = factor(policy_code, levels = keep_issues))

plat_lines2_sub <- plat_lines2 %>%
  filter(policy_code %in% keep_issues) %>%
  mutate(policy_code = factor(policy_code, levels = keep_issues))

three_issues<- ggplot(members_plot2_sub, aes(x = wordscores, fill = Party)) +
  geom_density(alpha = 0.35, color = "black", linewidth = 0.4) +
  geom_vline(
    data = plat_lines2_sub,
    aes(xintercept = wordscores, color = Party),
    linewidth = 1.2,
    show.legend = FALSE
  ) +
  facet_wrap(~ policy_code, scales = "fixed") +
  labs(
    title = "Campaign text Wordscores vs Party Platform section Wordscores",
    x = "Wordscores",
    y = "Density",
    fill = "Party"
  ) +
  scale_fill_manual(
    values = c("Democrat" = "steelblue", "Republican" = "firebrick")
  ) +
  scale_color_manual(
    values = c("Democrat" = "steelblue", "Republican" = "firebrick")
  ) +
  theme_minimal()

ggsave(
  "~/Downloads/three_issues.pdf",
  plot = three_issues
)

##residuals


overall_ws <- member_scores %>%
  mutate(
    weight = ifelse(
      is.na(wordscores_se) | wordscores_se == 0,
      1,
      1 / wordscores_se^2
    )
  ) %>%
  group_by(bioguide_id) %>%
  summarise(
    ws_overall = weighted.mean(wordscores, w = weight, na.rm = TRUE),
    .groups = "drop"
  )

member_issue_ws <- member_scores %>%
  left_join(overall_ws, by = "bioguide_id")

issue_residuals <- member_issue_ws %>%
  group_by(policy_code) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(wordscores ~ ws_overall, data = .x)),
    data = map2(data, model, ~ mutate(.x, ws_resid = resid(.y)))
  ) %>%
  unnest(data) %>%
  select(-model)

issue_residuals <- issue_residuals %>%
  left_join(
    members_plot %>%
      select(bioguide_id, Party) %>%
      distinct(),
    by = "bioguide_id"
  )

ggplot(issue_residuals, aes(x = ws_resid, fill = Party)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ policy_code, scales = "free") +
  theme_minimal()

##top residuals

most_unusual <- issue_residuals %>%
  mutate(abs_resid = abs(ws_resid)) %>%
  arrange(desc(abs_resid)) %>%
  select(bioguide_id, bioname, Party, policy_code, ws_resid) %>%
  head(30)

top_by_issue <- issue_residuals %>%
  group_by(policy_code) %>%
  slice_max(order_by = abs(ws_resid), n = 5) %>%
  ungroup() %>%
  select(policy_code, bioname, Party, ws_resid)

top_dem <- issue_residuals %>%
  filter(Party == "Democrat") %>%
  group_by(policy_code) %>%
  slice_max(abs(ws_resid), n = 5) %>%
  ungroup() %>%
  select(policy_code, bioname, ws_resid)

top_rep <- issue_residuals %>%
  filter(Party == "Republican") %>%
  group_by(policy_code) %>%
  slice_max(abs(ws_resid), n = 5) %>%
  ungroup() %>%
  select(policy_code, bioname, ws_resid)


library(ggrepel)

##overall

subset_platform_overall <- df2 %>%
  filter(
    year %in% c(2020, 2022),
    cand_party %in% c("Democrat", "Republican")
  ) %>%
  mutate(
    candidate_id = paste(
      candidate_webname,
      state_postal,
      cand_party,
      sep = "__"
    )
  )

candidate_docs_overall <- subset_platform_overall %>%
  arrange(candidate_webname, state_postal, cand_party, year, statement_id) %>%
  mutate(
    issue_text_clean = issue_text %>%
      str_squish() %>%
      str_to_lower()
  ) %>%
  group_by(candidate_id, cand_party) %>%
  summarise(
    all_text = paste(issue_text_clean, collapse = " "),
    .groups = "drop"
  ) %>%
  mutate(doc_id = candidate_id)

corp_overall <- corpus(candidate_docs_overall, text_field = "all_text")
docnames(corp_overall) <- candidate_docs_overall$doc_id

toks_overall <- tokens(
  corp_overall,
  remove_punct = TRUE,
  remove_symbols = TRUE
) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_wordstem()

dfm_overall <- dfm(toks_overall)
dfm_overall <- dfm_overall[rowSums(dfm_overall) > 0, ]

ref_overall <- candidate_docs_overall %>%
  filter(doc_id %in% docnames(dfm_overall)) %>%
  mutate(
    score = case_when(
      cand_party == "Democrat" ~ -1,
      cand_party == "Republican" ~ 1,
      TRUE ~ NA_real_
    )
  )

reference_scores_overall <- ref_overall$score[
  match(docnames(dfm_overall), ref_overall$doc_id)
]

ws_model_overall <- textmodel_wordscores(
  dfm_overall,
  y = reference_scores_overall
)

#score overall

ws_pred_overall <- predict(
  ws_model_overall,
  newdata = dfm_overall,
  se.fit = TRUE
)

overall_scores <- tibble(
  doc_id = docnames(dfm_overall),
  wordscores = ws_pred_overall$fit,
  wordscores_se = ws_pred_overall$se.fit
) %>%
  mutate(candidate_id = doc_id) %>%
  left_join(
    candidate_docs_overall %>%
      select(candidate_id, cand_party) %>%
      distinct(),
    by = "candidate_id"
  )

overall_lookup <- subset_platform_overall %>%
  filter(!is.na(BIOGUIDE_id)) %>%
  group_by(candidate_id) %>%
  summarise(
    bioguide_id = first(na.omit(BIOGUIDE_id)),
    bioname = first(candidate_webname),
    win_general = suppressWarnings(max(win_general, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    win_general = if_else(is.infinite(win_general), NA_real_, win_general)
  )

overall_scores <- overall_scores %>%
  left_join(overall_lookup, by = "candidate_id")

overall_winners <- overall_scores %>%
  filter(
    win_general == 1,
    !is.na(bioguide_id),
    cand_party %in% c("Democrat", "Republican")
  ) %>%
  group_by(bioguide_id) %>%
  summarise(
    wordscores = weighted.mean(
      wordscores,
      w = ifelse(
        is.na(wordscores_se) | wordscores_se == 0,
        1,
        1 / wordscores_se^2
      ),
      na.rm = TRUE
    ),
    wordscores_se = mean(wordscores_se, na.rm = TRUE),
    bioname = first(na.omit(bioname)),
    cand_party = first(na.omit(cand_party)),
    win_general = max(win_general, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Party = cand_party)

#use official congressional names 
overall_winners <- overall_winners %>%
  left_join(
    members_house %>% select(bioguide_id, bioname_official = bioname),
    by = "bioguide_id"
  ) %>%
  mutate(
    bioname = coalesce(bioname_official, bioname)
  ) %>%
  select(-bioname_official)


#score overall platforms

platform_docs_overall <- plat_docs %>%
  mutate(
    platform_text_clean = text %>%
      str_squish() %>%
      str_to_lower()
  ) %>%
  group_by(party) %>%
  summarise(
    all_text = paste(platform_text_clean, collapse = " "),
    .groups = "drop"
  ) %>%
  mutate(
    doc_id = case_when(
      party %in% c("D", "Democrat") ~ "D_overall",
      party %in% c("R", "Republican") ~ "R_overall",
      TRUE ~ as.character(party)
    )
  )

corp_plat_overall <- corpus(platform_docs_overall, text_field = "all_text")
docnames(corp_plat_overall) <- platform_docs_overall$doc_id

toks_plat_overall <- tokens(
  corp_plat_overall,
  remove_punct = TRUE,
  remove_symbols = TRUE
) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_wordstem()

dfm_plat_overall <- dfm(toks_plat_overall)

dfm_plat_overall_matched <- dfm_match(
  dfm_plat_overall,
  features = featnames(dfm_overall)
)

ws_pred_plat_overall <- predict(
  ws_model_overall,
  newdata = dfm_plat_overall_matched,
  se.fit = TRUE
)

platform_overall_wordscores <- tibble(
  doc_id = docnames(dfm_plat_overall_matched),
  wordscores = ws_pred_plat_overall$fit,
  wordscores_se = ws_pred_plat_overall$se.fit
) %>%
  mutate(
    Party = case_when(
      str_starts(doc_id, "D") ~ "Democrat",
      str_starts(doc_id, "R") ~ "Republican",
      TRUE ~ NA_character_
    )
  )

#select people

target_high_clean <- c(
  "DELGADO",
  "AXNE",
  "DEUTCH",
  "CRIST",
  "BUSTOS",
  "DEMINGS",
  "NICKEL",
  "O'HALLERAN",
  "KIRKPATRICK",
  "ROYBAL-ALLARD",
  "LAMB, Conor",
  "RYAN, Patrick",
  "PRICE, David",
  "KENNEDY",
  "NORCROSS",
  "CHENEY",
  "KINZINGER",
  "WOMACK",
  "ROGERS, Harold",
  "COLE, Tom",
  "CISCOMANI",
  "TURNER, Michael",
  "VALADAO",
  "CHAVEZ-DEREMER",
  "KEAN, Thomas",
  "FONG",
  "GONZALEZ, Anthony",
  "CALVERT",
  "NUNES",
  "LUCAS"
)

high_pattern_clean <- paste(target_high_clean, collapse = "|")

label_df <- overall_winners %>%
  filter(str_detect(
    bioname,
    regex(high_pattern_clean, ignore_case = TRUE)
  ))

#high dim 2 plot

high_dim2_plot<- ggplot(overall_winners, aes(x = wordscores, fill = Party)) +
  geom_density(alpha = 0.35, color = "black", linewidth = 0.4) +
  geom_vline(
    data = platform_overall_wordscores,
    aes(xintercept = wordscores, color = Party),
    inherit.aes = FALSE,
    linewidth = 1,
    linetype = "dashed",
    show.legend = TRUE
  ) +
  geom_segment(
    data = label_df,
    aes(
      x = wordscores,
      xend = wordscores,
      y = 0,
      yend = 0.15,
      color = Party
    ),
    linewidth = 0.8,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  ggrepel::geom_text_repel(
    data = label_df,
    aes(
      x = wordscores,
      y = 0,
      label = bioname,
      color = Party
    ),
    inherit.aes = FALSE,
    angle = 90,
    force = 5,
    size = 4,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c("Democrat" = "steelblue", "Republican" = "firebrick")
  ) +
  scale_color_manual(
    values = c("Democrat" = "steelblue", "Republican" = "firebrick")
  ) +
  labs(
    title = "Overall Wordscores Estimates for Legislators at the High End of Dimension 2",
    subtitle = "Scale trained on all 2020/2022 major-party campaign text; dashed lines show party platform positions",
    x = "Overall Campaign Wordscores",
    y = "Density",
    fill = "Party",
    color = "Party"
  ) +
  theme_minimal()

high_dim2_table <- label_df %>%
  select(
    Party,
    bioname,
    wordscores,
    wordscores_se
  ) %>%
  arrange(Party, desc(wordscores))

high_dim2_table

ggsave(
  "~/Downloads/high_dim2_plot.pdf",
  plot = high_dim2_plot
)

#select low dim 2 legis

target_low <- c( "BUSH", "TLAIB", "OMAR", "OCASIO-CORTEZ", "LEE, Summer", "BOWMAN", "PRESSLEY", "GARCÍA", "RAMIREZ", "CASAR", 
                 "FROST", "JAYAPAL", "LEE, Barbara", "POCAN", "ROY, Charles", "BIGGS", "MASSIE", "CRANE", 
                 "ROSENDALE", "GREENE", "GOOD, Bob", "NORMAN", "BRECHEEN", "GOSAR", "PERRY", "BOEBERT", "HICE", 
                 "BURLISON", "GAETZ" )

low_pattern_clean <- paste(target_low, collapse = "|")

label_df2 <- overall_winners %>%
  filter(str_detect(
    bioname,
    regex(low_pattern_clean, ignore_case = TRUE)
  ))


low_dim2_table <- label_df2 %>%
  select(
    Party,
    bioname,
    wordscores,
    wordscores_se
  ) %>%
  arrange(Party, desc(wordscores))


low_dim2_plot<- ggplot(overall_winners, aes(x = wordscores, fill = Party)) +
  geom_density(alpha = 0.35, color = "black", linewidth = 0.4) +
  geom_vline(
    data = platform_overall_wordscores,
    aes(xintercept = wordscores, color = Party),
    inherit.aes = FALSE,
    linewidth = 1,
    linetype = "dashed",
    show.legend = TRUE
  ) +
  geom_segment(
    data = label_df2,
    aes(
      x = wordscores,
      xend = wordscores,
      y = 0,
      yend = 0.15,
      color = Party
    ),
    linewidth = 0.8,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  ggrepel::geom_text_repel(
    data = label_df2,
    aes(
      x = wordscores,
      y = 0,
      label = bioname,
      color = Party
    ),
    inherit.aes = FALSE,
    angle = 90,
    force = 5,
    size = 4,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c("Democrat" = "steelblue", "Republican" = "firebrick")
  ) +
  scale_color_manual(
    values = c("Democrat" = "steelblue", "Republican" = "firebrick")
  ) +
  labs(
    title = "Overall Wordscores Estimates for Legislators at the Low End of Dimension 2",
    subtitle = "Scale trained on all 2020/2022 major-party campaign text; dashed lines show party platform positions",
    x = "Overall Campaign Wordscores",
    y = "Density",
    fill = "Party",
    color = "Party"
  ) +
  theme_minimal()

ggsave(
  "~/Downloads/low_dim2_plot.pdf",
  plot = low_dim2_plot
)

#party av/differences 

party_means <- overall_winners %>%
  group_by(Party) %>%
  summarise(
    party_mean_wordscore = mean(wordscores, na.rm = TRUE),
    .groups = "drop"
  )

platform_positions <- platform_overall_wordscores %>%
  select(Party, platform_wordscore = wordscores)

distance_df <- overall_winners %>%
  left_join(party_means, by = "Party") %>%
  left_join(platform_positions, by = "Party") %>%
  mutate(
    dist_platform_signed = wordscores - platform_wordscore,
    dist_party_mean_signed = wordscores - party_mean_wordscore,
    dim2_group = case_when(
      str_detect(
        bioname,
        regex(paste(target_high_clean, collapse = "|"), ignore_case = TRUE)
      ) ~ "High Dim 2",
      str_detect(
        bioname,
        regex(paste(target_low, collapse = "|"), ignore_case = TRUE)
      ) ~ "Low Dim 2",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(dim2_group))

distance_summary <- distance_df %>%
  group_by(dim2_group, Party) %>%
  summarise(
    avg_dist_platform = mean(dist_platform_signed, na.rm = TRUE),
    avg_dist_party_mean = mean(dist_party_mean_signed, na.rm = TRUE),
    min_wordscore = min(wordscores, na.rm = TRUE),
    max_wordscore = max(wordscores, na.rm = TRUE),
    span_wordscore = max(wordscores, na.rm = TRUE) - min(wordscores, na.rm = TRUE),
    sd_wordscore = sd(wordscores, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

distance_summary

#issue text lookup
issue_text_lookup <- df2 %>%
  filter(
    year %in% c(2020, 2022),
    cand_party %in% c("Democrat", "Republican"),
    !is.na(BIOGUIDE_id)
  ) %>%
  rename(bioguide_id = BIOGUIDE_id) %>%
  arrange(bioguide_id, policy_code, year, statement_id) %>%
  group_by(bioguide_id, policy_code) %>%
  summarise(
    issue_text = paste(str_squish(issue_text), collapse = " "),
    candidate_webname = first(na.omit(candidate_webname)),
    .groups = "drop"
  )

#function to find extremes

make_issue_extremes <- function(issue_name, file_stub) {
  
  issue_scores <- members_plot2 %>%
    filter(policy_code == issue_name) %>%
    left_join(issue_text_lookup, by = c("bioguide_id", "policy_code")) %>%
    filter(!is.na(candidate_webname)) %>%
    arrange(wordscores)
  
  extremes_combined <- bind_rows(
    issue_scores %>%
      slice_head(n = 50) %>%
      mutate(side = "Most Democratic"),
    issue_scores %>%
      slice_tail(n = 120) %>%
      mutate(side = "Most Republican")
  )
  
  republicans_most_democratic <- issue_scores %>%
    filter(Party == "Republican") %>%
    arrange(wordscores) %>%
    slice_head(n = 30)
  
  democrats_most_republican <- issue_scores %>%
    filter(Party == "Democrat") %>%
    arrange(wordscores) %>%
    slice_tail(n = 30)
  
  write.csv(
    extremes_combined,
    paste0("~/Downloads/", file_stub, "_extremes_combined.csv"),
    row.names = FALSE
  )
  
  write.csv(
    republicans_most_democratic,
    paste0("~/Downloads/republicans_most_democratic_", file_stub, ".csv"),
    row.names = FALSE
  )
  
  write.csv(
    democrats_most_republican,
    paste0("~/Downloads/democrats_most_republican_", file_stub, ".csv"),
    row.names = FALSE
  )
  
  list(
    extremes_combined = extremes_combined,
    republicans_most_democratic = republicans_most_democratic,
    democrats_most_republican = democrats_most_republican
  )
}


civil_rights_results <- make_issue_extremes(
  issue_name = "Civil Rights, Liberties, and Minority Issues",
  file_stub = "civil_rights"
)

international_affairs_results <- make_issue_extremes(
  issue_name = "International Affairs",
  file_stub = "international_affairs"
)

government_operations_results <- make_issue_extremes(
  issue_name = "Government Operations",
  file_stub = "government_operations"
)

civil_rights_results$extremes_combined %>%
  select(side, candidate_webname, Party, wordscores, issue_text)

international_affairs_results$extremes_combined %>%
  select(side, candidate_webname, Party, wordscores, issue_text)

government_operations_results$extremes_combined %>%
  select(side, candidate_webname, Party, wordscores, issue_text)

#polarization campaigns

keep_issues <- c(
  "International Affairs",
  "Immigration",
  "Healthcare",
  "Government Operations",
  "Energy and Environment",
  "Economics and Commerce",
  "Defense",
  "Crime",
  "Civil Rights, Liberties, and Minority Issues"
)

issue_polarization_winners <- members_plot2 %>%
  filter(policy_code %in% keep_issues) %>%
  mutate(
    policy_code = recode(
      policy_code,
      "Energy and Environment" = "Energy/Environment",
      "Economics and Commerce" = "Economics/Commerce",
      "Civil Rights, Liberties, and Minority Issues" = "Civil Rights"
    )
  ) %>%
  group_by(policy_code, cand_party) %>%
  summarise(
    mean_wordscores = mean(wordscores, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = cand_party,
    values_from = mean_wordscores
  ) %>%
  mutate(
    polarization_gap = abs(Republican - Democrat)
  ) %>%
  arrange(desc(polarization_gap))

campaign_polarization<- ggplot(issue_polarization_winners, aes(
  x = reorder(policy_code, polarization_gap),
  y = polarization_gap
)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Most Polarized Campaign Issues Among Winning Candidates",
    subtitle = "Wordscores model trained on all 2020 and 2022 major-party candidates",
    x = "Issue",
    y = "Absolute Democrat–Republican Wordscores Gap"
  ) +
  theme_minimal()

ggsave(
  "~/Downloads/campaign_polarization.pdf",
  plot = campaign_polarization
)

#platform polarization

platform_polarization <- plat_lines2 %>%
  filter(policy_code %in% keep_issues) %>%
  mutate(
    policy_code = recode(
      policy_code,
      "Energy and Environment" = "Energy/Environment",
      "Economics and Commerce" = "Economics/Commerce",
      "Civil Rights, Liberties, and Minority Issues" = "Civil Rights"
    )
  ) %>%
  group_by(policy_code, party) %>%
  summarise(
    mean_wordscores = mean(wordscores, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = party,
    values_from = mean_wordscores
  ) %>%
  mutate(
    polarization_gap = abs(R - D)
  ) %>%
  arrange(desc(polarization_gap))

platform_polarization_plot<- ggplot(platform_polarization, aes(
  x = reorder(policy_code, polarization_gap),
  y = polarization_gap
)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Most Polarized Party Platform Issues",
    subtitle = "Absolute Democrat–Republican Wordscores gap by issue",
    x = "Issue",
    y = "Absolute Democrat–Republican Wordscores Gap"
  ) +
  theme_minimal()

ggsave(
  "~/Downloads/platform_polarization_plot.pdf",
  plot = platform_polarization_plot
)

#info about training data 
candidate_counts <- subset_platform_overall %>%
  distinct(candidate_id, cand_party) %>%
  count(cand_party, name = "n_candidates")

candidate_counts

#info ab winners 
subset_platform_overall %>%
  distinct(candidate_id) %>%
  summarise(total_candidates = n())

winner_loser_counts <- subset_platform_overall %>%
  distinct(candidate_id, cand_party, win_general) %>%
  mutate(
    outcome = case_when(
      win_general == 1 ~ "Winner",
      win_general == 0 ~ "Loser",
      TRUE ~ "Missing outcome"
    )
  ) %>%
  count(cand_party, outcome)

winner_loser_counts

policy_counts <- subset_platform_overall %>%
  distinct(candidate_id, policy_code) %>%
  count(policy_code, sort = TRUE, name = "n_documents")

policy_counts