packages <- c("tidyverse", "pscl", "remotes", "issueirt", "tibble", "dplyr")
new_pkgs <- packages[!(packages %in% installed.packages()[,"Package"])]
setwd("~/IssueIRT")
if(length(new_pkgs)) {
  install.packages(new_pkgs, repos = "https://cloud.r-project.org", Ncpus = 4)
}

# install issueirt from GitHub if not yet installed
if(!requireNamespace("issueirt", quietly = TRUE)) {
  if(!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes", repos = "https://cloud.r-project.org")
  }
  remotes::install_github(
    "sooahnshin/issueirt",
    build_vignettes = FALSE,    # faster, no LaTeX needed
    dependencies = TRUE,
    upgrade = "never"
  )
}

#libraries used
library(tidyverse)
library(pscl)
library(issueirt)
library(tibble)
library(dplyr)

#data
HS118_votes     <- read.csv("H118_votes.csv", stringsAsFactors = FALSE)
HS118_rollcalls <- read.csv("HouseOnly118.csv", stringsAsFactors = FALSE) 
HS118_members   <- read.csv("H118_members.csv", stringsAsFactors = FALSE)

HS117_votes     <- read.csv("H117_votes.csv", stringsAsFactors = FALSE)
HS117_votes <- subset(HS117_votes, icpsr != 99913)
#removes joe biden? idk bizarre
HS117_rollcalls <- read.csv("H117_rollcalls.csv", stringsAsFactors = FALSE) 
HS117_members   <- read.csv("H117_members.csv", stringsAsFactors = FALSE)


#keep House, recode votes 1/0/NA and normalize key to character
keep <- HS118_votes$chamber == "House"
vdf  <- HS118_votes[keep, c("icpsr","rollnumber","cast_code")]
vdf$vote <- ifelse(vdf$cast_code == 1, 1L,
                   ifelse(vdf$cast_code == 6, 0L, NA_integer_))
vdf$icpsr_chr <- as.character(vdf$icpsr)

#base reshape to wide: columns become vote.<rollnumber>
wide <- reshape(
  vdf[, c("icpsr_chr","rollnumber","vote")],
  idvar   = "icpsr_chr",
  timevar = "rollnumber",
  direction = "wide"
)

#sort rows by icpsr, clean column names to be rollnumbers
wide <- wide[order(wide$icpsr_chr), ]
colnames(wide) <- sub("^vote\\.", "vote_", colnames(wide))  #keep a 'vote_' prefix

#party mapping
HS118_members$icpsr_chr <- as.character(HS118_members$icpsr)

#numeric party_code
HS118_members$party_code <- suppressWarnings(as.integer(HS118_members$party_code))

HS118_members$group <- ifelse(HS118_members$party_code == 100L, "D",
                              ifelse(HS118_members$party_code == 200L, "R", "O"))

#keep house only
if ("chamber" %in% names(HS118_members)) {
  HS118_members <- HS118_members[HS118_members$chamber == "House", ]
}

#deduplicate: keep last occurrence per ICPSR
o <- order(HS118_members$icpsr_chr) 
HS118_members <- HS118_members[o, ]
dup_keep <- !duplicated(HS118_members$icpsr_chr, fromLast = TRUE)
party_map <- HS118_members[dup_keep, c("icpsr_chr","group")]

#join
wide$.ord <- seq_len(nrow(wide))
wide_pm <- merge(wide, party_map, by = "icpsr_chr", all.x = TRUE, sort = FALSE)
wide_pm <- wide_pm[order(wide_pm$.ord), ]
wide_pm$.ord <- NULL


##build rollcall

#raw_legis & raw_bills as plain data.frames
raw_legis <- data.frame(
  id    = wide_pm$icpsr_chr,
  group = wide_pm$group,
  stringsAsFactors = FALSE
)

vote_cols <- grep("^vote_", names(wide_pm), value = TRUE)
raw_bills <- data.frame(
  id = sub("^vote_", "", vote_cols),
  stringsAsFactors = FALSE
)

#vote matrix with dimnames = ids
votes_mat <- as.matrix(wide_pm[, vote_cols])
rownames(votes_mat) <- wide_pm$icpsr_chr
colnames(votes_mat) <- sub("^vote_", "", vote_cols)

# pscl::rollcall expects data.frame (not tibble)
rc <- pscl::rollcall(votes_mat,
                     yea = 1, nay = 0, missing = NA,
                     legis.names = raw_legis$id, legis.data = raw_legis,
                     vote.names  = raw_bills$id, vote.data  = raw_bills)

##filter votes/legislators

is_unanimous_rc <- apply(rc$votes, 2, function(col) {
  x <- col[!is.na(col)]
  length(x) > 0 && (all(x == 1L) || all(x == 0L))
})

filtered <- issueirt::filter_votes(rc, lop = 0, minvotes = 20)
votes <- rc$votes[filtered$legis, filtered$bills, drop = FALSE]
kept_bill_ids <- colnames(rc$votes)[filtered$bills]

is_unanimous_kept <- is_unanimous_rc[kept_bill_ids]
sum(is_unanimous_kept, na.rm = TRUE)

bills  <- raw_bills[filtered$bills, , drop = FALSE]
legis  <- raw_legis[filtered$legis, , drop = FALSE]

#checks
stopifnot(!any(is.na(legis$id)), !any(is.na(bills$id)))
stopifnot(identical(rownames(votes), legis$id))
stopifnot(identical(colnames(votes), bills$id))

rc_input <- pscl::rollcall(
  votes,
  yea = 1, nay = 0, missing = NA,
  legis.names = rownames(votes),
  legis.data  = legis,
  vote.names  = colnames(votes),
  vote.data   = bills
)

rc118_input <- rc_input
votes118    <- votes
legis118    <- legis
bills118    <- bills

keep <- HS117_votes$chamber == "House"
vdf  <- HS117_votes[keep, c("icpsr","rollnumber","cast_code")]
vdf$vote <- ifelse(vdf$cast_code == 1, 1L,
                   ifelse(vdf$cast_code == 6, 0L, NA_integer_))
vdf$icpsr_chr <- as.character(vdf$icpsr)

#base reshape to wide: columns become vote.<rollnumber>
wide <- reshape(
  vdf[, c("icpsr_chr","rollnumber","vote")],
  idvar   = "icpsr_chr",
  timevar = "rollnumber",
  direction = "wide"
)

#sort rows by icpsr, clean column names to be rollnumbers
wide <- wide[order(wide$icpsr_chr), ]
colnames(wide) <- sub("^vote\\.", "vote_", colnames(wide))  

##party mapping (base)

HS117_members$icpsr_chr <- as.character(HS117_members$icpsr)

#numeric party_code
HS117_members$party_code <- suppressWarnings(as.integer(HS117_members$party_code))

HS117_members$group <- ifelse(HS117_members$party_code == 100L, "D",
                              ifelse(HS117_members$party_code == 200L, "R", "O"))

#keep house only
if ("chamber" %in% names(HS117_members)) {
  HS117_members <- HS117_members[HS117_members$chamber == "House", ]
}

#deduplicate: keep last occurrence per ICPSR (adjust rule if you prefer)
o <- order(HS117_members$icpsr_chr)  # stable
HS117_members <- HS117_members[o, ]
dup_keep <- !duplicated(HS117_members$icpsr_chr, fromLast = TRUE)
party_map <- HS117_members[dup_keep, c("icpsr_chr","group")]

#join
wide$.ord <- seq_len(nrow(wide))
wide_pm <- merge(wide, party_map, by = "icpsr_chr", all.x = TRUE, sort = FALSE)
wide_pm <- wide_pm[order(wide_pm$.ord), ]
wide_pm$.ord <- NULL

##build rollcall (base)

raw_legis <- data.frame(
  id    = wide_pm$icpsr_chr,
  group = wide_pm$group,
  stringsAsFactors = FALSE
)

vote_cols <- grep("^vote_", names(wide_pm), value = TRUE)
raw_bills <- data.frame(
  id = sub("^vote_", "", vote_cols),
  stringsAsFactors = FALSE
)

votes_mat <- as.matrix(wide_pm[, vote_cols])
rownames(votes_mat) <- wide_pm$icpsr_chr
colnames(votes_mat) <- sub("^vote_", "", vote_cols)

rc <- pscl::rollcall(votes_mat,
                     yea = 1, nay = 0, missing = NA,
                     legis.names = raw_legis$id, legis.data = raw_legis,
                     vote.names  = raw_bills$id, vote.data  = raw_bills)

##filter votes/legislators
is_unanimous_rc <- apply(rc$votes, 2, function(col) {
  x <- col[!is.na(col)]
  length(x) > 0 && (all(x == 1L) || all(x == 0L))
})

filtered <- issueirt::filter_votes(rc, lop = 0, minvotes = 20)
votes <- rc$votes[filtered$legis, filtered$bills, drop = FALSE]
kept_bill_ids <- colnames(rc$votes)[filtered$bills]

is_unanimous_kept <- is_unanimous_rc[kept_bill_ids]
sum(is_unanimous_kept, na.rm = TRUE)

bills  <- raw_bills[filtered$bills, , drop = FALSE]
legis  <- raw_legis[filtered$legis, , drop = FALSE]

#checks
stopifnot(!any(is.na(legis$id)), !any(is.na(bills$id)))
stopifnot(identical(rownames(votes), legis$id))
stopifnot(identical(colnames(votes), bills$id))

rc_input <- pscl::rollcall(
  votes,
  yea = 1, nay = 0, missing = NA,
  legis.names = rownames(votes),
  legis.data  = legis,
  vote.names  = colnames(votes),
  vote.data   = bills
)

# --- store 117th outputs ---
rc117_input <- rc_input
votes117    <- votes
legis117    <- legis
bills117    <- bills

# pull pieces directly from the per-congress rollcall objects
votes117 <- rc117_input$votes
votes118 <- rc118_input$votes

legis117 <- rc117_input$legis.data
legis118 <- rc118_input$legis.data

bills117 <- rc117_input$vote.data
bills118 <- rc118_input$vote.data

# make unique bill IDs: 117_<roll>, 118_<roll>
bill_ids117 <- paste0("117_", colnames(votes117))
bill_ids118 <- paste0("118_", colnames(votes118))

colnames(votes117) <- bill_ids117
colnames(votes118) <- bill_ids118

# union of legislators and all bill IDs
all_leg_ids  <- sort(unique(c(rownames(votes117), rownames(votes118))))
all_bill_ids <- c(bill_ids117, bill_ids118)

# big vote matrix
votes_all <- matrix(
  NA_integer_,
  nrow = length(all_leg_ids),
  ncol = length(all_bill_ids),
  dimnames = list(all_leg_ids, all_bill_ids)
)

# fill from each congress
votes_all[rownames(votes117), bill_ids117] <- votes117
votes_all[rownames(votes118), bill_ids118] <- votes118

# combine legislator data: keep latest congress info if duplicated
legis117$source_cong <- 117
legis118$source_cong <- 118

legis_all <- rbind(legis117, legis118)
legis_all <- legis_all[order(legis_all$id, legis_all$source_cong), ]
keep_leg  <- !duplicated(legis_all$id, fromLast = TRUE)
legis_all <- legis_all[keep_leg, ]

#align to row order of votes_all
legis_all <- legis_all[match(rownames(votes_all), legis_all$id), ]
stopifnot(identical(legis_all$id, rownames(votes_all)))

#combine bill data
bills117$roll_id  <- as.character(bills117$id)
bills117$congress <- 117
bills117$id       <- bill_ids117

bills118$roll_id  <- as.character(bills118$id)
bills118$congress <- 118
bills118$id       <- bill_ids118

bills_all <- rbind(bills117, bills118)
bills_all <- bills_all[match(colnames(votes_all), bills_all$id), ]
stopifnot(identical(bills_all$id, colnames(votes_all)))


#combined 
rc_input <- pscl::rollcall(
  votes_all,
  yea         = 1, nay = 0, missing = NA,
  legis.names = rownames(votes_all),
  legis.data  = legis_all,
  vote.names  = colnames(votes_all),
  vote.data   = bills_all
)


## DROP ULTRA-LOPSIDED VOTES
yea_counts <- colSums(rc_input$votes == 1, na.rm = TRUE)
nay_counts <- colSums(rc_input$votes == 0, na.rm = TRUE)
total_counts <- yea_counts + nay_counts
minority_counts <- pmin(yea_counts, nay_counts)

keep_votes <- minority_counts >= 0.025 * total_counts
table(keep_votes)

rc_input <- pscl::rollcall(
  rc_input$votes[, keep_votes, drop = FALSE],
  yea = 1, nay = 0, missing = NA,
  legis.names = rownames(rc_input$votes),
  legis.data  = rc_input$legis.data,
  vote.names  = colnames(rc_input$votes)[keep_votes],
  vote.data   = rc_input$vote.data[keep_votes, , drop = FALSE]
)

# convenience
votes <- rc_input$votes
legis <- rc_input$legis.data
bills <- rc_input$vote.data

nrow(votes); ncol(votes)


##starting values BIRT 

set.seed(1)
ideal <- pscl::ideal(
  rc_input,
  dropList   = list(lop = 0, legisMin = 0),
  priors     = NULL,
  startvals  = "eigen",
  d          = 2,
  maxiter    = 35000,
  thin       = 1,
  burnin     = 4800,
  impute     = FALSE,
  normalize  = FALSE,
  store.item = TRUE,
  file       = NULL,
  verbose    = FALSE
)

legis <- rc_input$legis.data  
table(rc_input$legis.data$group, useNA = "ifany")

# choose which party sits "on top" in 2D (D left, R right, O on top if present)
top_party <- if (any(legis$group == "O")) "O" else "R"

# find horizontal & vertical political rollcalls
pol_rc1 <- issueirt::find_pol_rc_horizontal(
  rc_input,
  party_code_col    = "group",
  liberal_code      = "D",
  conservative_code = "R",
  na_threshold      = 0.55   
)

pol_rc2 <- issueirt::find_pol_rc_vertical(
  ideal, rc_input, pol_rc1,
  party_code_col = "group",
  na_threshold   = 0.55,
  lop_threshold  = 0.10
)

# build constraints and post-process ideal
const_ls <- issueirt::find_constraints(
  ideal, rc_input,
  pol_rc1         = pol_rc1,
  pol_rc2         = pol_rc2,
  party_code_col  = "group",
  left_party_code = "D",
  top_party_code  = top_party,
  as_list         = TRUE
)

invisible(capture.output({
  ideal_pp <- pscl::postProcess(ideal, constraints = const_ls)
}))

#step 3- make issue codes

bills_117 <- read_csv("H117_rollcalls.csv", show_col_types = FALSE)
issue_labels_117 <- bills_117 %>%
  select(
    congress,
    rollnumber,
    issue_label = issue_code
  )

bills_118 <- read_csv("HouseOnly118.csv", show_col_types = FALSE)
issue_labels_118 <- bills_118 %>%
  select(
    congress,
    rollnumber,
    issue_label = issue_code
  )

issue_labels <- bind_rows(issue_labels_117, issue_labels_118)


##stack and recode to policy domains

issue_labels_both <- bind_rows(issue_labels_117, issue_labels_118) %>%
  mutate(
    issue_label = case_when(
      # DEFENSE
      issue_label == "Armed Forces and National Security" ~ "defense",
      
      # HEALTHCARE
      issue_label %in% c(
        "Health",
        "Social Welfare",
        "Families"
      ) ~ "healthcare",
      
      # INTERNATIONAL AFFAIRS
      issue_label %in% c(
        "International Affairs",
        "Foreign Trade and International Finance",
        "nternational Affairs"
      ) ~ "international_affairs",
      
      # ECONOMIC / COMMERCE
      issue_label %in% c(
        "Economics and Public Finance",
        "Finance and Financial Sector",
        "Commerce",
        "Taxation",
        "Labor and Employment"
      ) ~ "econ_commerce",
      
      # GOVERNMENT OPERATIONS
      issue_label %in% c(
        "Government Operations and Politics",
        "Congress"
      ) ~ "government_ops",
      
      # CRIME
      issue_label %in% c(
        "Crime and Law Enforcement"
      ) ~ "crime",
      
      # CIVIL RIGHTS
      issue_label %in% c(
        "Civil Rights and Liberties, Minority Issues",
        "Native Americans",
        "Law",
        "Arts, Culture, Religion"
      ) ~ "civil_rights",
      
      # IMMIGRATION
      issue_label == "Immigration" ~ "immigration",
      
      # ENVIRONMENT & ENERGY
      issue_label %in% c(
        "Environmental Protection",
        "Public Lands and Natural Resources",
        "Water Resources Development",
        "Energy",
        "Agriculture and Food",
        "Animals"
      ) ~ "environment_energy",
      
      # EVERYTHING ELSE
      TRUE ~ "generic"
    )
  )

##make roll_id + congress match in both tables

issue_labels_both <- issue_labels_both %>%
  mutate(
    roll_id  = as.character(rollnumber),
    congress = as.integer(congress)
  ) %>%
  select(congress, roll_id, issue_label)

bills_all <- bills_all %>%
  mutate(
    roll_id  = as.character(roll_id),
    congress = as.integer(congress)
  )

##add issue_label to every bill in bills_all
issue_labels_all <- bills_all %>%
  left_join(
    issue_labels_both,
    by = c("congress", "roll_id")
  ) %>%
  mutate(
    issue_label = replace_na(issue_label, "generic")
  )

#should match your final breakdown
issue_labels_all %>% count(issue_label, sort = TRUE)

tmp_join <- bills_all %>%
  left_join(
    issue_labels_both,
    by = c("congress", "roll_id")
  )

sum(is.na(tmp_join$issue_label))  #this should be the number you expect to be "other"

#align 
issue_labels_aligned <- rc_input$vote.data %>%
  select(id) %>%
  left_join(
    issue_labels_all %>% select(id, issue_label),
    by = "id"
  )

issue_labels_aligned %>%
  count(issue_label, sort = TRUE) %>%
  mutate(prop = n / sum(n))


#perfect 1-to-1 alignment
stopifnot(identical(issue_labels_aligned$id, rc_input$vote.data$id))

#one issue label per vote
stopifnot(nrow(issue_labels_aligned) == ncol(rc_input$votes))


#build issue_code + Stan input
issue_levels <- sort(unique(issue_labels_aligned$issue_label))

issue_code <- make_issue_code(
  issue_code_vec = issue_labels_aligned$issue_label,
  levels         = issue_levels
)

stan_input <- issueirt::make_stan_input(
  issue_code_vec = issue_code$issue_code_vec,
  rollcall       = rc_input,
  ideal          = ideal_pp,
  a              = 1,
  b              = 0.1,
  rho_init       = 9
)

##fit model

set.seed(1)
chains <- 3
init_spec = replicate(chains, stan_input$init, simplify = FALSE)

fit <- issueirt_stan(
  data    = stan_input$data,
  init    = init_spec,
  chains  = chains,
  warmup  = 1250,
  iter    = 2500,          
  cores   = chains,        
  seed    = 123
)

saveRDS(fit, file = "issueirt_117_118.rds")
fit <- readRDS("issueirt_117_118.rds")

posterior_samples <- post_process(stan_fit = fit, constraints = const_ls,
                                  legis_label = legis$id, as_mcmc = TRUE)
names(posterior_samples)

K <- stan_input$data$K
issue_label <- paste0("Issue ", seq_len(K))

posterior_summary_pp <- make_posterior_summary_postprocessed(
  stan_fit      = fit,
  constraints   = const_ls,
  issue_label   = issue_label,
  rc_label      = bills$id,
  legis_label   = legis$id,
  missing_label = NULL
)

names(posterior_summary_pp)

K <- length(issue_label)

issue_label_pretty <- c(
  "Civil Rights",          # 1
  "Crime",                 # 2
  "Economy/Commerce",      # 3
  "Defense",               # 4
  "Environment/Energy",    # 5
  "Other",                 # 6
  "Government Operations", # 7
  "Health",                # 8
  "Immigration",           # 9
  "International Affairs"  # 10
)


stopifnot(length(issue_label_pretty) == length(issue_label))

pretty_map <- setNames(issue_label_pretty, issue_label)


##2D IDEAL POINTS

plot_ideal(
  ideal_point_1d = posterior_summary_pp$x_postprocessed |>
    dplyr::filter(dimension == 1) |>
    dplyr::pull(mean),
  ideal_point_2d = posterior_summary_pp$x_postprocessed |>
    dplyr::filter(dimension == 2) |>
    dplyr::pull(mean),
  group = legis$group,
  p.title = "IssueIRT Estimates of Ideal Points (Postprocessed)",
  breaks.group = c("D","R"),
  values.shape = c(17, 19),
  values.color = c("steelblue", "firebrick")
)

##ISSUE-SPECIFIC IDEAL POINTS 

issue_irt <- get_ideal_points(
  stan_fit     = fit,
  issue_label  = issue_label,     
  legis_label  = legis$id,
  legis_group  = legis$group,
  dynamic      = FALSE
)

issue_irt <- issue_irt %>%
  dplyr::mutate(
    issue_label = issue_label[issue_index],  
    issue_label_pretty = factor(
      pretty_map[issue_label],
      levels = issue_label_pretty
    )
  )

issue_irt_pretty <- issue_irt %>%
  dplyr::mutate(issue_label = as.character(issue_label_pretty))

##Issue axis plots 

p_ls <- plot_issueaxis(
  stan_input = stan_input,
  posterior_summary = posterior_summary_pp,
  group = legis$group,
  breaks.group = c("D","R"),
  values.shape = c(17,19),
  values.color = c("steelblue", "firebrick")
)

p_ls_pretty <- Map(
  function(p, lab) p + ggtitle(paste0(lab)),
  p_ls,
  issue_label_pretty
)

for (i in seq_along(p_ls_pretty)) {
  print(p_ls_pretty[[i]])
}

##Mirrored violins with fixed scale + pretty facet labels

issue_irt_mirrored <- issue_irt_pretty %>%
  dplyr::filter(issue_label != "Other") %>%   # <-- this is the key line
  dplyr::mutate(
    mean = -mean,  # mirror so D left, R right
    legis_group = factor(legis_group, levels = c("D", "R"))
  )

ggplot(
  issue_irt_mirrored %>% 
    dplyr::filter(issue_label_pretty != "Other"),
  aes(x = legis_group, y = mean, fill = legis_group)
) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.35) +
  facet_wrap(~ issue_label_pretty, ncol = 3) +   # 3×3 layout
  scale_fill_manual(values = c("D" = "steelblue", "R" = "firebrick")) +
  labs(
    title = "Issue-Specific Ideal Points (Mirrored)",
    subtitle = "Distributions by party; comparable scale across issues",
    x = NULL,
    y = "Ideology (Liberal \u2190  → Conservative)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold")
  )

ggplot(
  issue_irt_mirrored %>% 
    dplyr::filter(issue_label_pretty != "Other"),
  aes(x = mean, fill = legis_group, color = legis_group)
) +
  geom_density(alpha = 0.35, linewidth = 0.8) +
  facet_wrap(~ issue_label_pretty, ncol = 3) +
  scale_fill_manual(values = c("D" = "steelblue", "R" = "firebrick")) +
  scale_color_manual(values = c("D" = "steelblue", "R" = "firebrick")) +
  labs(
    title = "Issue-Specific Ideal Points",
    subtitle = "Density distributions by party; comparable scale across issues",
    x = "Ideology (Liberal \u2190  → Conservative)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold")
  )

plot_issueirt(issueirt = issue_irt_mirrored,
              p.title = "Issue-specific ideal points (mirrored)",
              breaks.group = c("D","R"),
              values.shape = c(17,19),
              values.color = c("steelblue","firebrick"))


## add legislator names


legis_lookup <- bind_rows(HS117_members, HS118_members) %>%
  transmute(
    legis_label = as.character(icpsr),
    bioname,
    state_abbrev
  ) %>%
  distinct(legis_label, .keep_all = TRUE)

issue_irt_named <- issue_irt_pretty %>%
  mutate(legis_label = as.character(legis_label)) %>%
  left_join(legis_lookup, by = "legis_label")


#get table of those 3 sd from their parties mean
dat <- issue_irt_mirrored %>%
  mutate(theta_issue = mean)  

outliers_3sd <- dat %>%
  group_by(issue_label, legis_group) %>%
  mutate(
    party_mean = mean(theta_issue, na.rm = TRUE),
    party_sd   = sd(theta_issue, na.rm = TRUE),
    z          = (theta_issue - party_mean) / party_sd
  ) %>%
  ungroup() %>%
  filter(is.finite(z), abs(z) >= 3) %>%
  arrange(issue_label, legis_group, desc(abs(z))) %>%
  select(issue_label, legis_group, legis_label, theta_issue, party_mean, party_sd, z)
pretty_map <- setNames(issue_label_pretty, issue_label)

outliers_3sd <- outliers_3sd %>%
  mutate(issue = pretty_map[issue_label]) %>%
  select(issue_label, everything())   
outliers_3sd_named <- outliers_3sd %>%
  mutate(legis_label = as.character(legis_label)) %>%
  left_join(legis_lookup, by = "legis_label") %>%
  relocate(bioname, state_abbrev, .after = legis_label)
outliers_3sd_named %>%
  arrange(issue, legis_group, desc(abs(z))) %>%
  print(n = 200)

outliers_2_5sd <- dat %>%
  group_by(issue_label, legis_group) %>%
  mutate(
    party_mean = mean(theta_issue, na.rm = TRUE),
    party_sd   = sd(theta_issue, na.rm = TRUE),
    z          = (theta_issue - party_mean) / party_sd
  ) %>%
  ungroup() %>%
  filter(is.finite(z), abs(z) >= 2.5) %>%  
  arrange(issue_label, legis_group, desc(abs(z))) %>%
  select(issue_label, legis_group, legis_label,
         theta_issue, party_mean, party_sd, z)
pretty_map <- setNames(issue_label_pretty, issue_label)

outliers_2_5sd <- outliers_2_5sd %>%
  mutate(issue = pretty_map[issue_label]) %>%
  select(issue_label, everything())   

outliers_2_5sd_named <- outliers_2_5sd %>%
  mutate(legis_label = as.character(legis_label)) %>%
  left_join(legis_lookup, by = "legis_label") %>%
  relocate(bioname, state_abbrev, .after = legis_label)

outliers_2_5sd_named %>%
  arrange(issue, legis_group, desc(abs(z))) %>%
  print(n = 200)


#WHOCH ISSUES ARE CLOSEST/FURTHEST FROM THE AVERAGE AXIS FOR THAT ISSUE
roll_lookup <- bind_rows(
  HS117_rollcalls %>%
    transmute(
      rc_label   = paste0("117_", rollnumber),
      issue_raw  = issue_code,
      bill_number,
      vote_desc,
      vote_question,
      dtl_desc
    ),
  HS118_rollcalls %>%
    transmute(
      rc_label   = paste0("118_", rollnumber),
      issue_raw  = issue_code,
      bill_number,
      vote_desc,
      vote_question,
      dtl_desc
    )
) %>%
  mutate(
    issue_label = case_when(
      issue_raw == "Armed Forces and National Security" ~ "defense",
      issue_raw %in% c("Health", "Social Welfare", "Families") ~ "healthcare",
      issue_raw %in% c("International Affairs","Foreign Trade and International Finance","nternational Affairs") ~ "international_affairs",
      issue_raw %in% c("Economics and Public Finance","Finance and Financial Sector","Commerce","Taxation","Labor and Employment") ~ "econ_commerce",
      issue_raw %in% c("Government Operations and Politics","Congress") ~ "government_ops",
      issue_raw %in% c("Crime and Law Enforcement") ~ "crime",
      issue_raw %in% c("Civil Rights and Liberties, Minority Issues","Native Americans","Law","Arts, Culture, Religion") ~ "civil_rights",
      issue_raw == "Immigration" ~ "immigration",
      issue_raw %in% c("Environmental Protection","Public Lands and Natural Resources",
                       "Water Resources Development","Energy","Agriculture and Food","Animals") ~ "environment_energy",
      TRUE ~ "generic"
    )
  )

pretty_map <- setNames(issue_label_pretty, issue_label)

roll_lookup <- roll_lookup %>%
  mutate(issue_label_pretty = pretty_map[issue_label])



party_issue_means <- issue_irt_mirrored %>%
  group_by(issue_label_pretty, legis_group) %>%
  summarise(
    mean_theta = mean(mean, na.rm = TRUE),
    sd_theta   = sd(mean, na.rm = TRUE),
    n          = n(),
    .groups = "drop"
  )


#POLARIZATION BY ISSUE

polarization_tbl <- party_issue_means %>%
  select(issue_label_pretty, legis_group, mean_theta) %>%
  tidyr::pivot_wider(
    names_from = legis_group,
    values_from = mean_theta
  ) %>%
  mutate(
    polarization = abs(D - R)
  ) %>%
  arrange(desc(polarization))

polarization_tbl

ggplot(polarization_tbl,
       aes(x = reorder(issue_label_pretty, polarization),
           y = polarization)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Issue Polarization by Party",
    subtitle = "Absolute difference in issue-specific ideal points",
    x = NULL,
    y = "| Mean(D) − Mean(R) |"
  ) +
  theme_minimal()

upp2 <- posterior_summary_pp$u_antipodal  #Dim 2 item params

rc_to_issue <- tibble(
  rc_index = 1:stan_input$data$M,
  issue_index = stan_input$data$z
)

top_dim2_by_issue <- upp2 %>%
  left_join(rc_to_issue, by = "rc_index") %>%
  group_by(issue_index) %>%
  slice_max(order_by = abs(mean), n = 10, with_ties = FALSE) %>%  
  ungroup() %>%
  arrange(issue_index, desc(abs(mean))) %>%
  select(issue_index, rc_label, mean, sd, `2.5%`, `97.5%`)

top_dim2_by_issue <- top_dim2_by_issue %>%
  mutate(issue_label = issue_label_pretty[issue_index])

top_pos_dim2_by_issue <- upp2 %>%
  left_join(rc_to_issue, by = "rc_index") %>%
  group_by(issue_index) %>%
  slice_max(order_by = mean, n = 10, with_ties = FALSE) %>%
  ungroup()

top_pos_dim2_by_issue <- top_pos_dim2_by_issue %>%
  mutate(issue_label = issue_label_pretty[issue_index])

top_pos_dim2_by_issue_check <- top_pos_dim2_by_issue %>%
  select(rc_label, issue_index, mean) %>%
  left_join(roll_lookup %>% select(rc_label, issue_raw, issue_label, issue_label_pretty),
            by = "rc_label") %>%
  arrange(desc(mean))

top_pos_dim2_by_issue_check %>% 
  select(rc_label, mean, issue_index, issue_raw, issue_label, issue_label_pretty) %>%
  print(n = 30)

issue_key <- upp2 %>%
  left_join(rc_to_issue, by = "rc_index") %>%
  select(rc_label, issue_index) %>%
  left_join(roll_lookup %>% select(rc_label, issue_label_pretty), by = "rc_label") %>%
  filter(!is.na(issue_label_pretty)) %>%
  count(issue_index, issue_label_pretty, sort = TRUE) %>%
  group_by(issue_index) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup()

top_pos_dim2_by_issue_labeled <- top_pos_dim2_by_issue %>%
  left_join(issue_key, by = "issue_index")

top_neg_dim2_by_issue <- upp2 %>%
  left_join(rc_to_issue, by = "rc_index") %>%
  group_by(issue_index) %>%
  slice_min(order_by = mean, n = 10, with_ties = FALSE) %>%
  ungroup()

top_neg_dim2_by_issue <- top_neg_dim2_by_issue %>%
  mutate(issue_label = issue_label_pretty[issue_index])

xpp <- posterior_summary_pp$x_postprocessed

legis_dim2 <- xpp %>%
  filter(dimension == 2) %>%
  mutate(legis_label = as.character(legis_label)) %>%
  left_join(legis_lookup, by = "legis_label")

top15_high <- legis_dim2 %>%
  arrange(desc(mean)) %>%
  slice_head(n = 15) %>%
  mutate(side = "highest (Dim 2 +)")

top15_low <- legis_dim2 %>%
  arrange(mean) %>%
  slice_head(n = 15) %>%
  mutate(side = "lowest (Dim 2 −)")

extremes_30 <- bind_rows(top15_high, top15_low) %>%
  transmute(
    side,
    legis_label,
    bioname,
    state_abbrev,
    dim2_mean = mean,
    sd,
    `2.5%`,
    `97.5%`
  ) %>%
  arrange(side, desc(abs(dim2_mean)))


legis_dim2 <- xpp %>%
  filter(dimension == 2) %>%
  mutate(legis_label = as.character(legis_label)) %>%
  left_join(legis_lookup, by = "legis_label") %>%
  left_join(
    tibble(legis_label = as.character(legis$id), party = legis$group),
    by = "legis_label"
  )


extremes_by_party <- legis_dim2 %>%
  filter(!is.na(party)) %>%
  group_by(party) %>%
  mutate(side = if_else(mean >= 0, "highest (Dim 2 +)", "lowest (Dim 2 −)")) %>%
  group_by(party, side) %>%
  slice_max(order_by = abs(mean), n = 15, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    party,
    side,
    legis_label,
    bioname,
    state_abbrev,
    dim2_mean = mean,
    sd,
    `2.5%`,
    `97.5%`
  ) %>%
  arrange(party, side, desc(abs(dim2_mean)))

print(extremes_by_party, n = 60)

write.csv(extremes_by_party, "extremes_by_party.csv", row.names = FALSE)
write.csv(top_dim2_by_issue, "top_dim2_by_issue.csv", row.names = FALSE)

keep_issues_pretty <- c(
  "Government Operations",
  "Civil Rights, Liberties, and Minority Issues",
  "International Affairs"
)


df3 <- issue_irt_mirrored %>%
  mutate(
    issue_label_pretty = if_else(
      issue_label_pretty == "Civil Rights",
      "Civil Rights, Liberties, and Minority Issues",
      issue_label_pretty
    )
  ) %>%
  filter(issue_label_pretty %in% keep_issues_pretty) %>%
  mutate(issue_label_pretty = factor(issue_label_pretty, levels = keep_issues_pretty))

ggplot(
  df3,
  aes(x = legis_group, y = mean, fill = legis_group)
) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.35) +
  facet_wrap(~ issue_label_pretty, ncol = 3) +
  scale_fill_manual(
    values = c("D" = "steelblue", "R" = "firebrick"),
    name = "Party",
    labels = c("D" = "Democrat", "R" = "Republican")
  ) +
  scale_color_manual(
    values = c("D" = "steelblue", "R" = "firebrick"),
    name = "Party",
    labels = c("D" = "Democrat", "R" = "Republican")
  ) +
  labs(
    title = "Violin Plots of Issue-Specific Ideal Points",
    subtitle = "1D Projection of the 2D Model onto the Issue’s Axis",
    x = NULL,
    y = "Ideology (Liberal \u2190  → Conservative)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right"
  )

ggplot(
  df3,
  aes(x = mean, fill = legis_group, color = legis_group)
) +
  geom_density(alpha = 0.35, linewidth = 0.8) +
  facet_wrap(~ issue_label_pretty, ncol = 3) +
  scale_fill_manual(
    values = c("D" = "steelblue", "R" = "firebrick"),
    name = "Party",
    labels = c("D" = "Democrat", "R" = "Republican")
  ) +
  scale_color_manual(
    values = c("D" = "steelblue", "R" = "firebrick"),
    name = "Party",
    labels = c("D" = "Democrat", "R" = "Republican")
  ) +
  labs(
    title = "Density of Issue-Specific Ideal Points",
    subtitle = "1D Projection of the 2D Model onto the Issue’s Axis",
    x = "Ideology (Liberal \u2190  → Conservative)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",  
  )


order3 <- c("Government Operations", "Civil Rights", "International Affairs")
idx3 <- match(order3, issue_labels)  

order3_pretty <- c(
  "Government Operations",
  "Civil Rights, Liberties, and Minority Issues",
  "International Affairs"
)

idx3 <- match(order3, issue_labels)

p_ls_3 <- Map(
  function(p, lab) p + ggtitle(paste0(lab)),
  p_ls[idx3],
  order3_pretty
)

p_axis_row <- p_ls_3[[1]] | p_ls_3[[2]] | p_ls_3[[3]]

p_axis_row

#cosine dim 1 
upp1 <- posterior_summary_pp$u
upp2 <- posterior_summary_pp$u_antipodal

cosines <- tibble(
  rc_index = 1:nrow(upp1),
  a1 = upp1$mean,
  a2 = upp2$mean,
  se1 = upp1$sd,
  se2 = upp2$sd
) %>%
  mutate(
    magnitude = sqrt(a1^2 + a2^2),
    cos_dim1 = a1 / magnitude
  )

cosines <- cosines %>%
  left_join(rc_to_issue, by = "rc_index") %>%
  mutate(issue_label = issue_label_pretty[issue_index])

issue_cos <- cosines %>%
  group_by(issue_label) %>%
  summarise(
    mean_cos = mean(cos_dim1),
    se_cos = sd(cos_dim1)/sqrt(n()),
    n = n()
  )

ggplot(cosines, aes(x = issue_label, y = cos_dim1)) +
  #  cosines %>% filter(issue_label != "Other"),
  #aes(x = issue_label, y = cos_dim1)
##) +
  
  geom_jitter(color = "grey60", width = .2, alpha = .4, size = 1.5) +
  
  geom_point(
    data = issue_cos,
    aes(x = issue_label, y = mean_cos),
    size = 5,
    color = "firebrick",
    inherit.aes = FALSE
  ) +
  
  geom_errorbar(
    data = issue_cos,
    aes(
      x = issue_label,
      ymin = mean_cos - se_cos,
      ymax = mean_cos + se_cos
    ),
    width = 0,
    linewidth = 1,
    color = "firebrick",
    inherit.aes = FALSE
  ) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  coord_flip() +
  theme_minimal()

#cosine dim 2
cosines <- tibble(
  rc_index = 1:nrow(upp1),
  a1 = upp1$mean,
  a2 = upp2$mean
) %>%
  mutate(
    magnitude = sqrt(a1^2 + a2^2),
    cos_dim1 = a1 / magnitude,
    cos_dim2 = a2 / magnitude
  )

issue_cos2 <- cosines %>%
  left_join(rc_to_issue, by = "rc_index") %>%
  mutate(issue_label = issue_label_pretty[issue_index]) %>%
  group_by(issue_label) %>%
  summarise(
    mean_cos2 = mean(cos_dim2),
    se_cos2 = sd(cos_dim2)/sqrt(n()),
    n = n()
  )

ggplot(cosines %>%
         left_join(rc_to_issue, by="rc_index") %>%
         mutate(issue_label = issue_label_pretty[issue_index]),
       aes(x = issue_label, y = cos_dim2)) +
  
  geom_jitter(color="grey60", width=.2, alpha=.4, size=1.5) +
  
  geom_point(
    data = issue_cos2,
    aes(x = issue_label, y = mean_cos2),
    color = "firebrick",
    size = 5,
    inherit.aes = FALSE
  ) +
  
  geom_errorbar(
    data = issue_cos2,
    aes(x = issue_label,
        ymin = mean_cos2 - se_cos2,
        ymax = mean_cos2 + se_cos2),
    color = "firebrick",
    width = 0,
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  
  geom_hline(yintercept = c(-1, 0, 1), linetype = "dashed")+
  
  coord_flip() +
  theme_minimal() +
  labs(
    x="",
    y="Cosine Alignment with Dimension 2",
    title="Policy Domain Alignment with Ideological Dimension 2"
  )

ggplot(cosines, aes(cos_dim1, cos_dim2)) +
  geom_point(alpha=.3) +
  coord_fixed()
#abs value cosine dim1
cosines <- cosines %>%
  left_join(rc_to_issue, by = "rc_index") %>%
  mutate(issue_label = issue_label_pretty[issue_index])

cosines <- cosines %>%
  mutate(cosine_abs = abs(cos_dim1))

issue_cos_abs <- cosines %>%
  group_by(issue_label) %>%
  summarise(
    mean_cos_abs = mean(cosine_abs),
    se_cos_abs = sd(cosine_abs)/sqrt(n()),
    n = n()
  )

ggplot(cosines %>% filter(issue_label != "Other"),
       aes(x = issue_label, y = cosine_abs)) +
  
  geom_jitter(color = "grey60", width = .2, alpha = .4, size = 1.5) +
  
  geom_point(
    data = issue_cos_abs %>% filter(issue_label != "Other"),
    aes(x = issue_label, y = mean_cos_abs),
    size = 5,
    color = "firebrick",
    inherit.aes = FALSE
  ) +
  
  geom_errorbar(
    data = issue_cos_abs %>% filter(issue_label != "Other"),
    aes(
      x = issue_label,
      ymin = mean_cos_abs - se_cos_abs,
      ymax = mean_cos_abs + se_cos_abs
    ),
    width = 0,
    linewidth = 1,
    color = "firebrick",
    inherit.aes = FALSE
  ) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  coord_flip() +
  theme_minimal()

#abs value cosine dim2
cosines <- cosines %>%
  mutate(cosine2_abs = abs(cos_dim2))

issue_cos2_abs <- cosines %>%
  group_by(issue_label) %>%
  summarise(
    mean_cos2_abs = mean(cosine2_abs, na.rm = TRUE),
    se_cos2_abs = sd(cosine2_abs, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

ggplot(
  cosines %>% filter(issue_label != "Other"),
  aes(x = issue_label, y = cosine2_abs)
) +
  
  geom_jitter(color="grey60", width=.2, alpha=.4, size=1.5) +
  
  geom_point(
    data = issue_cos2_abs %>% filter(issue_label != "Other"),
    aes(x = issue_label, y = mean_cos2_abs),
    color = "firebrick",
    size = 5,
    inherit.aes = FALSE
  ) +
  
  geom_errorbar(
    data = issue_cos2_abs %>% filter(issue_label != "Other"),
    aes(
      x = issue_label,
      ymin = mean_cos2_abs - se_cos2_abs,
      ymax = mean_cos2_abs + se_cos2_abs
    ),
    color = "firebrick",
    width = 0,
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  
  geom_hline(yintercept = c(0,1), linetype="dashed") +
  
  coord_flip() +
  theme_minimal() +
  labs(
    x="",
    y="|Cosine Alignment with Dimension 2|",
    title="Strength of Policy Domain Alignment with Dimension 2"
  )

cosines %>%
  mutate(diff_abs = abs(abs(a1) - abs(a2))) %>%
  arrange(diff_abs) %>%
  select(rc_index, issue_label, a1, a2, cos_dim1, cos_dim2, cosine2_abs, diff_abs) %>%
  head(20)

cosines %>%
  filter(abs(cosine2_abs - 1/sqrt(2)) < 1e-6) %>%
  select(rc_index, issue_label, a1, a2, cos_dim1, cos_dim2, cosine2_abs)
