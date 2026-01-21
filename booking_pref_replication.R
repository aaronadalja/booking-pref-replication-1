#######################################################################################
### R replication code for:                                                         ###
### Consumer preferences for hotel sustainability attributes in online bookings     ###
#######################################################################################

# DATE: JANUARY 2026
# Editor: Aaron Adalja


rm(list = ls())
objects()
options(error=recover, scipen=999, max.print = 9999)

# Any package that is required by the script below is given here:----
# Check to see if packages are installed, if not install.
inst_pkgs = load_pkgs =  c("data.table", "tidyverse", "magrittr", "tidyselect", "stringr", "ggrepel", "ggpattern", "ggpubr",
                           "systemfit", "knitr", "lubridate", "mfx", "texreg","remotes", "logitr", "marginaleffects",
                           "margins", "gridExtra", "gtools", "openxlsx", "patchwork", "margins", "mlogit", "RSGHB", "nnet")
inst_pkgs = inst_pkgs[!(inst_pkgs %in% installed.packages()[,"Package"])]
if(length(inst_pkgs)) install.packages(inst_pkgs)

# Dynamically load packages---
pkgs_loaded = lapply(load_pkgs, require, character.only=T)

raw.low <- read.xlsx("./rawdata_low.xlsx", "data")
raw.low.cbc <- read.xlsx("./rawdata_low.xlsx", "cbc")
raw.med <- read.xlsx("./rawdata_med.xlsx", "data")
raw.med.cbc <- read.xlsx("./rawdata_med.xlsx", "cbc")
raw.high <- read.xlsx("./rawdata_high.xlsx", "data")
raw.high.cbc <- read.xlsx("./rawdata_high.xlsx", "cbc")

raw.low$variant <- "economy"
raw.med$variant <- "midscale"
raw.high$variant <- "upscale"

## RENAME NON-BINARY variable ##
raw.med %<>% rename(non_binary=`non-binary`)
raw.high %<>% rename(non_binary=`non-binary`)

## combine low/med/high survey data 
combined <- rbind(raw.low, raw.med, raw.high)

## RENAME bachelor's variable ##
combined %<>% rename(bachelors=`bachelor's`)

## combine low/med/high conjoint design data
combined.cbc <- rbind(raw.low.cbc, raw.med.cbc, raw.high.cbc)

## merge conjoint design with survey data
combined.cbc %<>% inner_join(combined, by="id")


## drop extraneous variables
clean <- combined.cbc %>% dplyr::select(id, task, alt, score, distance, sus_lvl, nightly_price, choice, status, atn_ck_1, travel_freq, 
                                        amenities, price, guest_rev, proximity, hotel_star, sust_level, other, other_text, sust_filter, book_directly, 
                                        waste, energy, water, local, biodiversity, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `10`, `11`, `12`, `13`, `14`, `15`, 
                                        birth_year, age, male, white, middle_east, black, native, asian, islander, hispanic, degree, bachelors, 
                                        employment, full_time, income, marital, married, minor, minor_num, atn_ck_2, variant, Last.Question.Seen)

# filter completed surveys only, re-code variables, construct NEP
clean1 <- clean %>% filter((status==1) | (Last.Question.Seen=="Thank you")) %>% mutate (
  n.score = case_when(
    score == "8.5" ~ 8.5,
    score == "7.5" ~ 7.5,
    score == "9.5" ~ 9.5,
    .default = 0),
  n.distance = case_when(
    distance == "1 mile" ~ 1,
    distance == "2 miles" ~ 2,
    distance == "0.5 mile" ~ 0.5,
    distance == "5 miles" ~ 5,
    .default = 0),
  f.sus_lvl = factor(sus_lvl, levels=c("None", "Level 1", "Level 2", "Level 3", "None of the options")),
  n.nightly_price = case_when(
    nightly_price =="138" ~ 138,
    nightly_price == "120" ~ 120,
    nightly_price == "124" ~ 124,
    nightly_price == "141" ~ 141,
    nightly_price == "168" ~ 168,
    nightly_price == "187" ~ 187,
    nightly_price == "201" ~ 201,
    nightly_price == "225" ~ 225,
    nightly_price == "254" ~ 254,
    nightly_price == "257" ~ 257,
    nightly_price == "269" ~ 269,
    nightly_price == "282" ~ 282,
    .default = 0),
  n.birth_year = case_when(
    is.na(birth_year) ~ NA,
    birth_year == "09/19/1986" ~ 1986,
    birth_year == "1111" ~ NA,
    .default = as.numeric(birth_year)),
  f.travel_freq = factor(travel_freq, levels=c("once", "2 - 3 times", "4 - 5 times", "6 - 10 times", "10+ times", "Prefer not to answer"),
                         labels=c("1", "2-3", "4-5", "6-10", "10+", NA)),
  amenities = ifelse(amenities==99, NA, amenities), 
  price = ifelse(price==99, NA, price), 
  guest_rev = ifelse(guest_rev==99, NA, guest_rev), 
  proximity = ifelse(proximity==99, NA, proximity), 
  hotel_star = ifelse(hotel_star==99, NA, hotel_star), 
  sust_level = ifelse(sust_level==99, NA, sust_level),
  sust_filter = ifelse(sust_filter==-1, 0, sust_filter),  # group together No/Doesn't matter
  waste = ifelse(waste==99, NA, waste), 
  energy = ifelse(energy==99, NA, energy), 
  water = ifelse(water==99, NA, water), 
  local = ifelse(local==99, NA, local), 
  biodiversity = ifelse(biodiversity==99, NA, biodiversity),
  q1 = ifelse(`1`==99, NA, `1` + 3),
  q2 = ifelse(`2`==99, NA, 6-(`2` + 3)),
  q3 = ifelse(`3`==99, NA, `3` + 3),
  q4 = ifelse(`4`==99, NA, 6-(`4`+ 3)),
  q5 = ifelse(`5`==99, NA, `5`+ 3),   #corrected to reflect pro-NEP
  q6 = ifelse(`6`==99, NA, 6-(`6` + 3)),
  q7 = ifelse(`7`==99, NA, `7` + 3),
  q8 = ifelse(`8`==99, NA, 6-(`8`+ 3)),
  q9 = ifelse(`9`==99, NA, `9` + 3),
  q10 = ifelse(`10`==99, NA, 6-(`10`+ 3)),
  q11 = ifelse(`11`==99, NA, `11` + 3),
  q12 = ifelse(`12`==99, NA, 6-(`12`+ 3)),
  q13 = ifelse(`13`==99, NA, `13` + 3),
  q14 = ifelse(`14`==99, NA, 6-(`14`+ 3)),
  q15 = ifelse(`15`==99, NA, `15` + 3),
  bachelors = (!is.na(degree) & degree %in% c("Bachelor’s degree in college (4-year)", "Graduate degree"))*1,
  incgt100k = (!is.na(income) & income %in% c("$100,000 to $149,999", "$150,000 or more"))*1,
  n.age = 2023-n.birth_year
  ) %>% rowwise () %>%
  mutate(
    NEP.mean = mean(c_across(q1:q15),na.rm=T),
    NEP.sum = sum(c_across(q1:q15),na.rm=T),
    NEP.na = sum(is.na(c_across(q1:q15)))    
  ) %>% ungroup()

# Create ID variables to be used later in mlogit
  clean2 <- clean1 %>% group_by(variant, id) %>% mutate(nid=cur_group_id()) %>% ungroup() %>% group_by(variant, id, task) %>% mutate(chid=cur_group_id()) %>% ungroup()

  #clean2 %>% group_by(id) %>% filter(row_number()==1) %>% ungroup() %>% select(q1:q15) %>% summarize_all(mean, na.rm=T)
  #clean2 %>% group_by(f.sus_lvl) %>% summarize(mkt_shr = sum(choice)/n())

  save.image("hotel_esg.RData")

  
  # Generate TABLE 2 - Compute Summary Statistics ----
  sumstats <- clean2 %>% 
    select(nid, variant, f.travel_freq, amenities:sust_level, sust_filter:biodiversity, male, white, black, asian, 
           hispanic, bachelors, full_time, incgt100k, married, minor, n.age, NEP.sum, NEP.mean, NEP.na) %>%
    group_by(nid) %>% filter(row_number()==1)
  
  #quantile(sumstats$NEP.sum, probs=c(0.25, 0.50, 0.75,1), type=1)
  #quantile(sumstats$NEP.mean, probs=c(0.25, 0.50, 0.75, 1))
  #ggplot(sumstats, aes(x=NEP.sum)) + geom_bar()
  
  sumstat_long <- sumstats %>% select(-f.travel_freq) %>% pivot_longer(amenities:NEP.na, names_to="varname", values_to="values") 
  #  pivot_wider(names_from="variant", values_from = "values")
  
  sumstat.sum <- sumstat_long %>% group_by(variant,varname) %>% 
    summarize(count = n(), meanval = mean(values, na.rm=T), sd=sd(values, na.rm=T))
  
  sum.total <- sumstat_long %>% group_by(varname) %>% 
    summarize(count = n(), meanval = mean(values, na.rm=T), sd=sd(values, na.rm=T)) %>%
    mutate(variant="total")
  
  sum.total <- rbind(sumstat.sum, sum.total)
  
  # TABLE 2
  write.csv(sum.total, "./results/table2.csv", row.names=F)
  
### Generate FIGURE 2 ----
# Generate simple histograms for variables of interest: 
keep1 <- c("amenities", "price", "guest_rev", "proximity", "hotel_star", "sust_level", "waste", "energy", "water", "local", "biodiversity")
keep2 <- c("sust_filter", "book_directly")

sum.hist <- sumstat_long %>% filter(varname %in% c(keep1, keep2) & !is.na(values)) %>% group_by(variant, varname, values) %>% summarize(count=n()) %>% 
  ungroup() %>% mutate(prop = count/sum(count), .by=c(variant, varname))
# Keep 1 variables
sum.hist1 <- sum.hist %>% filter(varname %in% keep1) %>% mutate(
  varname = factor(varname, levels=keep1, labels=c("Amenities", "Price", "Guest review scores", "Proximity to destination", "Hotel star rating",
                                                   "Sustainability initiatives", "Waste reduction", "Energy conservation", "Water conservation",
                                                   "Support local communities", "Protect biodiversity")),
  values = factor(values, levels=1:5, labels=c("Not at all", "Slightly", "Moderately", "Very", "Extremely")),
  variant = factor(variant, levels=c("economy", "midscale", "upscale"), labels=c("Economy", "Midscale", "Upscale"))
)
# Keep 2 variables - NOT USED
sum.hist2 <- sum.hist %>% filter(varname %in% keep2) %>% mutate(
  varname = factor(varname, levels=keep2, labels=c("Sustainability filter", "Book directly")),
  values = factor(values, levels=c(1,0), labels=c("Yes", "No / Doesn't matter")),
  variant = factor(variant, levels=c("economy", "midscale", "upscale"), labels=c("Economy", "Midscale", "Upscale"))
)
# Figure with Keep 1 variables
fig2a <- ggplot(sum.hist1, aes(x=values, y=prop, fill=variant)) +
  geom_bar(stat="identity", position="dodge2", width=0.85) +
  facet_wrap(~varname, ncol=3, scales = "free_y") +  # "free_y"
  labs(x="Importance", y="Percentage", fill="Segment") +
  scale_y_continuous(breaks=seq(0,1,0.1), labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="bottom")
# Figure with Keep 2 variables - NOT USED
fig2b <- ggplot(sum.hist2, aes(x=values, y=prop, fill=variant)) +
  geom_bar(stat="identity", position="dodge2", width=0.85) +
  facet_wrap(~varname, ncol=3, scales = "free_y") +  # "free_y"
  labs(x="Response", y="Percentage", fill="Segment") +
  scale_y_continuous(breaks=seq(0,1,0.1), labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(legend.position="bottom")

## CONSTRUCT DATA FOR BASIC MULTINOMIAL LOGIT WITH OPT-OUT OPTION DROPPED ## ----
  ml.ex <- clean2 %>% filter(alt != 4) %>% mutate(f.sus_lvl = factor(f.sus_lvl, levels=c("None", "Level 1", "Level 2", "Level 3")))


## Multinomial Logit Model ##  
low_ml_model <- multinom(choice ~ n.score + n.distance + f.sus_lvl + n.nightly_price, data = ml.ex, subset=(variant=="economy"), quiet = true)
summary(low_ml_model)
med_ml_model <- multinom(choice ~ n.score + n.distance + f.sus_lvl + n.nightly_price, data = ml.ex, subset=(variant=="midscale"), quiet = true)
summary(med_ml_model)
high_ml_model <- multinom(choice ~ n.score + n.distance + f.sus_lvl + n.nightly_price, data = ml.ex, subset=(variant=="upscale"), quiet = true)
summary(high_ml_model)

#Generate TABLE 3 ----
wordreg(list(low_ml_model, med_ml_model, high_ml_model), file="./results/table3.docx", custom.model.names = c("Economy", "Mid-Scale", "Upscale"), digits = 3,
        custom.coef.map = list("n.score"= "Guest Review Score", "n.distance"= "Proximity to Dest. (mi)", "f.sus_lvlLevel 1" = "Sustainability Level 1",
                               "f.sus_lvlLevel 2" = "Sustainability Level 2", "f.sus_lvlLevel 3" = "Sustainability Level 3", "n.nightly_price" = "Price per Night ($USD)"))  

# compute the marginal effects 
avg_slopes(low_ml_model)
avg_slopes(med_ml_model)
avg_slopes(high_ml_model)

mfx.low <- slopes(low_ml_model)
mfx.med <- slopes(med_ml_model)
mfx.high <- slopes(high_ml_model)

# Generate FIGURE 3 - plot the marginal effects ----
fig3_low <- plot_slopes(low_ml_model, type="probs", variables = "f.sus_lvl", condition="n.nightly_price") + theme_minimal() + scale_y_continuous(breaks=seq(0,0.4,0.05), limits=c(0.025,0.4)) +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), 
        title = element_text(size=10), legend.text = element_text(size=15),
        panel.spacing = unit(0.5, "cm"),  strip.text.y = element_text(size=20)) +
  labs(title = "Economy",
       x = "",
       y = "Marginal Effects")
fig3_low

fig3_med <- plot_slopes(med_ml_model, type="probs", variables = "f.sus_lvl", condition="n.nightly_price") + theme_minimal() + scale_y_continuous(breaks=seq(0,0.4,0.05), limits=c(0.025,0.4)) +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), 
        title = element_text(size=10), legend.text = element_text(size=15),
        panel.spacing = unit(0.5, "cm"),  strip.text.y = element_text(size=20)) +
  labs(title = "Midscale",
       x = "",
       y = "Marginal Effects")
fig3_med

fig3_high <- plot_slopes(high_ml_model, type="probs", variables = "f.sus_lvl", condition="n.nightly_price") + theme_minimal() + scale_y_continuous(breaks=seq(0,0.4,0.05), limits=c(0.025,0.4)) +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), 
        title = element_text(size=10), legend.text = element_text(size=15),
        panel.spacing = unit(0.5, "cm"),  strip.text.y = element_text(size=20)) +
  labs(title = "Upscale",
       x = "Price per Night ($USD)",
       y = "Marginal Effects")
fig3_high

#plot_comparisons(low_ml_model, type="probs", variables = "n.nightly_price", by = "f.sus_lvl")
#plot_slopes(low_ml_model, type="probs", variables = "n.nightly_price", by = "f.sus_lvl")

## CREATE DATA FOR MLOGIT PACKAGE (Random Parameters) ## ----
rpl.df <- clean2 %>% select(nid, chid, task, alt, choice, n.distance, n.score, f.sus_lvl, n.nightly_price, variant,
                            travel_freq, amenities, price, guest_rev, proximity, hotel_star, sust_level,
                            sust_filter, book_directly, waste, energy, water, local, biodiversity, n.birth_year, 
                            n.age, male, white, middle_east, black, native, asian, islander, hispanic, bachelors, 
                            full_time, incgt100k, married, minor,  NEP.sum, NEP.mean, NEP.na) %>%
  dfidx(idx=list(c("chid", "nid"), "alt"), choice="choice", drop.index=F)

## Mixed Logit Model (Random Parameters) ##
set.seed(37126)
low_rpl_model <- mlogit(choice ~ n.score + n.distance + f.sus_lvl + n.nightly_price | 0, data=rpl.df, subset=(variant=="economy"),
                       rpar = c(n.score = 'n', n.distance = 'n', `f.sus_lvlLevel 1` = 'n', `f.sus_lvlLevel 2` = 'n', `f.sus_lvlLevel 3` = 'n'),
                       R=100, halton=NA, panel=T)
summary(low_rpl_model)

set.seed(37126)
med_rpl_model <- mlogit(choice ~ n.score + n.distance + f.sus_lvl + n.nightly_price | 0, data=rpl.df, subset=(variant=="midscale"),
                        rpar = c(n.score = 'n', n.distance = 'n', `f.sus_lvlLevel 1` = 'n', `f.sus_lvlLevel 2` = 'n', `f.sus_lvlLevel 3` = 'n'),
                        R=100, halton=NA, panel=T)
summary(med_rpl_model)

set.seed(37126)
high_rpl_model <- mlogit(choice ~ n.score + n.distance + f.sus_lvl + n.nightly_price | 0, data=rpl.df, subset=(variant=="upscale"),
                        rpar = c(n.score = 'n', n.distance = 'n', `f.sus_lvlLevel 1` = 'n', `f.sus_lvlLevel 2` = 'n', `f.sus_lvlLevel 3` = 'n'),
                        R=100, halton=NA, panel=T)
summary(high_rpl_model)

#Generate TABLE 4 ----
wordreg(list(low_rpl_model, med_rpl_model, high_rpl_model), file="./results/table4.docx", custom.model.names = c("Economy", "Mid-Scale", "Upscale"), digits = 3,
        custom.coef.map = list("n.score"= "Guest Review Score", "n.distance"= "Proximity to Dest. (mi)", "f.sus_lvlLevel 1" = "Sustainability Level 1",
                               "f.sus_lvlLevel 2" = "Sustainability Level 2", "f.sus_lvlLevel 3" = "Sustainability Level 3", "n.nightly_price" = "Price per Night ($USD)",
                               "sd.n.score" = "SD Guest Rev. Score", "sd.n.distance" = "SD Proximity to Dest.", "sd.f.sus_lvlLevel 1" = "SD Sust. Level 1",
                               "sd.f.sus_lvlLevel 2" = "SD Sust. Level 2", "sd.f.sus_lvlLevel 3" = "SD Sust. Level 3"))  

## Calculate Marginal WTP ----
# 
# Functions for dealing with parameter uncertainty ----

# Returns multivariate normal draws of the coefficients of a model estimated
# using the mlogit package
getCoefDraws = function(model, numDraws=10^5) {
  coefs      = coef(model)
  hessian    = as.matrix(model$hessian)
  covariance = -1*(solve(hessian))
  draws      = as.data.frame(mvrnorm(numDraws, coefs, covariance))
  return(draws)
}

# Returns a confidence interval from a vector of values
getCI = function(df, alpha=0.025) {
  df = data.frame(
    estimate  = apply(df, 2, mean, na.rm=T),
    sd = apply(df, 2, sd, na.rm=T),
    lcl = apply(df, 2, function(x) {quantile(x, alpha, na.rm=T)}),
    ucl = apply(df, 2, function(x) {quantile(x, 1-alpha, na.rm=T)}))
  return(df)
}

# Wrap up the functions to be used together
get_WTP = function(model_list, index) {
  # Compute the WTP with uncertainty
  # Generate draws of coefficients:
  coef_draw.list <- lapply(model_list, getCoefDraws, numDraws = 10^5)
  #head(coef_draws)
  # Compute WTP for each coefficient draw:
  wtp_draw.list <- lapply(coef_draw.list, function(x) {-1*(x[,index] / x[,7])})
  #head(wtp_draws)
  # For each coefficient, get the mean and 95% confidence interval of WTP:
  wtpCI.list <- lapply(wtp_draw.list, getCI)
  df <- rbindlist(wtpCI.list) %>% mutate(varname=rep(row.names(wtpCI.list[[1]]),length(model_list)),
                                         variant=rep(names(model_list), each=nrow(wtpCI.list[[1]])))
  return(df)
}

## 
# Compute marginal WTP estimates ----
set.seed(4239)
wtp_total <- get_WTP(list(Economy=low_rpl_model, Midscale=med_rpl_model, Upscale=high_rpl_model), index=(1:5)) %>% mutate(
 varname = case_when(
    varname == "n.score" ~ "Guest Rev. Score",
    varname == "n.distance" ~ "Proximity to Dest.",
    varname == "f.sus_lvlLevel 1" ~ "Sust. Level 1",
    varname == "f.sus_lvlLevel 2" ~ "Sust. Level 2",
    varname == "f.sus_lvlLevel 3" ~ "Sust. Level 3",
    .default = NA)
)
  

## Generate FIGURE 4 - GRAPH WTP BY VARIANT AND ATTRIBUTE ----
# Create the side-by-side stacked bar chart with mean total tipping amount on the y-axis

fig4 <- ggplot(wtp_total, aes(x = varname, y = estimate, fill = varname)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks=seq(-25,55,5)) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.2, color="gray30", alpha=1) +
  labs(title = "",
       x = "Group",
       y = "Marginal WTP",
       fill = "Attribute") +
  geom_text(aes(label = after_stat(y), group = variant, fontface="bold"), 
            stat = 'summary', fun = function(x) round(x,2), vjust = 1.5, size=2, alpha=1) +
  #    facet_wrap(~f.accttype, nrow = 1, scales="free_x") + 
  facet_grid(rows = vars(variant), scales = "free_x", space = "free_x") +
  #  geom_text(aes(y = pct_loc, label = pct, fontface="bold"), colour = "white", alpha=1) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")),
         alpha = guide_legend(override.aes = list(pattern="none"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), 
        title = element_text(size=12), legend.text = element_text(size=6),
        panel.spacing = unit(0.5, "cm"),  strip.text.y = element_text(size=10), 
        legend.position="bottom", legend.title = element_text(size = 8)) +
  coord_flip()
fig4


## COMPUTE INDIVIDUAL WTP VALUES FROM RANDOM PARAMETERS LOGIT MODELS ----

# Helper function to get independent parameters
getIndpars <- function(model,variant) {
  indpar <- fitted(model, type = "parameters")
  # head(indpar)
  indpar %<>% mutate(
    wtp_1 = -1 * f.sus_lvlLevel.1 / coef(model)[7],
    wtp_2 = -1 * f.sus_lvlLevel.2 / coef(model)[7],
    wtp_3 = -1 * f.sus_lvlLevel.3 / coef(model)[7],
    variant = variant
    )
  # quantile(indpar$wtp, c(0.025, 0.975))
}

indpars <- rbindlist(list(getIndpars(low_rpl_model, "economy"), getIndpars(med_rpl_model, "midscale"), getIndpars(high_rpl_model, "upscale")))
indpars %>% group_by(variant) %>% summarize(l1=mean(wtp_1), l2=mean(wtp_2), l3=mean(wtp_3))

# Merge independent parameters with demographic & behavioral vars
indpars %<>% dplyr::inner_join(sumstats, by=c("id" = "nid", "variant"))

# Calculate NEP quartiles
# Helper cut function
cutD <- function(x,n) {
  cut(x, breaks=c(quantile(x, probs = seq(0, 1, by = 1/n), na.rm = T)), labels=c(1:n), 
      include.lowest=TRUE)
}

indpars %<>% dplyr::mutate(NEP.q = cutD(NEP.mean, 4), age.q = cutD(n.age, 4))

## Generate APPENDIX B FIGURES  ----
# Plot individual WTP distribution by variant and demographics
# Pivot longer first
indpars_long <- indpars %>% pivot_longer(wtp_1:wtp_3, names_to="sus_lvl", values_to="estimate") %>% mutate(
  sus_lvl = factor(sus_lvl, levels=c("wtp_1", "wtp_2", "wtp_3"), labels=c("Sust. Level 1", "Sust. Level 2", "Sust. Level 3")),
  variant = factor(variant, levels=c("economy", "midscale", "upscale"), labels=c("Economy", "Midscale", "Upscale"))
  )

# Sustainability filter 
plotdata <- indpars_long %>% filter(!is.na(sust_filter))
fig5a <- ggplot(data=plotdata, aes(x=estimate)) +
  geom_density(aes(fill=factor(sust_filter), colour=factor(sust_filter)), alpha=0.35) + 
  facet_grid(~sus_lvl, scale="free") +
  labs(x="Marginal WTP", y="Density", fill="Sust. Filter", colour="Sust. Filter") +
  theme_minimal() +   
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size=0), axis.title.y = element_text(size=0), legend.text = element_text(size=8),
        panel.spacing = unit(0.2, "cm"),  strip.text.x = element_text(size=0), 
        legend.position = c(0,1), legend.justification = c(0,1), 
        legend.title = element_text(size = 8), legend.key.size = unit(0.3, 'cm'))
fig5a

# Travel_frequency filter 
plotdata <- indpars_long %>% filter(f.travel_freq %in% levels(f.travel_freq)[1:5]) %>% mutate(f.travel_freq = droplevels(f.travel_freq))
fig5b <- ggplot(data=plotdata, aes(x=estimate)) +
  geom_density(aes(fill=factor(f.travel_freq), colour=f.travel_freq), alpha=0.2) + 
  facet_grid(~sus_lvl, scale="free") +
  labs(x="Marginal WTP", y="Density", fill="Travel Freq", colour="Travel Freq") +
  theme_minimal() +   
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size=0), axis.title.y = element_text(size=0), legend.text = element_text(size=8),
        panel.spacing = unit(0.2, "cm"),  strip.text.x = element_text(size=0), 
        legend.position = c(0,1), legend.justification = c(0,1), 
        legend.title = element_text(size = 8), legend.key.size = unit(0.3, 'cm'))
fig5b

# Book directly filter 
plotdata <- indpars_long %>% filter(!is.na(book_directly))
fig5c <- ggplot(data=plotdata, aes(x=estimate)) +
  geom_density(aes(fill=factor(book_directly), colour=factor(book_directly)), alpha=0.35) + 
  facet_grid(~sus_lvl, scale="free") +
  labs(x="Marginal WTP", y="Density", fill="Book Directly", colour="Book Directly") +
  theme_minimal() +   
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size=9), axis.title.y = element_text(size=0), legend.text = element_text(size=8),
        panel.spacing = unit(0.2, "cm"),  strip.text.x = element_text(size=0), 
        legend.position = c(0,1), legend.justification = c(0,1), 
        legend.title = element_text(size = 8), legend.key.size = unit(0.3, 'cm'))
fig5c

# Male 
plotdata <- indpars_long %>% filter(!is.na(male))
fig5d <- ggplot(data=plotdata, aes(x=estimate)) +
  geom_density(aes(fill=factor(male), colour=factor(male)), alpha=0.35) + 
  facet_grid(~sus_lvl, scale="free") +
  labs(x="Marginal WTP", y="Density", fill="Male", colour="Male") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size=0), axis.title.y = element_text(size=0), legend.text = element_text(size=8),
        panel.spacing = unit(0.2, "cm"),  strip.text.x = element_text(size=10), 
        legend.position = c(0,1), legend.justification = c(0,1), 
        legend.title = element_text(size = 8), legend.key.size = unit(0.3, 'cm'))
fig5d

# White
plotdata <- indpars_long %>% filter(!is.na(white))
fig5e <- ggplot(data=plotdata, aes(x=estimate)) +
  geom_density(aes(fill=factor(white), colour=factor(white)), alpha=0.35) + 
  facet_grid(~sus_lvl, scale="free") +
  labs(x="Marginal WTP", y="Density", fill="White", colour="White") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size=0), axis.title.y = element_text(size=0), legend.text = element_text(size=8),
        panel.spacing = unit(0.2, "cm"),  strip.text.x = element_text(size=10), 
        legend.position = c(0,1), legend.justification = c(0,1), 
        legend.title = element_text(size = 8), legend.key.size = unit(0.3, 'cm'))
fig5e

# Black
plotdata <- indpars_long %>% filter(!is.na(black))
fig5f <- ggplot(data=plotdata, aes(x=estimate)) +
  geom_density(aes(fill=factor(black), colour=factor(black)), alpha=0.35) + 
  facet_grid(~sus_lvl, scale="free") +
  labs(x="Marginal WTP", y="Density", fill="Black", colour="Black") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size=0), axis.title.y = element_text(size=0), legend.text = element_text(size=8),
        panel.spacing = unit(0.2, "cm"),  strip.text.x = element_text(size=0), 
        legend.position = c(0,1), legend.justification = c(0,1), 
        legend.title = element_text(size = 8), legend.key.size = unit(0.3, 'cm'))
fig5f

# Asian
plotdata <- indpars_long %>% filter(!is.na(asian))
fig5g <- ggplot(data=plotdata, aes(x=estimate)) +
  geom_density(aes(fill=factor(asian), colour=factor(asian)), alpha=0.35) + 
  facet_grid(~sus_lvl, scale="free") +
  labs(x="Marginal WTP", y="Density", fill="Asian", colour="Asian") +
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size=0), axis.title.y = element_text(size=0), legend.text = element_text(size=8),
        panel.spacing = unit(0.2, "cm"),  strip.text.x = element_text(size=0), 
        legend.position = c(0,1), legend.justification = c(0,1), 
        legend.title = element_text(size = 8), legend.key.size = unit(0.3, 'cm'))
fig5g

# Hispanic
plotdata <- indpars_long %>% filter(!is.na(hispanic))
fig5h <- ggplot(data=plotdata, aes(x=estimate)) +
  geom_density(aes(fill=factor(hispanic), colour=factor(hispanic)), alpha=0.35) + 
  facet_grid(~sus_lvl, scale="free") +
  labs(x="Marginal WTP", y="Density", fill="Hispanic", colour="Hispanic") +
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size=0), axis.title.y = element_text(size=0), legend.text = element_text(size=8),
        panel.spacing = unit(0.2, "cm"),  strip.text.x = element_text(size=0), 
        legend.position = c(0,1), legend.justification = c(0,1), 
        legend.title = element_text(size = 8), legend.key.size = unit(0.3, 'cm'))
fig5h

# Bachelor's degree or higher
plotdata <- indpars_long %>% filter(!is.na(bachelors))
fig5i <- ggplot(data=plotdata, aes(x=estimate)) +
  geom_density(aes(fill=factor(bachelors), colour=factor(bachelors)), alpha=0.35) + 
  facet_grid(~sus_lvl, scale="free") +
  labs(x="Marginal WTP", y="Density", fill="Bachelor's", colour="Bachelor's") +
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size=0), axis.title.y = element_text(size=0), legend.text = element_text(size=8),
        panel.spacing = unit(0.2, "cm"),  strip.text.x = element_text(size=0), 
        legend.position = c(0,1), legend.justification = c(0,1), 
        legend.title = element_text(size = 8), legend.key.size = unit(0.3, 'cm'))
fig5i

# Full time employment
plotdata <- indpars_long %>% filter(!is.na(full_time))
fig5j <- ggplot(data=plotdata, aes(x=estimate)) +
  geom_density(aes(fill=factor(full_time), colour=factor(full_time)), alpha=0.35) + 
  facet_grid(~sus_lvl, scale="free") +
  labs(x="Marginal WTP", y="Density", fill="Full time", colour="Full time") +
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size=0), axis.title.y = element_text(size=0), legend.text = element_text(size=8),
        panel.spacing = unit(0.2, "cm"),  strip.text.x = element_text(size=0), 
        legend.position = c(0,1), legend.justification = c(0,1), 
        legend.title = element_text(size = 8), legend.key.size = unit(0.3, 'cm'))
fig5j

# Income greater than $100k
plotdata <- indpars_long %>% filter(!is.na(incgt100k))
fig5k <- ggplot(data=plotdata, aes(x=estimate)) +
  geom_density(aes(fill=factor(incgt100k), colour=factor(incgt100k)), alpha=0.35) + 
  facet_grid(~sus_lvl, scale="free") +
  labs(x="Marginal WTP", y="Density", fill="Income > $100K", colour="Income > $100K") +
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size=0), axis.title.y = element_text(size=0), legend.text = element_text(size=8),
        panel.spacing = unit(0.2, "cm"),  strip.text.x = element_text(size=0), 
        legend.position = c(0,1), legend.justification = c(0,1), 
        legend.title = element_text(size = 8), legend.key.size = unit(0.3, 'cm'))
fig5k

# Married
plotdata <- indpars_long %>% filter(!is.na(married))
fig5l <- ggplot(data=plotdata, aes(x=estimate)) +
  geom_density(aes(fill=factor(married), colour=factor(married)), alpha=0.35) + 
  facet_grid(~sus_lvl, scale="free") +
  labs(x="Marginal WTP", y="Density", fill="Married", colour="Married") +
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size=0), axis.title.y = element_text(size=0), legend.text = element_text(size=8),
        panel.spacing = unit(0.2, "cm"),  strip.text.x = element_text(size=0), 
        legend.position = c(0,1), legend.justification = c(0,1), 
        legend.title = element_text(size = 8), legend.key.size = unit(0.3, 'cm'))
fig5l

# Minor in HH (children)
plotdata <- indpars_long %>% filter(!is.na(minor))
fig5m <- ggplot(data=plotdata, aes(x=estimate)) +
  geom_density(aes(fill=factor(minor), colour=factor(minor)), alpha=0.35) + 
  facet_grid(~sus_lvl, scale="free") +
  labs(x="Marginal WTP", y="Density", fill="Minor in HH", colour="Minor in HH") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size=9), axis.title.y = element_text(size=0), legend.text = element_text(size=8),
        panel.spacing = unit(0.2, "cm"),  strip.text.x = element_text(size=0), 
        legend.position = c(0,1), legend.justification = c(0,1), 
        legend.title = element_text(size = 8), legend.key.size = unit(0.3, 'cm'))
fig5m

# Age quartile
plotdata <- indpars_long %>% filter(!is.na(age.q))
fig5n <- ggplot(data=plotdata, aes(x=estimate)) +
  geom_density(aes(fill=age.q, colour=age.q), alpha=0.2) + 
  facet_grid(~sus_lvl, scale="free") +
  labs(x="Marginal WTP", y="Density", fill="Age Quartile", colour="Age Quartile") +
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size=0), axis.title.y = element_text(size=0), legend.text = element_text(size=8),
        panel.spacing = unit(0.2, "cm"),  strip.text.x = element_text(size=0), 
        legend.position = c(0,1), legend.justification = c(0,1), 
        legend.title = element_text(size = 8), legend.key.size = unit(0.3, 'cm'))
fig5n

# NEP quartile
plotdata <- indpars_long %>% filter(!is.na(NEP.q))
fig5o <- ggplot(data=plotdata, aes(x=estimate)) +
  geom_density(aes(fill=NEP.q, colour=NEP.q), alpha=0.2) + 
  facet_grid(~sus_lvl, scale="free") +
  labs(x="Marginal WTP", y="Density", fill="NEP Quartile", colour="NEP Quartile") +
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size=9), axis.title.y = element_text(size=0), legend.text = element_text(size=8),
        panel.spacing = unit(0.2, "cm"),  strip.text.x = element_text(size=0), 
        legend.position = c(0,1), legend.justification = c(0,1), 
        legend.title = element_text(size = 8), legend.key.size = unit(0.3, 'cm'))
fig5o


figure5.1 <- ggarrange(fig5a, fig5b, fig5c, fig5o, ncol=2, nrow=2)
figure5.2 <- ggarrange(fig5d, fig5e, 
                       #fig5f, fig5g, fig5h, 
                       fig5i, 
                       #fig5j, 
                       fig5k, fig5l, 
                       #fig5m, 
                       fig5n, ncol=2, nrow=3)

figure5 <- ggarrange(fig5d, fig5e,fig5i, fig5k, fig5l, fig5n, fig5a, fig5b, fig5c, fig5o, ncol=2, nrow=5)


## GENERATE RESULTS FOR TABLE 5 ----
## Estimate MIXL with interaction effects for individual demographics, behaviors, perceptions ####
## Interactions with: male, white, bachelors, incgt100k, married, n.age, sust_filter, book_directly, NEP.mean ##

## CREATE DATAFRAME WITH SUST. LEVEL DUMMIES AND "None of these options" (alt 4) INCLUDED AS SEPARATE DUMMY ##
intxn.data <- rpl.df %>% mutate(
  s1=(f.sus_lvl=="Level 1")*1,
  s2=(f.sus_lvl=="Level 2")*1,
  s3=(f.sus_lvl=="Level 3")*1,
  no_alt = ifelse(alt==4, 1, 0))

# MALE
set.seed(1833)
l_male <- mlogit(choice ~ n.score + n.distance + s1 + s2 + s3 + no_alt + s1:male + s2:male + s3:male + n.nightly_price | 0, data=intxn.data, subset=(variant=="economy"),
                        rpar = c(n.score = 'n', n.distance = 'n', s1 = 'n', s2 = 'n', s3 = 'n'),
                        R=100, halton=NA, panel=T)
set.seed(1833)
m_male <- mlogit(choice ~ n.score + n.distance + s1 + s2 + s3 + no_alt + s1:male + s2:male + s3:male + n.nightly_price | 0, data=intxn.data, subset=(variant=="midscale"),
                 rpar = c(n.score = 'n', n.distance = 'n', s1 = 'n', s2 = 'n', s3 = 'n'),
                 R=100, halton=NA, panel=T)
set.seed(1833)
h_male <- mlogit(choice ~ n.score + n.distance + s1 + s2 + s3 + no_alt + s1:male + s2:male + s3:male + n.nightly_price | 0, data=intxn.data, subset=(variant=="upscale"),
                 rpar = c(n.score = 'n', n.distance = 'n', s1 = 'n', s2 = 'n', s3 = 'n'),
                 R=100, halton=NA, panel=T)

#Generate Supplementary Tables with raw coefficients ----
wordreg(list(l_male, m_male, h_male), file="./results/app/male.docx", custom.model.names = c("Economy", "Mid-Scale", "Upscale"), digits = 3,
        custom.coef.map = list("n.score"= "Guest Review Score", "n.distance"= "Proximity to Dest. (mi)", "s1" = "Sust. Level 1",
                               "s2" = "Sust. Level 2", "s3" = "Sust. Level 3", "n.nightly_price" = "Price per Night ($USD)",
                               "s1:male" = "Sust. Level 1 * male", "s2:male" = "Sust. Level 2 * male", "s3:male" = "Sust. Level 3 * male", 
                               "sd.n.score" = "SD Guest Rev. Score", "sd.n.distance" = "SD Proximity to Dest.", "sd.s1" = "SD Sust. Level 1",
                               "sd.s2" = "SD Sust. Level 2", "sd.s3" = "SD Sust. Level 3"))  

# Compute marginal WTP estimates ----

set.seed(4239)
wtp_male <- get_WTP(list(Economy=l_male, Midscale=m_male, Upscale=h_male), index=(8:10)) %>% mutate(
  varname = case_when(
    varname == "s1:male" ~ "Sust. Level 1 * male",
    varname == "s2:male" ~ "Sust. Level 2 * male",
    varname == "s3:male" ~ "Sust. Level 3 * male",
    .default = NA)
)

# WHITE
set.seed(1833)
l_white <- mlogit(choice ~ n.score + n.distance + s1 + s2 + s3 + no_alt + s1:white + s2:white + s3:white + n.nightly_price | 0, data=intxn.data, subset=(variant=="economy"),
                 rpar = c(n.score = 'n', n.distance = 'n', s1 = 'n', s2 = 'n', s3 = 'n'),
                 R=100, halton=NA, panel=T)
set.seed(1833)
m_white <- mlogit(choice ~ n.score + n.distance + s1 + s2 + s3 + no_alt + s1:white + s2:white + s3:white + n.nightly_price | 0, data=intxn.data, subset=(variant=="midscale"),
                 rpar = c(n.score = 'n', n.distance = 'n', s1 = 'n', s2 = 'n', s3 = 'n'),
                 R=100, halton=NA, panel=T)
set.seed(1833)
h_white <- mlogit(choice ~ n.score + n.distance + s1 + s2 + s3 + no_alt + s1:white + s2:white + s3:white + n.nightly_price | 0, data=intxn.data, subset=(variant=="upscale"),
                 rpar = c(n.score = 'n', n.distance = 'n', s1 = 'n', s2 = 'n', s3 = 'n'),
                 R=100, halton=NA, panel=T)

#Generate Supplementary Tables with raw coefficients ----
wordreg(list(l_white, m_white, h_white), file="./results/app/white.docx", custom.model.names = c("Economy", "Mid-Scale", "Upscale"), digits = 3,
        custom.coef.map = list("n.score"= "Guest Review Score", "n.distance"= "Proximity to Dest. (mi)", "s1" = "Sust. Level 1",
                               "s2" = "Sust. Level 2", "s3" = "Sust. Level 3", "n.nightly_price" = "Price per Night ($USD)",
                               "s1:white" = "Sust. Level 1 * white", "s2:white" = "Sust. Level 2 * white", "s3:white" = "Sust. Level 3 * white", 
                               "sd.n.score" = "SD Guest Rev. Score", "sd.n.distance" = "SD Proximity to Dest.", "sd.s1" = "SD Sust. Level 1",
                               "sd.s2" = "SD Sust. Level 2", "sd.s3" = "SD Sust. Level 3"))  

# Compute marginal WTP estimates ----

set.seed(4239)
wtp_white <- get_WTP(list(Economy=l_white, Midscale=m_white, Upscale=h_white), index=(8:10)) %>% mutate(
  varname = case_when(
    varname == "s1:white" ~ "Sust. Level 1 * white",
    varname == "s2:white" ~ "Sust. Level 2 * white",
    varname == "s3:white" ~ "Sust. Level 3 * white",
    .default = NA)
)

# BACHELORS
set.seed(1833)
l_bachelors <- mlogit(choice ~ n.score + n.distance + s1 + s2 + s3 + no_alt + s1:bachelors + s2:bachelors + s3:bachelors + n.nightly_price | 0, data=intxn.data, subset=(variant=="economy"),
                 rpar = c(n.score = 'n', n.distance = 'n', s1 = 'n', s2 = 'n', s3 = 'n'),
                 R=100, halton=NA, panel=T)
set.seed(1833)
m_bachelors <- mlogit(choice ~ n.score + n.distance + s1 + s2 + s3 + no_alt + s1:bachelors + s2:bachelors + s3:bachelors + n.nightly_price | 0, data=intxn.data, subset=(variant=="midscale"),
                 rpar = c(n.score = 'n', n.distance = 'n', s1 = 'n', s2 = 'n', s3 = 'n'),
                 R=100, halton=NA, panel=T)
set.seed(1833)
h_bachelors <- mlogit(choice ~ n.score + n.distance + s1 + s2 + s3 + no_alt + s1:bachelors + s2:bachelors + s3:bachelors + n.nightly_price | 0, data=intxn.data, subset=(variant=="upscale"),
                 rpar = c(n.score = 'n', n.distance = 'n', s1 = 'n', s2 = 'n', s3 = 'n'),
                 R=100, halton=NA, panel=T)

#Generate Supplementary Tables with raw coefficients ----
wordreg(list(l_bachelors, m_bachelors, h_bachelors), file="./results/app/bachelors.docx", custom.model.names = c("Economy", "Mid-Scale", "Upscale"), digits = 3,
        custom.coef.map = list("n.score"= "Guest Review Score", "n.distance"= "Proximity to Dest. (mi)", "s1" = "Sust. Level 1",
                               "s2" = "Sust. Level 2", "s3" = "Sust. Level 3", "n.nightly_price" = "Price per Night ($USD)",
                               "s1:bachelors" = "Sust. Level 1 * bachelors", "s2:bachelors" = "Sust. Level 2 * bachelors", "s3:bachelors" = "Sust. Level 3 * bachelors", 
                               "sd.n.score" = "SD Guest Rev. Score", "sd.n.distance" = "SD Proximity to Dest.", "sd.s1" = "SD Sust. Level 1",
                               "sd.s2" = "SD Sust. Level 2", "sd.s3" = "SD Sust. Level 3"))  

# Compute marginal WTP estimates ----

set.seed(4239)
wtp_bachelors <- get_WTP(list(Economy=l_bachelors, Midscale=m_bachelors, Upscale=h_bachelors), index=(8:10)) %>% mutate(
  varname = case_when(
    varname == "s1:bachelors" ~ "Sust. Level 1 * bachelors",
    varname == "s2:bachelors" ~ "Sust. Level 2 * bachelors",
    varname == "s3:bachelors" ~ "Sust. Level 3 * bachelors",
    .default = NA)
)

# INCGT100K
set.seed(1833)
l_incgt100k <- mlogit(choice ~ n.score + n.distance + s1 + s2 + s3 + no_alt + s1:incgt100k + s2:incgt100k + s3:incgt100k + n.nightly_price | 0, data=intxn.data, subset=(variant=="economy"),
                 rpar = c(n.score = 'n', n.distance = 'n', s1 = 'n', s2 = 'n', s3 = 'n'),
                 R=100, halton=NA, panel=T)
set.seed(1833)
m_incgt100k <- mlogit(choice ~ n.score + n.distance + s1 + s2 + s3 + no_alt + s1:incgt100k + s2:incgt100k + s3:incgt100k + n.nightly_price | 0, data=intxn.data, subset=(variant=="midscale"),
                 rpar = c(n.score = 'n', n.distance = 'n', s1 = 'n', s2 = 'n', s3 = 'n'),
                 R=100, halton=NA, panel=T)
set.seed(1833)
h_incgt100k <- mlogit(choice ~ n.score + n.distance + s1 + s2 + s3 + no_alt + s1:incgt100k + s2:incgt100k + s3:incgt100k + n.nightly_price | 0, data=intxn.data, subset=(variant=="upscale"),
                 rpar = c(n.score = 'n', n.distance = 'n', s1 = 'n', s2 = 'n', s3 = 'n'),
                 R=100, halton=NA, panel=T)

#Generate Supplementary Tables with raw coefficients ----
wordreg(list(l_incgt100k, m_incgt100k, h_incgt100k), file="./results/app/incgt100k.docx", custom.model.names = c("Economy", "Mid-Scale", "Upscale"), digits = 3,
        custom.coef.map = list("n.score"= "Guest Review Score", "n.distance"= "Proximity to Dest. (mi)", "s1" = "Sust. Level 1",
                               "s2" = "Sust. Level 2", "s3" = "Sust. Level 3", "n.nightly_price" = "Price per Night ($USD)",
                               "s1:incgt100k" = "Sust. Level 1 * incgt100k", "s2:incgt100k" = "Sust. Level 2 * incgt100k", "s3:incgt100k" = "Sust. Level 3 * incgt100k", 
                               "sd.n.score" = "SD Guest Rev. Score", "sd.n.distance" = "SD Proximity to Dest.", "sd.s1" = "SD Sust. Level 1",
                               "sd.s2" = "SD Sust. Level 2", "sd.s3" = "SD Sust. Level 3"))  

# Compute marginal WTP estimates ----

set.seed(4239)
wtp_incgt100k <- get_WTP(list(Economy=l_incgt100k, Midscale=m_incgt100k, Upscale=h_incgt100k), index=(8:10)) %>% mutate(
  varname = case_when(
    varname == "s1:incgt100k" ~ "Sust. Level 1 * incgt100k",
    varname == "s2:incgt100k" ~ "Sust. Level 2 * incgt100k",
    varname == "s3:incgt100k" ~ "Sust. Level 3 * incgt100k",
    .default = NA)
)

# MARRIED
set.seed(1833)
l_married <- mlogit(choice ~ n.score + n.distance + s1 + s2 + s3 + no_alt + s1:married + s2:married + s3:married + n.nightly_price | 0, data=intxn.data, subset=(variant=="economy"),
                 rpar = c(n.score = 'n', n.distance = 'n', s1 = 'n', s2 = 'n', s3 = 'n'),
                 R=100, halton=NA, panel=T)
set.seed(1833)
m_married <- mlogit(choice ~ n.score + n.distance + s1 + s2 + s3 + no_alt + s1:married + s2:married + s3:married + n.nightly_price | 0, data=intxn.data, subset=(variant=="midscale"),
                 rpar = c(n.score = 'n', n.distance = 'n', s1 = 'n', s2 = 'n', s3 = 'n'),
                 R=100, halton=NA, panel=T)
set.seed(1833)
h_married <- mlogit(choice ~ n.score + n.distance + s1 + s2 + s3 + no_alt + s1:married + s2:married + s3:married + n.nightly_price | 0, data=intxn.data, subset=(variant=="upscale"),
                 rpar = c(n.score = 'n', n.distance = 'n', s1 = 'n', s2 = 'n', s3 = 'n'),
                 R=100, halton=NA, panel=T)

#Generate Supplementary Tables with raw coefficients ----
wordreg(list(l_married, m_married, h_married), file="./results/app/married.docx", custom.model.names = c("Economy", "Mid-Scale", "Upscale"), digits = 3,
        custom.coef.map = list("n.score"= "Guest Review Score", "n.distance"= "Proximity to Dest. (mi)", "s1" = "Sust. Level 1",
                               "s2" = "Sust. Level 2", "s3" = "Sust. Level 3", "n.nightly_price" = "Price per Night ($USD)",
                               "s1:married" = "Sust. Level 1 * married", "s2:married" = "Sust. Level 2 * married", "s3:married" = "Sust. Level 3 * married", 
                               "sd.n.score" = "SD Guest Rev. Score", "sd.n.distance" = "SD Proximity to Dest.", "sd.s1" = "SD Sust. Level 1",
                               "sd.s2" = "SD Sust. Level 2", "sd.s3" = "SD Sust. Level 3"))  

# Compute marginal WTP estimates ----

set.seed(4239)
wtp_married <- get_WTP(list(Economy=l_married, Midscale=m_married, Upscale=h_married), index=(8:10)) %>% mutate(
  varname = case_when(
    varname == "s1:married" ~ "Sust. Level 1 * married",
    varname == "s2:married" ~ "Sust. Level 2 * married",
    varname == "s3:married" ~ "Sust. Level 3 * married",
    .default = NA)
)

# N.AGE
set.seed(1833)
l_n.age <- mlogit(choice ~ n.score + n.distance + s1 + s2 + s3 + no_alt + s1:n.age + s2:n.age + s3:n.age + n.nightly_price | 0, data=intxn.data, subset=(variant=="economy"),
                 rpar = c(n.score = 'n', n.distance = 'n', s1 = 'n', s2 = 'n', s3 = 'n'),
                 R=100, halton=NA, panel=T)
set.seed(1833)
m_n.age <- mlogit(choice ~ n.score + n.distance + s1 + s2 + s3 + no_alt + s1:n.age + s2:n.age + s3:n.age + n.nightly_price | 0, data=intxn.data, subset=(variant=="midscale"),
                 rpar = c(n.score = 'n', n.distance = 'n', s1 = 'n', s2 = 'n', s3 = 'n'),
                 R=100, halton=NA, panel=T)
set.seed(1833)
h_n.age <- mlogit(choice ~ n.score + n.distance + s1 + s2 + s3 + no_alt + s1:n.age + s2:n.age + s3:n.age + n.nightly_price | 0, data=intxn.data, subset=(variant=="upscale"),
                 rpar = c(n.score = 'n', n.distance = 'n', s1 = 'n', s2 = 'n', s3 = 'n'),
                 R=100, halton=NA, panel=T)

#Generate Supplementary Tables with raw coefficients ----
wordreg(list(l_n.age, m_n.age, h_n.age), file="./results/app/n.age.docx", custom.model.names = c("Economy", "Mid-Scale", "Upscale"), digits = 3,
        custom.coef.map = list("n.score"= "Guest Review Score", "n.distance"= "Proximity to Dest. (mi)", "s1" = "Sust. Level 1",
                               "s2" = "Sust. Level 2", "s3" = "Sust. Level 3", "n.nightly_price" = "Price per Night ($USD)",
                               "s1:n.age" = "Sust. Level 1 * n.age", "s2:n.age" = "Sust. Level 2 * n.age", "s3:n.age" = "Sust. Level 3 * n.age", 
                               "sd.n.score" = "SD Guest Rev. Score", "sd.n.distance" = "SD Proximity to Dest.", "sd.s1" = "SD Sust. Level 1",
                               "sd.s2" = "SD Sust. Level 2", "sd.s3" = "SD Sust. Level 3"))  

# Compute marginal WTP estimates ----

set.seed(4239)
wtp_n.age <- get_WTP(list(Economy=l_n.age, Midscale=m_n.age, Upscale=h_n.age), index=(8:10)) %>% mutate(
  varname = case_when(
    varname == "s1:n.age" ~ "Sust. Level 1 * n.age",
    varname == "s2:n.age" ~ "Sust. Level 2 * n.age",
    varname == "s3:n.age" ~ "Sust. Level 3 * n.age",
    .default = NA)
)

# SUST_FILTER
set.seed(1833)
l_sust_filter <- mlogit(choice ~ n.score + n.distance + s1 + s2 + s3 + no_alt + s1:sust_filter + s2:sust_filter + s3:sust_filter + n.nightly_price | 0, data=intxn.data, subset=(variant=="economy"),
                 rpar = c(n.score = 'n', n.distance = 'n', s1 = 'n', s2 = 'n', s3 = 'n'),
                 R=100, halton=NA, panel=T)
set.seed(1833)
m_sust_filter <- mlogit(choice ~ n.score + n.distance + s1 + s2 + s3 + no_alt + s1:sust_filter + s2:sust_filter + s3:sust_filter + n.nightly_price | 0, data=intxn.data, subset=(variant=="midscale"),
                 rpar = c(n.score = 'n', n.distance = 'n', s1 = 'n', s2 = 'n', s3 = 'n'),
                 R=100, halton=NA, panel=T)
set.seed(1833)
h_sust_filter <- mlogit(choice ~ n.score + n.distance + s1 + s2 + s3 + no_alt + s1:sust_filter + s2:sust_filter + s3:sust_filter + n.nightly_price | 0, data=intxn.data, subset=(variant=="upscale"),
                 rpar = c(n.score = 'n', n.distance = 'n', s1 = 'n', s2 = 'n', s3 = 'n'),
                 R=100, halton=NA, panel=T)

#Generate Supplementary Tables with raw coefficients ----
wordreg(list(l_sust_filter, m_sust_filter, h_sust_filter), file="./results/app/sust_filter.docx", custom.model.names = c("Economy", "Mid-Scale", "Upscale"), digits = 3,
        custom.coef.map = list("n.score"= "Guest Review Score", "n.distance"= "Proximity to Dest. (mi)", "s1" = "Sust. Level 1",
                               "s2" = "Sust. Level 2", "s3" = "Sust. Level 3", "n.nightly_price" = "Price per Night ($USD)",
                               "s1:sust_filter" = "Sust. Level 1 * sust_filter", "s2:sust_filter" = "Sust. Level 2 * sust_filter", "s3:sust_filter" = "Sust. Level 3 * sust_filter", 
                               "sd.n.score" = "SD Guest Rev. Score", "sd.n.distance" = "SD Proximity to Dest.", "sd.s1" = "SD Sust. Level 1",
                               "sd.s2" = "SD Sust. Level 2", "sd.s3" = "SD Sust. Level 3"))  

# Compute marginal WTP estimates ----

set.seed(4239)
wtp_sust_filter <- get_WTP(list(Economy=l_sust_filter, Midscale=m_sust_filter, Upscale=h_sust_filter), index=(8:10)) %>% mutate(
  varname = case_when(
    varname == "s1:sust_filter" ~ "Sust. Level 1 * sust_filter",
    varname == "s2:sust_filter" ~ "Sust. Level 2 * sust_filter",
    varname == "s3:sust_filter" ~ "Sust. Level 3 * sust_filter",
    .default = NA)
)

# BOOK_DIRECTLY
set.seed(1833)
l_book_directly <- mlogit(choice ~ n.score + n.distance + s1 + s2 + s3 + no_alt + s1:book_directly + s2:book_directly + s3:book_directly + n.nightly_price | 0, data=intxn.data, subset=(variant=="economy"),
                 rpar = c(n.score = 'n', n.distance = 'n', s1 = 'n', s2 = 'n', s3 = 'n'),
                 R=100, halton=NA, panel=T)
set.seed(1833)
m_book_directly <- mlogit(choice ~ n.score + n.distance + s1 + s2 + s3 + no_alt + s1:book_directly + s2:book_directly + s3:book_directly + n.nightly_price | 0, data=intxn.data, subset=(variant=="midscale"),
                 rpar = c(n.score = 'n', n.distance = 'n', s1 = 'n', s2 = 'n', s3 = 'n'),
                 R=100, halton=NA, panel=T)
set.seed(1833)
h_book_directly <- mlogit(choice ~ n.score + n.distance + s1 + s2 + s3 + no_alt + s1:book_directly + s2:book_directly + s3:book_directly + n.nightly_price | 0, data=intxn.data, subset=(variant=="upscale"),
                 rpar = c(n.score = 'n', n.distance = 'n', s1 = 'n', s2 = 'n', s3 = 'n'),
                 R=100, halton=NA, panel=T)

#Generate Supplementary Tables with raw coefficients ----
wordreg(list(l_book_directly, m_book_directly, h_book_directly), file="./results/app/book_directly.docx", custom.model.names = c("Economy", "Mid-Scale", "Upscale"), digits = 3,
        custom.coef.map = list("n.score"= "Guest Review Score", "n.distance"= "Proximity to Dest. (mi)", "s1" = "Sust. Level 1",
                               "s2" = "Sust. Level 2", "s3" = "Sust. Level 3", "n.nightly_price" = "Price per Night ($USD)",
                               "s1:book_directly" = "Sust. Level 1 * book_directly", "s2:book_directly" = "Sust. Level 2 * book_directly", "s3:book_directly" = "Sust. Level 3 * book_directly", 
                               "sd.n.score" = "SD Guest Rev. Score", "sd.n.distance" = "SD Proximity to Dest.", "sd.s1" = "SD Sust. Level 1",
                               "sd.s2" = "SD Sust. Level 2", "sd.s3" = "SD Sust. Level 3"))  

# Compute marginal WTP estimates ----

set.seed(4239)
wtp_book_directly <- get_WTP(list(Economy=l_book_directly, Midscale=m_book_directly, Upscale=h_book_directly), index=(8:10)) %>% mutate(
  varname = case_when(
    varname == "s1:book_directly" ~ "Sust. Level 1 * book_directly",
    varname == "s2:book_directly" ~ "Sust. Level 2 * book_directly",
    varname == "s3:book_directly" ~ "Sust. Level 3 * book_directly",
    .default = NA)
)

# NEP.MEAN
set.seed(1833)
l_NEP.mean <- mlogit(choice ~ n.score + n.distance + s1 + s2 + s3 + no_alt + s1:NEP.mean + s2:NEP.mean + s3:NEP.mean + n.nightly_price | 0, data=intxn.data, subset=(variant=="economy"),
                 rpar = c(n.score = 'n', n.distance = 'n', s1 = 'n', s2 = 'n', s3 = 'n'),
                 R=100, halton=NA, panel=T)
set.seed(1833)
m_NEP.mean <- mlogit(choice ~ n.score + n.distance + s1 + s2 + s3 + no_alt + s1:NEP.mean + s2:NEP.mean + s3:NEP.mean + n.nightly_price | 0, data=intxn.data, subset=(variant=="midscale"),
                 rpar = c(n.score = 'n', n.distance = 'n', s1 = 'n', s2 = 'n', s3 = 'n'),
                 R=100, halton=NA, panel=T)
set.seed(1833)
h_NEP.mean <- mlogit(choice ~ n.score + n.distance + s1 + s2 + s3 + no_alt + s1:NEP.mean + s2:NEP.mean + s3:NEP.mean + n.nightly_price | 0, data=intxn.data, subset=(variant=="upscale"),
                 rpar = c(n.score = 'n', n.distance = 'n', s1 = 'n', s2 = 'n', s3 = 'n'),
                 R=100, halton=NA, panel=T)

#Generate Supplementary Tables with raw coefficients ----
wordreg(list(l_NEP.mean, m_NEP.mean, h_NEP.mean), file="./results/app/NEP.mean.docx", custom.model.names = c("Economy", "Mid-Scale", "Upscale"), digits = 3,
        custom.coef.map = list("n.score"= "Guest Review Score", "n.distance"= "Proximity to Dest. (mi)", "s1" = "Sust. Level 1",
                               "s2" = "Sust. Level 2", "s3" = "Sust. Level 3", "n.nightly_price" = "Price per Night ($USD)",
                               "s1:NEP.mean" = "Sust. Level 1 * NEP.mean", "s2:NEP.mean" = "Sust. Level 2 * NEP.mean", "s3:NEP.mean" = "Sust. Level 3 * NEP.mean", 
                               "sd.n.score" = "SD Guest Rev. Score", "sd.n.distance" = "SD Proximity to Dest.", "sd.s1" = "SD Sust. Level 1",
                               "sd.s2" = "SD Sust. Level 2", "sd.s3" = "SD Sust. Level 3"))  

# Compute marginal WTP estimates ----

set.seed(4239)
wtp_NEP.mean <- get_WTP(list(Economy=l_NEP.mean, Midscale=m_NEP.mean, Upscale=h_NEP.mean), index=(8:10)) %>% mutate(
  varname = case_when(
    varname == "s1:NEP.mean" ~ "Sust. Level 1 * NEP.mean",
    varname == "s2:NEP.mean" ~ "Sust. Level 2 * NEP.mean",
    varname == "s3:NEP.mean" ~ "Sust. Level 3 * NEP.mean",
    .default = NA)
)

# TABLE 5
wtp_intxn <- rbind(wtp_male, wtp_white, wtp_bachelors, wtp_incgt100k, wtp_married, wtp_n.age, wtp_sust_filter, wtp_book_directly, wtp_NEP.mean)
write.csv(wtp_intxn, "./results/table5.csv", row.names=F)



save.image("hotel_esg.RData")
