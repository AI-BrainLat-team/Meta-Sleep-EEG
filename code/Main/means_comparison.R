# means comparison
# Erase environment variables
rm(list = ls())
gc() 

if (!require("remotes")) {
    install.packages("remotes")
}
remotes::install_github("MathiasHarrer/dmetar")

# library(weightr)
# Download (if necesary) and load packages
packages = c( "tidyverse", "DT", "htmltools", "htmlwidgets",  "knitr", "hrbrthemes",
              "data.table", "reticulate", "kableExtra", "formattable", "ggplotify", 
              "rvest", "dmetar", "meta", "metafor", "esc", "easystats")#, "extrafont")

package.check <- lapply(
    packages,
    FUN = function(x) {
        if (!require(x, character.only = TRUE, quietly = TRUE)) {
            install.packages(x, dependencies = TRUE, quiet = TRUE)
            library(x, character.only = TRUE, quietly = TRUE)
        }
    }
)  

hrbrthemes::import_roboto_condensed()
data <- read_csv("/home/marcelo/Sync/BrainLat/2024/Meta-eeg/data/tidy/data_aov.csv")
save_path <- "/home/marcelo/Sync/BrainLat/2024/Meta-eeg/Results/combined/final/Main/"
colnames(data)
sort(unique(data$country))
str(data)
data$mean <- as.numeric(data$mean)
# Erase no use columns
data$`#` <- NULL
# data$G2_type <- NULL
#Generate proper labels for studies to graph
data$studlab <- gsub("_", " ", data$auth_year)
# data$sudlab <- gsub("(\\d{4})", "\\1)", data$studlab, perl=TRUE)


# columns_to_convert <- c("variable", "country", "auth_year", "G1", "G2", "G2_type","author") 
# for (col in columns_to_convert) {
#     data[[col]] <- factor(data[[col]])
# }

# # Check levels
# levels(data$variable)
# levels(data$country)
# levels(data$G1)
# levels(data$G2)
# levels(data$G2_type)

# subset data
data
rem <- data[(data$variable %in% c("REM_p")), ]
n1 <- data[(data$variable %in% c("N1_p")), ]
n2 <- data[(data$variable %in% c("N2_p")), ]
n3 <- data[(data$variable %in% c("N3_p")), ]
se_p <- data[(data$variable %in% c("SE_p")), ]
sl_m <- data[(data$variable %in% c("SL_m")), ]
tst_m <- data[(data$variable %in% c("TST_m")), ]
waso_m <- data[(data$variable %in% c("WASO_m")), ]

rem
# Variable: REM_p, n articles: 27
# Variable: N1_p, n articles: 23
# Variable: N2_p, n articles: 24
# Variable: N3_p, n articles: 13
# Variable: SE_p, n articles: 28
# Variable: SL_m, n articles: 21
# Variable: TST_m, n articles: 31
# Variable: WASO_m, n articles: 18

str(data)
nrow(rem)
nrow(n1)
nrow(n2)
nrow(n3)
nrow(se_p)
nrow(sl_m)
nrow(tst_m)
nrow(waso_m)

subset(rem , G == 'HC', select = c('mean'))

#### Welch's t-test HCs vs all Dementias
tt_rem <- t.test(subset(rem , G == 'HC', select = c('mean')), subset(rem , G != 'HC', select = c('mean')), paired = FALSE, var.equal = FALSE)
tt_n1 <- t.test(subset(n1 , G == 'HC', select = c('mean')), subset(n1 , G != 'HC', select = c('mean')), paired = FALSE, var.equal = FALSE)
tt_n2 <- t.test(subset(n2 , G == 'HC', select = c('mean')), subset(n2 , G != 'HC', select = c('mean')), paired = FALSE, var.equal = FALSE)
tt_n3 <- t.test(subset(n3 , G == 'HC', select = c('mean')), subset(n3 , G != 'HC', select = c('mean')), paired = FALSE, var.equal = FALSE)
tt_se_p <- t.test(subset(se_p , G == 'HC', select = c('mean')), subset(se_p , G != 'HC', select = c('mean')), paired = FALSE, var.equal = FALSE)
tt_sl_m <- t.test(subset(sl_m , G == 'HC', select = c('mean')), subset(sl_m , G != 'HC', select = c('mean')), paired = FALSE, var.equal = FALSE)
tt_tst_m <- t.test(subset(tst_m , G == 'HC', select = c('mean')), subset(tst_m , G != 'HC', select = c('mean')), paired = FALSE, var.equal = FALSE)
tt_waso_m <- t.test(subset(waso_m , G == 'HC', select = c('mean')), subset(waso_m , G != 'HC', select = c('mean')), paired = FALSE, var.equal = FALSE)

### N for G1 and G2
# REM
nrow(subset(rem , G =='HC'))
nrow(subset(rem , G !='HC'))
# N1
nrow(subset(n1 , G =='HC'))
nrow(subset(n1 , G !='HC'))
# N2
nrow(subset(n2 , G =='HC'))
nrow(subset(n2 , G !='HC'))
# N3
nrow(subset(n3 , G =='HC'))
nrow(subset(n3 , G !='HC'))
# SE
nrow(subset(se_p , G =='HC'))
nrow(subset(se_p , G !='HC'))
# SL
nrow(subset(sl_m , G =='HC'))
nrow(subset(sl_m , G !='HC'))
# TST
nrow(subset(tst_m , G =='HC'))
nrow(subset(tst_m , G !='HC'))
# WASO
nrow(subset(waso_m , G =='HC'))
nrow(subset(waso_m , G !='HC'))

print("REM")
print(tt_rem)
print("N1")
print(tt_n1)
print("N2")
print(tt_n2)
print("N3")
print(tt_n3)
print("SE_p")
print(tt_se_p)
print("SL_m")
print(tt_sl_m)
print("TST_m")
print(tt_tst_m)
print("WASO_m")
print(tt_waso_m)

#### Welch's t-test HCs vs FTD 
tt_rem <- t.test(subset(rem , G == 'HC', select = c('mean')), subset(rem , G == 'FTD', select = c('mean')), paired = FALSE, var.equal = FALSE)
tt_n1 <- t.test(subset(n1 , G == 'HC', select = c('mean')), subset(n1 , G == 'FTD', select = c('mean')), paired = FALSE, var.equal = FALSE)
tt_n2 <- t.test(subset(n2 , G == 'HC', select = c('mean')), subset(n2 , G == 'FTD', select = c('mean')), paired = FALSE, var.equal = FALSE)
tt_n3 <- t.test(subset(n3 , G == 'HC', select = c('mean')), subset(n3 , G == 'FTD', select = c('mean')), paired = FALSE, var.equal = FALSE)
tt_se_p <- t.test(subset(se_p , G == 'HC', select = c('mean')), subset(se_p ,G == 'FTD', select = c('mean')), paired = FALSE, var.equal = FALSE)
tt_sl_m <- t.test(subset(sl_m , G == 'HC', select = c('mean')), subset(sl_m , G == 'FTD', select = c('mean')), paired = FALSE, var.equal = FALSE)
tt_tst_m <- t.test(subset(tst_m , G == 'HC', select = c('mean')), subset(tst_m , G == 'FTD', select = c('mean')), paired = FALSE, var.equal = FALSE)
tt_waso_m <- t.test(subset(waso_m , G == 'HC', select = c('mean')), subset(waso_m ,G == 'FTD', select = c('mean')), paired = FALSE, var.equal = FALSE)


### N  G2
# FTD
nrow(subset(rem , G =='FTD'))
# N1
nrow(subset(n1 , G =='FTD'))
# N2
nrow(subset(n2 , G =='FTD'))
# N3
nrow(subset(n3 , G =='FTD'))
# SE
nrow(subset(se_p , G =='FTD'))
# SL
nrow(subset(sl_m , G =='FTD'))
# TST
nrow(subset(tst_m , G =='FTD'))
# WASO
nrow(subset(waso_m , G =='FTD'))

print("REM")
print(tt_rem)
print("N1")
print(tt_n1)
print("N2")
print(tt_n2)
print("N3")
print(tt_n3)
print("SE_p")
print(tt_se_p)
print("SL_m")
print(tt_sl_m)
print("TST_m")
print(tt_tst_m)
print("WASO_m")
print(tt_waso_m)


#### Welch's t-test HCs vs Non Ad - Dementias

tt_rem <- t.test(subset(rem , G == 'HC', select = c('mean')), subset(rem , !(G %in% c('AD','HC','Mild-AD')), select = c('mean')), paired = FALSE, var.equal = FALSE)
tt_n1 <- t.test(subset(n1 , G == 'HC', select = c('mean')), subset(n1 , !(G %in% c('AD','HC','Mild-AD')), select = c('mean')), paired = FALSE, var.equal = FALSE)
tt_n2 <- t.test(subset(n2 , G == 'HC', select = c('mean')), subset(n2 , !(G %in% c('AD','HC','Mild-AD')), select = c('mean')), paired = FALSE, var.equal = FALSE)
tt_n3 <- t.test(subset(n3 , G == 'HC', select = c('mean')), subset(n3 , !(G %in% c('AD','HC','Mild-AD')), select = c('mean')), paired = FALSE, var.equal = FALSE)
tt_se_p <- t.test(subset(se_p , G == 'HC', select = c('mean')), subset(se_p ,!(G %in% c('AD','HC','Mild-AD')), select = c('mean')), paired = FALSE, var.equal = FALSE)
tt_sl_m <- t.test(subset(sl_m , G == 'HC', select = c('mean')), subset(sl_m , !(G %in% c('AD','HC','Mild-AD')), select = c('mean')), paired = FALSE, var.equal = FALSE)
tt_tst_m <- t.test(subset(tst_m , G == 'HC', select = c('mean')), subset(tst_m , !(G %in% c('AD','HC','Mild-AD')), select = c('mean')), paired = FALSE, var.equal = FALSE)
tt_waso_m <- t.test(subset(waso_m , G == 'HC', select = c('mean')), subset(waso_m ,!(G %in% c('AD','HC','Mild-AD', 'Non-Amnestic AD')), select = c('mean')), paired = FALSE, var.equal = FALSE)


### N  G2
# Non AD dementias
nrow(subset(rem , !(G %in% c('AD','HC','Mild-AD'))))
# N1
nrow(subset(n1 ,!(G %in% c('AD','HC','Mild-AD'))))
# N2
nrow(subset(n2 ,!(G %in% c('AD','HC','Mild-AD'))))
# N3
nrow(subset(n3 , !(G %in% c('AD','HC','Mild-AD'))))
# SE
nrow(subset(se_p , !(G %in% c('AD','HC','Mild-AD'))))
# SL
nrow(subset(sl_m ,!(G %in% c('AD','HC','Mild-AD'))))
# TST
nrow(subset(tst_m ,!(G %in% c('AD','HC','Mild-AD'))))
# WASO
nrow(subset(waso_m ,!(G %in% c('AD','HC','Mild-AD'))))


print("REM")
print(tt_rem)
print("N1")
print(tt_n1)
print("N2")
print(tt_n2)
print("N3")
print(tt_n3)
print("SE_p")
print(tt_se_p)
print("SL_m")
print(tt_sl_m)
print("TST_m")
print(tt_tst_m)
print("WASO_m")
print(tt_waso_m)


### ANOVA

## REM
# HCs vs all dementias
rem_dem <- rem
rem_dem$G <- ifelse(rem_dem$G != "HC", "Dementia", rem_dem$G)

# Compute the analysis of variance
res_rem_aov <- aov(mean ~ G, data = rem_dem)
# Summary of the analysis
summary(res_rem_aov)
TukeyHSD(res_rem_aov)

# HCs vs all FTD
rem_dem2 <- subset(rem, (G %in% c("HC", "FTD")))

# Compute the analysis of variance
res_rem_aov2 <- aov(mean ~ G, data = rem_dem2)
# Summary of the analysis
summary(res_rem_aov2)
TukeyHSD(res_rem_aov2)

# HCs vs non AD dementias
rem_dem3 <- subset(rem, !(G %in% c('AD','Mild-AD', 'Non-Amnestic AD')))

# Compute the analysis of variance
res_rem_aov3 <- aov(mean ~ G, data = rem_dem3)
# Summary of the analysis
summary(res_rem_aov3)
TukeyHSD(res_rem_aov3)


## N1

# HCs vs all dementias
n1_dem <- n1
n1_dem$G <- ifelse(n1_dem$G != "HC", "Dementia", n1_dem$G)

# Compute the analysis of variance
res_n1_aov <- aov(mean ~ G, data = n1_dem)
# Summary of the analysis
summary(res_n1_aov)
TukeyHSD(res_n1_aov)

# HCs vs all FTD
n1_dem2 <- subset(n1, (G %in% c("HC", "FTD")))

# Compute the analysis of variance
res_n1_aov2 <- aov(mean ~ G, data = n1_dem2)
# Summary of the analysis
summary(res_n1_aov2)
TukeyHSD(res_n1_aov2)

# HCs vs non AD dementias
# Non AD dementias are FTD for N1 results.

## N2

# HCs vs all dementias
n2_dem <- n2
n2_dem$G <- ifelse(n2_dem$G != "HC", "Dementia", n2_dem$G)

# Compute the analysis of variance
res_n2_aov <- aov(mean ~ G, data = n2_dem)
# Summary of the analysis
summary(res_n2_aov)
TukeyHSD(res_n2_aov)

# HCs vs all FTD
n2_dem2 <- subset(n2, (G %in% c("HC", "FTD")))

# Compute the analysis of variance
res_n2_aov2 <- aov(mean ~ G, data = n2_dem2)
# Summary of the analysis
summary(res_n2_aov2)
TukeyHSD(res_n2_aov2)

# HCs vs non AD dementias
# Non AD dementias are FTD for N2 results.

## N3

# HCs vs all dementias
n3_dem <- n3
n3_dem$G <- ifelse(n3_dem$G != "HC", "Dementia", n3_dem$G)

# Compute the analysis of variance
res_n3_aov <- aov(mean ~ G, data = n3_dem)
# Summary of the analysis
summary(res_n3_aov)
TukeyHSD(res_n3_aov)

# HCs vs all FTD
n3_dem2 <- subset(n3, (G %in% c("HC", "FTD")))

# Compute the analysis of variance
res_n3_aov2 <- aov(mean ~ G, data = n3_dem2)
# Summary of the analysis
summary(res_n3_aov2)
TukeyHSD(res_n3_aov2)

# HCs vs non AD dementias
# Non AD dementias are FTD for N1 results.

## SE
# HCs vs all dementias
se_dem <- se_p
se_dem$G <- ifelse(se_dem$G != "HC", "Dementia", se_dem$G)

# Compute the analysis of variance
res_se_aov <- aov(mean ~ G, data = se_dem)
# Summary of the analysis
summary(res_se_aov)
TukeyHSD(res_se_aov)

# HCs vs all FTD
se_dem2 <- subset(se_p, (G %in% c("HC", "FTD")))

# Compute the analysis of variance
res_se_aov2 <- aov(mean ~ G, data = se_dem2)
# Summary of the analysis
summary(res_se_aov2)
TukeyHSD(res_se_aov2)

# HCs vs non AD dementias
se_dem3 <- subset(se_p, !(G %in% c('AD','Mild-AD', 'Non-Amnestic AD')))

# Compute the analysis of variance
res_se_aov3 <- aov(mean ~ G, data = se_dem3)
# Summary of the analysis
summary(res_se_aov3)
TukeyHSD(res_se_aov3)

## SL
# HCs vs all dementias
sl_dem <- sl_m
sl_dem$G <- ifelse(sl_dem$G != "HC", "Dementia", sl_dem$G)

# Compute the analysis of variance
res_sl_aov <- aov(mean ~ G, data = sl_dem)
# Summary of the analysis
summary(res_sl_aov)
TukeyHSD(res_sl_aov)

# HCs vs all FTD
sl_dem2 <- subset(sl_m, (G %in% c("HC", "FTD")))

# Compute the analysis of variance
res_sl_aov2 <- aov(mean ~ G, data = sl_dem2)
# Summary of the analysis
summary(res_sl_aov2)
TukeyHSD(res_sl_aov2)

# HCs vs non AD dementias
sl_dem3 <- subset(sl_m, !(G %in% c('AD','Mild-AD', 'Non-Amnestic AD')))

# Compute the analysis of variance
res_sl_aov3 <- aov(mean ~ G, data = sl_dem3)
# Summary of the analysis
summary(res_sl_aov3)
TukeyHSD(res_sl_aov3)

## TST
# HCs vs all dementias
tst_dem <- tst_m
tst_dem$G <- ifelse(tst_dem$G != "HC", "Dementia", tst_dem$G)

# Compute the analysis of variance
res_tst_aov <- aov(mean ~ G, data = tst_dem)
# Summary of the analysis
summary(res_tst_aov)
TukeyHSD(res_tst_aov)

# HCs vs all FTD
tst_dem2 <- subset(tst_m, (G %in% c("HC", "FTD")))

# Compute the analysis of variance
res_tst_aov2 <- aov(mean ~ G, data = tst_dem2)
# Summary of the analysis
summary(res_tst_aov2)
TukeyHSD(res_tst_aov2)

# HCs vs non AD dementias
tst_dem3 <- subset(tst_m, !(G %in% c('AD','Mild-AD', 'Non-Amnestic AD')))

# Compute the analysis of variance
res_tst_aov3 <- aov(mean ~ G, data = tst_dem3)
# Summary of the analysis
summary(res_tst_aov3)
TukeyHSD(res_tst_aov3)

## WASO
# HCs vs all dementias
wa_dem <- waso_m
wa_dem$G <- ifelse(wa_dem$G != "HC", "Dementia", wa_dem$G)

# Compute the analysis of variance
res_wa_aov <- aov(mean ~ G, data = wa_dem)
# Summary of the analysis
summary(res_wa_aov)
TukeyHSD(res_wa_aov)

# HCs vs all FTD
wa_dem2 <- subset(waso_m, (G %in% c("HC", "FTD")))

# Compute the analysis of variance
res_wa_aov2 <- aov(mean ~ G, data = wa_dem2)
# Summary of the analysis
summary(res_wa_aov2)
TukeyHSD(res_wa_aov2)

# HCs vs non AD dementias
wa_dem3 <- subset(waso_m, !(G %in% c('AD','Mild-AD', 'Non-Amnestic AD')))

# Compute the analysis of variance
res_wa_aov3 <- aov(mean ~ G, data = wa_dem3)
# Summary of the analysis
summary(res_wa_aov3)
TukeyHSD(res_wa_aov3)
