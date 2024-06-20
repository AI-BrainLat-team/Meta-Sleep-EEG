# Erase environment variables
rm(list = ls())
gc() 

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

# font_import(paths = "/home/marcelo/.fonts", pattern = "ArimoNerdFontPropo-Regular.ttf")
# loadfonts(device = "unix")

hrbrthemes::import_roboto_condensed()

data <- read_csv("/home/marcelo/Sync/BrainLat/2024/Meta-eeg/data/tidy/ad.csv")
save_path <- "/home/marcelo/Sync/BrainLat/2024/Meta-eeg/Results/AD/wo"
colnames(data)
unique(data$G2)

# Erase no use columns
data$`#` <- NULL
data$G2_type <- NULL
#Generate proper labels for studies to graph
data$studlab <- gsub("_", " ", data$auth_year)
# data$sudlab <- gsub("(\\d{4})", "\\1)", data$studlab, perl=TRUE)

columns_to_convert <- c("variable", "country", "auth_year", "G1", "G2","author") 

for (col in columns_to_convert) {
    data[[col]] <- factor(data[[col]])
}

length(unique(data$auth_year))

# Check levels
levels(data$variable)
levels(data$country)
levels(data$G1)
levels(data$G2)

data$mean_G2 <- as.numeric(data$mean_G2)

# subset data
rem <- data[(data$variable %in% c("REM_p")), ]
n1 <- data[(data$variable %in% c("N1_p")), ]
n2 <- data[(data$variable %in% c("N2_p")), ]
n3 <- data[(data$variable %in% c("N3_p")), ]
se_p <- data[(data$variable %in% c("SE_p")), ]
sl_m <- data[(data$variable %in% c("SL_m")), ]
tst_m <- data[(data$variable %in% c("TST_m")), ]
waso_m <- data[(data$variable %in% c("WASO_m")), ]


nrow(rem)
nrow(n1)
nrow(n2)
nrow(n3)
nrow(se_p)
nrow(sl_m)
nrow(tst_m)
nrow(waso_m)

rem %>%
    group_by('G2') %>%
    count(G2)

n1 %>%
    group_by('G2') %>%
    count(G2)

n2 %>%
    group_by('G2') %>%
    count(G2)

n3 %>%
    group_by('G2') %>%
    count(G2)

se_p %>%
    group_by('G2') %>%
    count(G2)

sl_m %>%
    group_by('G2') %>%
    count(G2)

tst_m %>%
    group_by('G2') %>%
    count(G2)

waso_m %>%
    group_by('G2') %>%
    count(G2)

####### Random effects models:
rem.m <- metacont(n.e = n2,
                  mean.e = mean_G2,
                  sd.e = sd_G2,
                  n.c = n1,
                  mean.c = mean_G1,
                  sd.c = sd_G1,
                  studlab = studlab,
                  data=rem, 
                  sm="SMD",
                  method.smd = "Hedges",
                  comb.fixed = FALSE, comb.random = TRUE,
                  prediction = TRUE, method.predict = "HK",
                  method.tau = "REML",
                  hank = TRUE,
                  title = 'REM (%) Meta-Analysis')
rem.m

n1.m <- metacont(n.e = n2,
                 mean.e = mean_G2,
                 sd.e = sd_G2,
                 n.c = n1,
                 mean.c = mean_G1,
                 sd.c = sd_G1,
                 studlab = studlab,
                 data=n1, 
                 sm="SMD",
                 method.smd = "Hedges",
                 comb.fixed = FALSE, comb.random = TRUE,
                 prediction = TRUE, method.predict = "HK",
                 method.tau = "REML",
                 hank = TRUE,
                 title = 'N1 (%) Meta-Analysis')
n1.m

n2.m <- metacont(n.e = n2,
                 mean.e = mean_G2,
                 sd.e = sd_G2,
                 n.c = n1,
                 mean.c = mean_G1,
                 sd.c = sd_G1,
                 studlab = studlab,
                 data=n2, 
                 sm="SMD",
                 method.smd = "Hedges",
                 comb.fixed = FALSE, comb.random = TRUE,
                 prediction = TRUE, method.predict = "HK",
                 method.tau = "REML",
                 hank = TRUE,
                 title = 'N2 (%) Meta-Analysis')
n2.m

n3.m <- metacont(n.e = n2,
                 mean.e = mean_G2,
                 sd.e = sd_G2,
                 n.c = n1,
                 mean.c = mean_G1,
                 sd.c = sd_G1,
                 studlab = studlab,
                 data=n3, 
                 sm="SMD",
                 method.smd = "Hedges",
                 comb.fixed = FALSE, comb.random = TRUE,
                 prediction = TRUE, method.predict = "HK",
                 method.tau = "REML",
                 hank = TRUE,
                 title = 'N3 (%) Meta-Analysis')
n3.m

se_p.m <- metacont(n.e = n2,
                   mean.e = mean_G2,
                   sd.e = sd_G2,
                   n.c = n1,
                   mean.c = mean_G1,
                   sd.c = sd_G1,
                   studlab = studlab,
                   data=se_p, 
                   sm="SMD",
                   method.smd = "Hedges",
                   comb.fixed = FALSE, comb.random = TRUE,
                   prediction = TRUE, method.predict = "HK",
                   method.tau = "REML",
                   hank = TRUE,
                   title = 'SE (%) Meta-Analysis')
se_p.m

sl_m.m <- metacont(n.e = n2,
                   mean.e = mean_G2,
                   sd.e = sd_G2,
                   n.c = n1,
                   mean.c = mean_G1,
                   sd.c = sd_G1,
                   studlab = studlab,
                   data=sl_m, 
                   sm="SMD",
                   method.smd = "Hedges",
                   comb.fixed = FALSE, comb.random = TRUE,
                   prediction = TRUE, method.predict = "HK",
                   method.tau = "REML",
                   hank = TRUE,
                   title = 'SL (min) Meta-Analysis')
sl_m.m

tst_m.m <- metacont(n.e = n2,
                    mean.e = mean_G2,
                    sd.e = sd_G2,
                    n.c = n1,
                    mean.c = mean_G1,
                    sd.c = sd_G1,
                    studlab = studlab,
                    data=tst_m, 
                    sm="SMD",
                    method.smd = "Hedges",
                    comb.fixed = FALSE, comb.random = TRUE,
                    prediction = TRUE, method.predict = "HK",
                    method.tau = "REML",
                    hank = TRUE,
                    title = 'TST (min) Meta-Analysis')
tst_m.m

waso_m.m <- metacont(n.e = n2,
                     mean.e = mean_G2,
                     sd.e = sd_G2,
                     n.c = n1,
                     mean.c = mean_G1,
                     sd.c = sd_G1,
                     studlab = studlab,
                     data=waso_m, 
                     sm="SMD",
                     method.smd = "Hedges",
                     comb.fixed = FALSE, comb.random = TRUE,
                     prediction = TRUE, method.predict = "HK",
                     method.tau = "REML",
                     hank = TRUE,
                     title = 'WASO (min) Meta-Analysis')
waso_m.m


#### Find outliers
find.outliers(rem.m) 
rem_outliers <- c("Montplaisir_1995", "Liguori_2017", "Liguori_2020", "Liguori_2014", "Rauchs_2008")
find.outliers(n1.m) 
n1_outliers <- c("Liu_2020", "De Gennaro_2017", "Liguori_2020", "Azami_2023", "Liguori_2014" )
find.outliers(n2.m)
n2_outliers <- c("Bonakis_2014", "Liu_2020" )
find.outliers(n3.m) 
n3_outliers <- c("Liu_2020", "Azami_2023" )
find.outliers(se_p.m)
se_p_outliers <- c("De Gennaro_2017", "Liguori_2020", "Liguori_2019" )
find.outliers(sl_m.m)
sl_m_outliers <- c("Montplaisir_1995" )
find.outliers(tst_m.m)
tst_m_outliers <- c("Chen_2012", "Gorgoni_2016", "Liu_2020", "De Gennaro_2017", "Liguori_2020" )
find.outliers(waso_m.m)
waso_m_outliers <- c("Liguori_2020", "Liguori_2019" )

### Remove oultiers from data
rem <- subset(rem, ! (auth_year %in% rem_outliers) )
n1 <- subset(n1, ! (auth_year %in% n1_outliers) )
n2 <- subset(n2, ! (auth_year %in% n2_outliers) )
n3 <- subset(n3, ! (auth_year %in% n3_outliers) )
se_p <- subset(se_p, ! (auth_year %in% se_p_outliers) )
sl_m <- subset(sl_m, ! (auth_year %in% sl_m_outliers) )
tst_m <- subset(tst_m, ! (auth_year %in% tst_m_outliers) )
waso_m <- subset(waso_m, ! (auth_year %in% waso_m_outliers))

# 
# 
# rem %>%
#     group_by('G2') %>%
#     count(G2)
# 
# n1 %>%
#     group_by('G2') %>%
#     count(G2)
# 
# n2 %>%
#     group_by('G2') %>%
#     count(G2)
# 
# n3 %>%
#     group_by('G2') %>%
#     count(G2)
# 
# se_p %>%
#     group_by('G2') %>%
#     count(G2)
# 
# sl_m %>%
#     group_by('G2') %>%
#     count(G2)
# 
# tst_m %>%
#     group_by('G2') %>%
#     count(G2)
# 
# waso_m %>%
#     group_by('G2') %>%
#     count(G2)


# Re-run the models
####### Random effects models without outliers:
rem.m <- metacont(n.e = n2,
                  mean.e = mean_G2,
                  sd.e = sd_G2,
                  n.c = n1,
                  mean.c = mean_G1,
                  sd.c = sd_G1,
                  studlab = studlab,
                  data=rem, 
                  sm="SMD",
                  method.smd = "Hedges",
                  comb.fixed = FALSE, comb.random = TRUE,
                  prediction = TRUE, method.predict = "HK",
                  method.tau = "REML",
                  hank = TRUE,
                  title = 'REM (%) Meta-Analysis')
rem.m

n1.m <- metacont(n.e = n2,
                 mean.e = mean_G2,
                 sd.e = sd_G2,
                 n.c = n1,
                 mean.c = mean_G1,
                 sd.c = sd_G1,
                 studlab = studlab,
                 data=n1, 
                 sm="SMD",
                 method.smd = "Hedges",
                 comb.fixed = FALSE, comb.random = TRUE,
                 prediction = TRUE, method.predict = "HK",
                 method.tau = "REML",
                 hank = TRUE,
                 title = 'N1 (%) Meta-Analysis')
n1.m

n2.m <- metacont(n.e = n2,
                 mean.e = mean_G2,
                 sd.e = sd_G2,
                 n.c = n1,
                 mean.c = mean_G1,
                 sd.c = sd_G1,
                 studlab = studlab,
                 data=n2, 
                 sm="SMD",
                 method.smd = "Hedges",
                 comb.fixed = FALSE, comb.random = TRUE,
                 prediction = TRUE, method.predict = "HK",
                 method.tau = "REML",
                 hank = TRUE,
                 title = 'N2 (%) Meta-Analysis')
n2.m

n3.m <- metacont(n.e = n2,
                 mean.e = mean_G2,
                 sd.e = sd_G2,
                 n.c = n1,
                 mean.c = mean_G1,
                 sd.c = sd_G1,
                 studlab = studlab,
                 data=n3,
                 sm="SMD",
                 method.smd = "Hedges",
                 comb.fixed = FALSE, comb.random = TRUE,
                 prediction = TRUE, method.predict = "HK",
                 method.tau = "REML",
                 hank = TRUE,
                 title = 'N3 (%) Meta-Analysis')
n3.m

se_p.m <- metacont(n.e = n2,
                   mean.e = mean_G2,
                   sd.e = sd_G2,
                   n.c = n1,
                   mean.c = mean_G1,
                   sd.c = sd_G1,
                   studlab = studlab,
                   data=se_p, 
                   sm="SMD",
                   method.smd = "Hedges",
                   comb.fixed = FALSE, comb.random = TRUE,
                   prediction = TRUE, method.predict = "HK",
                   method.tau = "REML",
                   hank = TRUE,
                   title = 'SE (%) Meta-Analysis')
se_p.m

sl_m.m <- metacont(n.e = n2,
                   mean.e = mean_G2,
                   sd.e = sd_G2,
                   n.c = n1,
                   mean.c = mean_G1,
                   sd.c = sd_G1,
                   studlab = studlab,
                   data=sl_m,
                   sm="SMD",
                   method.smd = "Hedges",
                   comb.fixed = FALSE, comb.random = TRUE,
                   prediction = TRUE, method.predict = "HK",
                   method.tau = "REML",
                   hank = TRUE,
                   title = 'SL (min) Meta-Analysis')
sl_m.m

tst_m.m <- metacont(n.e = n2,
                    mean.e = mean_G2,
                    sd.e = sd_G2,
                    n.c = n1,
                    mean.c = mean_G1,
                    sd.c = sd_G1,
                    studlab = studlab,
                    data=tst_m, 
                    sm="SMD",
                    method.smd = "Hedges",
                    comb.fixed = FALSE, comb.random = TRUE,
                    prediction = TRUE, method.predict = "HK",
                    method.tau = "REML",
                    hank = TRUE,
                    title = 'TST (min) Meta-Analysis')
tst_m.m

waso_m.m <- metacont(n.e = n2,
                     mean.e = mean_G2,
                     sd.e = sd_G2,
                     n.c = n1,
                     mean.c = mean_G1,
                     sd.c = sd_G1,
                     studlab = studlab,
                     data=waso_m,
                     sm="SMD",
                     method.smd = "Hedges",
                     comb.fixed = FALSE, comb.random = TRUE,
                     prediction = TRUE, method.predict = "HK",
                     method.tau = "REML",
                     hank = TRUE,
                     title = 'WASO (min) Meta-Analysis')
waso_m.m




setwd("/home/marcelo/Sync/BrainLat/2024/Meta-eeg/Results/AD/wo")
modelos <- list(rem.m, n1.m, n2.m, n3.m, se_p.m, sl_m.m, tst_m.m, waso_m.m)
####  Save results
### All predictors 
names <- c("REM_Meta.txt", "N1_Meta.txt","N2_Meta.txt","N3_Meta.txt",
           "se_p_Meta.txt", "sl_m_Meta.txt", "tst_m_Meta.txt","waso_m_Meta.txt")

for (i in 1:length(names)){
    # substr(names[i], start=1, stop=nchar(names[i])-3)
    sink(paste0(substr(names[i], start=1, stop=nchar(names[i])-3), "txt"))
    print(modelos[[i]])
    sink()
}

#####################
# Forest All predictors
library(extrafont)
# fonttable()
par(family = "Arial")  # Set the font family to Arial
par(cex.axis = 1.1, cex.lab = 1.1)
font = c('Arial')

### Forest plots
# names <- c("REM_p_Forest.png", "N1_p_Forest.png", "N2_p_Forest.png", "N3_p_Forest.png",
#            "se_p_Forest.png", "sl_m_Forest.png", "tst_m_Forest.png", "waso_m_Forest.png")

names <- c("REM_p_Forest.pdf", "N1_p_Forest.pdf", "N2_p_Forest.pdf", "N3_p_Forest.pdf",
           "se_p_Forest.pdf", "sl_m_Forest.pdf", "tst_m_Forest.pdf", "waso_m_Forest.pdf")

for (i in 1:length(names)){
    pdf(file=names[i], width = 9, height = 8, fonts =font, bg='white', title='')
    # png(file=names[i], width = 6, height = 8, unit='in', res=300,  bg='white')
    forest(modelos[[i]], sortvar = modelos[[i]]$TE, studlab = modelos[[i]]$studlab,
           prediction=TRUE, zero.pval = TRUE, 
           xlim=c(-5, 5), colgap="0 mm",colgap.studlab = "0 mm",cex = 2,
           # calcwidth.hetstat = TRUE,
           text.random = "Random effects model",
           text.predict="Prediction Interval",
           addrows.below.overall = 3, cex.axis=2, cex.lab=2, cex.main=2, cex.sub=2,
           fontfamily=font,#grid::gpar(fontfamily=font),
           print.tau2 = TRUE, print.I2 = TRUE,
           test.overall = TRUE,
           leftcols = c("studlab"), leftlabs=c("Author"),
           rightcols=c( "ci", "w.random"), rightlabs = c("CI-95%", "W"))
    dev.off()
}



### Enhanced Funnel plots
# names <- c("REM_funnel.png", "N1_funnel.png","N2_funnel.png","N3_funnel.png",
#            "se_p_funnel.png", "sl_m_funnel.png", "tst_m_funnel.png", "waso_m_funnel.png")

names <- c("REM_funnel.pdf", "N1_funnel.pdf","N2_funnel.pdf","N3_funnel.pdf",
           "se_p_funnel.pdf", "sl_m_funnel.pdf", "tst_m_funnel.pdf", "waso_m_funnel.pdf")

col.contour = c("gray75", "gray85", "gray95")

# Loop para generar y guardar los gráficos
for(i in 1:length(names)) {
    pdf(file = names[i], width = 9, height = 8, fonts =font, bg='white')
    # png(file=names[i], width = 6, height = 7, unit='in', res=300,  bg='white')
    # Generate funnel plot (we do not include study labels here)
    metafor::funnel(modelos[[i]], steps=5, yaxs = "i", lwd = 1, cex = 2, hlines='black', 
                    cex.axis = 1.1,cex.lab = 1.1,
                    contour = c(0.9, 0.95, 0.99), col.contour = col.contour, xlim = c(-5, 5))
    
    
    if (i != length(names)){
        # Add a legend
        legend(x = 2.7, y = 0.01,
               legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
               fill = col.contour)
    }
    else {
        legend(x = 2.8, y = 0.01,
               legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
               fill = col.contour)
    }
    # Puedes agregar un título aquí si lo necesitas, por ahora lo dejaré comentado
    # title("Contour-Enhanced Funnel Plot (" ... ")")
    
    dev.off()
}



### P-Curve plots
# names <- c("REM_pcurve.png", "N1_pcurve.png","N2_pcurve.png","N3_pcurve.png",
#            "se_p_pcurve.png", "sl_m_pcurve.png", "tst_m_pcurve.png", "waso_m_pcurve.png")

names <- c("REM_pcurve.pdf", "N1_pcurve.pdf","N2_pcurve.pdf","N3_pcurve.pdf",
           "se_p_pcurve.pdf", "sl_m_pcurve.pdf", "tst_m_pcurve.pdf", "waso_m_pcurve.pdf")

for (i in 1:length(names)){
    tryCatch({
        pdf(file = names[i], width = 9, height = 8, fonts =font, bg='white')
        # png(file = names[i], width = 6, height = 7, unit = 'in', res = 300, bg = 'white')
        sink(paste0(substr(names[i], start = 1, stop = nchar(names[i]) - 3), "txt"))
        print(pcurve(modelos[[i]]))
        dev.off()
        sink()
    }, error = function(e) {
        cat("Error occurred for", names[i], ":", conditionMessage(e), "\n")
    })
}

####  Eggers test
### All predictors 
names <- c("REM_Eggers.png", "N1_Eggers.png","N2_Eggers.png","N3_Eggers.png",
           "se_p_Eggers.png", "sl_m_Eggers.png", "tst_m_Eggers.png","waso_m_Eggers.png")

for (i in 1:length(names)){
    # substr(names[i], start=1, stop=nchar(names[i])-3)
    tryCatch({
        sink(paste0(substr(names[i], start=1, stop=nchar(names[i])-3), "txt"))
    print(eggers.test(modelos[[i]]))
    sink()
    }, error = function(e) {
        cat("Error occurred for", names[i], ":", conditionMessage(e), "\n")
    })
}

#### GOSH
par(cex.axis = 1.4, cex.lab = 1.5)

##GOSH
names <- c("REM_gosh.png", "N1_gosh.png","N2_gosh.png","N3_gosh.png",
           "se_p_gosh.png", "sl_m_gosh.png", "tst_m_gosh.png", "waso_m_gosh.png")

gosh_list <- list() # list to save gosh plots

for (i in 1:length(names)){
    png(file=names[i], width = 6, height = 7, unit='in', res=300,  bg='white')
    # png(file=names[i], bg = "white", pointsize=10, width=1400, height=960, res=300)
    par(mar=c(5, 4, 4, 2))
    sink(paste0(substr(names[i], start=1, stop=nchar(names[i])-3), "txt"))
    meta_rma <- rma(yi=modelos[[i]]$TE, sei=modelos[[i]]$seTE, method=modelos[[i]]$method.tau ,test="knha")
    res_gosh <- gosh(meta_rma, parallel = 'multicore', ncpus = 4, progbar=TRUE)
    gosh_list[[i]]<-res_gosh #Guardar res en una lista?
    print(res_gosh)
    # plot(res_gosh, alpha = 0.01, het="I2", out =outliers[[i]] , col=c("#1b8ec9","#0d5c0a"))
    # plot(res_gosh, alpha = 0.1, het="I2", col=c("#A6C0CE"))
    # plot(res_gosh, alpha = 0.2, het="I2", out = out[i], col=c("#3199cf","#0d5c0a") )
    plot(res_gosh, alpha = 0.2, het="I2", col=c("gray75","#0d5c0a"), cex.axis = 1.4,cex.lab = 1.5, xlim = c(-3, 3))
    
    dev.off()
    sink()
}

print("DONE!")
