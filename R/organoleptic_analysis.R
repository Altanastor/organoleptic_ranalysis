# Organoleptic analysis ---------------------------------------------------
library(dplyr)
library(pepa)
library(factoextra)
library(st4gi)
library(stringr)

# Quechualoma -------------------------------------------------------------

dquechua  <- readxl::read_xlsx("data/Organolepticos.xlsx",sheet = 2)
dquechua <-  as.data.frame(dquechua, stringsAsFactors=FALSE)
tnames <- names(dquechua) %>% stringr::str_replace_all(., pattern = " ",replacement = "_")
names(dquechua) <- tnames

adgquechua <- docomp("sum", c("APARIENCIA_EXTERNA", "COLOR_INTERNO","TEXTURA", "SABOR", "COLOR_DE_FRITURA"),
                              "VARIEDAD", data = dquechua)

temp <- docomp("sum", c("APARIENCIA_EXTERNA", "COLOR_INTERNO","TEXTURA", "SABOR", "COLOR_DE_FRITURA"),
                     c("VARIEDAD", "SEXO"), data = dquechua)

#Masculino-----------------------------
admq <- temp[temp$SEXO == "M" | temp$SEXO == "Male", ]
names(admq)[3:7] <- names(admq)[3:7] %>% paste0("-M")

#Femenino -----------------------------
adfq <- temp[temp$SEXO == "F" | temp$SEXO == "Female", ]
names(adfq)[3:7] <- names(adfq)[3:7] %>% paste0("-F")

# Juntar ambos sexos por columnas
adsq <- cbind(admq[, c(1, 3:7)], adfq[, 3:7])


#PCA general -----------------------------
rownames(adgquechua) <- adgquechua$VARIEDAD
adgquechua <- adgquechua[, -1]

principQuechua <- prcomp(adgquechua, center = T, scale. = T)
summary(principQuechua)
factoextra::fviz_pca(principQuechua, repel = T,
                     title = "Biplot of genotypes and attributes")

#PCA por sexo -----------------------------
rownames(adsq) <- adsq$VARIEDAD
adsq <- adsq[, -1]
princip <- prcomp(adsq, center = T, scale. = T)
summary(princip)
factoextra::fviz_pca(princip, repel = T,
                     title = "Biplot of genotypes and attributes by gender")



# Moyobamba ---------------------------------------------------------------

dmoyo <-  readxl::read_xlsx("data/Organolepticos.xlsx",sheet = 3)
dmoyo <- as.data.frame(dmoyo, stringsAsFactors=FALSE)
tnames <- names(dmoyo) %>% stringr::str_replace_all(., pattern = " ",replacement = "_")
names(dmoyo) <- tnames

adgmoyo <- docomp("sum", c("APARIENCIA_EXTERNA", "COLOR_INTERNO","TEXTURA", "SABOR", "COLOR_DE_FRITURA"),
                       "VARIEDAD", data = dmoyo)

temp <- docomp("sum", c("APARIENCIA_EXTERNA", "COLOR_INTERNO","TEXTURA", "SABOR", "COLOR_DE_FRITURA"),
               c("VARIEDAD", "SEXO"), data = dmoyo)

#Masculino-----------------------------
admyo <- temp[temp$SEXO == "M" | temp$SEXO == "Male", ]
names(admyo)[3:7] <- names(admyo)[3:7] %>% paste0("-M")

#Femenino -----------------------------
adfyo <- temp[temp$SEXO == "F" | temp$SEXO == "Female", ]
names(adfyo)[3:7] <- names(adfyo)[3:7] %>% paste0("-F")

# Juntar ambos sexos por columnas
adsyo <- cbind(admyo[, c(1, 3:7)], adfyo[, 3:7])


#PCA general -----------------------------
rownames(adgmoyo) <- adgmoyo$VARIEDAD
adgmoyo <- adgmoyo[, -1]

principMoyo <- prcomp(adgmoyo, center = T, scale. = T)
summary(principMoyo)
factoextra::fviz_pca(principMoyo, repel = T,
                     title = "Biplot of genotypes and attributes")


#PCA por sexo -----------------------------
rownames(adsyo) <- adsyo$VARIEDAD
adsyo <- adsyo[, -1]
princip <- prcomp(adsyo, center = T, scale. = T)
summary(princip)
factoextra::fviz_pca(princip, repel = T,
                     title = "Biplot of genotypes and attributes by gender")


