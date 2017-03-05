setwd("/Users/veikko/Desktop/GIS/Code")

strtail <- function(s,n=1) {
  if(n<0) 
    substring(s,1-n) 
  else 
    substring(s,nchar(s)-n+1)
  }
strhead <- function(s,n) {
  if(n<0)
    substr(s,1,nchar(s)+n) 
  else 
    substr(s,1,n)
}


# ajetaan aluedatat sisaan

koulutus_xlsx <- file.path("/Users/veikko/Desktop/GIS/Code/Osaaluedata", "koulutus_m.xlsx")

koulutus <- read.xlsx2(koulutus_xlsx, sheetIndex = 1, header = T, startRow = 1, endRow = 4591)
koulutus[koulutus == ".." | koulutus == "."] <- NA
# koulutus <- na.omit(koulutus)

Koulutus_2011B <- koulutus %>%
  dcast(Alue ~ Ikaryhma + Koulutus, value.var = "X2011BOTH")
Koulutus_2015B <- koulutus %>%
  dcast(Alue ~ Ikaryhma + Koulutus, value.var = "X2015BOTH")

# Tohtoritutkinto + Ylempi ja alempi korkeakoulututkinto = Korkeakoulutus
Koulutus_m <- as.data.frame(Koulutus_2011B["Alue"])
Koulutus_m["Korkeakoulutus2011"] <- (as.numeric(Koulutus_2011B[, 2]) + as.numeric(Koulutus_2011B[,7]))/  as.numeric(Koulutus_2011B[,3])
Koulutus_m["Korkeakoulutus2015"] <- (as.numeric(Koulutus_2015B[, 2]) + as.numeric(Koulutus_2015B[,7]))/  as.numeric(Koulutus_2015B[,3])

Koulutus_abs <- as.data.frame(Koulutus_2011B["Alue"])
Koulutus_abs["Korkeakoulutus2011"] <- (as.numeric(Koulutus_2011B[, 2]) + as.numeric(Koulutus_2011B[,7]))
Koulutus_abs["All_edu2011"] <- as.numeric(Koulutus_2011B[,3])
Koulutus_abs["Korkeakoulutus2015"] <- (as.numeric(Koulutus_2015B[, 2]) + as.numeric(Koulutus_2015B[,7]))
Koulutus_abs["All_edu2015"] <- as.numeric(Koulutus_2015B[,3])

# Taloudellinen tasa-arvo ja kulutusvoima asuntokunnittain

asuntokunta_xlsx <- file.path("/Users/veikko/Desktop/GIS/Code/Osaaluedata", "asuntokunta_tulot_gini_m.xlsx")

asuntokunta <- read.xlsx2(asuntokunta_xlsx, sheetIndex = 1, header = T, startRow = 1, endRow = 460)
asuntokunta[asuntokunta == ".." | asuntokunta == "."] <- NA

Asuntokunta_2011 <- asuntokunta %>%
  dcast(Alue ~ Tulot, value.var = "X2011")
colnames(Asuntokunta_2011) <- c("Alue", "Gini2011", "Kuluyks_keskiarvo2011", "Kuluyks_mediaani2011")

Asuntokunta_2014 <- asuntokunta %>%
  dcast(Alue ~ Tulot, value.var = "X2014")
colnames(Asuntokunta_2014) <- c("Alue", "Gini2014", "Kuluyks_keskiarvo2014", "Kuluyks_mediaani2014")

finaldf <- left_join(Asuntokunta_2011, Asuntokunta_2014, by = "Alue") %>%
  left_join(., Koulutus_m, by = "Alue")

finalabs <- left_join(Asuntokunta_2011, Asuntokunta_2014, by = "Alue") %>%
  left_join(., Koulutus_abs, by = "Alue")

# tyottomyys excel
tyottomat_xlsx <- file.path("/Users/veikko/Desktop/GIS/Code/Osaaluedata", "tyottomat_m.xlsx")

tyottomat <- read.xlsx2(tyottomat_xlsx, sheetIndex = 1, header = T, startRow = 1, endRow = 460)
tyottomat[tyottomat == ".." | tyottomat == "."] <- NA
Tyottomat <- tyottomat %>%
  select(-Tyottomat2010,-Tyottomat2014) %>%
  subset(Sukupuolet == "Molemmat") %>%
  select(-Sukupuolet)

Tyottomat_abs <- tyottomat %>%
  select(-Tyottomyysaste2010,-Tyottomyysaste2014) %>%
  subset(Sukupuolet == "Molemmat") %>%
  select(-Sukupuolet)

finalabs <- left_join(finalabs, Tyottomat_abs, by = "Alue")
finaldf <- left_join(finaldf, Tyottomat, by = "Alue")

# vaesto excel
vaesto_xlsx <- file.path("/Users/veikko/Desktop/GIS/Code/Osaaluedata", "vaesto_ja_kieli_m.xlsx")

vaesto <- read.xlsx2(vaesto_xlsx, sheetIndex = 1, header = T, startRow = 1, endRow = 5662)
vaesto[vaesto == ".." | vaesto == "."] <- NA

Vaesto_2011 <- vaesto %>%
  dcast(Alue ~ Ikaryhma, value.var = "Kaikki_aidinkielet2011")

# Vaesto_2011[,2:38]
# as.data.frame(lapply(Vaesto_2011[,2:38],as.numeric))

Vaesto_2011[,2:38] <- Vaesto_2011 %>%
  select(-Alue) %>%
  lapply(.,as.numeric)

# 31 to 37 --- eläkeläiset 65+
# 27 to 30   --- 45-64
# 24 to 26 --- 30-44
# 20 to 23 --- 18-29
# 2 to 19 ---  0-17
Vaesto_m <- Vaesto_2011 %>%
  select(1)
Vaesto_abs <- Vaesto_m

Vaesto_m["0-17v_2011"] <- rowSums(Vaesto_2011[,2:19]) / Vaesto_2011[, 38]
Vaesto_m["18-29v_2011"] <- rowSums(Vaesto_2011[,20:23]) / Vaesto_2011[, 38]
Vaesto_m["30-44v_2011"] <- rowSums(Vaesto_2011[,24:26]) / Vaesto_2011[, 38]
Vaesto_m["45-64v_2011"] <- rowSums(Vaesto_2011[,27:30]) / Vaesto_2011[, 38]
Vaesto_m["65-v_2011"] <- rowSums(Vaesto_2011[,31:37]) / Vaesto_2011[, 38]

Vaesto_abs["0-17v_2011"] <- rowSums(Vaesto_2011[,2:19])
Vaesto_abs["18-29v_2011"] <- rowSums(Vaesto_2011[,20:23]) 
Vaesto_abs["30-44v_2011"] <- rowSums(Vaesto_2011[,24:26]) 
Vaesto_abs["45-64v_2011"] <- rowSums(Vaesto_2011[,27:30])
Vaesto_abs["65-v_2011"] <- rowSums(Vaesto_2011[,31:37])
Vaesto_abs["All_agegroups_2011"] <- Vaesto_2011[, 38]

Vaesto_2015 <- vaesto %>%
  dcast(Alue ~ Ikaryhma, value.var = "Kaikki_aidinkielet2015")
Vaesto_2015[,2:38] <- Vaesto_2015 %>%
  select(-Alue) %>%
  lapply(.,as.numeric)

Vaesto_m["0-17v_2015"] <- rowSums(Vaesto_2015[,2:19]) / Vaesto_2015[, 38]
Vaesto_m["18-29v_2015"] <- rowSums(Vaesto_2015[,20:23]) / Vaesto_2015[, 38]
Vaesto_m["30-44v_2015"] <- rowSums(Vaesto_2015[,24:26]) / Vaesto_2015[, 38]
Vaesto_m["45-64v_2015"] <- rowSums(Vaesto_2015[,27:30]) / Vaesto_2015[, 38]
Vaesto_m["65-v_2015"] <- rowSums(Vaesto_2015[,31:37]) / Vaesto_2015[, 38]

Vaesto_abs["0-17v_2015"] <- rowSums(Vaesto_2015[,2:19])
Vaesto_abs["18-29v_2015"] <- rowSums(Vaesto_2015[,20:23])
Vaesto_abs["30-44v_2015"] <- rowSums(Vaesto_2015[,24:26])
Vaesto_abs["45-64v_2015"] <- rowSums(Vaesto_2015[,27:30]) 
Vaesto_abs["65-v_2015"] <- rowSums(Vaesto_2015[,31:37])
Vaesto_abs["All_agegroups_2015"] <- Vaesto_2015[, 38]

# Ruotsin kieliset
colnames(vaesto)

Ruotsi_2011 <- vaesto %>%
  dcast(Alue ~ Ikaryhma, value.var = "Ruotsi2011")

Ruotsi_2011[,2:38] <- Ruotsi_2011 %>%
  select(-Alue) %>%
  lapply(.,as.numeric)

Vaesto_m["Ruotsinkielisia_2011"]<- Ruotsi_2011$`Väestö yhteensä` / Vaesto_2011$`Väestö yhteensä`

Vaesto_abs["Ruotsinkielisia_2011"]<- Ruotsi_2011$`Väestö yhteensä`
Vaesto_abs["Kokovaesto_2011"] <- Vaesto_2011$`Väestö yhteensä`

Ruotsi_2015 <- vaesto %>%
  dcast(Alue ~ Ikaryhma, value.var = "Ruotsi2015")

Ruotsi_2015[,2:38] <- Ruotsi_2015 %>%
  select(-Alue) %>%
  lapply(.,as.numeric)

Vaesto_m["Ruotsinkielisia_2015"]<- Ruotsi_2015$`Väestö yhteensä` / Vaesto_2015$`Väestö yhteensä`

Vaesto_abs["Ruotsinkielisia_2015"]<- Ruotsi_2015$`Väestö yhteensä`
Vaesto_abs["Kokovaesto_2015"] <- Vaesto_2015$`Väestö yhteensä`

finalabs <- left_join(finalabs, Vaesto_abs, by = "Alue")
finaldf <- left_join(finaldf, Vaesto_m, by = "Alue")

# Tuloluokat

tulot_xlsx <- file.path("/Users/veikko/Desktop/GIS/Code/Osaaluedata", "tulonjako_m.xlsx")
tulotabs_xlsx <- file.path("/Users/veikko/Desktop/GIS/Code/Osaaluedata", "tulonjako_abs.xlsx")

tulot <- read.xlsx2(tulot_xlsx, sheetIndex = 1, header = T, startRow = 1, endRow = 1684)
tulotabs <- read.xlsx2(tulotabs_xlsx, sheetIndex = 1, header = T, startRow = 1, endRow = 1684)

tulotabs[tulotabs == ".." | tulotabs == "."] <- NA
tulot[tulot == ".." | tulot == "."] <- NA

tulot[,3] <- as.numeric(gsub(",", ".", tulot[,3]))
tulot[,4] <- as.numeric(gsub(",", ".", tulot[,4]))

tulotabs[,3:4] <- tulotabs[, 3:4] %>%
  lapply(., function(x){as.numeric(as.character(x))})

Tulot_2011 <- tulot %>%
  dcast(Alue ~ Desiili, value.var = "X2011")
Tulotabs_2011 <- tulotabs %>%
  dcast(Alue ~ Desiili, value.var = "X2011")

Tulot_abs <- Tulotabs_2011 %>%
  select(1)
Tulot_m <- Tulot_2011 %>%
  select(1)

Tulot_m["SuurituloisinX_2011"]<- Tulot_2011$`X (suurituloisin 10%)`
Tulot_m["Allemediaanitulot_2011"]<-rowSums(Tulot_2011[,3:6],  na.rm=TRUE) # jos ei ole mitaa arvoja ni tekee nollan

Tulot_abs["SuurituloisinX_2011"]<- Tulotabs_2011$`X (suurituloisin 10%)`
Tulot_abs["Allemediaanitulot_2011"]<-rowSums(Tulotabs_2011[,3:6],  na.rm=TRUE)
Tulot_abs["Kaik_tuloluokat_2011"]<- Tulotabs_2011$`Desiililuokat yhteensä`

Tulot_2014 <- tulot %>%
  dcast(Alue ~ Desiili, value.var = "X2014")
Tulotabs_2014 <- tulotabs %>%
  dcast(Alue ~ Desiili, value.var = "X2014")

Tulot_m["SuurituloisinX_2014"]<- Tulot_2014$`X (suurituloisin 10%)`
Tulot_m["Allemediaanitulot_2014"]<-rowSums(Tulot_2014[,3:6],  na.rm=TRUE) 

Tulot_abs["SuurituloisinX_2014"]<- Tulotabs_2014$`X (suurituloisin 10%)`
Tulot_abs["Allemediaanitulot_2014"]<-rowSums(Tulotabs_2014[,3:6],  na.rm=TRUE)
Tulot_abs["Kaik_tuloluokat_2014"]<- Tulotabs_2014$`Desiililuokat yhteensä`

finalabs <- left_join(finalabs, Tulot_abs, by = "Alue")
finaldf <- left_join(finaldf, Tulot_m, by = "Alue")


# Nämä on ok!
finalabs[,2] <- as.numeric(gsub(",", ".", finalabs[,2]))
finalabs[,5] <- as.numeric(gsub(",", ".", finalabs[,5]))

finalabs[,3:4] <- finalabs[,3:4] %>%
  lapply(.,as.numeric)
finalabs[,6:8] <- finalabs[,6:8] %>%
  lapply(.,as.numeric)
#finalabs[,12:13] <- finalabs[,12:13] %>%
#  lapply(.,as.numeric)
finalabs[,12] <- as.numeric(gsub(",", ".", finalabs[,12]))
finalabs[,13] <- as.numeric(gsub(",", ".", finalabs[,13]))
finalabs[,14] <- as.numeric(gsub(",", ".", finalabs[,14]))
finalabs[,15] <- as.numeric(gsub(",", ".", finalabs[,15]))

finalabs[,14:15] <- finalabs[,14:15] %>%
  round(., digits = 0)

# final df
finaldf[,3:4] <- finaldf[,3:4] %>%
  lapply(.,as.numeric)

finaldf[,6:7] <- finaldf[,6:7] %>%
  lapply(.,as.numeric)

finaldf[,2] <- as.numeric(gsub(",", ".", finaldf[,2]))
finaldf[,5] <- as.numeric(gsub(",", ".", finaldf[,5]))
finaldf[,10] <- as.numeric(gsub(",", ".", finaldf[,10]))
finaldf[,11] <- as.numeric(gsub(",", ".", finaldf[,11]))

summary(finaldf)
summary(finalabs)

# alueen tyopaikat ei siis siella asuvien tyopaikat älä aja!
tyopaikat_xlsx <- file.path("/Users/veikko/Desktop/GIS/Code/Osaaluedata", "tyopaikat_m.xlsx")

tyopaikat <- read.xlsx2(tyopaikat_xlsx, sheetIndex = 1, header = T, startRow = 1, endRow = 14689)
tyopaikat[tyopaikat == ".." | tyopaikat == "."] <- NA

tyopaikat <- tyopaikat %>%
  subset(.$TOIMIALAT == "Kaikki toimialat yhteensä" | .$TOIMIALAT == "C Teollisuus" | .$TOIMIALAT == "K Rahoitus- ja vakuutustoiminta" | .$TOIMIALAT == "O Julkinen hallinto ja maanpuolustus" | .$TOIMIALAT == "Q Terveys- ja sosiaalipalvelut")

Tyopaikat_2010 <- tyopaikat %>%
  dcast(Alue ~ TOIMIALAT, value.var = "X2010")

Tyopaikat_2010[,2:6] <- Tyopaikat_2010[,2:6] %>%
  lapply(.,as.numeric)

Tyopaikat_m <- Tyopaikat_2010 %>%
  select(1)

Tyopaikat_m["Teollisuus_2010"] <- round(Tyopaikat_2010[,2] / Tyopaikat_2010[,4], digits = 4)
Tyopaikat_m["Rahoitus_2010"] <- round(Tyopaikat_2010[,3] / Tyopaikat_2010[,4], digits = 4)
Tyopaikat_m["Julk_hallinto_2010"] <- round(Tyopaikat_2010[,5] / Tyopaikat_2010[,4], digits = 4)
Tyopaikat_m["Sos_terveys_2010"] <- round(Tyopaikat_2010[,6] / Tyopaikat_2010[,4] , digits = 4)


Tyopaikat_2013 <- tyopaikat %>%
  dcast(Alue ~ TOIMIALAT, value.var = "X2013")

Tyopaikat_2013[,2:6] <- Tyopaikat_2013[,2:6] %>%
  lapply(.,as.numeric)

Tyopaikat_m["Teollisuus_2013"] <- round(Tyopaikat_2013[,2] / Tyopaikat_2013[,4], digits = 4)
Tyopaikat_m["Rahoitus_2013"] <- round(Tyopaikat_2013[,3] / Tyopaikat_2013[,4], digits = 4)
Tyopaikat_m["Julk_hallinto_2013"] <- round(Tyopaikat_2013[,5] / Tyopaikat_2013[,4], digits = 4)
Tyopaikat_m["Sos_terveys_2013"] <- round(Tyopaikat_2013[,6] / Tyopaikat_2013[,4] , digits = 4)

finaldf <- left_join(finaldf, Tyopaikat_m, by = "Alue")

# sukupuolijako
sukupuoli_xlsx <- file.path("/Users/veikko/Desktop/GIS/Code/Osaaluedata", "sukupuoli_m.xlsx")

sukupuoli <- read.xlsx2(sukupuoli_xlsx, sheetIndex = 1, header = T, startRow = 1, endRow = 460)
sukupuoli[sukupuoli == ".." | sukupuoli == "."] <- NA

sukupuoli[,3:4] <- sukupuoli[,3:4] %>%
  lapply(., function(x){as.numeric(as.character(x))})

Sukupuoli_2011 <- sukupuoli %>%
  dcast(Alue ~ Sukupuoli, value.var = "X2011")

Sukupuoli_m <- Sukupuoli_2011 %>%
  select(1)

Sukupuoli_abs <- Sukupuoli_m

Sukupuoli_m["Naisia_2011"] <- round(Sukupuoli_2011[,3] / Sukupuoli_2011[,4], digits = 4)
Sukupuoli_abs["Naisia_2011"] <- Sukupuoli_2011[,3]
Sukupuoli_abs["Bothsexes_2011"] <- Sukupuoli_2011[,4]

Sukupuoli_2015 <- sukupuoli %>%
  dcast(Alue ~ Sukupuoli, value.var = "X2015")

Sukupuoli_m["Naisia_2015"] <- round(Sukupuoli_2015[,3] / Sukupuoli_2015[,4], digits = 4)

Sukupuoli_abs["Naisia_2015"] <- Sukupuoli_2015[,3]
Sukupuoli_abs["Both_sexes_2015"] <- Sukupuoli_2015[,4]

finaldf <- left_join(finaldf, Sukupuoli_m, by = "Alue")
finalabs <- left_join(finalabs, Sukupuoli_abs, by = "Alue")

# loppu käsittely

finaldf2 <- finaldf %>%
  separate(Alue, into = c("kuntakoodi","osaalue_nro", "osaalue_nimi", "erityiskirjain"), sep = " ")

finalabs2  <- finalabs %>%
  separate(Alue, into = c("kuntakoodi","osaalue_nro", "osaalue_nimi", "erityiskirjain"), sep = " ")

finaldf2$erityiskirjain <- finaldf2$erityiskirjain %>%
  ifelse(is.na(.), "", .)

finalabs2$erityiskirjain <- finalabs2$erityiskirjain %>%
  ifelse(is.na(.), "", .)

finaldf2 <- unite(finaldf2, "osaalue_nimi", osaalue_nimi, erityiskirjain, sep = " ")
finalabs2 <- unite(finalabs2, "osaalue_nimi", osaalue_nimi, erityiskirjain, sep = " ")

# finaldf2[,4: length(finaldf2)] <- finaldf2[,4: length(finaldf2)] %>%
#  lapply(function(x){round(x, digits = 4)}) 

finaldf2[,3] <- sapply(finaldf2[,3], function(x){
  if (strtail(x) == " ") 
    gsub('.{1}$', '', x)
  else
    x
})

finalabs2[,3] <- sapply(finalabs2[,3], function(x){
  if (strtail(x) == " ") 
    gsub('.{1}$', '', x)
  else
    x
})



finaldf2[,3] <- sapply(finaldf2[,3], function(x){
  if(grepl('\\(Ent.$', x))
    gsub('.{6}$', '', x)
  else x
})

finalabs2[,3] <- sapply(finalabs2[,3], function(x){
  if(grepl('\\(Ent.$', x))
    gsub('.{6}$', '', x)
  else x
})

# jaa vielä 2011 ja 2015 datasetteihin!
write.csv(finaldf2, file = "analyysimuuttujat.csv", fileEncoding = "utf-8", row.names=FALSE)
write.csv(finalabs2, file = "analyysimuuttujat_abs.csv", fileEncoding = "utf-8", row.names=FALSE)


osadata2015 <- finalabs2 %>%
  dplyr::select( kuntakoodi, osaalue_nro, osaalue_nimi, contains("2015"), contains("2014"))
  
osadata2011 <- finalabs2 %>%
  select( kuntakoodi, osaalue_nro, osaalue_nimi, contains("2011"), contains("2010"))

osadata2015 <- osadata2015[1:151,] 
osadata2011 <- osadata2011[1:151,] 

 write.csv(osadata2015_m, file = "osadata2015_m.csv", fileEncoding = "utf-8", row.names=FALSE)
# write.csv(osadata2011, file = "osadata2011.csv", fileEncoding = "utf-8", row.names=FALSE)

# otetaan NA:t pois, mutta ensin katsotaan milta tiedostot nayttavat!

osadata2015[which(osadata2015$osaalue_nro == 580), 17] <- 18  # Karhusaari 
osadata2015[32, 21] <- 0 # Kyläsaari 

# osadata2015_m <- osadata2015 %>% 
#  select(-(18:length(osadata2015)))

# osadata2011_m <- osadata2011 %>%
#  select(-(18:length(osadata2011)))

puuttuu2015 <- as.data.frame(osadata2015[rowSums(is.na(osadata2015)) > 0, 3])
puuttuu2011 <- as.data.frame(osadata2011[rowSums(is.na(osadata2011)) > 0, 3])

osadata2015_m <- na.omit(osadata2015)
osadata2011_m <- na.omit(osadata2011)


library(gdata)
gdata::keep(osadata2015_m,finalabs2, sure = T)
