library(odbc)
library(magrittr)
library(dplyr)
library(DBI)
library(tidyverse)
library(janitor)
library(reshape2)

# schlenks server db connection con1

con1 <- DBI::dbConnect(odbc(),
                       Driver = "ODBC Driver 18 for SQL Server",
                       Server = "srvsqlex.schlenk.corp",
                       TrustServerCertificate = "yes",
                       Database = "processdb",
                       uid = "statcon",
                       pwd = "16simulierteTiger?")

dat_names <- dbGetQuery(con1, "SELECT * FROM INFORMATION_SCHEMA.TABLES;") %>% pull(TABLE_NAME)

#dat_names <- c("moca_labor", "shiny", "sica_labor","spezification_neu","spezification","vmp_metallizationdata","batch","qc_VP13240","measurement","lab" )
dat_names <- c("shiny", "spezification_neu","spezification","batch","qc_VP13240","measurement","lab", "shiny", "specification" )


for (i in dat_names){
  
  data_schlenktables <- i
  
  assign(data_schlenktables , dbGetQuery(con1, paste0("SELECT * FROM processdb.dbo.", i, sep="")))
  
}

dbDisconnect(con1)

con <- DBI::dbConnect(odbc(),
                      Driver = "ODBC Driver 18 for SQL Server",
                      Server = "10.0.3.179,1433",
                      TrustServerCertificate = "yes",
                      Database = "stat01",
                      uid = "statcon",
                      pwd = "akhtQW_918273")  



shiny$measure  <- replace(shiny$measure,which(shiny$measure == "t-Span" ), "t-Span groß")
shiny$messwert <- paste(shiny$measuregroup, shiny$measure, sep="_")
shiny$materialnumber <- as.factor(shiny$materialnumber)

#vmp_metallizationdata <- vmp_metallizationdata %>% group_by(motherroll) %>% summarise(max(metdate))
#decomet <- merge(vmp_metallizationdata, qc_VP13240, by = "motherroll", all.y=TRUE)
decomet <- qc_VP13240

#Namen ändern
names(decomet)[names(decomet) == 'max(metdate)'] <- 'Herstellungsdatum'
names(decomet)[names(decomet) == 'sampledate'] <- 'Fertigungsdatum'
names(decomet)[names(decomet) == 'process'] <- 'Prozess'
names(decomet)[names(decomet) == 'manufacturer'] <- 'Lieferant'

#Vmp Batch Vorverarbeitung
#decomet$vmp_batch <- ifelse( startsWith(decomet$vmp_batch , "RH") , substring(decomet$vmp_batch, 3), decomet$vmp_batch)
decomet$vmp_batch <- as.numeric(decomet$vmp_batch)
decomet <- decomet[order( decomet$vmp_batch, decreasing = F ), ]
decomet$vmp_batch <- as.character(decomet$vmp_batch)
decomet$PSD90[212] <- 37.6
#
dbWriteTable(con, "Decomet_Prozess", decomet, overwrite = TRUE)


for (i in dat_names){
  
  data_statcontables <- i
  dbWriteTable(con, data_statcontables, eval(as.symbol(data_statcontables)), overwrite = TRUE)
  
}

########## Produktionschargen
###Moca
mocaidx <- grep("MOCA",shiny$material)
moca_production <- shiny[mocaidx,]
moca_production <- moca_production[order(moca_production$batch) , ]
moca_production <- moca_production %>% filter(batch != "")
moca_production$measure_value <- as.numeric(moca_production$measure_value)

names_vec_moca <- unique(moca_production$measure)
moca_production2 <- moca_production %>% pivot_wider(names_from = measure, values_from = measure_value, values_fill = NA)
grouping_vars_moca <- colnames(moca_production2[2:5])
moca_production3 <- moca_production2 %>%
  group_by(across(all_of(grouping_vars_moca)))%>%
  summarise_at(names_vec_moca,mean, na.rm = TRUE)


moca_production3[is.na(moca_production3)] <- NA

dbWriteTable(con, "MocaProduktion", moca_production3, overwrite = TRUE)


###Sica
sicaidx <- grep("SICA",shiny$material)
sica_production <- shiny[sicaidx,]
sica_production <- sica_production[order(sica_production$batch) , ]
sica_production <- sica_production %>% filter(batch != "")
names_vec_sica <-  unique(sica_production$measure)
sica_production$measure_value <- as.numeric(sica_production$measure_value)

sica_production2 <- sica_production %>% pivot_wider(names_from = measure, values_from = measure_value, values_fill = NA)
grouping_vars_sica <- colnames(sica_production2[2:5])
sica_production3 <- sica_production2 %>%
  group_by(across(all_of(grouping_vars_sica)))%>%
  summarise_at(names_vec_sica,mean, na.rm = TRUE)


sica_production3[is.na(sica_production3)] <- NA
dbWriteTable(con, "SicaProduktion", sica_production3, overwrite = TRUE)

#############################  VP Chargen

test <- shiny

test <- test[order(test$batch) , ]

test <- test %>% filter(batch != "")

test$measure_value <- as.numeric(test$measure_value)

test$measure <- gsub(" ", "", test$measure, fixed = TRUE)
test$measure <- gsub("°", "", test$measure, fixed = TRUE)

#names_vec <- unique(test$measure)

names_vec <- unique(test$messwert)
names_vec <- c("measurementdate", names_vec)

#test2 <- test %>% pivot_wider(names_from = measure, values_from = measure_value, values_fill = NA)

test2 <- test %>% pivot_wider(names_from = messwert, values_from = measure_value, values_fill = NA)


#grouping_vars <- colnames(test2[2:5])

grouping_vars <- colnames(test2[2:4])


test3 <- test2 %>%
  group_by(across(all_of(grouping_vars)))%>%
  summarise_at(names_vec,mean, na.rm = TRUE)


test3[is.na(test3)] <- NA



colnames(test3) <- gsub(" ", "", colnames(test3), fixed = TRUE)
colnames(test3) <- gsub("°", "", colnames(test3), fixed = TRUE)

spezification_neu$measure <- gsub(" ", "", spezification_neu$measure, fixed = TRUE)
spezification_neu$measure <- gsub("°", "", spezification_neu$measure, fixed = TRUE)

spezification_neu$product <- gsub("/", "", spezification_neu$product, fixed = TRUE)
spezification_neu$product <- gsub("\\", "", spezification_neu$product, fixed = TRUE)
spezification_neu$product <- gsub(" ", "", spezification_neu$product, fixed = TRUE)
spezification_neu$product <- gsub("HALB", "", spezification_neu$product, fixed = TRUE)
spezification_neu$product <- gsub("Decomet", "", spezification_neu$product, fixed = TRUE)
spezification_neu[spezification_neu$product == "MOCA_21_3_OO_GEM",]$product <- "MOCA_3_21_OO_GEM" 
spezification_neu[spezification_neu$product == "MOCA_21_3_OO_HOM",]$product <- "MOCA_3_21_OO_HOM"
spezification_neu$measure <- gsub("LaserNass", "", spezification_neu$measure, fixed = TRUE)

idx <- which(spezification$measure=="Aceton")

spezification$measure[idx] <- "aceton"

idx_error <- which(spezification_neu$measure=="Anteilkleinerals12nm")

spezification_neu <- spezification_neu[-c(idx_error),]

spezification_neu <- spezification_neu[-200,]
spezification_neu <- spezification_neu[-199,]


#Neue Spec also neu neu
specification$material  <- replace(specification$material,which(specification$material == "Decomet VP/13240" ), "VP13240")
specification$material  <- replace(specification$material,which(specification$material == "Decomet VP/14087" ), "VP14087")

specification$measure <- gsub(" ", "", specification$measure, fixed = TRUE)
specification$measuregroup_measure <- paste(specification$measuregroup,specification$measure, sep="_")
specification$measuregroup_measure<- gsub(" ", "", specification$measuregroup_measure, fixed = TRUE)

dbWriteTable(con, "specification", specification, overwrite = TRUE)

batch$batch_material<- gsub("Decomet", "", batch$batch_material, fixed = TRUE)
batch$batch_material<- gsub("_", "", batch$batch_material, fixed = TRUE)
batch$batch_material<- gsub("/", "", batch$batch_material, fixed = TRUE)
batch$batch_material<- gsub("\\", "", batch$batch_material, fixed = TRUE)
batch$batch_material<- gsub("HALB", "", batch$batch_material, fixed = TRUE)
batch$batch_material<- gsub(" ", "", batch$batch_material, fixed = TRUE)
#batch[batch$batch_material== "VP13240HALB",]$batch_material <- "VP13240"
#batch[batch$batch_material== "VP14087HALB",]$batch_material <- "VP14087"


dbWriteTable(con, "spezification_neu", spezification_neu, overwrite = TRUE)

alpha <- lab

alpha <- alpha[order(alpha$predecessor) , ]

alpha <- alpha %>% filter(predecessor != "")

alpha$measure_value <- gsub("\\,",  ".", alpha$measuredvalue)
alpha$measure_value <- as.numeric(alpha$measure_value)

names_alpha <- unique(alpha$measure)

alpha2 <- alpha %>% pivot_wider(names_from = measure, values_from = measuredvalue, values_fill = NA)

alpha2$batch <- trimws(alpha2$batch)

grouping_vars <- colnames(alpha2[2:5])
grouping_vars <- c(grouping_vars, "application")

alpha3 <- alpha2 %>%
  group_by(across(all_of(grouping_vars)))%>%
  summarise_at(names_alpha,mean, na.rm = TRUE)


alpha3[is.na(alpha3)] <- NA

SLY <- dplyr::filter(alpha3, grepl("SLY ",batch))
SLY <- SLY[ , colSums(is.na(SLY)) < nrow(SLY)]

dbWriteTable(con, "SicaLabor", SLY, overwrite = TRUE)

FE <- dplyr::filter(alpha3, grepl("Fe",batch))
FE <- FE[ , colSums(is.na(FE)) < nrow(FE)]

dbWriteTable(con, "MocaLabor", FE, overwrite = TRUE)

#FE$predecessor[1] <- "SLY 1711"

alpha_final <- merge(FE,SLY,by.x="predecessor", by.y="batch")
idx_alpha_final <-  grep("dd",  names(alpha_final))
alpha_final <- alpha_final[ ,-c(idx_alpha_final)]

dbWriteTable(con, "alpha", alpha_final, overwrite = TRUE)

late_merge <- merge(test3, batch, by= "batch")


#test4 <- merge(test3, batch, by= "batch", all.x = T)  

late_merge$material <- gsub(" HALB", "", late_merge$material, fixed = TRUE)
late_merge$batch_material <- gsub("HALB", "", late_merge$batch_material, fixed = TRUE)
late_merge$material <- gsub(" HAL", "", late_merge$material, fixed = TRUE)
late_merge$batch_material <- gsub("HAL", "", late_merge$batch_material, fixed = TRUE)
late_merge$material <- gsub("GEM", "", late_merge$material, fixed = TRUE)
late_merge$batch_material <- gsub("GEM", "", late_merge$batch_material, fixed = TRUE)
late_merge$material <- gsub("/", "", late_merge$material, fixed = TRUE)
late_merge$batch_material <- gsub("/", "", late_merge$batch_material, fixed = TRUE)
late_merge$material <- gsub("Decomet ", "", late_merge$material, fixed = TRUE)
late_merge$batch_material <- gsub("Decomet ", "", late_merge$batch_material, fixed = TRUE)
late_merge$material <- gsub(" ", "", late_merge$material, fixed = TRUE)
late_merge$batch_material <- gsub(" ", "", late_merge$batch_material, fixed = TRUE)


product_vars <- sort(unique(late_merge$batch_material))

#product_vars <- names(split_df)

product_vars <- gsub("/","", product_vars)

product_vars <- gsub("_","", product_vars)

product_vars <- gsub(" ","", product_vars)

split_df <- split(late_merge, f = late_merge$batch_material)


# for (i in seq(split_df)){
#   assign(product_vars[6], split_df[[6]])
# 
#   unique(eval(as.symbol(product_vars[6]))$product)
# 
#   idx <- which(spezification_neu$product==unique(eval(as.symbol(product_vars[6]))$product))
# 
#   selected_columns <- unique(na.omit(spezification_neu[idx,"measure"]))
# 
# 
#   selected_columns <-  c("batch","sampledate","predecessor","predecessor_product","materialname","material_SAP", selected_columns )
# 
#   #if(anyNA(selected_columns)==F)
#   assign(product_vars[6], dplyr::select(eval(as.symbol(product_vars[6])), selected_columns))
# 
# }


for (i in seq(split_df)){
  assign(product_vars[i], split_df[[i]])
}

VP13240 <- remove_empty(VP13240, which = c("cols"))
VP14087 <- remove_empty(VP14087, which = c("cols"))

names(VP13240)[names(VP13240) == 'batch_manufacturingdate'] <- 'Fertigungsdatum'
names(VP13240)[names(VP13240) == 'predecessor_manufacturingdate'] <- 'Herstellungsdatum'
names(VP14087)[names(VP14087) == 'batch_manufacturingdate'] <- 'Fertigungsdatum'
names(VP14087)[names(VP14087) == 'predecessor_manufacturingdate'] <- 'Herstellungsdatum'
names(sica_production)[names(sica_production) == 'measurementdate'] <- 'Herstellungsdatum'
names(sica_production)[names(sica_production) == 'measurementdate'] <- 'Fertigungsdatum'
names(moca_production)[names(sica_production) == 'measurementdate'] <- 'Herstellungsdatum'
names(moca_production)[names(sica_production) == 'measurementdate'] <- 'Fertigungsdatum'


for (i in product_vars){
  
  data_statcontables <- i
  if(ncol(eval(as.symbol(data_statcontables))) > 1)
    
    dbWriteTable(con, data_statcontables, eval(as.symbol(data_statcontables)), overwrite = TRUE)
  
}




