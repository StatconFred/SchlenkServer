### Connection
con1 <- DBI::dbConnect(odbc(),
                       Driver = "ODBC Driver 18 for SQL Server",
                       Server = "srvsqlex.schlenk.corp",
                       TrustServerCertificate = "yes",
                       Database = "processdb",
                       uid = "statcon",
                       pwd = "16simulierteTiger?")


vmp_metallizationdata <- dbGetQuery(con1, paste0("SELECT * FROM processdb.dbo.", "vmp_metallizationdata", sep=""))

dbDisconnect(con1)

vmp_metallizationdata_filtered <- vmp_metallizationdata %>% filter(weblength > 500)
vmp_metallizationdata_filtered <- vmp_metallizationdata_filtered %>% filter(weblength < 39500)
vmp_metallizationdata_filtered <- vmp_metallizationdata_filtered %>% filter(webspeed > 250)


vmp_metallizationdata_filtered <- vmp_metallizationdata_filtered %>% group_by(motherroll) %>% summarise(across(everything(), list(mean)))

vmp_metallizationdata_filtered$motherroll <- as.character(vmp_metallizationdata_filtered$motherroll)
vmp_metallizationdata_filtered$motherroll <- ifelse(nchar(vmp_metallizationdata_filtered$motherroll)< 10, paste0("60000",vmp_metallizationdata_filtered$motherroll),vmp_metallizationdata_filtered$motherroll)

vmp_metallizationdata_filtered_joined_decomet <- merge(vmp_metallizationdata_filtered, VP13240, by.x="motherroll", by.y="predecessor")

vmp_decomet_sly <- merge(vmp_metallizationdata_filtered_joined_decomet, SLY, by.x="batch", by.y="predecessor" )

FE$predecessor <- sub("_"," ", FE$predecessor)
vmp_decomet_sly_fe <- merge(vmp_decomet_sly, FE, by.x="batch.y", by.y = "predecessor")


FE$predecessor <- sub("_"," ", FE$predecessor)

#Daten schreiben
con <- DBI::dbConnect(odbc(),
                      Driver = "ODBC Driver 18 for SQL Server",
                      Server = "10.0.3.179,1433",
                      TrustServerCertificate = "yes",
                      Database = "stat01",
                      uid = "statcon",
                      pwd = "akhtQW_918273")  1

dbWriteTable(con, "zenexo", vmp_decomet_sly_fe, overwrite = TRUE)

write.csv(batch,"batch.csv")

#melt(vmp_metallizationdata_filtered, id.var = c("motherroll"), measure = 12:13)



#stack(vmp_metallizationdata_filtered[12:13])#

write.csv(batch,"metallizationdata.csv")

dbWriteTable(con, "shiny", shiny, overwrite = TRUE)
