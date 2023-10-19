library(readr)
library(tibble)
schlenk <- read_delim("schlenk.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
#dbWriteTable(con, "predictiontable", schlenk, overwrite = TRUE)
### Final Equation in Terms of Actual Factors

# STEM average particle thickness	 =
#   trimming	ja
# -150,60969656913	
# 0,0556071422534	Temperature cooling drum 
# 1,8201296778727	dry coat weight 8 passes
# 0,20008880996948	U/W Ten. Average 8 layers
# 7,6896920612799	boat power
# 35,223698345715	resistivity
# 0,85153009333327	wire feed rate
# -0,22586487804127	solid content of  ink (release coat)
# 1,6141266933512	normalized wire feed rate
# -18197,931326022	evaporation pressure
# -0,069805356245676	U/W Ten. Average 8 layers * resistivity
# -0,094374156835885	boat power * wire feed rate
# 1602,1826378525	boat power * evaporation pressure
# 2426,3587370466	normalized wire feed rate * evaporation pressure



levels(schlenk$trimming)

options(contrasts = c("contr.sum", "contr.poly"))
mod_lm <- lm(`STEM average particle thickness` ~  `Temperature cooling drum` + `dry coat weight 8 passes` + 
              `trimming`+ `U/W Ten. Average 8 layers` + `boat power` + `resistivity` + `wire feed rate` + 
              `solid content of ink (release coat)` + `normalized wire feed rate` + `evaporation pressure` + 
              `U/W Ten. Average 8 layers` : `resistivity` + `boat power` : `wire feed rate` + 
              `boat power` : `evaporation pressure` + `normalized wire feed rate` : `evaporation pressure` +
               `trimming` : `resistivity`,
               contrasts = list(`trimming` = "contr.sum"), data = schlenk)

summary(mod_lm )




new_obs <- head(schlenk,1)

test <- predict(mod_lm, newdata = new_obs, interval = "confidence")
round(test,2)
colnames(test)

options(contrasts = c("contr.treatment", "contr.poly"))
