library(xlsx)
library(strucchange)

setwd("E:/M.Sc. Semester III/Market Incentives and Agriculture in India/Term Paper/")


## read crop productivity data for 9 select crops into a dataframe
prod_df <- read.xlsx(file = 'agricultural_productivity.xls', sheetIndex = 'Sheet1', rowIndex = 8:65, colIndex = c(2:7,10,13:16,25:27), header = FALSE)
crops <- c('Rice','Wheat','Bajra','Jowar','Maize','Barley','Coarse_Cereals','Total_Pulses','Foodgrains','Groundnut','Sugarcane','RapeseedMustard','Cotton','Jute')
colnames(prod_df) <- crops

## store crop productivity data for different crops in different ts objects
for(i in index(crops)){
  ts_obj <- ts(prod_df[,crops[i]], start=c(1951, 1), end=c(2008, 1), frequency=1)
  assign(crops[i], ts_obj)
}

## store soyabean productivity data separately because it starts from 1971
soya <- read.xlsx(file = 'agricultural_productivity.xls', sheetIndex = 'Sheet1', rowIndex = 28:65, colIndex = 18, header = FALSE)
colnames(soya) <- 'Soyabean'
Soyabean <- ts(soya, start = c(1971,1), end = c(2008,1), frequency = 1)
################################################# Rice
bp.Rice <- breakpoints(Rice ~ 1)
summary(bp.Rice)
 
## the BIC chooses 5 breakpoints though
plot(bp.Rice)
plot(Rice, main = 'Structural Breaks in Rice Productivity', ylab = 'Rice Productivity (Kg/Hectare)')
lines(breakpoints(bp.Rice))

## confidence interval
ci.Rice <- confint(bp.Rice)
lines(ci.Rice)

################################################# Wheat
bp.Wheat <- breakpoints(Wheat ~ 1)
summary(bp.Wheat)

## the BIC chooses 5 breakpoints though
plot(bp.Wheat)
plot(Wheat, main = 'Structural Breaks in Wheat Productivity', ylab = 'Wheat Productivity (Kg/Hectare)')
lines(breakpoints(bp.Wheat))

## confidence interval
ci.Wheat <- confint(bp.Wheat)
lines(ci.Wheat)

################################################# Bajra
bp.Bajra <- breakpoints(Bajra ~ 1)
summary(bp.Bajra)

## the BIC chooses 5 breakpoints though
plot(bp.Bajra)
plot(Bajra, main = 'Structural Breaks in Bajra Productivity', ylab = 'Bajra Productivity (Kg/Hectare)')
lines(breakpoints(bp.Bajra))

## confidence interval
ci.Bajra <- confint(bp.Bajra)
lines(ci.Bajra)

################################################# Jowar
bp.Jowar <- breakpoints(Jowar ~ 1)
summary(bp.Jowar)

## the BIC chooses 5 breakpoints though
plot(bp.Jowar)
plot(Jowar, main = 'Structural Breaks in Jowar Productivity', ylab = 'Jowar Productivity (Kg/Hectare)')
lines(breakpoints(bp.Jowar))

## confidence interval
ci.Jowar <- confint(bp.Jowar)
lines(ci.Jowar)

################################################# Maize
bp.Maize <- breakpoints(Maize ~ 1)
summary(bp.Maize)

## the BIC chooses 5 breakpoints though
plot(bp.Maize)
plot(Maize, main = 'Structural Breaks in Maize Productivity', ylab = 'Maize Productivity (Kg/Hectare)')
lines(breakpoints(bp.Maize))

## confidence interval
ci.Maize <- confint(bp.Maize)
lines(ci.Maize)

################################################# Barley
bp.Barley <- breakpoints(Barley ~ 1)
summary(bp.Barley)

## the BIC chooses 5 breakpoints though
plot(bp.Barley)
plot(Barley, main = 'Structural Breaks in Barley Productivity', ylab = 'Barley Productivity (Kg/Hectare)')
lines(breakpoints(bp.Barley))

## confidence interval
ci.Barley <- confint(bp.Barley)
lines(ci.Barley)

################################################# Sugarcane
bp.Sugarcane <- breakpoints(Sugarcane ~ 1)
summary(bp.Sugarcane)

## the BIC chooses 5 breakpoints though
plot(bp.Sugarcane)
plot(Sugarcane, main = 'Structural Breaks in Sugarcane Productivity', ylab = 'Sugarcane Productivity (Kg/Hectare)')
lines(breakpoints(bp.Sugarcane))

## confidence interval
ci.Sugarcane <- confint(bp.Sugarcane)
lines(ci.Sugarcane)

################################################# Cotton
bp.Cotton <- breakpoints(Cotton ~ 1)
summary(bp.Cotton)

## the BIC chooses 5 breakpoints though
plot(bp.Cotton)
plot(Cotton, main = 'Structural Breaks in Cotton Productivity', ylab = 'Cotton Productivity (Kg/Hectare)')
lines(breakpoints(bp.Cotton))

## confidence interval
ci.Cotton <- confint(bp.Cotton)
lines(ci.Cotton)

################################################# Jute
bp.Jute <- breakpoints(Jute ~ 1)
summary(bp.Jute)

## the BIC chooses 5 breakpoints though
plot(bp.Jute)
plot(Jute, main = 'Structural Breaks in Jute Productivity', ylab = 'Jute Productivity (Kg/Hectare)')
lines(breakpoints(bp.Jute))

## confidence interval
ci.Jute <- confint(bp.Jute)
lines(ci.Jute)

################################################# Coarse_Cereals
bp.Coarse_Cereals <- breakpoints(Coarse_Cereals ~ 1)
summary(bp.Coarse_Cereals)

## the BIC chooses 5 breakpoints though
plot(bp.Coarse_Cereals)
plot(Coarse_Cereals, main = 'Structural Breaks in Coarse_Cereals Productivity', ylab = 'Coarse_Cereals Productivity (Kg/Hectare)')
lines(breakpoints(bp.Coarse_Cereals))

## confidence interval
ci.Coarse_Cereals <- confint(bp.Coarse_Cereals)
lines(ci.Coarse_Cereals)

################################################# Total_Pulses
bp.Total_Pulses <- breakpoints(Total_Pulses ~ 1)
summary(bp.Total_Pulses)

## the BIC chooses 5 breakpoints though
plot(bp.Total_Pulses)
plot(Total_Pulses, main = 'Structural Breaks in Total_Pulses Productivity', ylab = 'Total_Pulses Productivity (Kg/Hectare)')
lines(breakpoints(bp.Total_Pulses))

## confidence interval
ci.Total_Pulses <- confint(bp.Total_Pulses)
lines(ci.Total_Pulses)

################################################# Foodgrains
bp.Foodgrains <- breakpoints(Foodgrains ~ 1)
summary(bp.Foodgrains)

## the BIC chooses 5 breakpoints though
plot(bp.Foodgrains)
plot(Foodgrains, main = 'Structural Breaks in Foodgrains Productivity', ylab = 'Foodgrains Productivity (Kg/Hectare)')
lines(breakpoints(bp.Foodgrains))

## confidence interval
ci.Foodgrains <- confint(bp.Foodgrains)
lines(ci.Foodgrains)

################################################# Groundnut
bp.Groundnut <- breakpoints(Groundnut ~ 1)
summary(bp.Groundnut)

## the BIC chooses 5 breakpoints though
plot(bp.Groundnut)
plot(Groundnut, main = 'Structural Breaks in Groundnut Productivity', ylab = 'Groundnut Productivity (Kg/Hectare)')
lines(breakpoints(bp.Groundnut))

## confidence interval
ci.Groundnut <- confint(bp.Groundnut)
lines(ci.Groundnut)

################################################# Rapeseed + Mustard
bp.RapeseedMustard <- breakpoints(RapeseedMustard ~ 1)
summary(bp.RapeseedMustard)

## the BIC chooses 5 breakpoints though
plot(bp.RapeseedMustard)
plot(RapeseedMustard, main = 'Structural Breaks in Rapeseed & Mustard Productivity', ylab = 'Rapeseed & Mustard Productivity (Kg/Hectare)')
lines(breakpoints(bp.RapeseedMustard))

## confidence interval
ci.RapeseedMustard <- confint(bp.RapeseedMustard)
lines(ci.RapeseedMustard)

################################################# Soyabean
bp.Soyabean <- breakpoints(Soyabean ~ 1)
summary(bp.Soyabean)

## the BIC chooses 5 breakpoints though
plot(bp.Soyabean)
plot(Soyabean, main = 'Structural Breaks in Soyabean Productivity', ylab = 'Soyabean Productivity (Kg/Hectare)')
lines(breakpoints(bp.Soyabean))

## confidence interval
ci.Soyabean <- confint(bp.Soyabean)
lines(ci.Soyabean)

