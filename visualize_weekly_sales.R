

rm(list=ls(all=TRUE))
gc(reset=TRUE)

#### put the required packages here
require(lattice)

#### Read in all the data provided
load('./data/training_testing_data_v1.RData')


figure_format <- 'png'

for(dept in sort(unique(sort(dfTrain$Dept)))){
  
  #### Creat the corresponding dir
  filePath <- './visualization/weekly_sales'
  dir.create(filePath, showWarnings=FALSE, recursive=TRUE)
  if(figure_format=='pdf'){
    pdf(paste(filePath, '/Dept', dept,'.pdf', sep=''))
  }else if(figure_format=='png'){
    png(paste(filePath, '/Dept', dept,'.png', sep=''))
  }
  
  dfTrain2 <- subset(dfTrain, Dept==dept)
  # create scatter plot
  print(xyplot(log(Weekly_Sales)~Day_Index|Store,
               data=dfTrain2, main=paste('Dept: ', dept, sep=''), as.table=TRUE,
               strip = strip.custom(strip.names = TRUE, strip.levels = TRUE),
               par.strip.text = list(cex = 0.75)))
  dev.off()
}

# my_fuel_ps=dfTrain %>%
#   group_by(Store,Day_Index) %>%
#   summarize(Fuel_price=mean(Fuel_Price, na.rm=TRUE))
# 
# my_fuels=dfTrain%>% group_by(Store) %>% summarise(B = sum(Weekly_Sales))

require(dplyr)
require(lattice)
my_fuel_ps=dfTrain %>% group_by(Store,Day_Index) %>% summarize(Fuel_price=mean(Fuel_Price, na.rm=TRUE), Unemployment=mean(Unemployment, na.rm=TRUE), wk_sales=sum(Weekly_Sales,na.rm=TRUE), IsHoliday=max(as.logical(IsHoliday)))

#To get list of holidays in one year
sub = subset(dfTrain, Store==1 & Dept == 1)
verticalValues = which(sub$IsHoliday == TRUE)
holidayNames = sub$Holiday[verticalValues]
holidayNames = gsub("_", " ", holidayNames)

#Function to draw lines on a weekly plot
fDrawLinesForWeeklyData <- function(ypos, cex=0.9){
  trellis.focus("panel", 1, 1) 
  panel.grid()
  panel.abline(v = verticalValues, col=1:4)
  panel.text(verticalValues, ypos, holidayNames, cex=cex, pos=4, col=1:4, srt=90)
  trellis.unfocus()
}

season_panel = function(x,y)
{
  panel.xyplot(x, y, type='o',pch=19, cex = 0.3) 
  panel.abline(v = verticalValues, col=1:4)
  panel.text(verticalValues, 0, holidayNames, cex=0.3, pos=4, col=1:4, srt=90)
}

weekly_panel = function(x,y)
{
  panel.xyplot(x, y, type='o', pch=19, cex = 0.3) 
  panel.abline(v = verticalValues, col=1:4)
  panel.text(verticalValues, 3, holidayNames, cex=0.3, pos=4, col=1:4, srt=90)
}

#To plot variation of fuel price with time
xyplot(jitter(Fuel_price)~jitter(Day_Index),
  data=my_fuel_ps, groups=Store,as.table=TRUE, xlim=c(0,nrow(sub)),
  strip = strip.custom(strip.names = TRUE, strip.levels = TRUE),
  par.strip.text = list(cex = 0.75),pch=19,type="o",cex=.3, main = "Variation of Fuel Prices every week from 2/5/2010 to 10/19/2012", xlab="Week Index", ylab="Fuel Price")
fDrawLinesForWeeklyData(3.75)

xyplot(Fuel_price~Day_Index|Store,xlim = c(0,nrow(sub)),
       data=my_fuel_ps, groups=Store,as.table=TRUE,
       strip = strip.custom(strip.names = TRUE, strip.levels = TRUE),
       par.strip.text = list(cex = 0.75),pch=19,type="o",cex=.3, main = "Variation in fuel price every week from 2/5/2010 to 10/19/2012", xlab="Week Index", ylab="Fuel Price",panel=weekly_panel)

#To plot variation in sales as a function of Fuel price
plot.new()
par(mfrow = c(5, 9))
for ( i in 1:45)
{
  sub = subset(my_fuel_ps, Store == i)
  #fit <- lm(wk_sales ~ Fuel_price, data=sub)
  plotTitle = paste("Variation in weekly sales as a function of fuel price every week from 2/5/2010 to 10/19/2012 for Store", i)
  xyplot(wk_sales ~ Fuel_price,
         data=sub, groups=Store,as.table=TRUE,
         strip = strip.custom(strip.names = TRUE, strip.levels = TRUE),
         par.strip.text = list(cex = 0.75),pch=19,type="p",cex=.3, main = plotTitle, ylab="Weekly Sales", xlab="Fuel Price",panel=panel.smooth)
}

#To plot variation of unemployment every week
xyplot(Unemployment~Day_Index,
       data=my_fuel_ps, groups=Store,as.table=TRUE,
       strip = strip.custom(strip.names = TRUE, strip.levels = TRUE),
       par.strip.text = list(cex = 0.75),pch=19,type="o",cex=.3, main = "Variation in unemployment every week from 2/5/2010 to 10/19/2012", xlab="Week Index", ylab="Unemployment Rate",xlim = c(0,nrow(sub)))
fDrawLinesForWeeklyData(12)

xyplot(Unemployment~Day_Index|Store,xlim = c(0,nrow(sub)),
       data=my_fuel_ps, groups=Store,as.table=TRUE,
       strip = strip.custom(strip.names = TRUE, strip.levels = TRUE),
       par.strip.text = list(cex = 0.75),pch=19,type="o",cex=.3, main = "Variation in unemployment every week from 2/5/2010 to 10/19/2012", xlab="Week Index", ylab="Unemployment Rate",panel=weekly_panel)

#To plot variation of weekly sales every week
xyplot(wk_sales~Day_Index,xlim = c(0,nrow(sub)),
       data=my_fuel_ps, groups=Store,as.table=TRUE,
       strip = strip.custom(strip.names = TRUE, strip.levels = TRUE),
       par.strip.text = list(cex = 0.75),pch=19,type="o",cex=.3, main = "Variation in sales every week from 2/5/2010 to 10/19/2012", xlab="Week Index", ylab="Sales")
fDrawLinesForWeeklyData(3000000)

xyplot(wk_sales~Day_Index|Store,
       data=my_fuel_ps, groups=Store,as.table=TRUE,
       strip = strip.custom(strip.names = TRUE, strip.levels = TRUE),
       par.strip.text = list(cex = 0.75),pch=19,type="o",cex=.3, main = "Variation in sales every week from 2/5/2010 to 10/19/2012", xlab="Week Index", ylab="Sales", ,panel=weekly_panel)

#To plot seasonal fuel rates
my_fuel_ps=dfTrain %>% group_by(Store,Day_Index) %>% summarize(Fuel_price=mean(Fuel_Price, na.rm=TRUE), wk_sales=sum(Weekly_Sales,na.rm=TRUE), holiday=max(as.logical(IsHoliday)), Season= median(as.numeric(as.character(Season)), na.rm=TRUE))
seasonlist=my_fuel_ps$Season
seasonlist.index4 = which(seasonlist == 4)
seasonlist[seasonlist.index4] = 0
my_fuel_ps$Season=seasonlist

xyplot(Fuel_price~Day_Index, xlim = c(0,nrow(sub)),xlab="Week Index", ylab="Fuel Price", main="Variation in fuel price every season from 2/5/2010 to 10/19/2012",
   data=my_fuel_ps, group = season,as.table=TRUE,
   strip = strip.custom(strip.names = TRUE, strip.levels = TRUE),
   par.strip.text = list(cex = 0.75), pch = 19, cex=0.3)
fDrawLinesForWeeklyData(3.75)

xyplot(Fuel_price~Day_Index|Store, xlim = c(0,nrow(sub)),xlab="Week Index", ylab="Fuel Price", main="Variation in fuel price every season from 2/5/2010 to 10/19/2012",
   data=my_fuel_ps, group = season,as.table=TRUE,
   strip = strip.custom(strip.names = TRUE, strip.levels = TRUE),
   par.strip.text = list(cex = 0.75), pch = 19, cex=0.3)

print(xyplot(wk_sales~Day_Index|Store,
             data=my_fuel_ps, groups=season,as.table=TRUE,
             strip = strip.custom(strip.names = TRUE, strip.levels = TRUE),
             par.strip.text = list(cex = 0.75), panel=season_panel))

xyplot(wk_sales~Day_Index , xlim = c(0,nrow(sub)),xlab="Week Index", ylab="Sales", main="Variation in sales every season from 2/5/2010 to 10/19/2012",
             data=my_fuel_ps, groups=season,as.table=TRUE,
             strip = strip.custom(strip.names = TRUE, strip.levels = TRUE),
             par.strip.text = list(cex = 0.75))

my_fuel_ps=dfTrain %>%   group_by(Store,Day_Index) %>%  summarize(Fuel_price=mean(Fuel_Price, na.rm=TRUE))

my_fuels=dfTrain%>% group_by(Store) %>% summarise(B = sum(Weekly_Sales))


#Summarize temperature for each store
myTable=dfTrain %>% group_by(Store,Day_Index) %>% summarize(Fuel_Price=mean(Fuel_Price, na.rm=TRUE), Unemployment=mean(Unemployment, na.rm=TRUE), Weekly_Sales=sum(Weekly_Sales,na.rm=TRUE), Temperature=mean(Temperature,na.rm=TRUE), IsHoliday=max(as.logical(IsHoliday)))

temperatureTable=list()
for ( i in 1:45)
{
  sub = subset(myTable, Store == i)
  temperatureTable = c(temperatureTable, (myTable$Temperature))
}
temperatureMatrix= matrix(unlist(temperatureTable), ncol=143, nrow=45, byrow=T)
temperatureMatrix = t(temperatureMatrix)
temperatureMatrix = data.frame(temperatureMatrix)
boxplot(temperatureMatrix, col=1:45, xlab="Stores", ylab="Temperature (Fahranheit)", main = "Temperatures at different store locations", names=c(1:45))

#Top 10 Stores by Sales for the given time period
topsales=dfTrain %>% group_by(Store) %>% summarize(Weekly_Sales=sum(Weekly_Sales,na.rm=TRUE))
totalsales=sum(dfTrain$Weekly_Sales)
percentSales = topsales$Weekly_Sales * 100 / totalsales
topsales=cbind(topsales,percentSales)
topsales = topsales[order(-topsales$percentSales),]
cat("Top 10 Stores by Sales is: \n")
print(topsales[1:10,])
cat("Total contribution of these stores:", sum(topsales$Weekly_Sales[1:10]) * 100 / totalsales, "percent")

#Top 10 departments by sales for the given time period
topsales=dfTrain %>% group_by(Dept) %>% summarize(Weekly_Sales=sum(Weekly_Sales,na.rm=TRUE))
percentSales = topsales$Weekly_Sales * 100 / totalsales
topsales=cbind(topsales,percentSales)
topsales = topsales[order(-topsales$percentSales),]
cat("Top 10 Stores by Sales is: \n")
print(topsales[1:10,])
cat("Total contribution of these stores:", sum(topsales$Weekly_Sales[1:10]) * 100 / totalsales, "percent")

# weekly sales ~ fuel prices
my_fuel_ps=dfTrain %>%  group_by(Store,Day_Index) %>%  summarize(Fuel_price=mean(Fuel_Price, na.rm=TRUE), wk_sales=sum(Weekly_Sales,na.rm=TRUE), holiday=max(as.logical(IsHoliday)))
#Weekly Sales versus Fuel Price
xyplot(wk_sales~Fuel_price|Store,
       data=my_fuel_ps, groups=Store,as.table=TRUE,
       strip = strip.custom(strip.names = TRUE, strip.levels = TRUE),
       par.strip.text = list(cex = 0.75))
xyplot(wk_sales~Fuel_price|Store, xlab="Fuel price", ylab="Weekly Sales",
       data=my_fuel_ps, groups=holiday,as.table=TRUE,
       strip = strip.custom(strip.names = TRUE, strip.levels = TRUE),
       par.strip.text = list(cex = 0.75), cex=0.6)

# For store ONE sales by week and the holidays colored diffrently. (Store no-20 and TYPE-0)
sub_store1 =subset(my_fuel_ps, Store==20)
seasonlist = sub_store1$Season
seasonlist.s0 = which(seasonlist == 0)
seasonlist[seasonlist.s0] = "Winter"
seasonlist.s1 = which(seasonlist == 1)
seasonlist[seasonlist.s1] = "Spring"
seasonlist.s0 = which(seasonlist == 2)
seasonlist[seasonlist.s0] = "Summer"
seasonlist.s1 = which(seasonlist == 3)
seasonlist[seasonlist.s1] = "Fall"
sub_store1$Season = seasonlist

xyplot(wk_sales~Day_Index|Season,
       data=sub_store1, groups=holiday,as.table=TRUE,
       strip = strip.custom(strip.names = TRUE, strip.levels = TRUE),
       par.strip.text = list(cex = 0.75), cex=0.6, pch=19, main="Weekly sales for a type 0 store (Store 20)", xlab="Week Index", ylab="Sales")


#for store44 (TYPE -1)
sub_store42 =subset(my_fuel_ps, Store==44)
seasonlist = sub_store42$Season
seasonlist.s0 = which(seasonlist == 0)
seasonlist[seasonlist.s0] = "Winter"
seasonlist.s1 = which(seasonlist == 1)
seasonlist[seasonlist.s1] = "Spring"
seasonlist.s0 = which(seasonlist == 2)
seasonlist[seasonlist.s0] = "Summer"
seasonlist.s1 = which(seasonlist == 3)
seasonlist[seasonlist.s1] = "Fall"
sub_store42$Season = seasonlist
xyplot(wk_sales~Day_Index|Season,
       data=sub_store42, groups=holiday,as.table=TRUE,
       strip = strip.custom(strip.names = TRUE, strip.levels = TRUE),
       par.strip.text = list(cex = 0.75), cex=0.6, pch=19, main="Weekly sales for a type 1 store (Store 44)", xlab="Week Index", ylab="Sales")
