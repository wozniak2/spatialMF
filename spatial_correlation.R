# load excell library
library(openxlsx)

# some shapefile tools
require(sp)
require(maptools)
require(spatstat)
require(spdep)
require(rgdal)
library(plm)

# please change path if needed
setwd("/Users/wozni/Google Drive/UAM/HUB/spatialcor/")

# load shapefile
a1 <- readOGR(".","aglo")
a1 <- spTransform(a1, CRS("+proj=longlat +datum=WGS84"))
a1 <- fortify(a1)

library(ggmap)
library(ggplot2)

# register in GoogleMaps with API
register_google(key = "provide your API here")

# download the map
map <- get_map(location = "Poznań", zoom=11, scale=2)
 
ggmap(map, extend="device", maptype="terrain", color="color") + 
        geom_polygon(data=a1)

# create spatial weights matrix
swm <- poly2nb(a1, queen = TRUE)
swm_w <- nb2listw(swm, style="W", zero.policy=TRUE)

# load data
require(readxl)
panel <- read_xlsx('data_cor_panel_rev1.xlsx')
# change string to date
library(lubridate)
panel$date <- parse_date_time(panel$time, "Ym")
panel$y <- parse_date_time(panel$year, "Y")
# declare main data set to be panel with time and space indexes
library(plm)
panel <- pdata.frame(panel, index = c("name","date"))
purtest(panel$urate, lags="AIC", pmax=6, exo="intercept", test = "madwu")

# plot time series of panel data
library(lattice)

xyplot(PUP_vacrate + JOB_vacrate ~ date|name , data=panel, type="l", cex=1, col=2:1,
       par.strip.text = list(cex = 0.9), transparent=TRUE, 
       strip = strip.custom(bg="grey"),
       grid=TRUE, lwd=2,
       xlab=list(label="Time", cex=1),
       ylab=list(label="New vacancies", cex=1), 
       scales=list(relation="free", cex=0.7, x=list(rot=45, at=c(1,6,12,18,24,32), labels=c("01.2018", "06.2018", "12.2018", "06.2019", "12.2019", "12"))),
       key=list(space="top", lines=list(col=c("red","black"), lty=c(1,1), lwd=3), columns=2,
                text=list(c("public labor office","job portals")), cex.title=1))

xyplot(unem + JOB_vac ~ date|name , data=panel, type="l", cex=1, col=2:1,
       par.strip.text = list(cex = 0.9), transparent=TRUE, 
       strip = strip.custom(bg="grey"),
       grid=TRUE, lwd=2,
       xlab=list(label="Time", cex=1),
       ylab=list(label="New vacancies", cex=1), 
       scales=list(relation="free", cex=0.7, x=list(rot=45, at=c(1,6,12), labels=c("01", "06", "12"))),
       key=list(space="top", lines=list(col=c("red","black"), lty=c(1,1), lwd=3), columns=2,
                text=list(c("public labor office","job portals")), cex.title=1))
# cross correaltion

require(corrr)
cr <- ccf(panel$JOB_vac, panel$unem, lag=3)

res.cor <- correlate(panel[,5:14])
res.cor

summary((panel[,5:14]))
sd(panel$JOB_vac)
# extract months to become factors

## 2019 year
Jan_2019 <- subset(panel, panel$date == "2019-01-01")
df <- as.data.frame(Jan_2019)

Jun_2019 <- subset(panel, panel$date == "2019-06-01")
Dec_2019 <- subset(panel, panel$date == "2019-12-01")
df$Jun_2019 <- Jun_2019$JOB_vacrate
df[df$Jun_2019 > 0.025, "Jun_2019"] <- 0.025


df$Dec_2019 <- Dec_2019$JOB_vacrate
df[df$Dec_2019 > 0.025, "Dec_2019"] <- 0.025
colnames(df)[12] <- "Jan_2019"
df[df$Jan_2019 > 0.025, "Jan_2019"] <- 0.025

## 2018 year
Jan_2018 <- subset(panel, panel$date == "2018-01-01")
Jun_2018 <- subset(panel, panel$date == "2018-06-01")
Dec_2018 <- subset(panel, panel$date == "2018-12-01")

df$Jan_2018 <- Jan_2018$JOB_vacrate
df$Jun_2018 <- Jun_2018$JOB_vacrate
df[df$Jun_2018 > 0.025, "Jun_2018"] <- 0.025


df$Dec_2018 <- Dec_2018$JOB_vacrate
df[df$Dec_2018 > 0.025, "Dec_2018"] <- 0.025

df[df$Jan_2018 > 0.025, "Jan_2018"] <- 0.025



require(tigris)
library(RColorBrewer)
require(spacetime)
geo <- geo_join(a1, df, by_sp = "osm_id", by_df = "osm_id" )

# plot dynamics on map
dev.off()
my.palette <- brewer.pal(n = 6, name = "OrRd")
library(ggplot2)
spplot(geo, c("Jan_2019", "Jun_2019", "Dec_2019", "Jan_2018", "Jun_2018", "Dec_2018"), col.regions = my.palette, cuts = 5, col = "grey",
       main=list(label="Job vacancy rate",cex=1))

spplot(geo, c("sdem"), col.regions = my.palette, cuts = 4, col = "grey",
       main=list(label="Vacancy ratio",cex=1))

## 2019 year
Jan_2019 <- subset(panel, panel$date == "2019-01-01")
df2 <- as.data.frame(Jan_2019)
colnames(df2)[14] <- "Jan_2019"

df2$Jun_2019 <- Jun_2019$urate
df2$Dec_2019 <- Dec_2019$urate

## 2018 year
df2$Jan_2018 <- Jan_2018$urate
df2$Jun_2018 <- Jun_2018$urate
df2$Dec_2018 <- Dec_2018$urate

geo2 <- geo_join(a1, df2, by_sp = "osm_id", by_df = "osm_id" )

my.palette <- brewer.pal(n = 8, name = "OrRd")
spplot(geo2, c("Jan_2019", "Jun_2019", "Dec_2019", "Jan_2018", "Jun_2018", "Dec_2018"), col.regions = my.palette, cuts = 7, col = "grey",
       main=list(label="Unemployment rate",cex=1))



# monthly to quarterly
require(zoo)
period <- as.yearqtr(panel$date)

a <- aggregate(panel$urate ~ panel$name + period, FUN="mean")
b <- aggregate(panel$JOB_vacrate ~ panel$name + period, FUN="mean")
c <- aggregate(panel$PUP_vacrate ~ panel$name + period, FUN="mean")
a$jv <- b$`panel$JOB_vacrate`
a$pv <- c$`panel$PUP_vacrate`
colnames(a)[3] <- "ur"
colnames(a)[1] <- "name"


yearly <- aggregate(a, by=list(a$name), FUN="mean")

q1 <- subset(a, a$period=="2019 Q1")
q2 <- subset(a, a$period=="2019 Q2")
q3 <- subset(a, a$period=="2019 Q3")
q4 <- subset(a, a$period=="2019 Q4")

set.seed(1234)

moran.mc(q3$pv, swm_w, nsim=120)
moran.mc(q3$pv, mi2.sw, nsim=120)
moran.mc(q3$pv, gg.sw, nsim=99)



moran.mc(q4$ur, swm_w, nsim=99)



geary.mc(q3$pv, swm_w, nsim=99)

geary.mc(q4$pv, mi2.sw, nsim=99)

geary.mc(q3$pv, gg.sw, nsim=99)


geary.mc(q1$pv, gg.sw, nsim=919)


dev.off()
moran.plot(q1$ur, swm_w)




legend("topleft", legend = labels, fill = c("red", "pink", "lightblue", "blue"), bty = "n")





# Local Geary
library(RColorBrewer)
require(classInt)
par(mfrow=c(1,3), mar=c(0,0,0,0))

locG <- localG(q1$ur, swm.w)
nclassint <- 3
colpal <- brewer.pal(nclassint,"PiYG")
cat <- classIntervals(locG, nclassint, style = "jenks", na.ignore=T)
color.z <- findColours(cat, colpal)

plot(a1, col= color.z, border=T)
title("Neighbors based", line = -25, cex.main=1.5)
legend("topleft", legend = c("high","low"),
       fill=color.z,pt.cex = 1.5, cex=1.5, bty="n")
plot(a2, col=2, add=T)

locG <- localG(q1$ur, mi1.sw)
nclassint <- 3
colpal <- brewer.pal(nclassint,"PiYG")
cat <- classIntervals(locG, nclassint, style = "jenks", na.ignore=T)
color.z <- findColours(cat, colpal)
plot(a1, col= color.z, border=T)
title("Distance based", line = -25, cex.main=1.5)

locG <- localG(q1$ur, gg.sw)
nclassint <- 3
colpal <- brewer.pal(nclassint,"PiYG")
cat <- classIntervals(locG, nclassint, style = "jenks", na.ignore=T)
color.z <- findColours(cat, colpal)
plot(a1, col= color.z, border=T)
title("Graph based", line = -25, cex.main=1.5)



