# load excell library
library(openxlsx)

# some shapefile tools
require(sp)
require(maptools)
require(spatstat)
require(spdep)
require(rgdal)
require(RANN)


# please change path if needed
setwd("/Users/wozni/Google Drive/UAM/HUB/spatialcor/")

# load shapefile
a1 <- readOGR(".","aglo")
a1 <- spTransform(a1, CRS("+proj=longlat +datum=WGS84"))
a2 <- readOGR(".","rail2")
coords <- coordinates(a1)

dev.off()

plot(a1)
plot(a2, col=2, add=T)

# create spatial weights matrix
swm <- poly2nb(a1, queen = T)
swm.w <- nb2listw(swm, style="W")

nn1 <- knearneigh(coords, k=3)  # Creates a matrix of nn indexes
mi1.nlist <- knn2nb(nn1, row.names = NULL, sym = FALSE)  # create neighbor list
mi1.sw <- nb2listw(mi1.nlist)  # create a spatial weights matrix

nn2 <- knearneigh(coords, k=5)  # Creates a matrix of nn indexes
mi2.nlist <- knn2nb(nn2, row.names = NULL, sym = FALSE)  # create neighbor list
mi2.sw <- nb2listw(mi2.nlist)  # create a spatial weights matrix

col.tri.nb <- tri2nb(coords)
gg <- graph2nb(gabrielneigh(coords), sym=TRUE)
gg.sw <- nb2listw(gg)


par(mfrow=c(1,3), mar=c(0,0,0,0), oma=c(0,0,0,0))
plot(a1, border="grey60", lwd=2, main="neighbors-based", line=-2)
plot(swm.w, coords, cex=0.8, lwd=2, col="tomato3", add=T)
plot(a1, border="grey60", lwd=2, main="distance-based (k=3)", line=-2)
plot(mi1.sw, coords, cex=0.8, lwd=2, col="tomato3", add=T)
plot(a1, border="grey60", lwd=2, main="graph-based", line=-2)
plot(gg, coords, cex=0.8, lwd=2, col="tomato3", add=T)


# data to Durbin model
a <- aggregate(outflow ~ year + name, panel, sum)
b <- aggregate(unem ~ year + name, panel, mean)
c <- aggregate(JOB_vac ~ year + name, panel, sum)
d <- aggregate(PUP_vac ~ year + name, panel, sum)

a$unem <- b$unem
a$jv <- c$JOB_vac
a$pv <- d$PUP_vac  
  
# load data
panel_reg <- subset(panel, select = c("name", "date", "outflow", "inflow", "unem", "JOB_vac", "PUP_vac"))

# to estimation 0s are not allowed in data frame
panel_reg$PUP_vac[panel_reg$PUP_vac == 0.00] <- 1

# spatial estimation
require(splm)
require(spatialreg)

panel_reg$spunem <- slag(log(panel_reg$unem), listw = mi1.sw)
panel_reg$spJOB_vac <- slag(log(panel_reg$JOB_vac), listw = mi1.sw)
panel_reg$spinflow <- slag(log(panel_reg$inflow), listw = mi1.sw)
panel_reg$spPUP_vac <- slag(log(panel_reg$PUP_vac), listw = mi1.sw)
  
f <- log(outflow) ~ log(unem) + log(JOB_vac) + log(inflow)
slx <- log(outflow) ~ log(unem) + log(inflow) + log(JOB_vac) + spunem + spJOB_vac + spinflow
sar <- log(outflow) ~ log(unem) + log(inflow) + log(JOB_vac)
sdem <- log(outflow) ~ log(unem) + log(inflow) + log(JOB_vac) + spunem + spJOB_vac + spinflow

GM_error <- spgm(log(outflow) ~ log(unem) + log(inflow) + log(JOB_vac) + spunem + spJOB_vac + spinflow, data = panel_reg, c("name", "date"), lag = TRUE,
                 listw = mi1.sw, model = "within", spatial.error = T)

spat.pan.jv.sar <- spml(log(outflow) ~ log(unem) + log(inflow) + log(JOB_vac), data = panel_reg, c("name", "date"), listw = mi1.sw,
                    lag = TRUE, spatial.error = "none", model = "within", effect = "individual",
                    quiet = TRUE)

spat.pan.jv.sar.t <- spml(log(outflow) ~ log(unem) + log(inflow) + log(JOB_vac), data = panel_reg, c("name", "date"), listw = mi1.sw,
                        lag = TRUE, spatial.error = "none", model = "within", effect = "time",
                        quiet = TRUE)


spat.pan.jv.slx <- plm(log(outflow) ~ log(unem) + log(inflow) + log(JOB_vac) + spunem + spJOB_vac + spinflow, data = panel_reg, index=c("name", "date"),
                        model = "within", effect = "individual", quiet = TRUE)

spat.pan.jv.slx.t <- plm(log(outflow) ~ log(unem) + log(inflow) + log(JOB_vac) + spunem + spJOB_vac + spinflow, data = panel_reg, index=c("name", "date"),
                       model = "within", effect = "time", quiet = TRUE)



spat.pan.jv.sdem <- spml(log(outflow) ~ log(unem) + log(inflow) + log(JOB_vac) + spunem + spJOB_vac + spinflow, data = panel_reg, c("name", "date"), listw = mi1.sw,
                        lag = T, spatial.error = "none", model = "within", effect = "individual",
                        quiet = TRUE)


spat.pan.jv.pl <- spml(log(outflow) ~ log(unem) + log(PUP_vac), data = panel_reg, c("name", "date"), listw = mi1.sw,
                    lag = TRUE, spatial.error = "b", model = "pooling", effect = "individual", na.action = na.fail,
                    quiet = TRUE, zero.policy = NULL,  tol.solve = 0.1, control = list(), legacy = FALSE)


spat.pan.pv <- spml(log(outflow) ~ log(unem) + log(inflow) + log(PUP_vac), data = panel_reg, c("name", "date"), listw = swm.w,
                    lag = FALSE, spatial.error = "none", model = "pooling", effect = "individual", method = "eigen", na.action = na.fail,
                    quiet = T)

test1 <- bsktest(x = sdem, data = panel_reg, listw = swm.w, test = "LM2")
test1

options(scipen=999)
summary(spat.pan.jv.sar)
summary(spat.pan.jv.sar.t)
summary(spat.pan.jv.slx)
summary(spat.pan.jv.slx.t)
summary(spat.pan.jv.sdem)

pFtest(spat.pan.jv.slx.t, spat.pan.jv.slx)

plmtest(spat.pan.jv.sar, data=panel_reg, effect="individual", type = "honda")

summary(spat.pan.jv.pl)
summary(spat.pan.jv.sdem)$rsqr
summary(GM_error)

eff <- effects(spat.pan.jv.sar)
eff.t <- effects(spat.pan.jv.sar.t)
summary(fixef(spat.pan.jv.slx.t))

f <- log(outflow) ~ log(unem) + log(PUP_vac) + log(inflow)
slx <- log(outflow) ~ log(unem) + log(inflow) + log(PUP_vac) + spunem + spPUP_vac + spinflow
sar <- log(outflow) ~ log(unem) + log(inflow) + log(PUP_vac)
sdem <- log(outflow) ~ log(unem) + log(inflow) + log(PUP_vac) + spunem + spPUP_vac + spinflow

spat.pan.pv.sar <- spml(log(outflow) ~ log(unem) + log(inflow) + log(PUP_vac), data = panel_reg, c("name", "date"), listw = mi1.sw,
                        lag = TRUE, spatial.error = "none", model = "within", effect = "individual",
                        quiet = TRUE)

spat.pan.pv.slx <- plm(log(outflow) ~ log(unem) + log(inflow) + log(PUP_vac) + spunem + spPUP_vac + spinflow, data = panel_reg, index=c("name", "date"),
                       model = "within", effect = "individual", quiet = TRUE)

spat.pan.pv.sdem <- spml(log(outflow) ~ log(unem) + log(inflow) + log(PUP_vac) + spunem + spPUP_vac + spinflow, data = panel_reg, c("name", "date"), listw = mi1.sw,
                         lag = T, spatial.error = "none", model = "within", effect = "individual",
                         quiet = TRUE)

summary(spat.pan.pv.sar)
summary(spat.pan.pv.slx)
summary(spat.pan.pv.sdem)
summary(spat.pan.pv.sdem)$rsqr


test1 <- sphtest(x = sdem, data = panel_reg, listw = mi1.sw, spatial.model = "error", method = "ML")
phtest(x=f, data=panel_reg)

test2 <- bsktest(x = sdem, data = panel_reg, listw = swm.w, test = "LM1")
test2
plmtest(x=slx, data=panel_reg, type="bp")

AICsplm(spat.pan.pv.sdem, criterion = "AIC")

eff1 <- effects(spat.pan.jv.sdem)
eff2 <- effects(spat.pan.jv.sar)

df <- data.frame(eff1$SETable[,1])
df$sdem <- eff2$SETable[,1]
colnames(df)[1] <- "sar"
df$osm_id <- data_cor$osm_id

require(plm)
pan.fe <- plm(log(outflow) ~ log(unem) + log(inflow) + log(PUP_vac), data = panel_reg, index= c("name", "date"), model = "within")
pan.pooled <- plm(log(outflow) ~ log(unem) + log(inflow) + log(JOB_vac), data = panel_reg, index= c("name", "date"), model = "pooling")
pan.pooled <- plm(log(outflow) ~ log(unem) + log(inflow) + log(PUP_vac), data = panel_reg, index= c("name", "date"), model = "pooling")

phtest(f, data=panel_reg)
sphtest(slx, data=panel_reg, listw = mi1.sw, spatial.model = "error", method="ML")

summary(pan.fe)
summary(pan.pooled)

AIC_adj(pan.fe)
aicbic_plm(pan.pooled, "BIC")

spat.durbin <- lagsarlm(log(outflow) ~ log(unem) + log(jv), data = a, Durbin=TRUE,
                        method="eigen", quiet=NULL, zero.policy=NULL, interval=NULL,listw = mi1.sw)

summary(spat.durbin)

