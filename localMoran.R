# compute local Moran and local Geary indicators of spatial correlation
options(scipen=999)
locm <- localmoran(q3$jv, swm_w)

# locG <- localG(q1$ur, swm_w)

library(classInt)
library(dplyr)

# define variable
myvar <- q1$ur
# Define significance for the maps
significance <- 0.15
plot.only.significant <- F

# listw object
listw <- swm_w
# Create the lagged variable
lagvar <- lag.listw(listw, myvar)
# get the mean of each
m.myvar <- mean(myvar)
m.lagvar <- mean(lagvar)

n <- length(a1)
vec <- c(1:n)
vec <- ifelse(locm[,5] < significance, 1,0)
# Derive quadrants
q <- c(1:n) 

for (i in 1:n) {   
  if (myvar[[i]]>=m.myvar & lagvar[[i]]>=m.lagvar) q[i] <- 1
  if (myvar[[i]]<m.myvar & lagvar[[i]]<m.lagvar) q[i] <- 2
  if (myvar[[i]]<m.myvar & lagvar[[i]]>=m.lagvar) q[i] <- 3   
  if (myvar[[i]]>=m.myvar & lagvar[[i]]<m.lagvar) q[i] <- 4
}

# set coloring scheme
q.all <- q
colors <- c(1:n)
for (i in 1:n) {
  if (q.all[i]==1) colors[i] <- "red"
  if (q.all[i]==2) colors[i] <- "blue"
  if (q.all[i]==3) colors[i] <- "lightblue"
  if (q.all[i]==4) colors[i] <- "pink"
  if (q.all[i]==0) colors[i] <- "white"   
  if (q.all[i]>4) colors[i] <- "white"
}
# Mark all non-significant regions white
locm.dt <- q*vec
colors1 <- colors
for (i in 1:n)
{
  if ( !(is.na (locm.dt[i])) )  {
    if (locm.dt[i]==0) colors1[i] <- "white"
  }
}

colors2 <- colors
colors2 <- paste(colors2,vec)
pos = list()
for (i in 1:n) {
  pos[[i]] <- c(which(q1$ur==colors2["blue 0"]))
}

blue0 <- which(colors2=="blue 0")
red0 <- which(colors2=="red 0")
lightblue0 <- which(colors2=="lightblue 0")
pink0 <- which(colors2=="pink 0")
lb <- 6
labels=c("High-High", "High-Low", "Low-High", "Low-Low")

# plot the map
# if (plot.only.significant==TRUE) plot(a1, col=colors1,border=F) else
b = "grey" 
par(mfrow=c(1,3), mar=c(0,0,0,0))
plot(a1, col=colors1,border=b)
plot(a1[blue0,],border="blue",lwd=0.5,add=T)
plot(a1[lightblue0,],border="lightblue",add=T,lwd=0.5)
plot(a1[red0,],border="red",add=T,lwd=0.5)
plot(a1[pink0,],border="pink",add=T,lwd=0.5)

legend("topleft", legend = labels, fill = c("red", "pink", "lightblue", "blue"), bty = "n")

locm <- localmoran(q2$ur, mi1.sw)
# define variable
myvar <- q1$ur
# Define significance for the maps

plot.only.significant <- F

# listw object
listw <- mi1.sw
# Create the lagged variable
lagvar <- lag.listw(listw, myvar)
# get the mean of each
m.myvar <- mean(myvar)
m.lagvar <- mean(lagvar)

n <- length(a1)
vec <- c(1:n)
vec <- ifelse(locm[,5] < significance, 1,0)
# Derive quadrants
q <- c(1:n) 

for (i in 1:n) {   
  if (myvar[[i]]>=m.myvar & lagvar[[i]]>=m.lagvar) q[i] <- 1
  if (myvar[[i]]<m.myvar & lagvar[[i]]<m.lagvar) q[i] <- 2
  if (myvar[[i]]<m.myvar & lagvar[[i]]>=m.lagvar) q[i] <- 3   
  if (myvar[[i]]>=m.myvar & lagvar[[i]]<m.lagvar) q[i] <- 4
}

# set coloring scheme
q.all <- q
colors <- c(1:n)
for (i in 1:n) {
  if (q.all[i]==1) colors[i] <- "red"
  if (q.all[i]==2) colors[i] <- "blue"
  if (q.all[i]==3) colors[i] <- "lightblue"
  if (q.all[i]==4) colors[i] <- "pink"
  if (q.all[i]==0) colors[i] <- "white"   
  if (q.all[i]>4) colors[i] <- "white"
}
# Mark all non-significant regions white
locm.dt <- q*vec
colors1 <- colors
for (i in 1:n)
{
  if ( !(is.na (locm.dt[i])) )  {
    if (locm.dt[i]==0) colors1[i] <- "white"
  }
}

plot(a1, col=colors1,border="lightgrey")
plot(a1[blue0,],border="blue",lwd=0.5,add=T)
plot(a1[lightblue0,],border="lightblue",add=T,lwd=0.5)
plot(a1[red0,],border="red",add=T,lwd=0.5)
plot(a1[pink0,],border="pink",add=T,lwd=0.5)

locm <- localmoran(q2$ur, gg.sw)
# define variable
myvar <- q1$ur
# Define significance for the maps

plot.only.significant <- F

# listw object
listw <- gg.sw
# Create the lagged variable
lagvar <- lag.listw(listw, myvar)
# get the mean of each
m.myvar <- mean(myvar)
m.lagvar <- mean(lagvar)

n <- length(a1)
vec <- c(1:n)
vec <- ifelse(locm[,5] < significance, 1,0)
# Derive quadrants
q <- c(1:n) 

for (i in 1:n) {   
  if (myvar[[i]]>=m.myvar & lagvar[[i]]>=m.lagvar) q[i] <- 1
  if (myvar[[i]]<m.myvar & lagvar[[i]]<m.lagvar) q[i] <- 2
  if (myvar[[i]]<m.myvar & lagvar[[i]]>=m.lagvar) q[i] <- 3   
  if (myvar[[i]]>=m.myvar & lagvar[[i]]<m.lagvar) q[i] <- 4
}

# set coloring scheme
q.all <- q
colors <- c(1:n)
for (i in 1:n) {
  if (q.all[i]==1) colors[i] <- "red"
  if (q.all[i]==2) colors[i] <- "blue"
  if (q.all[i]==3) colors[i] <- "lightblue"
  if (q.all[i]==4) colors[i] <- "pink"
  if (q.all[i]==0) colors[i] <- "white"   
  if (q.all[i]>4) colors[i] <- "white"
}
# Mark all non-significant regions white
locm.dt <- q*vec
colors1 <- colors
for (i in 1:n)
{
  if ( !(is.na (locm.dt[i])) )  {
    if (locm.dt[i]==0) colors1[i] <- "white"
  }
}

plot(a1, col=colors1,border=b)
plot(a1[blue0,],border="blue",lwd=0.5,add=T)
plot(a1[lightblue0,],border="lightblue",add=T,lwd=0.5)
plot(a1[red0,],border="red",add=T,lwd=0.5)
plot(a1[pink0,],border="pink",add=T,lwd=0.5)