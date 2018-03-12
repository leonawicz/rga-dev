library(RGoogleAnalytics)
load("tokens.RData")
tokens <- tokens2
ids <- ids2
token.ids <- rep(1, 4)

lapply(tokens, ValidateToken)

queryList <- function(i, a, b, id){
	Init(start.date=a, end.date=b,
		dimensions="ga:date, ga:latitude, ga:longitude", metrics="ga:pageviews, ga:avgTimeOnPage, ga:pageviewsPerSession, ga:entranceRate",
		max.results=20000, sort="ga:date", table.id=id[[i]])
}

ql <- lapply(1:length(ids), queryList, a="2014-04-01", b="2016-03-08", id=ids)
query <- lapply(ql, QueryBuilder)

d <- lapply(1:4,
	function(i, ...){
		tk <- tokens[token.ids[i]]
		cbind(Acct=names(tk), Site=names(ids)[i], GetReportData(query[[i]], tk[[1]])) },
	query, ids, tokens, token.ids)

library(gridExtra)
library(data.table)
library(dplyr)
library(ggplot2)
library(geosphere)

d <- bind_rows(d)
d <- mutate(d, totalTime=pageviews*avgTimeOnPage, latitude=as.numeric(latitude), longitude=as.numeric(longitude))
d <- d %>% filter(!(latitude==0 & longitude==0))
set.seed(1)
d <- d %>% group_by(Site) %>% mutate(EstTime=sqrt(sample(avgTimeOnPage[avgTimeOnPage!=0], n(), rep=T)+1))

save(d, file="data.RData")
load("data.RData")

p <- SpatialPoints(cbind(d$longitude, d$latitude), proj4string=CRS("+proj=longlat +datum=WGS84"))
fbks <- match(64.8378, p@coords[,2])[1]
bfld <- match(40.015, p@coords[,2])[1] # -105.2705 
get_paths <- function(x, idx, ...){
  gcInt <- function(x, x1, x2){
    x <- gcIntermediate(x[x1,], x[x2,], ...)
    if(is.list(x)){
      x <- x %>% purrr::map2(c(x1, x1 + 0.5), ~data.frame(.x, .y)) %>% bind_rows %>% setnames(c("long", "lat", "group"))
    } else x <- data.frame(x, x1) %>% setnames(c("long", "lat", "group"))
    x
  }
  x <- purrr::map(setdiff(1:length(x), idx), ~gcInt(x, .x, idx)) %>% bind_rows
  x
}
  
paths.fbks <- get_paths(p, fbks, breakAtDateLine=FALSE, addStartEnd=TRUE)
paths.bfld <- get_paths(p, bfld, breakAtDateLine=FALSE, addStartEnd=TRUE)
save(d, paths.bfld, paths.fbks, file="rga_appUseGeo.RData")
#paths.fbks.brk <- get_paths(p, fbks, breakAtDateLine=TRUE, addStartEnd=TRUE)
#paths.bfld.brk <- get_paths(p, bfld, breakAtDateLine=TRUE, addStartEnd=TRUE)

library(gridExtra)
library(raster)
library(data.table)
library(dplyr)
library(ggplot2)
library(geosphere)

setwd("/atlas_scratch/mfleonawicz/projects/rga")
load("rga_appUseGeo.RData")
eb <- element_blank()
theme_blank <- theme(axis.line=eb, axis.text.x=eb, axis.text.y=eb, axis.ticks=eb, axis.title.x=eb, axis.title.y=eb,
  legend.position="none", panel.background=eb, panel.border=eb, panel.grid.major=eb, panel.grid.minor=eb, plot.background=element_rect(colour="transparent", fill="transparent"))
world <- map_data("world")

# flat map
g0 <- ggplot() + geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="black", colour="gray") +
  geom_path(data=paths.bfld.brk, aes(lon, lat , group=group), colour="#FFFFFF02") +
  #geom_point(data=d, aes(x=longitude, y=latitude, group=NULL, size=EstTime), colour="#FFFFFF30") +
  #geom_path(data=paths, aes(lon, lat , group=group), colour="#1E90FF30") +
  #geom_point(data=d, aes(x=longitude, y=latitude, group=NULL, size=EstTime), shape=21, fill="#1E90FF50", colour="white") + 
  theme_blank + #coord_map("ortho", orientation=c(41, -74, 0)) +
  #annotate("text", x=-41, y=33, label="App Traffic\nDay One", colour="white", family="mono", fontface="bold", size=8) +
  #annotate("text", x=-41, y=33, label="App Traffic\nDay One", colour="#1E90FF60", family="mono", fontface="bold", size=8)
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
  coord_cartesian(xlim=c(-180, 180), ylim=c(-60, 90))
  
pdf("visitor_map_2014-04-01_2016-03-08.pdf", width=18, height=7.5, bg="black")
g0
dev.off()

df_segs <- function(d, seg.size, seg.frac.max=1/3, n.frames, replicates=1, direction="fixed"){
  n <- nrow(d)
  if(n < 3) stop("Data not appropriate for this operation.")
  if(is.numeric(seg.size)){
    stopifnot(seg.size >= 2)
    z <- round(runif(2, 2, seg.size))
    z[z > n] <- n
  } else {
    if(n*seg.frac.max < 3) stop("seg.frac.max is too small.")
    if(seg.size=="uniform"){
      z <- round(runif(2, 2, n*seg.frac.max))
    } else if(seg.size=="normal") {
      mn <- mean(1:n)
      stddev <- mn/6
      z <- round(rnorm(2, mn, stddev))
    }
  }
  n1 <- ceiling(diff(c((z[1] - z[2]), n))/z[1])
  if(n.frames - n1 < 100) stop("Insufficient frames")
  offset <- sample(0:(n.frames - n1), replicates)
  f <- function(k, d, n, n1, z, offset){
    ind2 <- z[1]*k
    ind1 <- max(ind2 - z[2], 1)
    if(ind2 > n) ind2 <- n
    d <- slice(d, ind1:ind2)
    purrr::map(offset, ~mutate(d, group=ifelse(replicates==1, group, group + as.numeric(sprintf(".%d", k))), frameID=.x + k)) %>% bind_rows
  }
  print(d$group[1])
  if(direction=="reverse") d <- mutate(d, long=rev(long), lat=rev(lat))
  if(direction=="random" && rnorm(1) < 0) d <- mutate(d, long=rev(long), lat=rev(lat))
  d <- purrr::map(1:n1, ~f(.x, d, n, n1, z, offset)) %>% bind_rows %>% arrange(group, frameID)
  d
}

library(parallel)
n.frames <- 900
set.seed(1)
#system.time( paths <- paths.bfld %>% split(.$group) %>% purrr::map(~df_segs(.x, 5, 1/3, n.frames, replicates=1, direction="random")) %>% bind_rows )
paths.fbks <- mutate(paths.fbks, group=group + max(paths.bfld$group))
paths <- paths.bfld %>% bind_rows(paths.fbks) %>% split(.$group)
paths <- mclapply(paths, df_segs, seg.size=5, n.frames=n.frames, replicates=10, direction="random", mc.cores=32) %>% bind_rows

saveRDS(paths, "paths.rds")
paths <- readRDS("paths.rds")

#library(marmap)
#bath <- getNOAA.bathy(lon1=-180, lon2=180, lat1=-90, lat2=90, resolution=10, keep=TRUE)[]
#d.bath <- expand.grid(long=seq(-180, 180, length=nrow(bath)), lat=seq(-90, 90, length=ncol(bath))) %>% data.table
#d.bath$z <- as.numeric(bath)
d.bath <- read.csv("marmap_coord_-180;-90;180;90_res_10.csv") %>% data.table %>% setnames(c("long", "lat", "z"))

r <- raster(extent(-180,180,-90,90), res=1/6)
projection(r) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#r <- setValues(r, t(bath[, ncol(bath):1]))
r <- setValues(r, d.bath$z) #t(bath[, ncol(bath):1]))
#d.bath <- r %>% aggregate(10) %>% rasterToPoints %>% data.table %>% setnames(c("long", "lat", "z"))

library(png)
x <- readPNG("maptiles_001.png", native=TRUE)
rx <- raster(extent(0, ncol(x), 0, nrow(x)), res=1)
rx <- setValues(rx, as.numeric(x))
plot(rx)
ry <- disaggregate(rx, 2)
ry <- focal(ry, w=matrix(1, 3, 3), mean)
rv <- rasterToPoints(ry)
rv <- data.table(rv) %>% setnames(c("x", "y", "z"))
g <- ggplot(rv, aes(x, y, fill=z)) + geom_raster()
png("maptiles_001_smoothed.png", width=4*1920, height=4*1080, res=300, bg="transparent")
print(g)
dev.off()
  
mapproj <- function(lat,long,cenlat,cenlong){
    lat_0 <- lat
    lon_0 <- long
    d2r=pi/180; lat=lat*d2r; long=long*d2r; cenlat=cenlat*d2r; cenlong=cenlong*d2r
    #x=cos(lat)*sin(long-cenlong)
    #y=cos(cenlat)*sin(lat)-sin(cenlat)*cos(lat)*cos(long-cenlong)
    front=sin(cenlat)*sin(lat)+cos(cenlat)*cos(lat)*cos(long-cenlong) > 0
    data.table(long=lon_0, lat=lat_0, front=front)
}

pgc_points <- function(lon, lat, n=200){
    x <- destPoint(c(lon, lat), c(0, 120, 240), pi*6378137/(2*1.005)) %>% data.frame
    purrr::map2(c(1,2,3), c(2,3,1), ~gcIntermediate(x[.x,], x[.y,], n=n, breakAtDateLine=FALSE, addStartEnd=TRUE) %>% data.table) %>% rbindlist
}

horizon <- pgc_points(lon, lat)

ggplot() + geom_path(data=world, aes(x=long, y=lat, group=group), colour="black") +
  geom_line(data=horizon, aes(x=lon, y=lat, group=NULL, size=NULL), colour="orange", size=2) +
  theme_blank + coord_map("ortho", orientation=c(lat, lon, 0))

n.period <- 120
lon_seq <- rep(seq(0, 360, length.out=n.period + 1)[-(n.period + 1)], length=n.frames)
#sq2 <- 10*sin(seq(-pi,pi,length=length(sq)))
lat_seq <- rep(41, length(lon_seq))

save_maps <- function(x, lon_seq, lat_seq, col=NULL, type="network", z.range=NULL){
  if(is.null(col)) col <- switch(type, network=c("#FFFFFF25", "#1E90FF25", "#FFFFFF", "#1E90FF50"), maptiles=c("black", "steelblue4"), maplines="white")
  i <- x$frameID[1]
  if(type=="network") x.lead <- group_by(x, group) %>% slice(n())
  g <- ggplot(x, aes(long, lat))
  if(type=="maptiles"){
    if(is.null(z.range)) z.range <- range(x$z, na.rm=TRUE)
    g <- ggplot(x, aes(long, lat, fill=z)) + geom_tile() + scale_fill_gradientn(colors=col, limits=z.range)
  } else {
    g <- ggplot(x, aes(long, lat, group=group))
    if(type=="maplines") g <- g + geom_path(colour=col)
    if(type=="network") g <- g + geom_path(colour=col[2]) + geom_path(colour=col[1]) + geom_point(data=x.lead, colour=col[3], size=0.6) + geom_point(data=x.lead, colour=col[4], size=0.3)
  }
  g <- g + theme_blank + coord_map("ortho", orientation=c(lat_seq[i], lon_seq[i], 23.4))
  #outDir <- if(type=="maptiles") sprintf(paste0(type, "_%03d"), i) else type
  dir.create(outDir <- file.path("frames", type), recursive=TRUE, showWarnings=FALSE)
  png(sprintf(paste0(outDir, "/", type, "_%03d.png"), i), width=4*1920, height=4*1080, res=300, bg="transparent")
  print(g)
  dev.off()
  NULL
}

paths <- paths %>% split(.$frameID)
d.bath.agg <- r %>% aggregate(2) %>% rasterToPoints %>% data.table %>% setnames(c("long", "lat", "z"))
d.tiles <- mclapply(1:n.period, function(i, x, lon, lat){
  left_join(x, mapproj(x$lat, x$long, lat[i], lon[i])) %>% filter(front) %>% dplyr::select(-front) %>% mutate(frameID=i)
  }, x=d.bath.agg, lat=lat_seq, lon=lon_seq, mc.cores=32)
z.range <- purrr::map(d.tiles, ~range(.x$z, na.rm=TRUE)) %>% unlist %>% range
d.world <- purrr::map(1:n.period, ~mutate(world, frameID=.x))

mclapply(paths, save_maps, lon_seq, lat_seq, type="network", mc.cores=32)
mclapply(d.world, save_maps, lon_seq, lat_seq, type="maplines", mc.cores=32)
system.time( mclapply(d.tiles, save_maps, lon_seq, lat_seq, type="maptiles", z.range=z.range, mc.cores=30) )


for(i in seq_along(sq)){
  paths.sub <- filter(paths, frameID==i)
  paths.sub.lead <- group_by(paths.sub, group) %>% slice(n())
  d.tiles.sub <- left_join(d.tiles, mapproj(d.tiles$lat, d.tiles$long, lat, lon)) %>% filter(front) %>% dplyr::select(-front)
  g <- ggplot(data=world, aes(long, lat, group=group)) +
  geom_tile(data=d.tiles.sub, aes(group=NULL, fill=z)) + scale_fill_gradientn(colors=c("black", "steelblue4")) +
  geom_path(colour="black") + 
  #geom_path(data=paths.sub, colour=clrs[2]) +
  #geom_point(data=sample_frac(d, 0.1), aes(x=longitude, y=latitude, group=NULL, size=EstTime), colour=clrs[1]) +
  #geom_path(data=paths.sub, colour=clrs[1]) +
  #geom_point(data=paths.sub.lead, colour=clrs[4], size=0.7) +
  #geom_point(data=paths.sub.lead, colour=clrs[3], size=0.4) +
  theme_blank + coord_map("ortho", orientation=c(lat, lon, 23.4))
  system.time({
  png(sprintf("frames/_test_%03d.png", i), width=3*1920, height=3*1080, res=300)
  print(g)
  dev.off()
  })
}

g1 <- ggplot() + geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="black", colour="gray") +
  geom_path(data=paths.bfld, aes(long, lat , group=group), colour=clrs[2]) +
  geom_point(data=d, aes(x=longitude, y=latitude, group=NULL, size=EstTime), colour=clrs[1]) +
  geom_path(data=paths.bfld, aes(long, lat , group=group), colour=clrs[1]) +
  #geom_point(data=d, aes(x=longitude, y=latitude, group=NULL, size=EstTime), shape=21, fill=clrs[2], colour="white") +
  theme_blank + coord_map("ortho", orientation=c(41, -74, 0)) #+
  #annotate("text", x=-41, y=33, label="App Traffic\nDay One", colour="white", family="mono", fontface="bold", size=8) +
  #annotate("text", x=-41, y=33, label="App Traffic\nDay One", colour="#1E90FF60", family="mono", fontface="bold", size=8)
  #scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + coord_cartesian(xlim=c(-180, 180), ylim=c(-60, 90))
g2 <- ggplot() + geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="black", colour="gray") +
  geom_path(data=paths.bfld, aes(long, lat , group=group), colour=clrs[2]) +
  geom_point(data=d, aes(x=longitude, y=latitude, group=NULL, size=EstTime), colour=clrs[1]) +
  geom_path(data=paths.bfld, aes(long, lat , group=group), colour=clrs[1]) +
  #geom_point(data=d, aes(x=longitude, y=latitude, group=NULL, size=EstTime), shape=21, fill=clrs[2], colour="white") +
  theme_blank + coord_map("ortho", orientation=c(20, 140, 0))
  #scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + coord_cartesian(xlim=c(-180, 180), ylim=c(-60, 90))
pdf("visitor_map_2014-04-01_2016-03-08.pdf", width=16, height=8, bg="black")
grid.arrange(g1, g2, ncol=2)
dev.off()

d[, list(sum(pageviews), sum(totalTime)), by=Acct]
d[, list(sum(pageviews), sum(totalTime)), by=Site]

d["Shiny", list(sum(pageviews), sum(totalTime)), by=pageTitle]
