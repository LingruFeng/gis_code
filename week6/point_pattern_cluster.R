########################
#在这个分析中，我们将分析蓝色名人纪念碑的点分布模式。
#我们想要回答的问题是:“对于任何一个特定的伦敦行政区，该行政区内的蓝色纪念碑是随机分布的，还是呈现出某种分散或聚集的模式?”
########################

library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(tidyverse)

###########   1. set up the data   ################

##First, get the London Borough Boundaries
LondonBoroughs <- st_read(here::here("statistical-gis-boundaries-london", "ESRI", "London_Borough_Excluding_MHW.shp"))

#Pull out Londo. filter()函数用于对数据帧进行子集化，保留满足条件的所有行。
#st_transform函数用于转换sf（simple feature）的投影坐标
library(stringr)
BoroughMap <- LondonBoroughs %>%
  dplyr::filter(.,str_detect(GSS_CODE, "^E09"))%>%
  st_transform(., 27700)

#快速查看生成的伦敦地图和数据信息
qtm(BoroughMap)
summary(BoroughMap)

##Now get the location of all Blue Plaques in the City
BluePlaques <- st_read("https://s3.eu-west-2.amazonaws.com/openplaques/open-plaques-london-2018-04-08.geojson")%>%
  st_transform(.,27700)
summary(BluePlaques)

#plot the blue plaques in the city
tmap_mode("view")
tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaques) +
  tm_dots(col = "blue")


############  2. data cleaning   ###############

#删除重复点
#remove duplicates
library(tidyverse)
BluePlaques <- distinct(BluePlaques)


#删除在伦敦边界外的错误点
BluePlaquesSub <- BluePlaques[BoroughMap,]
#check to see that they've been removed
tmap_mode("view")
tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")

# 为了加快分析速度，缩小研究区域，选择Harrow这一个borough作为study area
#extract the borough

Harrow <- BoroughMap %>%
  dplyr::filter(., NAME=="Harrow")
#Check to see that the correct borough has been pulled out
tmap_mode("view")
tm_shape(Harrow) +
  tm_polygons(col = NA, alpha = 0.5)

#clip the data to our single borough
BluePlaquesSub <- BluePlaques[Harrow,]
#check that it's worked
tmap_mode("view")
tm_shape(Harrow) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")

#为了使用spatstat分析点分布，需要做一些处理
#create an observation window(ppp project) for spatstat to carry out its analysis within
#now set a window as the borough boundary
window <- as.owin(Harrow)
plot(window)

#create a ppp object
BluePlaquesSub<- BluePlaquesSub %>%
  as(., 'Spatial')

BluePlaquesSub.ppp <- ppp(x=BluePlaquesSub@coords[,1],
                          y=BluePlaquesSub@coords[,2],
                          window=window)
# Have a look at the new ppp object
BluePlaquesSub.ppp %>%
  plot(.,pch=4,cex=1, 
       main="Blue Plaques Harrow")

############# 3. point pattern analysis #################

###########  3.1 Kernel Density Estimation ######################

# The size and shape of the Kernel affects the density pattern produced, 
# but it is very easy to produce a Kernel Density Estimation (KDE) map
#from a ppp object using the density() function.

BluePlaquesSub.ppp %>%
  density(., sigma=200) %>%
  plot()

###########  3.2 Quadrat Analysis ##############
# 这个研究的目的是探究研究点的分布和‘complete spatial randomness’ — CSR是否相同
#First plot the points
plot(BluePlaquesSub.ppp,
     pch=16,
     cex=0.5, 
     main="Blue Plaques in Harrow")

#now count the points in that fall in a 6 x 6
#grid overlaid across the windowBluePlaquesSub.ppp2<-BluePlaquesSub.ppp %>%
BluePlaquesSub.ppp %>%
  quadratcount(.,nx = 6, ny = 6)%>%
  plot(., add=T, col="red")


# run the quadrat count then save it to the table
Qcount_1 <- BluePlaquesSub.ppp %>%
  quadratcount(.,nx = 6, ny = 6) %>%
  as.data.frame() 
# 将一个格子里有相同点数的group为一组进行统计
Qcount <- Qcount_1 %>%
  dplyr::count(Var1=Freq)%>%
  dplyr::rename(Freqquadratcount=n)

# check 一下表格的数据类型
Qcount %>% 
  summarise_all(class)

#calculate our expected probabilities based on the Poisson distribution
sums <- Qcount %>%
  #calculate the total blue plaques (Var * Freq)
  mutate(total = Var1 * Freqquadratcount) %>%
  dplyr::summarise(across(everything(), sum))%>%
  dplyr::select(-Var1) 

lambda<- Qcount%>%
  #calculate lambda
  mutate(total = Var1 * Freqquadratcount)%>%
  dplyr::summarise(across(everything(), sum)) %>%
  mutate(lambda=total/Freqquadratcount) %>%
  dplyr::select(lambda)%>%
  pull(lambda)

QCountTable <- Qcount %>%
  mutate(Pr=((lambda^Var1)*exp(-lambda))/factorial(Var1))%>%
  #now calculate the expected counts based on our total number of plaques
  #and save them to the table
  mutate(Expected= (round(Pr * sum(Freqquadratcount), 0)))

#Compare the frequency distributions of the observed and expected point patterns
plot(c(1,5),c(0,14), type="n",
     xlab="Number of Blue Plaques (Red=Observed,Blue=Expected)", 
     ylab="Frequency of Occurances")
points(QCountTable$Freqquadratcount, 
       col="Red", 
       type="o", 
       lwd=3)
points(QCountTable$Expected, col="Blue", 
       type="o", 
       lwd=3)

# 用chi square test检验两个实际和随机产生的频数是否是一致的
# 可以在teststats里查看p-value
# if p< 0.05, this indicates that we do have clustering in our points.
teststats <- quadrat.test(BluePlaquesSub.ppp, nx = 6, ny = 6)

# In the new plot, we can see three figures for each quadrant. 
# The top-left figure is the observed count of points; 
# the top-right is the Poisson expected number of points; 
# the bottom value is the residual value (also known as Pearson residual value), 
# or (Observed - Expected) / Sqrt(Expected).
plot(BluePlaquesSub.ppp,pch=16,cex=0.5, main="Blue Plaques in Harrow")
plot(teststats, col = "red",add=T)


################## 3.3 Ripley’s K #######################
# quadrat analysis有一定的限制，根据人为设置方块的大小，结果有所不同
# Ripley’s K test能避免这个问题
# 图中红线是破松分布理论K值，黑线是实际K值
# K值在红线上，数据在这个距离范围内cluster；反之dispersed
K <- BluePlaquesSub.ppp %>%
  Kest(., correction="border") %>%
  plot()

############## Density-based spatial clustering of applications with noise: DBSCAN
############## 基于密度的空间聚类与噪声应用:DBSCAN
# discovering clusters in space： DBSCAN
library(raster)
library(fpc)
# check the coordinate reference system of the Harrow spatial polygon:
st_geometry(BoroughMap)

#first extract the points from the spatial points data frame
BluePlaquesSubPoints <- BluePlaquesSub %>%
  coordinates(.)%>%
  as.data.frame()

#now run the dbscan analysis
db <- BluePlaquesSubPoints %>%
  fpc::dbscan(.,eps = 700, MinPts = 4)

#now plot the results
plot(db, BluePlaquesSubPoints, main = "DBSCAN Output", frame = F)
plot(BoroughMap, add=T)

# used to find suitable eps value based on the knee in plot
# k is no of nearest neighbours used, use min points
library(dbscan)

BluePlaquesSubPoints%>%
  dbscan::kNNdistplot(.,k=4)

# kNNdistplot中每有一个弯曲幅度大的地方就有一个cluster，用ggplot来让图像更直观
library(ggplot2)
# 查看db的数据情况（summary）
db
#可以看到点被聚类到了0/1/2/3/4五类
db$cluster
#把聚类结果赋值到BluePlaquesSubPoints
BluePlaquesSubPoints<- BluePlaquesSubPoints %>%
  mutate(dbcluster=db$cluster)


chulls <- BluePlaquesSubPoints %>%
  group_by(dbcluster) %>%
  dplyr::mutate(hull = 1:n(),
                hull = factor(hull, chull(coords.x1, coords.x2)))%>%
  arrange(hull)

#chulls2 <- ddply(BluePlaquesSubPoints, .(dbcluster), 
#  function(df) df[chull(df$coords.x1, df$coords.x2), ])

#其中dbcluster被分到0的点不算聚类，删去
chulls <- chulls %>%
  filter(dbcluster >=1)

#创建一个ggplot object
dbplot <- ggplot(data=BluePlaquesSubPoints, 
                 mapping=aes(coords.x1,coords.x2, colour=dbcluster, fill=dbcluster)) 
#add the points in
dbplot <- dbplot + geom_point()
#now the convex hulls
dbplot <- dbplot + geom_polygon(data = chulls, 
                                aes(coords.x1,coords.x2, group=dbcluster), 
                                alpha = 0.5) 
#now plot, setting the coordinates to scale correctly and as a black and white plot 
#(just for the hell of it)...
dbplot+ theme_bw()+ coord_equal()

###add a basemap
##First get the bbox in lat long for Harrow
HarrowWGSbb <- Harrow %>%
  st_transform(., 4326)%>%
  st_bbox()

#Now convert the basemap to British National Grid
library(OpenStreetMap)

basemap <- OpenStreetMap::openmap(c(HarrowWGSbb[2],HarrowWGSbb[1]),c(HarrowWGSbb[4],HarrowWGSbb[3]),
                                  zoom=NULL,
                                  "stamen-toner")

# convert the basemap to British National Grid
basemap_bng <- openproj(basemap, projection="+init=epsg:27700")

autoplot(basemap_bng) + 
  geom_point(data=BluePlaquesSubPoints, 
             aes(coords.x1,coords.x2, 
                 colour=dbcluster, 
                 fill=dbcluster)) + 
  geom_polygon(data = chulls, 
               aes(coords.x1,coords.x2, 
                   group=dbcluster,
                   fill=dbcluster), 
               alpha = 0.5)  




##########  Analysing Spatial Autocorrelation with Moran’s I, LISA and friends  ##########

###################  read the ward data  ##################
LondonWards <- st_read(here::here("statistical-gis-boundaries-london", 
                                  "ESRI",
                                  "London_Ward.shp"))
LondonWardsMerged <- st_read(here::here( 
                                        "statistical-gis-boundaries-london", 
                                        "ESRI",
                                        "London_Ward_CityMerged.shp"))%>%st_transform(.,27700)
library(janitor)
WardData <- read_csv("ward-profiles-excel-version.csv", 
                     na = c("NA", "n/a")) %>% clean_names()

# 将WardData里的average_gcse_capped_point_scores_2014列left join到LondonWardsMerged里
LondonWardsMerged <- LondonWardsMerged %>% 
  left_join(WardData, 
            by = c("GSS_CODE" = "new_code"))
LondonWardsMerged <-distinct(LondonWardsMerged,GSS_CODE, ward_name, average_gcse_capped_point_scores_2014)

#have a look to check that it's in the right projection
st_crs(LondonWardsMerged)

tmap_mode("view")
tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaques) +
  tm_dots(col = "blue")

###############  data cleaning  #################
# lose the blue plaques that fall outside of London
summary(BluePlaques)
BluePlaquesSub <- BluePlaques[LondonWardsMerged,]

tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")

#############  Data manipulation  #################
library(sf)
points_sf_joined <- st_join(LondonWardsMerged,BluePlaquesSub) %>%
  add_count(ward_name)%>%
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the points per ward
  mutate(density=n/area)%>%
  #select density and some other variables 
  dplyr::select(density, ward_name, gss_code, n, average_gcse_capped_point_scores_2014)

# a quick choropleth map to see how we are getting on
points_sf_joined<- points_sf_joined %>%                    
  group_by(gss_code) %>%         
  summarise(density = first(density),
            wardname= first(ward_name),
            plaquecount= first(n))
# 使用不同代码，mn和points_sf_joined生成相同数据
mn <- points_sf_joined %>% distinct(.keep_all = TRUE)  
mn <- mn %>%  select(gss_code,density=density,wardname=ward_name,plaquecount=n)

tm_shape(points_sf_joined) +
  tm_polygons("density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("wardname", "density"),
              title="Blue Plaque Density")

# 用spdep包计算spatial autocorrelation
library(spdep)

#First calculate the centroids of all Wards in London
coordsW <- points_sf_joined%>%
  st_centroid()%>%
  st_geometry()
plot(coordsW,axes=TRUE)

# start with a simple binary matrix of queen’s case neighbours (otherwise known as Contiguity edges corners)
# 该方法是指将具有共享边或共享角的多边形包含在目标多边形的计算中
#create a neighbours list
LWard_nb <- points_sf_joined %>%
  poly2nb(., queen=T)

#plot them
plot(LWard_nb, st_geometry(coordsW), col="red")
#add a map underneath
plot(points_sf_joined$geometry, add=T)

#create a spatial weights object from these weights
Lward.lw <- LWard_nb %>%
  nb2listw(., style="C")
head(Lward.lw$neighbours)


#################  calculate the Moran’s I and other associated statistics  ######################
# Moran’s I test tells us whether we have clustered values (close to 1) or dispersed values (close to -1)
I_LWard_Global_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  moran.test(., Lward.lw)

I_LWard_Global_Density

#################  calculate the Geary’s C and other associated statistics  ######################
C_LWard_Global_Density <- 
  points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  geary.test(., Lward.lw)

C_LWard_Global_Density

#################  calculate the Global G and other associated statistics  ######################
G_LWard_Global_Density <- 
  points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  globalG.test(., Lward.lw)

G_LWard_Global_Density


#use the localmoran function to generate I for each ward in the city

I_LWard_Local_count <- points_sf_joined %>%
  pull(plaquecount) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

I_LWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

#what does the output (the localMoran object) look like?
slice_head(I_LWard_Local_Density, n=5)

points_sf_joined <- points_sf_joined %>%
  mutate(plaque_count_I = as.numeric(I_LWard_Local_count$Ii))%>%
  mutate(plaque_count_Iz =as.numeric(I_LWard_Local_count$Z.Ii))%>%
  mutate(density_I =as.numeric(I_LWard_Local_Density$Ii))%>%
  mutate(density_Iz =as.numeric(I_LWard_Local_Density$Z.Ii))


#####################  Mapping outputs  ##########################
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
MoranColours<- rev(brewer.pal(8, "RdGy"))

tm_shape(points_sf_joined) +
  tm_polygons("plaque_count_Iz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Blue Plaques in London")

# What about the G statisic for hot and cold spots?
Gi_LWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localG(., Lward.lw)

head(Gi_LWard_Local_Density)

points_sf_joined <- points_sf_joined %>%
  mutate(density_G = as.numeric(Gi_LWard_Local_Density))

GIColours<- rev(brewer.pal(8, "RdBu"))

#now plot on an interactive map
tm_shape(points_sf_joined) +
  tm_polygons("density_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, Blue Plaques in London")

#############  Other variables: Average GSCE scores  #################
#use head to see what other variables are in the data file
head(points_sf_joined, n=2)
# Or print out the class of each column，although we need to drop the geometry.
Datatypelist <- LondonWardsMerged %>% 
  st_drop_geometry()%>%
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")
Datatypelist

# use the localmoran function to generate I for each ward of Average GSCE scores in the city
I_LWard_Local_GCSE <- LondonWardsMerged %>%
  arrange(GSS_CODE)%>%
  pull(average_gcse_capped_point_scores_2014) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

points_sf_joined <- points_sf_joined %>%
  arrange(gss_code)%>%
  mutate(GCSE_LocIz = as.numeric(I_LWard_Local_GCSE$Z.Ii))

tm_shape(points_sf_joined) +
  tm_polygons("GCSE_LocIz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, GCSE Scores")


# the Gi* statistic to look at clusters of high and low scores
G_LWard_Local_GCSE <- LondonWardsMerged %>%
  arrange(GSS_CODE)%>%
  pull(average_gcse_capped_point_scores_2014) %>%
  as.vector()%>%
  localG(., Lward.lw)

points_sf_joined <- points_sf_joined %>%
  arrange(gss_code)%>%
  mutate(GCSE_LocGiz = as.numeric(G_LWard_Local_GCSE))

tm_shape(points_sf_joined) +
  tm_polygons("GCSE_LocGiz",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, GCSE Scores")
