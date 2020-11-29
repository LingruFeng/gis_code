######################3 Advanced raster analysis  ##########################

#用googledrive下载数据
library("googledrive")

o<-drive_download("https://drive.google.com/open?id=1MV7ym_LW3Pz3MxHrk-qErN1c_nR0NWXy",
                  path="prac7_data/exampleGoogleDrivedata/LC08_L1TP_203023_20190513_20190521_01_T1.tar.gz", 
                 overwrite=T)

#解压文件
library(tidyverse)
library(fs)
library(stringr)
library(utils)

listfiles<-dir_info(here::here("prac7_data", "Lsatdata")) %>%
  dplyr::filter(str_detect(path, ".gz")) %>%
  dplyr::select(path)%>%
  dplyr::pull()%>%
  #print out the .gz file
  print()%>%
  as.character()%>%
  utils::untar(exdir=here::here("prac7_data", "Lsatdata"))

###################  1. Loading  #####################
# we are going to be using a Landsat 8 raster of Manchester.
# The vector shape file for Manchester has been taken from an ESRI repository.
## listing all possible libraries that all presenters may need following each practical
library(sp)
library(raster)
library(rgeos)
library(rgdal)
library(rasterVis)
library(ggplot2)

# Now let’s list all our Landsat bands except band 8 along with our study area shapefile. 
# Each band is a seperate .TIF file.
library(stringr)
library(raster)
library(fs)
library(sf)
library(tidyverse)

# List your raster files excluding band 8 using the patter argument
listlandsat<-dir_info(here::here("prac7_data", "Lsatdata"))%>%
  dplyr::filter(str_detect(path, "[B123456790].TIF")) %>%
  dplyr::select(path)%>%
  pull()%>%
  as.character()%>%
  # Load our raster layers into a stack
  stack()

# Load the manchester boundary
manchester_boundary <- st_read(here::here("prac7_data", 
                                          "Manchester_boundary",
                                          "Manchester_boundary.shp"))

#check they have the same Coordinate Reference System (CRS)
crs(manchester_boundary)
crs(listlandsat)


######################### 2. Resampling band 8#####################
# 因为band8原数据有问题，需要resample再加回到listlandsat的stack里
# get band 8
b8list<-dir_info(here::here("prac7_data", "Lsatdata"))%>%
  dplyr::filter(str_detect(path, "[B8].TIF")) %>%
  dplyr::select(path)%>%
  pull()%>%
  as.character()%>%
  raster()
# Then, resample() and write out the new layer, resampling takes awhile
## ngb is a nearest neighbour sampling method
b8correct <- b8list%>%
  resample(., listlandsat$LC08_L1TP_203023_20190513_20190521_01_T1_B1, 
           method = "ngb") %>%
  # Write out the raster
  writeRaster(.,str_c(here::here("prac7_data", 
                                 "Lsatdata"), 
                      names(b8list), 
                      sep="/"),
              format='GTiff', 
              overwrite=TRUE)
# Load band 8 and add it to our raster stack
b8backin<-dir_info(here::here("prac7_data", "Lsatdata"))%>%
  dplyr::filter(str_detect(path, "[B8].tif")) %>%
  dplyr::select(path)%>%
  pull()%>%
  as.character()%>%
  raster()

listlandsat <- listlandsat %>%
  addLayer(., b8backin)

#We can compare it to see if both rasters have the same extent, number of rows and columns, 
# projection, resolution and origin
raster::compareRaster(listlandsat$LC08_L1TP_203023_20190513_20190521_01_T1_B1,
                      listlandsat$LC08_L1TP_203023_20190513_20190521_01_T1_B8)

################# 3. Clipping ########################
# first crop it to the extent of the shapefile and then mask it as we have done in previous practicals
lsatmask <- listlandsat %>%
  # now crop our temp data to the extent
  crop(.,manchester_boundary)%>%
  mask(.,  manchester_boundary)

# add mask to the filenames within the raster stack
names(lsatmask) <- names(lsatmask)%>%
  str_c(., 
        "mask", 
        sep="_")

# I need to write mine out in another location
outputfilenames <-
  str_c("prac7_data/Lsatdata/", "mask/", names(lsatmask) ,sep="")

lsatmask %>%
  writeRaster(., outputfilenames, 
              bylayer=TRUE, 
              format='GTiff', 
              overwrite=TRUE)

######################  Data exploration  #####################
###########  1. More loading and manipulating  ###################
# either read them back in from the saved file:

manc_files<-dir_info(here::here("prac7_data", "Lsatdata","mask")) %>%
  dplyr::filter(str_detect(path, "[B1234567]_mask.tif")) %>%
  dplyr::filter(str_detect(path, "B11", negate=TRUE))%>%
  dplyr::select(path)%>%
  pull()%>%
  stack()

# or extract them from the original stack
#manc<-stack(lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B1_mask,
#            lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B2_mask,
#            lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B3_mask,
#            lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B4_mask,
#            lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B5_mask,
#            lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B6_mask,
#            lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B7_mask)

# Name the Bands based on where they sample the electromagentic spectrum
names(manc_files) <- c('ultra-blue', 'blue', 'green', 'red', 'NIR', 'SWIR1', 'SWIR2') 
# some specific information from a raster stack
crs(manc_files) # projection
extent(manc_files) # extent
ncell(manc_files) # number of cells
dim(manc_files) # number of rows, columns, layers
nlayers(manc_files) # number of layers
res(manc_files) # xres, yres
manc <- manc_files

####################### 2.Plotting data  ############################
# true colour composite
manc_rgb <- stack(manc$red, manc$green, manc$blue)
# false colour composite
manc_false <- stack(manc$NIR, manc$red, manc$green)

manc_rgb %>%
  plotRGB(.,axes=TRUE, stretch="lin")

manc_false%>%
  plotRGB(.,axes=TRUE, stretch="lin")

########################## 3.Data similarity #######################
# Looking at single bands
plot(manc$SWIR2)

## How are these bands different?
#set the plot window size (2 by 2)
par(mfrow = c(2,2))
#plot the bands
plot(manc$blue, main = "Blue")
plot(manc$green, main = "Green")
plot(manc$red, main = "Red")
plot(manc$NIR, main = "NIR")

## Look at the stats of these bands
pairs(manc[[1:7]])

library(ggplot2)
library(GGally)

manc %>%
  as.data.frame(., na.rm=TRUE)%>%
  sample_n(., 100)%>%
  ggpairs(.,axisLabels="none")


####################  Basic raster calculations ######################
# generate additional raster layers, the first of which is NDVI
################## 1. NDVI(归一化植被指数) ##########################
# function structure:
#myfunction <- function(arg1, arg2, ... ){
#  statements
#  return(object)
#}
# 这里写了一个function，放在NDVI_function.R文件中，再引用
source('NDVI_function.R')
ndvi <- NDVIfun(manc$NIR, manc$red)

# check the output
ndvi %>%
  plot(.,col = rev(terrain.colors(10)), main = "Landsat-NDVI")
# Let's look at the histogram for this dataset
ndvi %>%
  hist(., breaks = 40, main = "NDVI Histogram", xlim = c(-.3,.8))
 
# 给植被数据重分类，认为在总波段3/4分位数以上的为植被
veg <- ndvi %>%
  reclassify(., cbind(-Inf, 0.3, NA))

veg %>%
  plot(.,main = 'Possible Veg cover')

# 与曼哈顿联系起来观察植被分布
manc_rgb %>%
  plotRGB(.,axes = TRUE, stretch = "lin", main = "Landsat True Color Composite")

veg %>%
  plot(., add=TRUE, legend=FALSE)


################### Advanced raster calculations ##############################
# 为了探究城市温度与城市区域是否有关系

################# 1 Calucating tempearture from Landsat data ###################
library(RStoolbox)

MTL<-dir_info(here::here("prac7_data", "Lsatdata")) %>%
  dplyr::filter(str_detect(path, "MTL.txt")) %>%
  dplyr::select(path)%>%
  pull()%>%
  readMeta()

#To see all the attributes
head(MTL)

# extract the values from the readMTL variable for Band 10
offsetandgain <-MTL %>%
  getMeta("B10_dn", metaData = ., what = "CALRAD")

offsetandgain
##OR  
offsetandgain_1 <- subset(MTL$CALRAD, rownames(MTL$CALRAD) == "B10_dn")

# Run the calculation using the band 10 raster layer
TOA <- offsetandgain$gain *
  lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B10_mask + 
  offsetandgain$offset

Calidata <- MTL$CALBT%>%
  as.data.frame()%>%
  mutate(Band=rownames(.))%>%
  filter(Band=="B10_dn")

# subset the columns
K1 <- Calidata %>%
  dplyr::select(K1)%>%
  pull()

K2 <- Calidata %>%
  dplyr::select(K2)%>%
  pull()

Brighttemp <- (K2 / log((K1 / TOA) + 1))
facveg <- (ndvi-0.2/0.5-0.2)^2
emiss <- 0.004*facveg+0.986
Boltzmann <- 1.38*10e-23
Plank <- 6.626*10e-34
c <- 2.998*10e8

p <- Plank*(c/Boltzmann)
#define remaining varaibles
lambda <- 1.09e-5
#run the LST calculation
LST <- Brighttemp/(1 +(lambda*Brighttemp/p)*log(emiss))
# check the values
LST
LST <- LST-273.15
plot(LST)

############### Calucating urban area from Landsat data #######################
# Normalized Difference Built-up Index归一化差分合成指数 (NDBI)
NDBI=((lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B6_mask-
         lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B5_mask)/
        (lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B6_mask+
           lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B5_mask))
NDBIfunexample <- NDVIfun(lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B6_mask,
                          lsatmask$LC08_L1TP_203023_20190513_20190521_01_T1_B5_mask)

################ Urban area and temperature relationship #################
plot(values(NDBI), values(LST))
# stack the layers
computeddata <- LST%>%
  stack(.,NDBI)%>%
  as.data.frame()%>%
  na.omit()%>%
  # take a random subset
  sample_n(., 500)%>%
  dplyr::rename(Temp="layer.1", NDBI="layer.2")

# check the output
plot(computeddata$Temp, computeddata$NDBI)

# Transfrom the data to a data.frame to work with ggplot, then plot
library(plotly)
library(htmlwidgets)
heat<-ggplot(computeddata, aes(x = NDBI, y = Temp))+
  geom_point(alpha=2, colour = "#51A0D5")+
  labs(x = "Temperature", 
       y = "Urban index",
       title = "Manchester urban and temperature relationship")+
  geom_smooth(method='lm', se=FALSE)+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

# interactive plot
ggplotly(heat)

computeddatafull <- LST%>%
  stack(.,NDBI)%>%
  as.data.frame()%>%
  na.omit()%>%
  # take a random subset
  dplyr::rename(Temp="layer.1", NDBI="layer.2")

# How about plotting the whole dataset rather than a random subset
hexbins <- ggplot(computeddatafull, 
                  aes(x=NDBI, y=Temp)) +
  geom_hex(bins=100, na.rm=TRUE) +
  labs(fill = "Count per bin")+
  geom_smooth(method='lm', se=FALSE, size=0.6)+
  theme_bw()

ggplotly(hexbins)

############# Statistical summary ################
library(rstatix)
Correlation <- computeddatafull %>%
  cor_test(Temp, NDBI, use = "complete.obs", method = c("pearson"))

Correlation
# statistic value (or t, or test statistic), we can work out the critical t value using:
abs(qt(0.05/2, 198268))
# Within this formula
# 0.05 is the confidence level (95%)
# 2 means a 2 sided test
# 198268 is the degrees of freedom (df), being the number of values we have -2
computeddatafull %>%
  pull(Temp)%>%
  length()












