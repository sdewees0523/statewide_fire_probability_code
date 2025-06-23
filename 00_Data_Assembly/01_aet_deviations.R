library(here)
library(raster)

## Loading in normals and multiplying by 3, to get 3 year value for calculating deviations
aet_normal <- raster(here("data", "climate_normals", "aet_Norm.tif"))*3
cwd_normal <- raster(here("data", "climate_normals", "cwd_Norm.tif"))*3

## Reading in yearly aet values from BCM data
aet_2001 <- raster(here("data", "climate_oneyears", "extracted_features2001_2001.tiff"), band = 8)
aet_2002 <- raster(here("data", "climate_oneyears", "extracted_features2002_2002.tiff"), band = 8)
aet_2003 <- raster(here("data", "climate_oneyears", "extracted_features2003_2003.tiff"), band = 8)
aet_2004 <- raster(here("data", "climate_oneyears", "extracted_features2004_2004.tiff"), band = 8)
aet_2005 <- raster(here("data", "climate_oneyears", "extracted_features2005_2005.tiff"), band = 8)
aet_2006 <- raster(here("data", "climate_oneyears", "extracted_features2006_2006.tiff"), band = 8)
aet_2007 <- raster(here("data", "climate_oneyears", "extracted_features2007_2007.tiff"), band = 8)
aet_2008 <- raster(here("data", "climate_oneyears", "extracted_features2008_2008.tiff"), band = 8)
aet_2009 <- raster(here("data", "climate_oneyears", "extracted_features2009_2009.tiff"), band = 8)
aet_2010 <- raster(here("data", "climate_oneyears", "extracted_features2010_2010.tiff"), band = 8)
aet_2011 <- raster(here("data", "climate_oneyears", "extracted_features2011_2011.tiff"), band = 8)
aet_2012 <- raster(here("data", "climate_oneyears", "extracted_features2012_2012.tiff"), band = 8)
aet_2013 <- raster(here("data", "climate_oneyears", "extracted_features2013_2013.tiff"), band = 8)
aet_2014 <- raster(here("data", "climate_oneyears", "extracted_features2014_2014.tiff"), band = 8)
aet_2015 <- raster(here("data", "climate_oneyears", "extracted_features2015_2015.tiff"), band = 8)
aet_2016 <- raster(here("data", "climate_oneyears", "extracted_features2016_2016.tiff"), band = 8)
aet_2017 <- raster(here("data", "climate_oneyears", "extracted_features2017_2017.tiff"), band = 1)
aet_2018 <- raster(here("data", "climate_oneyears", "extracted_features2018_2018.tiff"), band = 1)
aet_2019 <- raster(here("data", "climate_oneyears", "extracted_features2019_2019.tiff"), band = 1)
aet_2020 <- raster(here("data", "climate_oneyears", "extracted_features2020_2020.tiff"), band = 1)

## Calculating three year aet values
aet_sum2003 <- aet_2001 + aet_2002 + aet_2003
aet_sum2004 <- aet_2002 + aet_2003 + aet_2004
aet_sum2005 <- aet_2003 + aet_2004 + aet_2005
aet_sum2006 <- aet_2004 + aet_2005 + aet_2006
aet_sum2007 <- aet_2005 + aet_2006 + aet_2007
aet_sum2008 <- aet_2006 + aet_2007 + aet_2008
aet_sum2009 <- aet_2007 + aet_2008 + aet_2009
aet_sum2010 <- aet_2008 + aet_2009 + aet_2010
aet_sum2011 <- aet_2009 + aet_2010 + aet_2011
aet_sum2012 <- aet_2010 + aet_2011 + aet_2012
aet_sum2013 <- aet_2011 + aet_2012 + aet_2013
aet_sum2014 <- aet_2012 + aet_2013 + aet_2014
aet_sum2015 <- aet_2013 + aet_2014 + aet_2015
aet_sum2016 <- aet_2014 + aet_2015 + aet_2016
aet_sum2017 <- aet_2015 + aet_2016 + aet_2017
aet_sum2018 <- aet_2016 + aet_2017 + aet_2018
aet_sum2019 <- aet_2017 + aet_2018 + aet_2019
aet_sum2020 <- aet_2018 + aet_2019 + aet_2020

## Calculating yearly three year aet deviation from 3-year normal
aet2003_dev <- aet_sum2003-aet_normal
aet2004_dev <- aet_sum2004-aet_normal
aet2005_dev <- aet_sum2005-aet_normal
aet2006_dev <- aet_sum2006-aet_normal
aet2007_dev <- aet_sum2007-aet_normal
aet2008_dev <- aet_sum2008-aet_normal
aet2009_dev <- aet_sum2009-aet_normal
aet2010_dev <- aet_sum2010-aet_normal
aet2011_dev <- aet_sum2011-aet_normal
aet2012_dev <- aet_sum2012-aet_normal
aet2013_dev <- aet_sum2013-aet_normal
aet2014_dev <- aet_sum2014-aet_normal
aet2015_dev <- aet_sum2015-aet_normal
aet2016_dev <- aet_sum2016-aet_normal
aet2017_dev <- aet_sum2017-aet_normal
aet2018_dev <- aet_sum2018-aet_normal
aet2019_dev <- aet_sum2019-aet_normal
aet2020_dev <- aet_sum2020-aet_normal

## Writing yearly 3-year aet deviations to disk
writeRaster(aet2003_dev, here("data", "aet", "aet2003_dev.tif"))
writeRaster(aet2004_dev, here("data", "aet", "aet2004_dev.tif"))
writeRaster(aet2005_dev, here("data", "aet", "aet2005_dev.tif"))
writeRaster(aet2006_dev, here("data", "aet", "aet2006_dev.tif"))
writeRaster(aet2007_dev, here("data", "aet", "aet2007_dev.tif"))
writeRaster(aet2008_dev, here("data", "aet", "aet2008_dev.tif"))
writeRaster(aet2009_dev, here("data", "aet", "aet2009_dev.tif"))
writeRaster(aet2010_dev, here("data", "aet", "aet2010_dev.tif"))
writeRaster(aet2011_dev, here("data", "aet", "aet2011_dev.tif"))
writeRaster(aet2012_dev, here("data", "aet", "aet2012_dev.tif"))
writeRaster(aet2013_dev, here("data", "aet", "aet2013_dev.tif"))
writeRaster(aet2014_dev, here("data", "aet", "aet2014_dev.tif"))
writeRaster(aet2015_dev, here("data", "aet", "aet2015_dev.tif"))
writeRaster(aet2016_dev, here("data", "aet", "aet2016_dev.tif"))
writeRaster(aet2017_dev, here("data", "aet", "aet2017_dev.tif"))
writeRaster(aet2018_dev, here("data", "aet", "aet2018_dev.tif"))
writeRaster(aet2019_dev, here("data", "aet", "aet2019_dev.tif"))
writeRaster(aet2020_dev, here("data", "aet", "aet2020_dev.tif"))

## Reading in yearly cwd data from BCM
cwd_2001 <- raster(here("data", "climate_oneyears", "extracted_features2001_2001.tiff"), band = 22)
cwd_2002 <- raster(here("data", "climate_oneyears", "extracted_features2002_2002.tiff"), band = 22)
cwd_2003 <- raster(here("data", "climate_oneyears", "extracted_features2003_2003.tiff"), band = 22)
cwd_2004 <- raster(here("data", "climate_oneyears", "extracted_features2004_2004.tiff"), band = 22)
cwd_2005 <- raster(here("data", "climate_oneyears", "extracted_features2005_2005.tiff"), band = 22)
cwd_2006 <- raster(here("data", "climate_oneyears", "extracted_features2006_2006.tiff"), band = 22)
cwd_2007 <- raster(here("data", "climate_oneyears", "extracted_features2007_2007.tiff"), band = 22)
cwd_2008 <- raster(here("data", "climate_oneyears", "extracted_features2008_2008.tiff"), band = 22)
cwd_2009 <- raster(here("data", "climate_oneyears", "extracted_features2009_2009.tiff"), band = 22)
cwd_2010 <- raster(here("data", "climate_oneyears", "extracted_features2010_2010.tiff"), band = 22)
cwd_2011 <- raster(here("data", "climate_oneyears", "extracted_features2011_2011.tiff"), band = 22)
cwd_2012 <- raster(here("data", "climate_oneyears", "extracted_features2012_2012.tiff"), band = 22)
cwd_2013 <- raster(here("data", "climate_oneyears", "extracted_features2013_2013.tiff"), band = 22)
cwd_2014 <- raster(here("data", "climate_oneyears", "extracted_features2014_2014.tiff"), band = 22)
cwd_2015 <- raster(here("data", "climate_oneyears", "extracted_features2015_2015.tiff"), band = 22)
cwd_2016 <- raster(here("data", "climate_oneyears", "extracted_features2016_2016.tiff"), band = 22)
cwd_2017 <- raster(here("data", "climate_oneyears", "extracted_features2017_2017.tiff"), band = 2)
cwd_2018 <- raster(here("data", "climate_oneyears", "extracted_features2018_2018.tiff"), band = 2)
cwd_2019 <- raster(here("data", "climate_oneyears", "extracted_features2019_2019.tiff"), band = 2)
cwd_2020 <- raster(here("data", "climate_oneyears", "extracted_features2020_2020.tiff"), band = 2)

## Caclculating three year cwd values
cwd_sum2003 <- cwd_2001 + cwd_2002 + cwd_2003
cwd_sum2004 <- cwd_2002 + cwd_2003 + cwd_2004
cwd_sum2005 <- cwd_2003 + cwd_2004 + cwd_2005
cwd_sum2006 <- cwd_2004 + cwd_2005 + cwd_2006
cwd_sum2007 <- cwd_2005 + cwd_2006 + cwd_2007
cwd_sum2008 <- cwd_2006 + cwd_2007 + cwd_2008
cwd_sum2009 <- cwd_2007 + cwd_2008 + cwd_2009
cwd_sum2010 <- cwd_2008 + cwd_2009 + cwd_2010
cwd_sum2011 <- cwd_2009 + cwd_2010 + cwd_2011
cwd_sum2012 <- cwd_2010 + cwd_2011 + cwd_2012
cwd_sum2013 <- cwd_2011 + cwd_2012 + cwd_2013
cwd_sum2014 <- cwd_2012 + cwd_2013 + cwd_2014
cwd_sum2015 <- cwd_2013 + cwd_2014 + cwd_2015
cwd_sum2016 <- cwd_2014 + cwd_2015 + cwd_2016
cwd_sum2017 <- cwd_2015 + cwd_2016 + cwd_2017
cwd_sum2018 <- cwd_2016 + cwd_2017 + cwd_2018
cwd_sum2019 <- cwd_2017 + cwd_2018 + cwd_2019
cwd_sum2020 <- cwd_2018 + cwd_2019 + cwd_2020

## Calculating yearly 3-year cwd deviation from 3-year normal
cwd2003_dev <- cwd_sum2003-cwd_normal
cwd2004_dev <- cwd_sum2004-cwd_normal
cwd2005_dev <- cwd_sum2005-cwd_normal
cwd2006_dev <- cwd_sum2006-cwd_normal
cwd2007_dev <- cwd_sum2007-cwd_normal
cwd2008_dev <- cwd_sum2008-cwd_normal
cwd2009_dev <- cwd_sum2009-cwd_normal
cwd2010_dev <- cwd_sum2010-cwd_normal
cwd2011_dev <- cwd_sum2011-cwd_normal
cwd2012_dev <- cwd_sum2012-cwd_normal
cwd2013_dev <- cwd_sum2013-cwd_normal
cwd2014_dev <- cwd_sum2014-cwd_normal
cwd2015_dev <- cwd_sum2015-cwd_normal
cwd2016_dev <- cwd_sum2016-cwd_normal
cwd2017_dev <- cwd_sum2017-cwd_normal
cwd2018_dev <- cwd_sum2018-cwd_normal
cwd2019_dev <- cwd_sum2019-cwd_normal
cwd2020_dev <- cwd_sum2020-cwd_normal

## Writing yearly 3-year cwd deviations to disk
writeRaster(cwd2003_dev, here("data", "cwd", "cwd2003_dev.tif"))
writeRaster(cwd2004_dev, here("data", "cwd", "cwd2004_dev.tif"))
writeRaster(cwd2005_dev, here("data", "cwd", "cwd2005_dev.tif"))
writeRaster(cwd2006_dev, here("data", "cwd", "cwd2006_dev.tif"))
writeRaster(cwd2007_dev, here("data", "cwd", "cwd2007_dev.tif"))
writeRaster(cwd2008_dev, here("data", "cwd", "cwd2008_dev.tif"))
writeRaster(cwd2009_dev, here("data", "cwd", "cwd2009_dev.tif"))
writeRaster(cwd2010_dev, here("data", "cwd", "cwd2010_dev.tif"))
writeRaster(cwd2011_dev, here("data", "cwd", "cwd2011_dev.tif"))
writeRaster(cwd2012_dev, here("data", "cwd", "cwd2012_dev.tif"))
writeRaster(cwd2013_dev, here("data", "cwd", "cwd2013_dev.tif"))
writeRaster(cwd2014_dev, here("data", "cwd", "cwd2014_dev.tif"))
writeRaster(cwd2015_dev, here("data", "cwd", "cwd2015_dev.tif"))
writeRaster(cwd2016_dev, here("data", "cwd", "cwd2016_dev.tif"))
writeRaster(cwd2017_dev, here("data", "cwd", "cwd2017_dev.tif"))
writeRaster(cwd2018_dev, here("data", "cwd", "cwd2018_dev.tif"))
writeRaster(cwd2019_dev, here("data", "cwd", "cwd2019_dev.tif"))
writeRaster(cwd2020_dev, here("data", "cwd", "cwd2020_dev.tif"))

