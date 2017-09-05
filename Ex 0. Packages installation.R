# Please Run this command to install all packages at once

if(!require(c('sdm', 'raster', 'rgdal', 'usdm', 'dismo', 'rJava', 
              'randomForest', 'sp', 'ggplot2', 'pROC', 'ROCR', 'maps', 
              'mapdata',  'effects', "gbm", "survival", 'lattice', 'splines', 
              'parallel', 'mda', 'class', 'mgcv', 'nlme', 'mgcv', 'earth',
              'plotmo', 'plotrix', 'TeachingDemos',   'rpart', 'kernlab', 
              'e1071', 'brt'))){
  
  install.packages(c('sdm', 'raster', 'rgdal', 'usdm', 'dismo', 'rJava', 
                     'randomForest', 'sp', 'ggplot2', 'pROC', 'ROCR', 'maps', 
                     'mapdata',  'effects', "gbm", "survival", 'lattice', 'splines', 
                     'parallel', 'mda', 'class', 'mgcv', 'nlme', 'mgcv', 'earth',
                     'plotmo', 'plotrix', 'TeachingDemos',   'rpart', 'kernlab', 
                     'e1071', 'brt'), dependencies = TRUE)
  
  library('sdm', 'raster', 'rgdal', 'usdm', 'dismo', 'rJava', 
          'randomForest', 'sp', 'ggplot2', 'pROC', 'ROCR', 'maps', 
          'mapdata',  'effects', "gbm", "survival", 'lattice', 'splines', 
          'parallel', 'mda', 'class', 'mgcv', 'nlme', 'mgcv', 'earth',
          'plotmo', 'plotrix', 'TeachingDemos',   'rpart', 'kernlab', 
          'e1071', 'brt')
}

