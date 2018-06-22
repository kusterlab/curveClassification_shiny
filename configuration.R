
# width used for the webpage

pagewidth <- 1300

#global setting of classification algorithm used in the app. Every algorithm implemented in the mlr package can be applied, if probability can be 
#predicted and if factors are supported. However, the corresponding package needs to be installed on the machine.

classifier <-  "classif.randomForest"



# Limits the upload size to 1GB

options(shiny.maxRequestSize=1000*1024^2)
