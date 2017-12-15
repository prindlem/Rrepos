install.packages('readr')
install.packages("h2o")
library(h2o)
library(readr)

#Loading data from the home directory
train <- read_csv("~/Digit Recognizer/train.csv")
test <- read_csv("~/Digit Recognizer/test.csv")

# Create a 28*28 matrix with pixel color values
m = matrix(unlist(train[10,-1]),nrow = 28,byrow = T)

# Plot that matrix
image(m,col=grey.colors(255))

rotate <- function(x) t(apply(x, 2, rev)) 

par(mfrow=c(2,3))
lapply(1:6, 
       function(x) image(
         rotate(matrix(unlist(train[x,-1]),nrow = 28,byrow = T)),
         col=grey.colors(255),
         xlab=train[x,1]
       )
)