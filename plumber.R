# plumber.R
# package: https://CRAN.R-project.org/package=plumber 
# this example: https://github.com/WinVector/PDSwR2/tree/master/Buzz

library("randomForest")

lst <- readRDS("thRS500.RDS")
varslist <- lst$varslist
fmodel <- lst$fmodel
buzztest <- lst$buzztest
rm(list = "lst")



#* Score a data frame.
#* @param d data frame to score
#* @post /score_data
function(d) { 	
  predict(fmodel, newdata = d, type = "prob")
}

#* Show a column of the i-th row of the data sample.
#* @param i the row index
#* @param col the column name
#* @post /show_row_i_col
function(i, col) { 	
  buzztest[i, col, drop = TRUE]
}

#* Score the i-th row of the data sample.
#* @param i the row index
#* @post /score_row_i
function(i) { 	
  dat <- buzztest[i, ]
  predict(fmodel, newdata = dat, type = "prob")
}

#* Score the i-th row of the data sample, with a modified explanatory variable.
#* @param i the row index
#* @param col the column name
#* @param newval the new value for the column col
#* @post /score_row_i_modified
function(i, col, newval) { 	
  dat <- buzztest[i, ]
  dat[1, col] <- newval
  predict(fmodel, newdata = dat, type = "prob")
}

