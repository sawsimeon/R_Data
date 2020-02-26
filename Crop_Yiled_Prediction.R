library(RCurl)
corn <- getURL("https://raw.githubusercontent.com/saeedkhaki92/CNN-RNN-Yield-Prediction/master/corn_data_csv.csv")
corn_data <- read.csv(text = corn)
corn_id <- getURL("https://raw.githubusercontent.com/saeedkhaki92/CNN-RNN-Yield-Prediction/master/Corn_Loc_ID.csv")
corn_id_data <- read.csv(text = corn_id)

