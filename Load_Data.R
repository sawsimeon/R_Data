load("bxBooks.RData")
colnames(bxBooks) <- gsub(".", "_", colnames(bxBooks), fixed=T)
colnames(bxBookRatings) <- gsub(".", "_", colnames(bxBookRatings), fixed=T)
colnames(bxUsers) <- gsub(".", "_", colnames(bxUsers), fixed=T)


Sys.setlocale('LC_ALL','C') # to deal with the non-US characters
# remove parentheticals, which are usually
# at the end of the title. First get rid of the open paren
booktokens <- gsub("(", "#", bxBooks$Book_Title, fixed=T)
booktokens <- gsub("^#", "(", booktokens)
booktokens <- gsub("#.*$", "", booktokens) # leaves a trailing white space
cleantitles <- sub("[[:space:]]+$","",booktokens) # save these

booktokens <- tolower(cleantitles)
Books <- data.frame(ISBN=bxBooks$ISBN, token=booktokens, title=cleantitles)

library(sqldf)
# picks a unique isbn for every token  -- this is the number of unique tokens
bookmap <- sqldf('SELECT min(ISBN) as misbn,
                        token 
                 FROM Books
                 GROUP BY token')

# displaymap has a title for every unique token
displaymap <- sqldf('SELECT Books.title as title,
                           bookmap.token as token
                   FROM Books,
                        bookmap
                   WHERE Books.ISBN=bookmap.misbn')

# bookdata1 is shorter than bxBookRatings because
# some of the rated books are not in the bxBooks data
bookdata1 <- sqldf('SELECT ratings.User_ID as userid,
                         Books.token as token,
                         ratings.Book_Rating as rating
                  FROM Books,
                       bxBookRatings as ratings
                  WHERE ratings.ISBN=Books.ISBN')

# add the displayname
bookdata <- merge(bookdata1, displaymap, by="token")

write.table(bookdata, file="bookdata.tsv",
            sep="\t", row.names=F, col.names=T)


library(RCurl)
x <- getURL("https://raw.githubusercontent.com/WinVector/PDSwR2/master/UCICar/car.data.csv")
uciCar <- read.csv(text = x)
class(uciCar)
summary(uciCar)
dim(uciCar)

customer_data = readRDS("custdata.RDS")
summary(customer_data)

summary(customer_data$income)
summary(customer_data$age)
summary(customer_data$income)

incomeK = customer_data$income/1000
summary(incomeK)

library(ggplot2)
ggplot(customer_data, aes(x = gas_usage)) +
geom_histogram(binwidth = 10, fill = "gray")

library(scales)
ggplot(customer_data, aes(x = income)) +
geom_density() + scale_x_continuous(labels = dollar)

ggplot(customer_data, aes(x = state_of_res)) +
geom_bar(fill = "gray") + coord_flip()

library(WVPlots)
ClevelandDotPlot(customer_data, "state_of_res",
sort = 1, title = "Customers by state") +
coord_flip()

x <- runif(100)
y <- x^2 + 0.2*2
ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
geom_line() 

customer_data2 <- subset(customer_data,
0 < age & age < 100 &
0 < income & income < 200000)
cor(customer_data2$age, customer_data2$income)

set.seed(245566)
customer_data_samp <- 
dplyr::sample_frac(customer_data2, size = 0.1, replace = FALSE)

ggplot(customer_data_samp, aes(x = age, y = income)) +
geom_point() + geom_smooth() + 
ggtitle("Income as a function of age")

library(WVPlots)

HexBinPlot(customer_data2, "age", "income", "Income as a function of age") +
geom_smooth(color = "black", se = FALSE)

ggplot(customer_data, aes(x = marital_status, fill = health_ins)) +
geom_bar()

ggplot(customer_data, aes(x = marital_status, fill = health_ins)) +
geom_bar(position = "dodge")

ShadowPlot(customer_data, "marital_status", "health_ins",
title = "Health insurance status by marital status")

ggplot(customer_data, aes(x = marital_status, fill = health_ins)) +
geom_bar(position = "fill")

cdata <- subset(customer_data, !is.na(housing_type))

ggplot(cdata, aes(x = housing_type, fill = marital_status)) +
geom_bar(position = "dodge") +
scale_fill_brewer(palette = "Dark2") +
coord_flip()

ggplot(cdata, aes(x = marital_status)) +
geom_bar(fill = "darkgray") +
facet_wrap(~housing_type, scale = "free_x") +
coord_flip()

customer_data3 = subset(customer_data2, marital_status %in% 
c("Never Married", "Widowed"))

ggplot(customer_data3, aes(x = age, color = marital_status, 
linetype = marital_status)) +
geom_density() + scale_color_brewer(palette = "Dark2")

ShadowHist(customer_data3, "age", "marital_status",
"Age distribution for never married vs. widowed populations", binwidth = 5)

ggplot(customer_data2, aes(x = age)) +
geom_density() + facet_wrap(~marital_status)

