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

library(dplyr)
customer_data = readRDS("custdata.RDS")

customer_data <- customer_data %>%
mutate(age = na_if(age, 0), 
income = ifelse(income < 0, NA, income))

customer_data <- customer_data %>%
mutate(gas_with_rent = (gas_usage ==1),
gas_with_electricity = (gas_usage ==2),
no_gas_bill = (gas_usage ==3)) %>%
mutate(gas_usage = ifelse(gas_usage < 4, NA, gas_usage))

count_missing = function(df) {
    sapply(df, FUN = function(col) sum(is.na(col)))
}

nacounts <- count_missing(customer_data)
hasNA = which(nacounts > 0)
nacounts[hasNA]

varlist <- setdiff(colnames(customer_data), c("custid", "health_ins"))

library(vtreat)

treatment_plan <- design_missingness_treatment(customer_data, varlist = varlist)
training_prepared <- prepare(treatment_plan, customer_data)

colnames(customer_data)
colnames(training_prepared)

nacounts <- sapply(training_prepared, FUN = function(col) sum(is.na(col)))
sum(nacounts)

htmissing <- which(is.na(customer_data$housing_type))

columns_to_look_at <- c("custid", "is_employed", "num_vehicles",
"housing_type", "health_ins")

customer_data[htmissing, columns_to_look_at] %>% head()

columns_to_look_at <- c("custid", "is_employed", "is_employed_isBAD",
"num_vehicles", "num_vehicles_isBAD",
"housing_type", "health_ins")

training_prepared[htmissing, columns_to_look_at] %>% head()

customer_data %>%
summarize(mean_vehicles = mean(num_vehicles, na.rm = TRUE),
mean_employed = mean(as.numeric(is_employed), na.rm = TRUE))

library(dplyr)
median_income_table <- readRDS("median_income.RDS")
head(median_income_table)

training_prepared <- training_prepared %>% 
left_join(., median_income_table, by = "state_of_res") %>%
mutate(income_normalized = income/median_income)

head(training_prepared[, c("income", "median_income", "income_normalized")])

summary(training_prepared$income_normalized)

summary(training_prepared$age)

mean_age <- mean(training_prepared$age)
age_normalized <- training_prepared$age/mean_age
summary(age_normalized)

(mean_age <- mean(training_prepared$age))

(sd_age <- sd(training_prepared$age))

print(mean_age + c(-sd_age, sd_age))

training_prepared$scaled_age <- (training_prepared$age - mean_age) / sd_age

training_prepared %>% filter(abs(age - mean_age) < sd_age) %>%
select(age, scaled_age) %>%
head()

training_prepared %>%
filter(abs(age - mean_age) > sd_age) %>%
select(age, scaled_age) %>%
head()

dataf <- training_prepared[, c("age", "income", "num_vehicles", "gas_usage")]
summary(dataf)

dataf_scaled <- scale(dataf, center = TRUE, scale = TRUE)
summary(dataf_scaled)

(means <- attr(dataf_scaled, "scaled:center"))

(sds <- attr(dataf_scaled, "scaled:scale"))

newdata <- customer_data
library(vtreat)
newdata_treated <- prepare(treatment_plan, newdata)

new_dataf <- newdata_treated[, c("age", "income", "num_vehicles", "gas_usage")]

dataf_scaled <- scale(new_dataf, center= means, scale = sds)

set.seed(25643)
customer_data$gp <- runif(nrow(customer_data))
customer_test <- subset(customer_data, gp <= 0.1)
customer_train <- subset(customer_data, gp > 0.1)

dim(customer_test)
dim(customer_data)

household_data <- readRDS("hhdata.RDS")
hh <- unique(household_data$household_id)

set.seed(243674)
households <- data.frame(household_id = hh,
gp = runif(length(hh)),
stringsAsFactors = FALSE)

household_data <- dplyr::left_join(household_data, households, by = "household_id")

library(ggplot2)
summary(iris)

head(iris)

ggplot(iris, aes(x = Petal.Length, y = Petal.Width,
shape = Species, color = Species)) +
geom_point(size = 2) +
ggtitle("Petal dimensions by iris species: al measurements")

columns_we_want <- c("Petal.Length", "Petal.Width", "Species")
rows_we_want <- iris$Petal.Length > 2

head(iris)

iris_base <- iris[rows_we_want, columns_we_want, drop = FALSE]

head(iris_base)

library(data.table)

iris_data.table <- as.data.table(iris)

columns_we_want <- c("Petal.Length", "Petal.Width", "Species")
rows_we_want <- iris_data.table$Petal.Length > 2

iris_data.table <- iris_data.table[rows_we_want, ..columns_we_want]
head(iris_data.table)

library(dplyr)

iris_dplyr <- iris %>%
select(.,
Petal.Length, Petal.Width, Species) %>%
filter(.,
Petal.Length > 2)

head(iris_dplyr)

library(ggplot2)
data(msleep)

str(msleep)
summary(msleep)

clean_base_1 <- msleep[complete.cases(msleep), , drop = FALSE]

summary(clean_base_1)

nrow(clean_base_1)

clean_base_2 <- na.omit(msleep)
nrow(clean_base_2)

library(data.table)
msleep_data.table <- as.data.table(msleep)

clean_data.table = msleep_data.table[complete.cases(msleep_data.table), ]
nrow(clean_data.table)

library(dplyr)

clean_dplyr <- msleep %>%
filter(., complete.cases(.))

nrow(clean_dplyr)
## 154/568

purchases <- wrapr::build_frame(
    "day", "hour", "n_purchase" |
    1, 9, 5 |
    2, 9, 3 |
    2, 11, 5 |
    1, 13, 1 |
    2, 13, 3 |
    1, 14, 1 
)

order_index <- with(purchases, order(day, hour))

purchases_ordered <- purchases[order_index , , drop = FALSE]
purchases_ordered$running_total <- cumsum(purchases_ordered$n_purchase)

purchases_ordered

library("data.table")

DT_purchases <- as.data.table(purchases)
order_cols <- c("day", "hour")
setorderv(DT_purchases, order_cols)
DT_purchases[, running_total := cumsum(n_purchase)]
print(DT_purchases)

library(dplyr)

res <- purchases %>%
arrange(., day, hour) %>%
mutate(., running_total = cumsum(n_purchase))

print(res)

order_indx <- with(purchases, order(day, hour))
purchases_ordered <- purchases[order_index, , drop = FALSE]

data_list <- split(purchases_ordered, purchases_ordered$day)

data_list <- lapply(
    data_list, 
    function(di) {
        di$running_total <- cumsum(di$n_purchase)
        di
    }
)

purchases_ordered <- do.call(base::rbind,  data_list)
rownames(purchases_ordered) <- NULL

purchases_ordered

library(datasets)
library(ggplot2)
summary(airquality)
library(lubridate)
library(ggplot2)

datestr <- function(day, month, year) {
    paste(day, month, year, sep = "-")
}

airquality_with_date <- airquality
airquality_with_date$date <- with(airquality_with_date, 
dmy(datestr(Day, Month, 1973)))

airquality_with_date <- airquality_with_date[, c("Ozone", "date"), drop = FALSE]

head(airquality_with_date)

ggplot(airquality_with_date, aes(x = date, y = Ozone)) +
geom_point() +
geom_line() + 
xlab("Date") +
ggtitle("New York ozone reading, May 1 - Setp 30, 1973")

library(wrapr)
airquality %.>%
transform(., date = dmy(datestr(Day, Month, 1973))) %.>%
subset(., !is.na(Ozone), select = c("Ozone", "date")) %.>%
head(.)

library(data.table)

DT_airquality <- as.data.table(airquality) [
    , date := dmy(datestr(Day, Month, 1973))
] [
    , c("Ozone", "date")
]

head(DT_airquality)

library(dplyr)

airquality_with_date2 <- airquality %>%
mutate(., date = dmy(datestr(Day, Month, 1973))) %>%
select(., Ozone, date)

head(airquality_with_date2)

library(zoo)

airquality_corrected <- airquality_with_date
airquality_corrected$OzoneCorrected <- na.locf(airquality_corrected$Ozone, na.rm = FALSE)
summary(airquality_corrected)

ggplot(airquality_corrected, aes(x = date, y = Ozone)) +
geom_point(aes(y = Ozone)) +
geom_line(aes(y = OzoneCorrected)) +
ggtitle("New York Ozone Reading, May 1 - Sept 30, 1973",
subtitle = "(corrected)") + 
xlab("Date")

library(ggplot2)
library(datasets)
data(iris)
head(iris)
iris_summary <- aggregate(
    cbind(Petal.Length, Petal.Width) ~ Species,
    data = iris,
    FUN = mean
)
print(iris_summary)

library(ggplot2)
ggplot(mapping = aes(x = Petal.Length, y = Petal.Width,
shape = Species, color = Species)) +
geom_point(data = iris,
alpha = 0.5) +
geom_point(data = iris_summary, size = 5) +
ggtitle("Average Petal dimensions by iris species\n(with raw data for reference)")

productTable <- wrapr::build_frame(
"productID", "price" |
"p1" , 9.99 |
"p2" , 16.29 |
"p3" , 19.99 |
"p4" , 5.49 |
"p5" , 24.49 )
salesTable <- wrapr::build_frame(
"productID", "sold_store", "sold_online" |
"p1" , 6 , 64 |
"p2" , 31 , 1 |
"p3" , 30 , 23 |
"p4" , 31 , 67 |
"p5" , 43 , 51 )

productTable2 <- wrapr::build_frame(
"productID", "price" |
"n1" , 25.49 |
"n2" , 33.99 |
"n3" , 17.99 )

productTable$productID <- factor(productTable$productID)
productTable2$productID <- factor(productTable2$productID)

rbind_base = rbind(productTable, productTable2)

str(rbind_base)
library(data.table)

rbindlist(list(productTable, 
productTable2))

library(dplyr)
bind_rows(list(productTable, productTable2))

productTable_marked <- productTable
productTable_marked$table <- "productTable"
productTable2_marked <- productTable2
productTable2_marked$table <- "productTable2"

rbind_base <- rbind(productTable_marked,
productTable2_marked)

rbind_base

tables <- split(rbind_base, rbind_base$table)
tables

library(data.table)
dt <- as.data.table(rbind_base)

f <- function(.BY, .SD) {
    max(.SD$price)
}

dt[, max_price := f(.BY, .SD), by = table]

print(dt)

library(data.table)

dt <- as.data.table(rbind_base)
grouping_column <- "table"
dt[, max_price := max(price), by = eval(grouping_column)]

print(dt)

rbind_base %>%
group_by(., table) %>%
mutate(., max_price = max(price)) %>%
ungroup(.)

cbind(productTable, salesTable[, -1])
library(data.table)
cbind(as.data.table(productTable),
as.data.table(salesTable[, -1]))

productTable <- wrapr::build_frame(
"productID", "price" |
"p1" , 9.99 |
"p3" , 19.99 |
"p4" , 5.49 |
"p5" , 24.49 )
salesTable <- wrapr::build_frame(
"productID", "unitsSold" |
"p1" , 10 |
"p2" , 43 |
"p3" , 55 |
"p4" , 8 )

merge(productTable, salesTable, by = "productID", all.x = TRUE)

library(data.table)

productTable_data.table <- as.data.table(productTable)
salesTable_data.table <- as.data.table(salesTable)

#index notation for join
#ideasis rows are produced fro each row insides the []
salesTable_data.table[productTable_data.table, on = "productID"]

merge(productTable, salesTable, by = "productID", all.x = TRUE)

library(data.table)

joined_table <- productTable
joined_table$unitsSold <- salesTable$unitsSold[match(joined_table$productID, salesTable$productID)]
print(joined_table)

library(dplyr)

left_join(productTable, salesTable, by  = "productID")

merge(productTable, salesTable, by = "productID")

library(data.table)

productTable_data.table <- as.data.table(productTable)
sales_Table_data.table <- as.data.table(salesTable)

merge(productTable, salesTable, by = "productID")

library(dplyr)
inner_join(productTable, salesTable, by = "productID")

merge(productTable, salesTable, by = "productID", all = TRUE)

library(data.table)
productTable_data.table <- as.data.table(productTable)
salesTable_data.table <- as.data.table(salesTable)

merge(productTable_data.table, salesTable_data.table, by = "productID", all = TRUE)

library(dplyr)

full_join(productTable, salesTable, by = "productID")

library("data.table")
quotes <- data.table(
bid = c(5, 5, 7, 8),
ask = c(6, 6, 8, 10),
bid_quantity = c(100, 100, 100, 100),
ask_quantity = c(100, 100, 100, 100),
when = as.POSIXct(strptime(
c("2018-10-18 1:03:17",
"2018-10-18 2:12:23",
"2018-10-18 2:15:00",
"2018-10-18 2:17:51"),
"%Y-%m-%d %H:%M:%S")))

print(quotes)

trades <- data.table(
trade_id = c(32525, 32526),
price = c(5.5, 9),
quantity = c(100, 200),
when = as.POSIXct(strptime(
c("2018-10-18 2:13:42",
"2018-10-18 2:19:20"),
"%Y-%m-%d %H:%M:%S")))

print(trades)

quotes[, quote_time := when]
trades[ , trade_time := when]
quotes[ trades, on = "when", roll = TRUE] [
    , .(quote_time, bid, price, ask, trade_id, trade_time)
]

library("datasets")
library("xts")
# move the date index into a column
dates <- index(as.xts(time(Seatbelts)))
Seatbelts <- data.frame(Seatbelts)
Seatbelts$date <- dates
# restrict down to 1982 and 1983

Seatbelts <- Seatbelts[(Seatbelts$date >= as.yearmon("มี.ค. 1982")) &
(Seatbelts$date <= as.yearmon("ธ.ค. 1983")), , drop = FALSE]

Seatbelts$date <- as.Date(Seatbelts$date)
# mark if the seatbelt law was in effect
Seatbelts$law <- ifelse(Seatbelts$law == 1, "new law", "pre-law")
# limit down to the columns we want
Seatbelts <- Seatbelts[, c("date", "DriversKilled", "front", "rear", "law")]

head(Seatbelts)

library(datasets)
library(data.table)
library(ggplot2)

ChickWeight <- data.frame(ChickWeight)
ChickWeight$Diet <- NULL
# pad names with zeros
padz <- function(x, n=max(nchar(x))) gsub(" ", "0", formatC(x, width=n))
# append "Chick" to the chick ids
ChickWeight$Chick <- paste0("Chick", padz(as.character(ChickWeight$Chick)))
head(ChickWeight)

ChickSummary <- as.data.table(ChickWeight)
ChickSummary <- ChickSummary[, .(count =  .N, weight = mean(weight),
q1_weight = quantile(weight, probs = 0.25),
q2_weight = quantile(weight, probs = 0.75)), by = Time]

head(ChickSummary)

library(ggplot2)

ChickSummary <- cdata::unpivot_to_blocks(
    ChickSummary, 
    nameForNewKeyColumn = "measurement",
    nameForNewValueColumn = "value",
    columnsToTakeFrom = c("count", "weight")
)

ChickSummary$q1_weight[ChickSummary$measurement== "count"] <- NA
ChickSummary$q2_weight[ChickSummary$measurement == "count"] <- NA
CW <- ChickWeight
CW$measurement <- "weight"

ggplot(ChickSummary, aes(x = Time, y = value, color = measurement))  +
geom_line(data = CW, aes(x = Time, y = weight, group = Chick), color = "LightGray") + 
geom_line(size = 2) + 
geom_ribbon(aes(ymin = q1_weight, ymax = q2_weight), alpha = 0.3, colour = NA) +
facet_wrap(~measurement, ncol = 1, scales = "free_y") +
theme(legend.position = "none") +
ylab(NULL) + 
ggtitle("Chick Weight and Count Measurements by Time",
subtitle = "25% through 75% quantiles of weight shown shaded around mean")

library(data.table)

ChickWeight_wide2 <- dcast.data.table(
    as.data.table(ChickWeight),
    Chick ~ Time, 
    value.var = "weight"
)

library("tidyr")

ChickWeight_wide1 <- spread(ChickWeight, key = Time,
value = weight)

head(ChickWeight_wide1)

library(RCurl)
x <- getURL("https://raw.githubusercontent.com/WinVector/zmPDSwR/master/Spambase/spamD.tsv")
spamD <- read.csv(text = x, sep = "\t")

spamTrain <- subset(spamD, spamD$rgroup >= 10)
spamTest <- subset(spamD, spamD$rgroup < 10)

spamVars <- setdiff(colnames(spamD), list('rgroup', 'spam'))

spamFormula <- as.formula(paste('spam == "spam"',
paste(spamVars, collapse = ' + '), sep = ' ~ '))

spamModel <- glm(spamFormula, family = binomial(link = 'logit'), 
data = spamTrain)

spamTrain$pred <- predict(spamModel, newdata = spamTrain, type = 'response')
spamTest$pred <- predict(spamModel, newdata = spamTest, type = "response")

sample <- spamTest[c(7, 35, 224, 327), c('spam', 'pred')]
print(sample)

confmat_spam <- table(truth = spamTest$spam,
prediction = ifelse(spamTest$pred > 0.5,
"spam", "non-spam"))
print(confmat_spam)

(confmat_spam[1,1] + confmat_spam[2,2]) / sum(confmat_spam)

confmat_akismet <- as.table(matrix(data = c(288-1, 17, 1, 13882-17), nrow =2, ncol = 2))

rownames(confmat_akismet) <- rownames(confmat_spam)
colnames(confmat_akismet) <- colnames(confmat_spam)

print(confmat_akismet)

(confmat_akismet[1,1] + confmat_akismet[2,2]) / sum(confmat_akismet)

confmat_spam[2,2] / (confmat_spam[2,2] + confmat_spam[1,2])

confmat_akismet[2,2] / (confmat_akismet[2, 2] + confmat_akismet[1, 2])

precision <- confmat_spam[2, 2] / (confmat_spam[2, 2] + confmat_spam[1, 2])
recall <- confmat_spam[2, 2] / (confmat_spam[2, 2] + confmat_spam[2, 1])

(F1 <- 2 * precision * recall / (precision + recall))

set.seed(234641)
N <- nrow(spamTest)
pull_out_ix <- sample.int(N, 100, replace = FALSE)
removed = spamTest[pull_out_ix, ]

get_performance <- function(sTest) {
    proportion <- mean(sTest$spam == "spam")
    confmat_spam <- table(truth = sTest$spam, prediction = ifelse(sTest$pred > 0.5, "spam", "non-spam"))
    precision <- confmat_spam[2, 2]/sum(confmat_spam[, 2])
    recall <- confmat_spam[2, 2]/sum(confmat_spam[2,])
    list(spam_proportion = proportion, 
    confmat_spam = confmat_spam,
    precision = precision, recall = recall)
}

sTest <- spamTest[-pull_out_ix, ]
get_performance(sTest)

get_performance(rbind(sTest, subset(removed, spam == "spam")))

get_performance(rbind(sTest, subset(removed, spam == "non-spam")))

confmat_spam[1, 1] / (confmat_spam[1, 1] + confmat_spam[1, 2])


library(RCurl)
x <- getURL("https://raw.githubusercontent.com/WinVector/PDSwR2/2e6ab6ff703fa6e0a23a0ba0e33e3d0ec8f48ffc/cricketchirps/crickets.csv")
crickets <- read.csv(text = x)

cricket_model <- lm(temperatureF ~ chirp_rate, data = crickets)
crickets$temp_pred <- predict(cricket_model, newdata = crickets)

error_sq <- (crickets$temp_pred - crickets$temperatureF)^2

(RMSE <- sqrt(mean(error_sq)))

error_sq <- (crickets$temp_pred - crickets$temperatureF)^2
numerator <- sum(error_sq)
delta_sq <- (mean(crickets$temperatureF) - crickets$temperatureF)^2
denominator = sum(delta_sq)

(R2 <- 1 - numerator/denominator)

library(WVPlots)
DoubleDensityPlot(spamTest, xvar = "pred", truthVar = "spam",
title = "Distribution of scores for spam filter")

library(WVPlots)
ROCPlot(spamTest, xvar = "pred",
truthVar = "spam",
truthTarget = "spam",
title = "Spam filter test performance")

library(sigr)
calcAUC(spamTest$pred, spamTest$spam== 'spam')

ylogpy <- function(y, py) {
    logpy = ifelse(py > 0, log(py), 0)
    y*logpy
}

y <- spamTest$spam == 'spam'

sum(ylogpy(y, spamTest$pred) +
ylogpy(1- y, 1-spamTest$pred))

(pNull <- mean(spamTrain$spam == "spam"))

sum(ylogpy(y, pNull) + ylogpy(1-y, 1-pNull))
library(sigr)

(deviance <- calcDeviance(spamTest$pred, spamTest$spam == 'spam'))

(nullDeviance <- calcDeviance(pNull, spamTest$spam == 'spam'))

(pseudoR2 <- 1 - deviance/nullDeviance)

iris <- iris

iris$class <- as.numeric(iris$Species == "setosa")

set.seed(2345)

intrain <- runif(nrow(iris)) < 0.75
train <- iris[intrain, ]
test <- iris[!intrain, ]

head(train)



fit_iris_example = function(variable_matrix, labelvec) {
library(xgboost)
  cv = xgb.cv(variable_matrix, label = labelvec,
              params=list(
                objective="binary:logistic"
              ),
              nfold=5,
              nrounds=100,
              print_every_n=10,
              metrics="logloss")

  evalframe = as.data.frame(cv$evaluation_log)
  NROUNDS = which.min(evalframe$test_logloss_mean)

  model = xgboost(data=variable_matrix, label=labelvec,
                  params=list(
                    objective="binary:logistic"
                  ),
                  nrounds=NROUNDS,
                  verbose=FALSE)

  model
}

input <- as.matrix(train[, 1:4])
model <- fit_iris_example(input, train$class)

library(lime)
explainer <- lime::lime(train[, 1:4],
model = model,
bin_continuous = TRUE, n_bins = 10)
(example <- test[5, 1:4, drop = FALSE])
test$class[5]
round(predict(model, newdata = as.matrix(example)))

explanation <- lime::explain(example,
explainer, n_labels = 1, n_features = 4)

plot_features(explanation)
(exapmle <- test[c(13, 24), 1:4])

test$class[c(13, 24)]

round(predict(model, newdata = as.matrix(example)))

explanation <- explain(example, explainer, n_labels = 1, n_features = 4, kernel_width = 0.5)

plot_features(explanation)

(example <- test[c(13, 24), 1:4])

test@class[c(13, 24)]

round(predict(model, newdata = as.matrix(example

library(zeallot)

c(texts, labels) %<-% readRDS("IMDBtrain.RDS")

list(text = texts[1], label = labels[1])

list(text = texts[12], label = labels[12])



library(wrapr)
library(xgboost)
# if this fails, make sure text2vec is installed:
# install.packages("text2vec")
library(text2vec)


#
# function that takes a training corpus (texts)
# and returns a vocabulary: 10,000 words that
# appear in at least 10% of the documents, but
# fewer than half.
#
create_pruned_vocabulary <- function(texts) {
  # create an iterator over the training set
  it_train <- itoken(texts,
                    preprocessor = tolower,
                    tokenizer = word_tokenizer,
                    ids = names(texts),
                    progressbar = FALSE)

  # tiny stop word list
  stop_words <- qc(the, a, an, this, that, those, i, you)
  vocab <- create_vocabulary(it_train, stopwords = stop_words)

  # prune the vocabulary
  # prune anything too common (appears in over half the documents)
  # prune anything too rare (appears in less than 0.1% of the documents)
  # limit to 10,000 words after that
  pruned_vocab <- prune_vocabulary(
    vocab,
    doc_proportion_max = 0.5,
    doc_proportion_min = 0.001,
    vocab_term_max = 10000
  )

  pruned_vocab
}


# take a corpus and a vocabulary
# and return a sparse matrix (of the kind xgboost will take)
# rows are documents, columns are vocab words
# this representation loses the order or the words in the documents
make_matrix <- function(texts, vocab) {
  iter <- itoken(texts,
                preprocessor = tolower,
                tokenizer = word_tokenizer,
                ids = names(texts),
                progressbar = FALSE)
  create_dtm(iter, vocab_vectorizer(vocab))
}

#
# Input:
# - dtm_train: document term matrix of class dgCmatrix
# - labelvvec: numeric vector of class labels (1 is positive class)
#
# Returns:
# - xgboost model
#
fit_imdb_model <- function(dtm_train, labels) {
  # run this estimate the number of rounds
  # needed for xgboost
  # cv <- xgb.cv(dtm_train, label = labels,
  #             params=list(
  #               objective="binary:logistic"
  #               ),
  #             nfold=5,
  #             nrounds=500,
  #             print_every_n=10,
  #             metrics="logloss")
  #
  # evalframe <- as.data.frame(cv$evaluation_log)
  # NROUNDS <- which.min(evalframe$test_logloss_mean)

  # we've run it already, so here's a good answer
  NROUNDS <- 371


  model <- xgboost(data=dtm_train, label=labels,
                  params=list(
                    objective="binary:logistic"
                  ),
                  nrounds=NROUNDS,
                  verbose=FALSE)

  model
}

vocab <- create_pruned_vocabulary(texts)
dtm_train <- make_matrix(texts, vocab)
model <- fit_imdb_model(dtm_train, labels)

c(test_txt, test_labels) %<-% readRDS("IMDBtest.RDS")

dtm_test <- make_matrix(test_txt, vocab)

predicted <- predict(model, newdata = dtm_test)

teframe <- data.frame(true_label = test_labels, pred = predicted)

(cmat <- with(teframe, table(truth = true_label, pred = pred > 0.5)))

sum(diag(cmat))/sum(cmat)

library(WVPlots)
DoubleDensityPlot(teframe, "pred", "true_label",
"Distribution of test prediction scores")

explainer <- lime(texts, model = model,
preprocess = function(x) make_matrix(x, vocab))

casename <- "test_19552";
sample_case <- test_txt[casename]
pred_prob <- predict(model, make_matrix(sample_case, vocab))
list(text = sample_case, 
label = test_labels[casename],
prediction = round(pred_prob))

explanation <- lime::explain(sample_case, explainer, n_labels = 1, n_features = 5)

plot_features(explanation)

plot_text_explanations(explanation)

casenames <- c("test_12034", "test_10294")
sample_cases <- test_txt[casenames]
pred_probs <- predict(model, newdata = make_matrix(sample_cases, vocab))
list(texts = sample_cases, 
labels = test_labels[casenames],
predictions = round(pred_probs))

explanation <- lime::explain(sample_cases, explainer,
n_labels = 1, n_features = 5)

plot_features(explanation)

plot_features(explanation)
plot_text_explanations(explanation)

predict(model, newdata = make_matrix(sample_cases[2], vocab))

psub <- readRDS("psub.RDS")

set.seed(3454351)
gp <- runif(nrow(psub))

dtrain <- subset(psub, gp >= 0.5)
dtest <- subset(psub, gp < 0.5)

model <- lm(log10(PINCP) ~ AGEP + SEX + COW + SCHL, data = dtrain)

dtest$predLogPINCP <- predict(model, newdata = dtest)
dtrain$predLogPINCP <- predict(model, newdata = dtrain) 

library(ggplot2)
ggplot(data = dtest, aes(x = predLogPINCP, y = log10(PINCP))) +
geom_point(alpha = 0.2, color = "darkgray") +
geom_smooth(color = "darkblue") +
geom_line(aes(x = log10(PINCP),
y = log10(PINCP)),
color = "blue", linetype = 2) +
coord_cartesian(xlim = c(4, 5.25),
ylim = c(3.5, 5.5))

ggplot(data = dtest, aes( x= predLogPINCP, 
y = predLogPINCP - log10(PINCP))) + 
geom_point(alpha = 0.2, color = "darkgray") + 
geom_smooth(color = "darkblue") + 
ylab("residual error (prediction - actual)")

rsq <- function(y, f) { 1 - sum((y - f)^2)/sum((y - mean(y))^2)}

rsq(log10(dtrian$PINCP), dtrain$predLogPINCP)

rsq(log10(dtest$PINCP), dtest$predLogPINCP)

rmse <- function(y, f) { sqrt(mean((y-f)^2))}

rmse(log10(dtrain$PINCP), dtrain$predLogPINCP)

rmse(log10(dtest$PINCP), dtest$predLogPINCP)

(resids_train <- summary(log10(dtrain$PINCP) -
predict(model, newdata = dtrian)))

(resids_test <- summary(log10(dtest$PINCP) -
predict(model, newdata = dtest)))

(df <- nrow(dtrain) - nrow(summary(model)$coefficients))

(modelResidualError <- sqrt(sum(residuals(model)^2)/df))

load("NatalRiskData.rData")
train <- sdata[sdata$ORIGRANDGROUP <= 5, ]
test <- sdata[sdata$ORIGRANDGROUP > 5, ]

complications <- c("ULD_MECO", "ULD_PRECIP", 
"ULD_BREECH")

riskfactors <- c("URF_DIAB", "URF_CHYPER", "URF_PHYPER",
"URF_ECLAM")
y <- "atRisk"
x <- c("PWGT","UPREVIS", "CIG_REC", "GESTREC3",
"DPLURAL", complications, riskfactors)
library(wrapr)
fmla <- mk_formula(y, x)

print(fmla)

model <- glm(fmla, data = train, family = binomial(link = "logit"))

train$pred <- predict(model, newdata = train, type = "response")
test$pred <- predict(model, newdata = test, type = "response")

sum(train$atRisk == TRUE)
sum(train$pred)

premature <- subset(train, GESTREC3 == "< 37 weeks")
sum(premature$atRisk == TRUE)
sum(premature$pred)

library(WVPlots)
DoubleDensityPlot(train, "pred", "atRisk",
title = "Distribution of natality risk scores")

library("WVPlots")
library(ggplot2)
plt <- PRTPlot(train, "pred", "atRisk", TRUE, 
plotvars = c("enrichment", "recall"),
thresholdrange = c(0, 0.05),
title = "Enrichment/recall vs. threshold for natality mdoel")
plt + geom_vline(xintercept = 0.02, color = "red", linetype = 2)

(ctab.test <- table(pred = test$pred > 0.02, atRisk = test$atRisk))

(precision <- ctab.test[2, 2]/ sum(ctab.test[2,]))

(recall <- ctab.test[2, 2]/ sum(ctab.test[, 2]))

(enrichment <- precision / mean(as.numeric(test$atRisk)))

coefficients(model)

loglikelihood <- function(y, py) {
  sum(y * log(py) + (1-y) *log(1 - py))
}

(pnull <- mean(as.numeric(train$atRisk)))
(null.dev <- -2 *loglikelihood(as.numeric(train$atRisk), pnull))

(model$null.deviance)

pred <- predict(model, newdata = train, type = "response")

(resid.dev <- -2 * loglikelihood(as.numeric(train$atRisk), pred))

model$deviance

testy <- as.numeric(test$atRisk)
testpred <- predict(model, newdata = test, type = "response")
(pnull.test <- mean(testy))

(null.dev.test <- -2 * loglikelihood(testy, pnull.test))

(resid.dev.test <- -2 * loglikelihood(testy, testpred))

pr2 <- 1 - (resid.dev/null.dev)
print(pr2)

pr2.test <- 1 - (resid.dev.test / null.dev.test)
print(pr2.test)

df.null = dim(train)[[1]] - 1

df.model = dim(train)[[1]] - length(model$coefficients)

(df.null <- dim(train)[[1]] - 1)
(df.model <- dim(train)[[1]] - length(model$coefficients))

(delDev <- null.dev - resid.dev)
(deldf <- df.null - df.model)
(p <- pchisq(delDev, deldf, lower.tail = FALSE))

aic <- 2 * (length(model$coefficients) - 
loglikelihood(as.numeric(train$atRisk), pred))
aic


library(RCurl)
x <- getURL("https://raw.githubusercontent.com/WinVector/PDSwR2/master/UCICar/car.data.csv")
cars <- read.csv(text = x)

vars <- setdiff(colnames(cars), "rating")

cars$fail <- cars$rating == "unacc"
outcome <- "fail"
set.seed(24351)
gp <- runif(nrow(cars))

library(zeallot)
c(cars_test, cars_train) %<-% split(cars, gp < 0.7)

nrow(cars_test)
nrow(cars_train)

library(wrapr)

(fmla <- mk_formula(outcome, vars))

model_glm <- glm(fmla, data = cars_train,
family = binomial)

summary(model_glm)

coefs <- coef(model_glm)[-1]
coef_frame <- data.frame(coef = names(coefs),
value = coefs)

library(ggplot2)
ggplot(coef_frame, aes(x = coef, y = value)) +
geom_pointrange(aes(ymin = 0, ymax = value)) +
ggtitle("Coefficients of logistic regression model") +
coord_flip()

cars_test$pred_glm <- predict(model_glm, newdata = cars_test, type = "response")

library(sigr)

confmat <- function(dframe, predvar) {
  cmat <- table(truth = ifelse(dframe$fail, "unacceptable", "passed"),
  prediction = ifelse(dframe[[predvar]] > 0.5,
  "unacceptable", "passed"))
  accuracy <- sum(diag(cmat)) / sum(cmat)
  deviance <- calcDeviance(dframe[[predvar]], dframe$fail)
  list(confusion_matrix = cmat, 
  accuracy = accuracy, 
  deviance = deviance)
}

confmat(cars_test, "pred_glm")

library(glmnet)
library(glmnetUtils)

(model_ridge <- cv.glmnet(formula = fmla, 
data = cars_train, alpha = 0,   
family = "binomial"))
(coefs <- coef(model_ridge))

coef_frame <- data.frame(coef = rownames(coefs)[-1],
value = coefs[-1, 1])

ggplot(coef_frame, aes(x = coef, y = value)) +
geom_pointrange(aes(ymin = 0, ymax= value)) +
ggtitle("Coefficients of ridge model") +
coord_flip()

prediction <- predict(model_ridge, newdata = cars_test,
type = "response")

cars_test$pred_ridge <- as.numeric(prediction)

confmat(cars_test, "pred_ridge")

prediction <- predict(model_ridge, 
newdata = cars_test, type = "response",
s = model_ridge$lambda.min)

(elastic_net <- cva.glmnet(fmla, cars_train, family = "binomial"))

get_cvm <- function(model) {
  index <- match(model$lambda.1se, model$lambda)
  model$cvm[index]
}

enet_performance <- data.frame(alpha = elastic_net$alpha)
models <- elastic_net$modlist
enet_performance$cvm <- vapply(models, get_cvm, numeric(1))

minix <- which.min(enet_performance$cvm)

(best_alpha <- elastic_net$alpha[minix])

ggplot(enet_performance, aes(x = alpha, y = cvm)) +
geom_point() + 
geom_line() + 
geom_vline(xintercept = best_alpha, color = "red", linetype = 2) +
ggtitle("CV loss as a function of alpha")

(model_enet <- cv.glmnet(fmla, cars_train,
alpha = best_alpha, family = "binomial"))

prediction <- predict(model_enet, newdata = cars_test, type = "response")

cars_test$pred_enet <- as.numeric(prediction)

confmat(cars_test, "pred_enet")


d <- read.table("orange_small_train.data.gz", 
header = TRUE, sep = "\t", na.strings = c("NA", ''))

library(RCurl)
x <- getURL("https://raw.githubusercontent.com/WinVector/PDSwR2/master/KDD2009/orange_small_train_churn.labels.txt")
churn <- read.table(text = x, header = FALSE, sep = "\t")
set.seed(729375)
rgroup <- base::sample(c("train", "calibrate", "test"),
nrow(d),
prob = c(0.8, 0.1, 0.1),
replace = TRUE)
dTrain <- d[rgroup == 'train', , drop = FALSE]
dCal <- d[rgroup == 'calibrate', , drop = FALSE]
dTrainAll <- d[rgroup %in% c('train', 'calibrate'), , drop = FALSE]
dTest <- d[rgroup == 'test', , drop = FALSE]

outcome <- 'churn'
vars <- setdiff(colnames(dTrainAll), outcome)

rm(list = c('d', 'churn', 'rgroup'))

load("KDD2009.Rdata")
outcome_summary <- table(
  churn = dTrain[, outcome],
  useNA = 'ifany'
)

library(wrapr)

outcome <- "churn"
vars <- setdiff(colnames(dTrainAll), outcome)

formula1 <- mk_formula("churn", vars, outcome_target = 1)
model1 <- glm(formula1, data = dTrainAll, family = binomial)
model2 <- glm((churn == 1) ~ Var1, data = dTrainAll, family = binomial)
summary(model2)
##  310/ 568
library(wrapr)
oucome <- 'chrun'
var <- setdiff(colnames(dTrainAll), outcome)
formula1 <- mk_formula("churn", vars, outcome_target = 1)
model1 <- glm(formula1, data = dTrainAll, family = binomial)

model2 <- glm((churn ==1)) ~ Var1, data = dTrainAll, family = binomial)
summary(model2)

dim(dTrainAll)

head(dTrainAll$VAr200)

length(unique(dTrainAll$Var200))

library(vtreat)

(parallel_cluster <- parallel::makeCluster(parallel::detectCores()))

treatment_plan <- vtreat::designTreatmentsC(
  dTrain,
  varlist = vars,
  outcomename = "churn",
  outcometarget = 1,
  verbose = FALSE,
  parallelCluster = parallel_cluster
)
library(vtreat)
dTrain_treated <- prepare(treatment_plan, dTrain, 
parallelCluster = parallel_cluster)
head(colnames(dTrain))
head(colnames(dTrain_treated))

score_frame <- treatment_plan$scoreFrame
t(subset(score_frame, origName %in% c("Var126", "Var189")))

t(subset(score_frame, origName == "Var218"))

comparison <- data.frame(original218 = dTrain$Var218,
impact218 = dTrain_treated$Var218_catB)

head(comparison)

treatment_plan_2 <- design_missingness_treatment(dTrain, varlist = vars)
dtrain_2 <- prepare(treatment_plan_2, dTrain)
head(dtrain_2$Var218)

model <- glm(churn ==1 ~ Var218, data = dtrain_2,
family = "binomial")

pred <- predict(model, newdata = dtrain_2, 
type  = "response")

(prevalence <- mean(dTrain$churn == 1))

logit <- function(p) {
  log(p/(1-p))
}

comparison$glm218 <- logit(pred) - logit(prevalence)

head(comparison)

score_frame[score_frame$origName == "Var200", , drop = FALSE]

dCal_treated <- prepare(treatment_plan,
dCal,
parallelCluster = parallel_cluster)

library(sigr)

calcAUC(dTrain_treated$Var200_catB, dTrain_treated$churn)

calcAUC(dCal_treated$Var200_catB, dCal_treated$churn)

library(vtreat)

parallel_cluster <- parallel::makeCluster(parallel::detectCores())

cross_frame_experiment <- vtreat::mkCrossFrameCExperiment(
  dTrainAll,
  varlist = vars,
  outcomename = "churn",
  outcometarget = 1,
  verbose = FALSE,
  parallelCluster = parallel_cluster
)

dTrainAll_treated <- cross_frame_experiment$crossFrame
treatment_plan <- cross_frame_experiment$treatments
score_frame <- treatment_plan$scoreFrame

dTest_treated <- prepare(treatment_plan, dTest,
parallelCluster = parallel_cluster)
library(sigr)
calcAUC(dTrainAll_treated$Var200_catB, dTrainAll_treated$churn)
calcAUC(dTest_treated$Var200_catB, dTest_treated$churn)
k <- 1
(significance_cutoff <- k / nrow(score_frame))
score_frame$selected <- score_frame$sig <- significance_cutoff

suppressPackageStartupMessages(library(dplyr))

score_frame %>%
group_by(., code, selected) %>%
summarize(., count = n()) %>%
ungroup(.) %>%
cdata::pivot_to_rowrecs(.,
columnToTakeKeysFrom = "selected",
columnToTakeValuesFrom = "count",
rowKeyColumns = "code", 
sep = "=")

library(wrapr)
newvars <- score_frame$varName[score_frame$selected]

f <- mk_formula("churn", newvars, outcome_target = 1)
model <- glm(f, data = dTrainAll_treated, family = binomial)

library(sigr)

dTest_treated$glm_pred <- predict(model, newdata = dTest_treated, type = "response")

calcAUC(dTest_treated$glm_pred, dTest_treated$churn == 1)

permTestAUC(dTest_treated, "glm_pred", "churn", yTarget = 1)

var_aucs <- vapply(newvars, function(vi) {
  calcAUC(dTrainAll_treated[[vi]], dTrainAll_treated$churn == 1)
}, numeric(1))

(test_train_aucs <- var_aucs[var_aucs >= max(var_aucs)])

table(prediction = dTest_treated$glm_pred >= 0.5,
truth = dTest$churn)

table(prediction = dTest_treated$glm_pred>0.15,
truth = dTest$churn)

auto_mpg <- readRDS("auto_mpg.RDS")
library(wrapr)
vars <- c("cylinders", "displacement", 
"horsepower", "weight", "acceleration",
"model_year", "origin")
f <- mk_formula("mpg", vars)
model <- lm(f, data = auto_mpg)
auto_mpg$prediction <- predict(model, newdata = auto_mpg)

str(auto_mpg[!complete.cases(auto_mpg), , drop = FALSE])
library(vtreat)
cfe <- mkCrossFrameNExperiment(auto_mpg, vars, "mpg", verbose = FALSE)
treatment_plan <- cfe$treatments
auto_mpg_treated <- cfe$crossFrame
score_frame <- treatment_plan$scoreFrame
new_vars <- score_frame$varName

newf <- mk_formula("mpg", new_vars)
new_model <- lm(newf, data = auto_mpg_treated)
auto_mpg$prediction <- predict(new_model, newdata = auto_mpg_treated)
str(auto_mpg[!complete.cases(auto_mpg), , drop = FALSE])
library(wrapr)

d <- build_frame(
"x1" , "x2" , "x3", "y" |
1 , "a" , 6 , 10 |
NA_real_, "b" , 7 , 20 |
3 , NA_character_, 8 , 30 )

d <- build_frame(
"x1" , "x2" , "x3", "y" |
1 , "a" , 6 , 10 |
NA_real_, "b" , 7 , 20 |
3 , NA_character_, 8 , 30 )

print(d)
plan2 <- vtreat::designTreatmentsZ(d, varlist = c("x1", "x2", "x3"), verbose = FALSE)
vtreat::prepare(plan2, d)

d <- build_frame(
"x1" , "x2" , "x3", "y" |
1 , "a" , 6 , 10 |
NA_real_, "b" , 7 , 20 |
3 , NA_character_, 8 , 30 )

print(d)

plan3 <- vtreat::designTreatmentsN(d, varlist = c("x1", "x2", "x3"),
outcomename = "y", codeRestriction = "catN", verbose = FALSE)
vtreat::prepare(plan3, d)

plan4 <- vtreat::designTreatmentsC(d, varlist = c("x1", "x2", "x3"),
outcomename = "y",
outcometarget = 20, 
codeRestriction = "catB", 
verbose = FALSE)

vtreat::prepare(plan4, d)
class(plan4)
names(plan4)
plan4$scoreFrame

set.seed(2019)

d <- data.frame(
  x_bad = sample(letters, 100, replace = TRUE),
  y = rnorm(100),
  stringsAsFactors = FALSE
)

d$x_good <- ifelse(d$y > rnorm(100), "non-neg", "neg")

head(d)

plan5 <- vtreat::designTreatmentsN(d, 
varlist = c("x_bad", "x_good"), 
outcomename = "y", 
codeRestriction = "catN",
minFraction = 2,
verbose = FALSE)

class(plan5)

print(plan5)
training_data1 <- vtreat::prepare(plan5, d)

cfe <- vtreat::mkCrossFrameNExperiment(d, 
varlist = c("x_bad", "x_good"),
outcomename = "y", 
codeRestriction = "catN", 
minFraction = 2, 
verbose = FALSE)

plan6 <- cfe$treatments
training_data2 <- cfe$crossFrame

res2 <- vtreat::patch_columns_into_frame(d, training_data2)
head(res2)

sigr::wrapFTest(res2, "x_bad_catN", "y")

sigr::wrapFTest(res2, "x_good_catN", "y")

plan6$scoreFrame

library(RCurl)
x <- getURL("https://raw.githubusercontent.com/WinVector/PDSwR2/master/Protein/protein.txt")
protein <- read.csv(text = x, sep = "\t", header = TRUE)
summary(protein)

vars_to_use <- colnames(protein)[-1]
pmatrix <- scale(protein[, vars_to_use])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

rm_scales <- function(scaled_matrix) {
  attr(scaled_matrix, "scaled:center") <- NULL
  attr(scaled_matrix, "scaled:scale") <- NULL
  scaled_matrix
}

pmatrix <- rm_scales(pmatrix)
distmat <- dist(pmatrix, method = "euclidean")
pfit <- hclust(distmat, method = "ward.D")
plot(pfit, labels = protein$Country)
rect.hclust(pfit, k = 5)
groups <- cutree(pfit, k = 5)

print_clusters <- function(data, groups, columns) {
  groupedD <- split(data, groups)
  lapply(groupedD,
  function(df) df[, columns])
}

cols_to_print <- wrapr::qc(Country, RedMeat, Fish, Fr.Veg)
print_clusters(protein, groups, cols_to_print)

library(ggplot2)
princ <- prcomp(pmatrix)
nComp <- 2
project <- predict(princ, pmatrix)[, 1:nComp]
project_plus <- cbind(as.data.frame(project),
cluster = as.factor(groups),
country = protein$Country)

ggplot(project_plus, aes(x = PC1, y = PC2)) +
geom_point(data = as.data.frame(project), color = "darkgrey") +
geom_point() + geom_text(aes(label = country),
hjust = 0, vjust = 1) + 
facet_wrap(~cluster, ncol = 3, labeller = label_both)

library(fpc)
kbest_p <- 5
cboot_hclust <- clusterboot(pmatrix, 
clustermethod = hclustCBI,
method = "ward.D", k = kbest_p)

summary(cboot_hclust$result)

groups <- cboot_hclust$result$partition
print_clusters(protein, groups, cols_to_print)

cboot_hclust$bootmean
cboot_hclust$bootbrd

sqr_edist <- function(x, y) {
  sum((x - y)^2)
}

wss_cluster <- function(clustermat) {
  c0 <- colMeans(clustermat)
  sum(apply(clustermat, 1, FUN = function(row) {sqr_edist(row, c0)}))
}

wss_total <- function(dmatrix, labels) {
  wsstot  <- 0
  k <- length(unique(labels))
  for (i in 1:k)
  wsstot <- wsstot + wss_cluster(subset(dmatrix, labels == i))
  wsstot
}

wss_total(pmatrix, groups)

get_wss <- function(dmatrix, max_clusters) {
  wss = numeric(max_clusters)
}

wss[1] <- wss_cluster(pmatrix)

kbest_p <- 5
pclusters <- kmeans(pmatrix, kbest_p, nstart = 100,
iter.max = 100)
summary(pclusters)
pclusters$centers

pclusters$size
groups <- pclusters$cluster
cols_to_print <- wrapr::qc(Country, RedMeat, Fish, Fr.Veg)
print_clusters(protein, groups, cols_to_print)

clustering_ch <- kmeansruns(pmatrix, krange = 1:10,
criterion = "ch")
clustering_ch$bestk

clustering_asw <- kmeansruns(pmatrix, krange = 1:10, criterion = "asw")
clustering_asw$bestk

clustering_asw$crit
clustering_ch$crit
clustering_meas$ch_crit

library(arules)
bookbaskets <- read.transactions("bookdata.tsv.gz",
format = "single",
header = TRUE,
sep = "\t",
cols = c("userid", "title"),
rm.duplicates = TRUE)

class(bookbaskets)
bookbaskets
dim(bookbaskets)
colnames(bookbaskets)[1:5]
rownames(bookbaskets)[1:5]
basketSizes <- size(bookbaskets)
summary(basketSizes)

quantile(basketSizes, probs = seq(0, 1, 0.1)) 
library(ggplot2)
ggplot(data.frame(count = basketSizes)) + 
geom_density(aes(x = count)) +
scale_x_log10()

bookCount <- itemFrequency(bookbaskets, "absolute")

summary(bookCount)

orderedBooks <- sort(bookCount, decreasing = TRUE)
knitr::kable(orderedBooks[1:10])

orderedBooks[1] / nrow(bookbaskets)

bookbaskets_use <- bookbaskets[basketSizes > 1]
dim(bookbaskets_use)

rules <- apriori(bookbaskets_use, 
parameter = list(support = 0.002, confidence = 0.75))

summary(rules)

measures <- interestMeasure(rules, measure = c("coverage", "fishersExactTest"),
transactions = bookbaskets_use)

summary(measures)

library(magrittr)

rules %>%
sort(., by = "confidence") %>%
head(., n = 5) %>%
inspect(.)

brules <- apriori(bookbaskets_use,
parameter = list(support = 0.001, 
confidence = 0.6), 
appearance = list(rhs = c("The Lovely Bones: A Novel"),
default = "1hs"))

library(RCurl)
x <- getURL("https://raw.githubusercontent.com/WinVector/PDSwR2/master/Spambase/spamD.tsv")
spamD <- read.csv(text = x, header = TRUE, sep = "\t")
spamD$isSpam <- spamD$spam == "spam"
spamTrain <- subset(spamD, spamD$rgroup >= 10)
spamTest <- subset(spamD, spamD$rgroup < 10)
spamVars <- setdiff(colnames(spamD), list('rgroup', 'spam', 'isSpam'))

library(wrapr)
spamFormula <- mk_formula('isSpam', spamVars)

loglikelihood <- function(y, py) {
  pysmooth <- ifelse(py == 0, 1e-12,
  ifelse(py == 1, 1 - 1e-12, py))
  sum(y * log(pysmooth) + (1 - y) * log(1 - pysmooth))
}

accuracyMeasures <- function(pred, truth, name = "model") {
  dev.norm <- -2 * loglikelihood(as.numeric(truth), pred) / length(pred)
  ctable <- table(truth = truth, pred = (pred > 0.5))
  accuracy <- sum(diag(ctable)) / sum(ctable)
  precision <- ctable[2, 2] / sum(ctable[, 2])
  recall <- ctable[2, 2] / sum(ctable[2, ])
  f1 <- 2 * precision * recall / (precision + recall)
  data.frame(model = name, accracy = accuracy, f1 = f1, dev.norm)
}

library(rpart)

treemodel <- rpart(spamFormula, spamTrain, method = "class")
library(rpart.plot)
rpart.plot(treemodel, type = 5, extra = 6)
predTrain <- predict(treemodel, newdata= spamTrain)[, 2]
trainperf_tree <- accuracyMeasures(predTrain, spamTrain$spam == "spam", name = "tree, Training")

library(randomForest)
lst <- readRDS("thRS500.RDS")
varslist <- lst$varlist
fmodel <- lst$model
buzztest <- lst$buzztest
rm(list = "lst")
function(d) {
  predict(fmodel, newdata = d, type = "prob")
}

library(plumber)
r <- plumb("plumber.R")
r$run(port=8000)


library(plumber)

url <- "https://www.google.com/search?sa=X&biw=852&bih=558&q=PTG+gas+station&npsic=0&rflfq=1&rlha=0&rllag=13757509,100573530,13384&tbm=lcl&ved=2ahUKEwi837Se6v3nAhUSA3IKHT9dBjUQjGp6BAgLEDg&tbs=lrf:!1m5!1u8!2m3!8m2!1u8050!3e1!1m4!1u3!2m2!3m1!1e1!2m4!1e17!4m2!17m1!1e2!2m1!1e3!3sIAE,lf:1,lf_ui:3&rldoc=1#rlfi=hd:;si:;mv:[[14.370161099999999,100.7780215],[13.499176199999999,100.185136]];start:20;tbs:lrf:!1m4!1u3!2m2!3m1!1e1!2m1!1e3!2m4!1e17!4m2!17m1!1e2!3sIAE,lf:1,lf_ui:3"
url <- "https://www.google.com/search?sa=X&biw=852&bih=558&q=PTG+gas+station&npsic=0&rflfq=1&rlha=0&rllag=13757509,100573530,13384&tbm=lcl&ved=2ahUKEwi837Se6v3nAhUSA3IKHT9dBjUQjGp6BAgLEDg&tbs=lrf:!1m5!1u8!2m3!8m2!1u8050!3e1!1m4!1u3!2m2!3m1!1e1!2m4!1e17!4m2!17m1!1e2!2m1!1e3!3sIAE,lf:1,lf_ui:3&rldoc=1#rlfi=hd:;si:;mv:[[13.917875299999999,100.7112169],[13.613467,100.34524449999999]];tbs:lrf:!1m4!1u3!2m2!3m1!1e1!2m1!1e3!2m4!1e17!4m2!17m1!1e2!3sIAE,lf:1,lf_ui:3"

### Get Table from html

library(rvest)

zip_code <- read_html("https://en.youbianku.com/Thailand")
html_code <- html_nodes(zip_code)
html_table <- html_table(html_code)
html_table[2][[1]]

library(ggmap)
library(maps)
library(mapdata)
library(jsonlite)
w2hr <- map_data("world2Hires")
thailand <- subset(w2hr, region == "Thailand")

encontrar <- function(lugar, radius, keyword) {
  # radius in meter
  coor <- paste(lugar[1], lugar[2], sep = ",")
  baseurl <- "https://maps.googleapis.com/maps/api/place/nearbysearch/json?"
  google_key <- c("API_Key")
  q <- paste(baseurl, "location=", coor, "&radius=", radius, "&types=Gas+stations&keyword=", keyword, "&key=", google_key, sep = "")
  print(q)
  data1 <-  fromJSON(q)
  lat_long <- data.frame(lat = data1$results$geometry$location$lat, 
  long = data1$results$geometry$location$lng)
  sitios <- data1$results$name
  result <- cbind(sitios, lat_long)
  return(result)

}

coordenadas <- c(13.33554, 99.21360)

encontrar(lugar = coordenadas, radius = 500, keyword = "PT")



