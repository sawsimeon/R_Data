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

