library(keras)

mnist <- dataset_mnist()
train_images <- mnist$train$x
train_labels <- mnist$train$y 
test_images <- mnist$test$x 
test_labels <- mnist$test$y 

str(train_images)
str(train_labels)

str(test_images)
str(test_labels)

network <- keras_model_sequential() %>%
layer_dense(units = 512, activation = "relu", input_shape = c(28 * 28)) %>%
layer_dense(units = 10, activation = "softmax")

network %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
)

train_images <- array_reshape(train_images, c(60000, 28 * 28))
train_images <- train_images / 255

test_images <- array_reshape(test_images, c(10000, 28 * 28))
test_images <- test_images / 255

train_labels <- to_categorical(train_labels)
test_labels <- to_categorical(test_labels)

network %>% fit(train_images, train_labels, epochs = 5, batch_size = 128)

metrics <- network %>% evaluate(test_images, test_labels)

metrics

network %>% predict_classes(test_images[1:10, ])

x <- c(12, 3, 6, 14, 10)

str(x)

dim(as.array(x))

x <- matrix(rep(0, 3*5), nrow = 3, ncol = 5)
x
dim(x)

x <- array(rep(0, 2*3*2), dim = c(2, 3, 2))
str(x)
dim(x)

library(keras)

mnist <- dataset_mnist()

train_images <- mnist$train$x 
train_labels <- mnist$train$y 
test_images <- mnist$test$x 
test_labels <- mnist$test$y  

length(dim(train_images))

dim(train_images)

typeof(train_images)

digit <- train_images[5, , ]
plot(as.raster(digit, max = 255))

my_slice <- train_images[10:99, , ]
dim(my_slice)

my_slice <- train_images[10:99, 1:28, 1:28]
dim(my_slice)

my_slice <- train_images[, 15:28, 15:28]

batch <- train_images[1:128, , ]
batch <- train_images[129:256, , ]

x <- matrix(c(0, 1, 
2, 3, 
4, 5), nrow = 3, ncol = 2, byrow = TRUE)
x
x <- array_reshape(x, dim = c(6, 1))
x
x <- array_reshape(x, dim = c(2, 3))
x

x <- matrix(0, nrow = 300, ncol = 20)
dim(x)

x <- t(x)
dim(x)

## page 43/ 341
library(keras) 
imdb <- dataset_imdb(num_words = 10000)
c(c(train_data, train_labels), c(test_data, test_labels)) %<-% imdb
str(train_data[[1]])
train_labels[[1]]
max(sapply(train_data, max))
word_index <- dataset_imdb_word_index()
reverse_word_index <- names(word_index)

vectorize_sequences <- function(sequences, dimension = 10000) {
    results <- matrix(0, nrow = length(sequences), ncol = dimension)
    for (i in 1:length(sequences)) 
    results[i, sequences[[i]]] <- 1
    results
}

x_train <- vectorize_sequences(train_data)
x_test <- vectorize_sequences(test_data)

str(x_train[1, ])

y_train <- as.numeric(train_labels)
y_test <- as.numeric(test_labels)

library(keras)

model <- keras_model_sequential() %>%
layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>%
layer_dense(units = 16, activation = "relu") %>%
layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
    optimizer = "rmsprop",
    loss = "binary_crossentropy",
    metrics = c("accuracy")
)

val_indices <- 1:10000

x_val <- x_train[val_indices, ]
partial_x_train <- x_train[-val_indices, ]

y_val <- y_train[val_indices]
partial_y_train <- y_train[-val_indices]

model %>% compile(
    optimizer = "rmsprop",
    loss = "binary_crossentropy",
    metrics = c("accuracy")
)

history <- model %>% fit(
    partial_x_train,
    partial_y_train, 
    epochs= 20,
    batch_size = 512, 
    validation_data = list(x_val, y_val)
)

str(history)