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
plot(history)

history_df <- as.data.frame(history)
str(history_df)

model <- keras_model_sequential() %>%
layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>%
layer_dense(units = 16, activation = "relu") %>%
layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
    optimizer = "rmsprop",
    loss = "binary_crossentropy",
    metrics = c("accuracy")
)

model %>% fit(x_train, y_train, epochs = 4, batch_size = 512)
results <- model %>% evaluate(x_test, y_test)

results

model %>% predict(x_test[1:10, ])

library(keras)
reuters <- dataset_reuters(num_words = 10000)
c(c(train_data, train_labels), c(test_data, test_labels)) %<-% reuters
length(train_data)
length(test_data)

train_data[[1]]

word_index <- dataset_reuters_word_index()
reverse_word_index <- names(word_index)
names(reverse_word_index) <- word_index
decoded_newswire <- sapply(train_data[[1]], function(index) {
    word <- if (index >= 3) reverse_word_index[[as.character(index - 3)]]
    if (!is.null(word)) word else "?"
})
train_labels[[1]]

vectorize_sequences <- function(sequences, dimension = 10000) {
    results <- matrix(0, nrow = length(sequences), ncol = dimension)
    for (i in 1:length(sequences)) 
    results[i, sequences[[i]]] <- 1
    results
}

x_train <- vectorize_sequences(train_data)
x_test <- vectorize_sequences(test_data)

to_one_hot <- function(labels, dimension = 46) {
    results <- matrix(0, nrow = length(labels), ncol = dimension)
    for (i in 1:length(labels)) 
    results[i, labels[[i]]] <- 1
    results
}

one_hot_train_labels <- to_one_hot(train_labels)
one_hot_test_labels <- to_one_hot(test_labels)

one_hot_train_labels <- to_categorical(train_labels)
one_hot_test_labels <- to_categorical(test_labels)

model <- keras_model_sequential() %>%
layer_dense(units = 64, activation = "relu", input_shape = c(10000)) %>%
layer_dense(units = 64, activation = "relu") %>%
layer_dense(units = 46, activation = "softmax")

model %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
)

val_indices <- 1:1000

x_val <- x_train[val_indices, ]
partial_x_train <- x_train[-val_indices, ]

y_val <- one_hot_train_labels[val_indices, ]
partial_y_train <- one_hot_train_labels[-val_indices, ]

history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = 20, 
    batch_size = 512,
    validation_data = list(x_val, y_val)
)

plot(history)

model <- keras_model_sequential() %>%
layer_dense(units = 64, activation = "relu", input_shape = c(10000)) %>%
layer_dense(units = 64, activation = "relu") %>%
layer_dense(units = 46, activation = "softmax")

model %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
)

history <- model %>% fit (
    partial_x_train, 
    partial_y_train,
    epochs = 9,
    batch_size = 512,
    validation_data = list(x_val, y_val)
)

results <- model %>% evaluate(x_test, one_hot_test_labels)
results

test_labels_copy <- test_labels
test_labels_copy <- sample(test_labels_copy)
length(which(test_labels == test_labels_copy)) / length(test_labels)

predictions <- model %>% predict(x_test)
dim(predictions)
sum(predictions[1, ])
which.max(predictions[1, ])
model %>% compile(
    optimizer = "rmsprop",
    loss = "sparse_categorical_crossentropy",
    metrics = c("accuracy")
)

model <- keras_model_sequential() %>%
layer_dense(units = 64, activation = "relu", input_shape  = c(10000))  %>%
layer_dense(units = 4, activation = "relu") %>%
layer_dense(units = 46, activation = "softmax")

model %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
)

model %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = 20,
    batch_size = 128,
    validation_data = list(x_val, y_val)
)


library(keras)

dataset <- dataset_boston_housing()
c(c(train_data, train_targets), c(test_data, test_targests)) %<-% dataset
str(train_data)
str(test_data)

str(train_targets)

mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)

train_data <- scale(train_data, center = mean, scale = std)
test_data <- scale(test_data, center = mean, scale = std)

build_model <- function() {
    model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu",
    input_shape = dim(train_data)[[2]]) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)

    model %>% compile(
        optimizer = "rmsprop",
        loss = "mse",
        metrics = c("mae")
    )
}

k <- 4
indices <- sample(1:nrow(train_data))
folds <- cut(indices, breaks = k, labels = FALSE)

num_epochs <- 100
all_scores <- c()
for (i in 1:k) {
    cat("processing fold #", i, "\n")

    val_indices <- which(folds == i, arr.ind = TRUE)
    val_data <- train_data[val_indices, ]
    val_targets <- train_targets[val_indices]

    partial_train_data <- train_data[-val_indices, ]
    partial_train_targets <- train_targets[-val_indices]

    model <- build_model()

    model %>% fit(partial_train_data, partial_train_targets, 
    epochs = num_epochs, batch_size = 1, verbose = 0)
    results <- model %>% evaluate(val_data, val_targets, verbose = 0)
    all_scores <- c(all_scores, results$mae)
}

all_scores
mean(all_scores)



num_epochs <- 500
all_mae_histories <- NULL

for (i in 1:k) {
    cat("processing fold #", i, "\n")

    val_indices <- which(folds == i, arr.ind = TRUE)
    val_data <- train_data[val_indices, ]
    val_targets <- train_targets[val_indices]

    partial_train_data <- train_data[-val_indices, ]
    partial_train_targets <- train_targets[-val_indices]

    model <- build_model()

    histor <- model %>% fit(partial_train_data, partial_train_targets, 
    validation_data <- list(val_data, val_targets),
    epochs = num_epochs, batch_size = 1, verbose = 0)
    mae_history <- history$metrics$val_mean_absolute_error
    all_mae_histories <- rbind(all_mae_histories, mae_history)
    
}

model <- build_model()

model %>% fit(train_data, train_targets, 
epochs = 80, batch_size = 16, verbose = 0)
result <- model %>% evaluate(test_data, train_targets)

library(keras)
model <- keras_model_sequential() %>%
layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
input_shape = c(28, 28, 1)) %>%
layer_max_pooling_2d(pool_size = c(2, 2)) %>%
layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
layer_max_pooling_2d(pool_size = c(2, 2)) %>%
layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu")

model

model <- model %>%
layer_flatten() %>%
layer_dense(units = 64, activation = "relu") %>%
layer_dense(units = 10, activation = "softmax")
model

mnist <- dataset_mnist()
c(c(train_images, train_labels), c(test_images, test_labels)) %<-% mnist

train_images <- array_reshape(train_images, c(60000, 28, 28, 1))
train_images <- train_images/ 255

test_images <- array_reshape(test_images, c(10000, 28, 28, 1))
test_images <- test_images/ 255

train_labels <- to_categorical(train_labels)
test_labels <- to_categorical(test_labels)

model %>% compile(
    optimizer = "rmsprop", 
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
)

model %>% fit(train_images, train_labels, epochs = 5, batch_size = 64)

results <- model %>% evaluate(test_images, test_labels)
results

model_no_max_pool <- keras_model_sequential() %>%
layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
input_shape = c(28, 28, 1)) %>%
layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu")
model_no_max_pool

## Page 128 of 341

library(keras)

model <- keras_model_sequential() %>%
layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
input_shape = c(150, 150, 3)) %>%
layer_max_pooling_2d(pool_size = c(2, 2)) %>%
layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
layer_max_pooling_2d(pool_size = c(2, 2)) %>%
layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>%
layer_max_pooling_2d(pool_size = c(2, 2)) %>%
layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>%
layer_max_pooling_2d(pool_size = c(2, 2)) %>%
layer_flatten() %>%
layer_dense(units = 512, activation = "relu") %>%
layer_dense(units = 1, activation = "sigmoid")

summary(model)

model %>% compile(
    loss = "binary_crossentropy",
    optimizer = optimizer_rmsprop(lr = 1e-4),
    metrics = c("acc")
)

train_datagen <- image_data_generator(rescale = 1/255)
validation_datagen <- image_data_generator(rescale = 1/255)

train_dir <- c("C:/Users/Saw/Downloads/cats_and_dogs_small/data/train")
train_generator <- flow_images_from_directory(
    train_dir, 
    train_datagen,
    target_size = c(150, 150), 
    batch_size = 20, 
    class_mode = "binary"
)

validation_dir <- c("C:/Users/Saw/Downloads/cats_and_dogs_small/data/test")

validation_generator <- flow_images_from_directory(
    validation_dir,
    validation_datagen,
    target_size = c(150, 150),
    batch_size = 20,
    class_mode = "binary"
)

batch <- generator_next(train_generator)
str(batch)

history <- model %>% fit_generator(
    train_generator,
    steps_per_epoch = 100,
    epochs = 30,
    validation_data = validation_generator,
    validation_steps = 50
)

model %>% save_model_hdf5("cats_and_dogs_small_1.h5")

plot(history)

datagen <- image_data_generator(
    rescale = 1/255,
    rotation_range = 40,
    width_shift_range = 0.2,
    height_shift_range = 0.2,
    shear_range = 0.2,
    zoom_range = 0.2,
    horizontal_flip = TRUE,
    fill_mode = "nearest"
)

train_cats_dir <- c("C:/Users/Saw/Downloads/cats_and_dogs_small/data/train/cats")

fnames <- list.files(train_cats_dir, full.names = TRUE)

img_path <- fnames[[3]]

img <- image_load(img_path, target_size = c(150, 150))

img_array <- image_to_array(img)

img_array <- array_reshape(img_array, c(1, 150, 150, 3))

augmentation_generator <- flow_images_from_data(
    img_array,
    generator = datagen,
    batch_size = 1
)

op <- par(mfrow = c(2, 2), pty = "s", mar = c(1, 0, 1, 0))
for (i in 1:4) {
    batch <- generator_next(augmentation_generator)
    plot(as.raster(batch[1,,,]))
}
par(op)

model <- keras_model_sequential() %>% 
layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
input_shape = c(150, 150, 3))  %>% 
layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>% 
layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>% 
layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>% 
layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
layer_flatten() %>% 
layer_dropout(rate = 0.5) %>% 
layer_dense(units = 512, activation = "relu") %>% 
layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
    loss = "binary_crossentropy",
    optimizer = optimizer_rmsprop(lr = 1e-4),
    metrics = c("acc")
)

datagen <- image_data_generator(
    rescale = 1/255,
    rotation_range = 40,
    width_shift_range = 0.2,
    height_shift_range = 0.2,
    shear_range = 0.2,
    zoom_range = 0.2,
    horizontal_flip = TRUE
)

test_datagen <- image_data_generator(rescale = 1/255)

train_generator <- flow_images_from_directory(
    train_dir,
    datagen,
    target_size = c(150, 150),
    batch_size = 32, 
    class_mode = "binary"
)

validation_generator <- flow_images_from_directory(
    validation_dir, 
    test_datagen,
    target_size = c(150, 150),
    batch_size = 32,
    class_mode = "binary"
)

history <- model %>% fit_generator(
    train_generator,
    steps_per_epoch = 100,
    epochs = 100,
    validation_data = validation_generator,
    validation_steps = 50
)

model %>% save_model_hdf5("cats_and_dogs_small_2.h5")

library(keras)

 conv_base <- application_vgg16(
     weights = "imagenet",
     include_top = FALSE,
     input_shape = c(150, 150, 3)
 )

 ### 140/341
 library(keras)
 model <- load_model_hdf5("cats_and_dogs_small_2.h5")
 model

 img_path <- "C:/Users/Saw/Downloads/cats_and_dogs_small/data/test/cats/cat.0.jpg"

 img <- image_load(img_path, target_size = c(150, 150))
 img_tensor <- image_to_array(img)
 img_tensor <- array_reshape(img_tensor, c(1, 150, 150, 3))
 img_tensor <- img_tensor/255
 dim(img_tensor)

 plot(as.raster(img_tensor[1, , , ]))

 layer_outputs <- lapply(model$layers[1:8], function(layer) layer$output)
 activation_model <- keras_model(inputs = model$input, outputs = layer_outputs)

 activations <- activation_model %>% predict(img_tensor)
first_layer_activation <- activations[[1]]
dim(first_layer_activation)

plot_channel <- function(channel) {
    rotate <- function(x) t(apply(x, 2, rev))
    image(rotate(channel), axes = FALSE, asp = 1, 
    col = terrain.colors(12))
}

plot_channel(first_layer_activation[1, , , 2])
plot_channel(first_layer_activation[1, , , 7])

image_size = 58
images_per_row <- 16

for (i in 1:8) {
    layer_activation <- activations[[i]]
    layer_name <- model$layers[[i]]$name 
    
    n_features <- dim(layer_activation)[[4]]
    n_cols <- n_features %/% images_per_row

    png(paste0("cat_activations_", i, "_", layer_name, ".png"),
    width = image_size * images_per_row,
    height = image_size * n_cols)

    op <- par(mfrow = c(n_cols, images_per_row), mai = rep_len(0.02, 4))

    for (col in 0:(n_cols - 1)) {
        for (row in 0:(images_per_row-1)) {
            channel_image <- layer_activation[1, , , (col*images_per_row) + row + 1]
            plot_channel(channel_image)
        }
    }
    par(op)
    dev.off()
}

library(keras)
K <- backend()

model <- application_vgg16(
    weights = "imagenet",
    include_top = FALSE
)
library(keras)
model <- load_model_hdf5("vgg16.h5")

imdb_dir <- "C:/Users/Saw/Downloads/aclImdb"
train_dir <- file.path(imdb_dir, "train")

labels <- c()
texts <- c()

for (label_type in c("neg", "pos")) {
    label <- switch(label_type, neg = 0, pos = 1)
    dir_name <- file.path(train_dir, label_type)
    for (fname in list.files(dir_name, pattern = glob2rx("*.txt"),
    full.names = TRUE)) {
        text <- c(texts, readChar(fname, file.info(fname)$size))
        labels <- c(labels, label)
    }
}

library(keras)
maxlen <- 100
training_samples <- 200
validation_samples <- 10000
max_words <- 10000

tokenizer <- text_tokenizer(num_words = max_words) %>%
fit_text_tokenizer(texts)

sequences <- texts_to_sequences(tokenizer, texts)

word_index <- tokenizer$word_index

cat("Found", length(word_idnex), "unique tokens.\n")

data <- pad_sequences(sequences, maxlen = maxlen)

labels <- as.array(labels)
cat("Shape of data tensor:", dim(data), "\n")
cat("Shape of label tensor:", dim(labels), "\n")

indices <- sample(1:rnow(data))
training_indices <- indices[1:training_samples]
library(devtools)

### page 184 / 341

library(keras)
model <- keras_model_sequential() %>% 
layer_embedding(input_dim = 10000, output_dim = 32) %>% 
layer_simple_rnn(units = 32)
summary(model)

model <- keras_model_sequential() %>% 
layer_embedding(input_dim = 10000, output_dim = 32) %>% 
layer_simple_rnn(units = 32, return_sequences = TRUE)
summary(model)

model <- keras_model_sequential() %>% 
layer_embedding(input_dim = 10000, output_dim = 32) %>% 
layer_simple_rnn(units = 32, return_sequences = TRUE) %>% 
layer_simple_rnn(units = 32, return_sequences = TRUE) %>% 
layer_simple_rnn(units = 32, return_sequences = TRUE) %>% 
layer_simple_rnn(units = 32)

summary(model)

## Preprocess the Data 

library(keras)

max_features <- 10000

maxlen <- 500

batch_size = 32

cat("Loading data...\n")
imdb <- dataset_imdb(num_words = max_features)
c(c(input_train, y_train), c(input_test, y_test)) %<-% imdb
cat(length(input_train), "train sequences\n")
cat(length(input_test), "test sequences\n")
cat("Pad sequences (samples x times) \n")
input_train <- pad_sequences(input_train, maxlen = maxlen)
input_test <- pad_sequences(input_test, maxlen = maxlen)
cat("input_train shape:", dim(input_train), "\n")
cat("input_test shape:", dim(input_test), "\n")

model <- keras_model_sequential() %>% 
layer_embedding(input_dim = max_features, output_dim = 32) %>% 
layer_simple_rnn(units = 32) %>% 
layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
    optimizer = "rmsprop", 
    loss = "binary_crossentropy", 
    metrics = c("acc")
)

history <- model %>% fit(
    input_train, y_train, 
    epochs = 10,
    batch_size = 128, 
    validation_split = 0.2
)

plot(history)

model <- keras_model_sequential() %>% 
layer_embedding(input_dim = max_features, output_dim = 32) %>% 
layer_lstm(units = 32) %>% 
layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
    optimizer = "rmsprop",
    loss = "binary_crossentropy", 
    metrics = c("acc")
)

history <- model %>% fit(
    input_train, y_train,
    epochs = 10,
    batch_size = 128, 
    validatioN_split = 0.2
)

plot(history)

library(tibble)
library(readr)

data_dir <- c("C:/Users/Saw/Downloads/jena_climate")
fname <- file.path(data_dir, "jena_climate_2009_2016.csv")
data <- read_csv(fname)
glimpse(data)
library(ggplot2)
ggplot(data, aes(x = 1:nrow(data), y = `T (degC)`)) +
geom_line()

head(data)

ggplot(data[1:1440, ], aes(x = 1:1440, y = `T (degC)`)) +
geom_line()

sequence_generator <- function(start) {
    value <- start - 1
    function() {
        value <<- value + 1
        value
    }
}

gen <- sequence_generator(10)
gen()
gen()

data <- data.matrix(data[, -1])
train_data <- data[1:200000,]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data <- scale(data, center = mean, scale = std)

generator <- function(data, lookback, delay, min_index, max_index,
 shuffle = FALSE, batch_size = 128, step = 6) {
 if (is.null(max_index))
 max_index <- nrow(data) - delay - 1
 i <- min_index + lookback
 function() {
 if (shuffle) {
 rows <- sample(c((min_index+lookback):max_index), size = batch_size)
 } else {
 if (i + batch_size >= max_index)
 i <<- min_index + lookback
 rows <- c(i:min(i+batch_size, max_index))
 i <<- i + length(rows)
 }
 samples <- array(0, dim = c(length(rows),
 lookback / step,
 dim(data)[[-1]]))
 targets <- array(0, dim = c(length(rows)))
 for (j in 1:length(rows)) {
 indices <- seq(rows[[j]] - lookback, rows[[j]],
 length.out = dim(samples)[[2]])
 samples[j,,] <- data[indices,]
 targets[[j]] <- data[rows[[j]] + delay,2]
 }
 list(samples, targets)
 }
}


lookback <- 1440
step <- 6
delay <- 144
batch_size = 128

train_gen <- generator(
    data, 
    lookback = lookback,
    delay = delay,
    min_index = 1,
    max_index = 200000,
    shuffle = TRUE,
    step = step,
    batch_size = batch_size
)

val_gen <- generator(
    data, 
    lookback = lookback,
    delay = delay,
    min_index = 200001,
    max_index = 30000, 
    step = step,
    batch_size = batch_size
)

test_gen <- generator(
    data, 
    lookback = lookback,
    delay = delay,
    min_index = 300001,
    max_index = NULL,
    step = step,
    batch_size = batch_size
)

val_steps <- (300000-200001-lookback) / batch_size
test_steps <- (nrow(data) - 300001 - lookback) / batch_size

evaluate_naive_method <- function() {
    batch_maes <- c()
    for (step in 1:val_steps) {
        c(samples, targets) %<-% val_gen()
        preds <- samples[, dim(samples)[[2]],2]
        mae <- mean(abs(preds - targets))
        batch_maes <- c(batch_maes, mae)
    }
    print(mean(batch_maes))
}

evaluate_naive_method()

#### 206/341

library(keras)

model <- keras_model_sequential() %>% 
layer_flatten(input_shape = c(lookback / step, dim(data)[-1])) %>% 
layer_dense(units = 32, activation = "relu") %>% layer_dense(units = 1)

library(keras)
max_features <- 10000
max_len <- 500

cat("Loading data...\n")
imdb <- dataset_imdb(num_words = max_features)
c(c(x_train, y_train), c(x_test, y_test)) %<-% imdb
cat(length(x_train), "train sequences\n")
cat(length(x_test), "test seqeucnes")

cat("Loading data...\n")
imdb <- dataset_imdb(num_words = max_features)
c(c(x_train, y_train), c(x_test, y_test)) %<-% imdb
cat(length(x_train), "train sequences\n")
cat(length(x_test), "test sequences")
cat("Pad sequences (samples x time)\n")
x_train <- pad_sequences(x_train, maxlen = max_len)
x_test <- pad_sequences(x_test, maxlen = max_len)
cat("x_train shape:", dim(x_train), "\n")
cat("x_test shape:", dim(x_test), "\n")


model <- keras_model_sequential() %>% 
layer_embedding(input_dim = max_features, output_dim = 128,
input_length = max_len) %>% 
layer_conv_1d(filters = 32, kernel_size = 7, activation = "relu") %>% 
layer_max_pooling_1d(pool_size = 5) %>% 
layer_conv_1d(filters = 32, kernel_size = 7, activation = "relu") %>% 
layer_global_max_pooling_1d() %>% 
layer_dense(units = 1)

summary(model)

model %>% compile(
 optimizer = optimizer_rmsprop(lr = 1e-4),
 loss = "binary_crossentropy",
 metrics = c("acc")
)

history <- model %>% fit(
    x_train, y_train,
    epoches = 10,
    batch_size = 128,
    validation_split = 0.2
)

plot(history)


model <- keras_model_sequential() %>%
 layer_conv_1d(filters = 32, kernel_size = 5, activation = "relu",
 input_shape = list(NULL, dim(data)[[-1]])) %>%
 layer_max_pooling_1d(pool_size = 3) %>%
 layer_conv_1d(filters = 32, kernel_size = 5, activation = "relu") %>%
 layer_max_pooling_1d(pool_size = 3) %>%
 layer_conv_1d(filters = 32, kernel_size = 5, activation = "relu") %>%
 layer_global_max_pooling_1d() %>%
 layer_dense(units = 1)
model %>% compile(
 optimizer = optimizer_rmsprop(),
 loss = "mae"
)
history <- model %>% fit_generator(
 train_gen,
 steps_per_epoch = 500,
 epochs = 20,
 validation_data = val_gen,
 validation_steps = val_steps
)


#### page 229 of 341
