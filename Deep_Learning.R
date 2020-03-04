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