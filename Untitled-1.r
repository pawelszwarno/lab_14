# PIPES

ten_lst <- 1:10
print(ten_lst)
install.packages("magrittr")
library(magrittr)
ten_lst %<>% log2 %>% sin %>% sum %>% sqrt
print(ten_lst)
data(iris)
print(head(iris,3))
mean_iris <- iris %>% aggregate(. ~ Species, . , mean)
print(mean_iris)

# PLOT
library(ggplot2)
wykres_1 <- ggplot(data=iris, aes(x=Sepal.Length)) + 
    geom_histogram(aes(fill=Species, color=Species), bins=20) +
    geom_vline(data=mean_iris, aes(xintercept=Sepal.Length, color=Species), linetype="dashed") +
    labs(title="Sepal length for iris species")

ggsave("/home/wykres_1.jpg", plot=wykres_1)

library("GGally")
wykres_2 <- ggpairs(data=iris, aes(color=Species))
ggsave("/home/wykres_2.jpg", plot=wykres_2)

# CLUSTERING
library(cluster)
x <- iris[ ,1:4]
y <- iris[ ,5]
sum_sqr <- c()
for (i in 1:10)
{
    res <- kmeans(x,i)
    sum_sqr <- append(sum_sqr, res$tot.withinss)
}
print(sum_sqr)

wykres_3 <- ggplot(data.frame(iteration = 1:length(sum_sqr), value = sum_sqr), aes(x=iteration, y=sum_sqr)) +
            geom_line()
ggsave("/home/wykres_3.jpg", plot=wykres_3)

res <- kmeans(x,3)
wykres_4 <- ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, color=res$cluster)) + geom_point()
ggsave("/home/wyres_4.jpg", plot=wykres_4)

wykres_5 <- ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, color=Species)) + geom_point()
ggsave("/home/wyres_5.jpg", plot=wykres_5)
