library(RankAggreg)
library(clValid)
data(mouse)
express <- mouse[1:100, c("M1", "M2", "M3", "NC1", "NC2", "NC3")]
rownames(express) <- mouse$ID[1:100]
set.seed(100)
result <- clValid(express, 5, clMethods = c("hierarchical", "fanny", "model",
"kmeans", "sota", "pam", "clara", "agnes", "diana"), validation = c("internal",
"stability"))
res <- result
BruteAggreg(res$ranks, 9, res$weights, "Spearman")