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

data(geneLists)
top25CE <- RankAggreg(geneLists, 25, seed = 100, rho = .01)

top25CEw <- RankAggreg(geneLists, 25, seed = 100, importance = c(1, 2, 1, 1, 2), rho = .01)

top25GA <- RankAggreg(geneLists, 25, seed = 100, method = "GA", maxIter = 3000, convIn = 50)

