seg.df <- read.csv("sales.csv")

# 1
summary(seg.df)

# 2
hist(seg.df$visitsMonth,xlab="visitsMonth", col="steelblue3")

# 3-1
selected_vars <- seg.df[,1:8]
seg.k <- kmeans(selected_vars,centers=4)
cluster_assignments <-seg.k$cluster
table(cluster_assignments)

# 3-2
seg.summ <- function(data, groups){
  aggregate(data, list(groups),function(x) mean(as.numeric(x)))
}

cluster_means <- seg.summ(seg.df$spendToDate, cluster_assignments)
colnames(cluster_means) <- c("Cluster", "MeanSpendToDate")
cluster_means

# 3-3
model <- lm(seg.df$spendToDate ~ as.factor(cluster_assignments))
anova_result <- anova(model)
anova_result

