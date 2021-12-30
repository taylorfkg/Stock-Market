library(tidyverse)
library(data.table)

#clustering packages
library(factoextra)
library(NbClust)
library(FactoMineR)
library(cluster)


test <- transpose(horizontal_comparison_grid)

colnames(test) <- test[1,]

test <- test[-1,]

df <- test %>% select("TTM Net Income":"TTM Cash flow to net income")

rownames(df) <- test$ins_name

df$`TTM EPS (Basic) Growth` <- gsub("%","",df$`TTM EPS (Basic) Growth`)

df <- df %>% mutate_if(is.character,as.numeric)

is.na(df)<-sapply(df, is.infinite)
df[is.na(df)]<-0

df_scale <- scale(df)
rownames(df_scale) <- test$ins_name

df_scale2 <- df %>% select(contains("CAGR"),contains("Growth"),contains("%"),
                           contains("PS"),contains("Ratio"),
                           contains("Return"),contains("Per Share"),
                           contains("Yield"),contains("To Equity"),contains("EV/EBITDA"))


#df_scale2 <- df %>% select(contains("CAGR"),contains("Growth"),contains("%"),`TTM Debt To Equity`)

df_scale2 <- df %>% select(`TTM Net Income CAGR`,`TTM Sales/Revenue CAGR`,`TTM FCF CAGR`,
                           `TTM Free Cash Flow`,`TTM EPS (Basic)`,
                           `TTM EPS (Basic) Growth`,`TTM EPS CAGR`,`TTM Book Value PS`,
                           `TTM Debt To Equity`,`TTM Book Value PS CAGR`,`TTM Gross Profit Margin %`,
                           `TTM Net Profit Margin %`,`TTM Operating Profit Margin %`,`TTM Return On Equity`,
                           `TTM Dividends Per Share`,`TTM Dividends Yield`
                             
                           )

rownames(df_scale2) <- test$ins_name
df_scale2 <- scale(df_scale2)


#------------------------------------------------------------------------------
#PCA

res.pca <- PCA(df_scale2,  graph = FALSE)
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 70))

# Extract the results for variables
var <- get_pca_var(res.pca)
# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
# Control variable colors using their contributions to the principle axis
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
) + theme_minimal() + ggtitle("Variables - PCA")

#------------------------------------------------------------------------------------
#Cluster

fviz_nbclust(df_scale2,kmeans,k.max = 10, method = "wss",nstart = 25)+labs(subtitle = "Elbow method")
fviz_nbclust(df_scale2, kmeans,k.max = 10, method = "silhouette",nstart = 25)+labs(subtitle = "Silhouette method")
fviz_nbclust(df_scale2, kmeans,k.max = 10, method = "gap_stat",nstart = 50)+labs(subtitle = "Gap method")

# Compute the number of clusters
nb <- NbClust(df_scale2, distance = "euclidean", min.nc = 2,
              max.nc = 100, method = "median", index ="all")
# 
# Visualize the result
fviz_nbclust(nb) + theme_minimal()

#----------------------------------------------------

#PCA-ALL

res.pca <- PCA(df_scale,  graph = FALSE)
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 70))

# Extract the results for variables
var <- get_pca_var(res.pca)
# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
# Control variable colors using their contributions to the principle axis
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
) + theme_minimal() + ggtitle("Variables - PCA")

# PAM clustering-ALL
df_cluster <- eclust(df_scale, "pam", k = 100, graph = FALSE)
# Visualize pam clusters
fviz_cluster(df_cluster, geom = c("point","text"), 
             ellipse.type = "convex",show.clust.cent = TRUE,ellipse = T,
             repel = F)+
  labs(title = "",subtitle = "")

df_results <- df

rownames(df_results) <- test$ins_name

df_results$name <- row.names(df_results)

df_results$Cluster_ALL <- as.numeric(df_cluster$cluster)

#----------------------------------------------------

#PCA-SELECTED

res.pca2 <- PCA(df_scale2,  graph = FALSE)
fviz_screeplot(res.pca2, addlabels = TRUE, ylim = c(0, 70))

# Extract the results for variables
var2 <- get_pca_var(res.pca2)
# Contributions of variables to PC1
fviz_contrib(res.pca2, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca2, choice = "var", axes = 2, top = 10)
# Control variable colors using their contributions to the principle axis
fviz_pca_var(res.pca2, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
) + theme_minimal() + ggtitle("Variables - PCA")

# PAM clustering-SELECTED
df_cluster2 <- eclust(df_scale2, "pam", k = 10, graph = FALSE)
# Visualize pam clusters
fviz_cluster(df_cluster2, geom = c("point","text"), 
             ellipse.type = "convex",show.clust.cent = TRUE,ellipse = T,
             repel = F)+
  labs(title = "",subtitle = "")

df_results$Cluster_SELECTED <- as.numeric(df_cluster2$cluster)

fwrite(df_results,"stocks_clustering.csv")
