library(writexl)

FILE_CLUSTER <- "data/raw/LH_clusters.txt"
FILE_PITS <- "data/raw/LH_frec_80.txt"


CLUSTER_1 <- 1
CLUSTER_2 <- 2
CLUSTER_3 <- 3


pits_df <- as.data.frame(read.table(FILE_PITS, sep = "\t", header = TRUE, stringsAsFactors = FALSE))
cluster_df <- as.data.frame(read.table(FILE_CLUSTER, sep = "\t", header = TRUE, stringsAsFactors = FALSE))

#print(pits_df)
#print(cluster_df)

# cambia el nombre de parela!
cluster_1_names <- cluster_df[cluster_df$cluster == CLUSTER_1, ]$parcel
cluster_2_names <- cluster_df[cluster_df$cluster == CLUSTER_2, ]$parcel
cluster_3_names <- cluster_df[cluster_df$cluster == CLUSTER_3, ]$parcel

  

cluster_1 <- pits_df[, cluster_1_names]
cluster_2 <- pits_df[, cluster_2_names]
cluster_3 <- pits_df[, cluster_3_names]

pits_df$cluster_1 <- rowSums(cluster_1)
pits_df$cluster_2 <- rowSums(cluster_2)
pits_df$cluster_3 <- rowSums(cluster_3)
print(pits_df)

write_xlsx(pits_df, "outputs/final_pits.xlsx")

#########################################



