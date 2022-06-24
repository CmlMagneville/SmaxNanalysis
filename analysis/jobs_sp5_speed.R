## 0 - Load data ####
devtools::load_all()
abund_list <- readRDS(here::here("transformed_data", "all_abund_list.rds"))
dist_df <- read.csv(here::here("data", "dist_df.csv"), row.names = 1)

# ICRS: keep only 9 cameras (6 LD and 3 SD):
dist_df <- dist_df[c(2, 4, 6, 7, 8, 9, 10, 11, 12), c(2, 4, 6, 7, 8, 9, 10, 11, 12)]


## 1 - Prepare data ####


# PREPARE DATA
clean_abund_list <- abund_list

# ICRS: keep only 9 cameras:
for (i in (1:length(clean_abund_list))) {
  for (j in (1:length(clean_abund_list[[i]]))) {
    clean_abund_list[[i]][[j]] <- clean_abund_list[[i]][[j]][, which(colnames(clean_abund_list[[i]][[j]]) %in%
                                                                       c("A1", "A2", "B1", "B2",
                                                                         "C1", "C2", "D", "F", "H"))]
  }
}

# create a dataframe which will contain maxN values for all sp:
maxN_all <- as.data.frame(matrix(ncol = 5, nrow = 1))
colnames(maxN_all) <- c("species_nm", "pose_nb", "maxN", "SmaxN", "SmaxN_timestep")


SmaxN_TH <- automat.maxN.spbysp(species_nm = "Thalassoma_hardwicke",
                                abund_list = clean_abund_list,
                                dist_df = dist_df,
                                fish_speed = 1,
                                os = "windows",
                                nb_cores = 3)

saveRDS(SmaxN_TH, here::here("transformed_data", "SmaxN_TH_raw_speed2.rds"))
