# returns the number of available hardware threads, and if it is FALSE, returns the number of physical cores:
no_cores <- parallel::detectCores(logical = TRUE)

# allocate this nb of available cores to R:
cl <- parallel::makeCluster(no_cores-1)
doParallel::registerDoParallel(cl)


SmaxN.single.inp <- function(list_input) {
  SmaxN::SmaxN.computation(dist_df = list_input$dist_df,
                           speed = list_input$speed,
                           abund_df = list_input$abund_df)
}

parallel::clusterExport(cl, "SmaxN.single.inp")

i <- 1

paral_list <- list(
  list(dist_df = dist_df[which(rownames(dist_df) %in% colnames(clean_abund_list[[i]][[1]])),
                         which(colnames(dist_df) %in% colnames(clean_abund_list[[i]][[1]]))],
       speed = fish_speed,
       abund_df = clean_abund_list[[i]][[1]]),
  list(dist_df = dist_df[which(rownames(dist_df) %in% colnames(clean_abund_list[[i]][[2]])),
                         which(colnames(dist_df) %in% colnames(clean_abund_list[[i]][[2]]))],
       speed = fish_speed,
       abund_df = clean_abund_list[[i]][[2]]),
  list(dist_df = dist_df[which(rownames(dist_df) %in% colnames(clean_abund_list[[i]][[3]])),
                         which(colnames(dist_df) %in% colnames(clean_abund_list[[i]][[3]]))],
       speed = fish_speed,
       abund_df = clean_abund_list[[i]][[3]]))


start.time <- Sys.time()
maxN_data <- parallel::parLapply(cl, paral_list, fun = SmaxN.single.inp)
end.time <- Sys.time()
time.taken <- end.time - start.time


# tourne actuellement pour une espece et les trois tableaux des poses de cette espece
# si marche: sera a adatpter:
# - la paral_list pour que rende bien le bon format et ok pour parLapply
# - le stockage des resultats dans le tableau en s'aidant de la sortie maxN_data qui tourne sur l'exemple actuellement

# Error in checkForRemoteErrors(val) :
# 3 nodes produced errors; first error: les arguments impliquent des nombres de lignes différents : 1, 0

# peut-être erreur sur la fonction de base donc essai:


start.time <- Sys.time()
Pose1 <- SmaxN::SmaxN.computation(dist_df = dist_df[which(rownames(dist_df) %in% colnames(clean_abund_list[[i]][[1]])),
                                                    which(colnames(dist_df) %in% colnames(clean_abund_list[[i]][[1]]))],
                                  speed = 0.5,
                                  abund_df = clean_abund_list[[i]][[1]])
end.time <- Sys.time()
time.taken <- end.time - start.time

# effectivement pb sur la fonction SmaxN.computation: a regler! Pb regle :)

saveRDS(dist_df[which(rownames(dist_df) %in% colnames(clean_abund_list[[i]][[1]])),
          which(colnames(dist_df) %in% colnames(clean_abund_list[[i]][[1]]))], "dist_df_ex.rds")

saveRDS(clean_abund_list[[i]][[1]], "abund_df_ex.rds")


# essai de nouveau pour un tableau et une espece:
start.time <- Sys.time()
Pose1 <- SmaxN::SmaxN.computation(dist_df = dist_df[which(rownames(dist_df) %in% colnames(clean_abund_list[[i]][[1]])),
                                                    which(colnames(dist_df) %in% colnames(clean_abund_list[[i]][[1]]))],
                                  speed = 0.5,
                                  abund_df = clean_abund_list[[i]][[1]])
end.time <- Sys.time()
time.taken <- end.time - start.time

# Ca a pris 1.67 heure avec SmaxN = 6 contre maxN = 2 and SmaxN_timestep = 2


# essai relance sur liste faite à la main: seems to work
start.time <- Sys.time()
maxN_data <- parallel::parLapply(cl, paral_list, fun = SmaxN.single.inp)
end.time <- Sys.time()
time.taken <- end.time - start.time


# si a marche: regarder a quoi ressemble la sortie de maxN_data pour adapter le
# ... code mettre dans maxN_sp puis maxN_all
# Erreur au bout de 3.68heures:  2 nodes produced errors; first error: valeur manquante là où TRUE / FALSE est requis

# donc je lance SmaxN.computation pour chaque tableau indiv (les deux derniers) pour voir ou est l'erreur:

start.time <- Sys.time()
Pose2 <- SmaxN::SmaxN.computation(dist_df = dist_df[which(rownames(dist_df) %in% colnames(clean_abund_list[[i]][[2]])),
                                                    which(colnames(dist_df) %in% colnames(clean_abund_list[[i]][[2]]))],
                                  speed = 0.5,
                                  abund_df = clean_abund_list[[i]][[2]])
end.time <- Sys.time()
time.taken <- end.time - start.time


# Error in if (S > SmaxN_small_UI) { :
# missing value where TRUE/FALSE needed
# ok c'etait un probleme de max sans na.rm = TRUE donc corrige (et frame of possible reduit sans NA donc doublement ok)
# reessaie:

start.time <- Sys.time()
Pose2 <- SmaxN::SmaxN.computation(dist_df = dist_df[which(rownames(dist_df) %in% colnames(clean_abund_list[[i]][[2]])),
                                                    which(colnames(dist_df) %in% colnames(clean_abund_list[[i]][[2]]))],
                                  speed = 0.5,
                                  abund_df = clean_abund_list[[i]][[2]])
end.time <- Sys.time()
time.taken <- end.time - start.time


# rend la fenetre de Rstudio blanche: bug, essai avec la pose 3:

start.time <- Sys.time()
Pose3 <- SmaxN::SmaxN.computation(dist_df = dist_df[which(rownames(dist_df) %in% colnames(clean_abund_list[[i]][[3]])),
                                                    which(colnames(dist_df) %in% colnames(clean_abund_list[[i]][[3]]))],
                                  speed = 0.5,
                                  abund_df = clean_abund_list[[i]][[3]])
end.time <- Sys.time()
time.taken <- end.time - start.time


# idem: page blanche essai de nouveau Pose 1 pour voir si pb de code ou de tableau
# marche !

start.time <- Sys.time()
Pose1 <- SmaxN::SmaxN.computation(dist_df = dist_df[which(rownames(dist_df) %in% colnames(clean_abund_list[[i]][[1]])),
                                                    which(colnames(dist_df) %in% colnames(clean_abund_list[[i]][[1]]))],
                                  speed = 0.5,
                                  abund_df = clean_abund_list[[i]][[1]])
end.time <- Sys.time()
time.taken <- end.time - start.time


# apres avoir redemarre le pc, reessaie Pose2:

start.time <- Sys.time()
Pose2 <- SmaxN::SmaxN.computation(dist_df = dist_df[which(rownames(dist_df) %in% colnames(clean_abund_list[[i]][[2]])),
                                                    which(colnames(dist_df) %in% colnames(clean_abund_list[[i]][[2]]))],
                                  speed = 0.5,
                                  abund_df = clean_abund_list[[i]][[2]])
end.time <- Sys.time()
time.taken <- end.time - start.time

# idem page blanche: coupe le tableau en deux:

data <- clean_abund_list[[i]][[2]][c(500:600), ]
start.time <- Sys.time()
Pose2_demi <- SmaxN::SmaxN.computation(dist_df = dist_df[which(rownames(dist_df) %in% colnames(data)),
                                                    which(colnames(dist_df) %in% colnames(data))],
                                  speed = 0.5,
                                  abund_df = data)
end.time <- Sys.time()
time.taken <- end.time - start.time


# 100 premieres lignes ok
# 500 premieres lignes tres long pas fini
# 100 a 200 lignes ok 11sec
# 200 a 300 lignes ok mais 4min
# 300 a 400 lignes immediat big = small
# 400 a 500 lignes ok 15 sec
# 500 a 600 lignes ok 7 sec


#essaie enlever print lundi et ressaie sur 200 lignes (1-200) (ok 5sec)
# sur 300 lignes (1-300) (ok 7 minutes)
# sur 400 lignes (1-400) (ok 10 minutes)
# sur 400 lignes (1-420) (ok 13 minutes)
# sur 400 lignes (1-430) (ok 10 minutes)
# sur tout le tableau 2: 10.79 heures
# essai nouveau tableau 1: 19 minutes
# tableau 3: 50 minutes
# tous les tableaux: 11h



data <- clean_abund_list[[i]][[3]]
start.time <- Sys.time()
Pose2_demi <- SmaxN::SmaxN.computation(dist_df = dist_df[which(rownames(dist_df) %in% colnames(data)),
                                                         which(colnames(dist_df) %in% colnames(data))],
                                       speed = 0.5,
                                       abund_df = data)
end.time <- Sys.time()
time.taken <- end.time - start.time

