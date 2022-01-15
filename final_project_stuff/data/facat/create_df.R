library(tidyverse)
library(stringr)

# Vars
roi <- c("ahip","mofc","phip")
nroi <- length(roi)

subnum <- c(1,2,3,4,5,8,9,10,11,12,15,16,17,18,19,20,21,22,23,24,25,26,27,28,30,31,32)
nsub <- length(subnum)

# Load the different files
cat_filenames <- list.files(pattern = "cat")
train_filenames <- list.files(pattern = "train.txt")

train_measures <- c("train_itemrep","train_itemrep2","train_catrep")
cat_measures <- "cat_catrep"

rsa_list <- list()
for (i in 1:nroi) {
  # Load train
  tmp <- rio::import(train_filenames[i])
  tmp <- tmp %>% 
    select(V5, V6, V7)
  colnames(tmp) <- str_c(roi[i], "_" , train_measures)
  
  # Load cat
  tmp2 <- rio::import(cat_filenames[i])
  tmp2 <- tmp2 %>% 
    select(V4)
  colnames(tmp2) <- str_c(roi[i], "_", cat_measures)
  
  tmp <- cbind(tmp,tmp2)
  rsa_list[[i]] <- tmp
}

rsa <- map_dfc(rsa_list, as.tibble) %>% 
  mutate(subnum = subnum) %>% 
  select(subnum, 1:16)

write.csv(rsa, "rsa.csv")

# Merge with behavioral data
behav <- rio::import("facat_clean.xlsx")

facat <- left_join(behav, rsa, by = "subnum")

write_csv(facat, "../facat.csv")

# Check what the fuck is going on with shared irrelevant and same face

reps <- list()
for (i in 1:nroi) {
  # Load train
  tmp <- rio::import(train_filenames[i])
  tmp <- tmp %>% 
    select(V1,V3)
  colnames(tmp) <- str_c(roi[i], "_" , c("sameface","sharedirrelevant"))

  reps[[i]] <- tmp
}

map(reps, cor)


# Check all measures bc this is weird.

rsa_colnames <- c("sameface",
                  "samecategory",
                  "sharedirrelevant",
                  "noshared")

reps <- list()
for (i in 1:nroi) {
  # Load train
  tmp <- rio::import(train_filenames[i]) %>% 
    select(1:4)
  colnames(tmp) <- str_c(roi[i], "_" , rsa_colnames)
  
  reps[[i]] <- tmp
}

map(reps, cor)

# Get the early v. late training representations
trainearly_filenames <- list.files(pattern = "trainEarly")
trainlate_filenames <- list.files(pattern = "trainLate")

idx <- str_detect(trainearly_filenames, pattern = "ahip|phip|mofc")

trainearly_filenames <- trainearly_filenames[idx]
trainlate_filenames <- trainlate_filenames[idx]

train_measures <- c("itemrep","itemrep2","catrep")

rsa_list <- list()
for (i in 1:nroi) {
  
  # Load early training
  tmp <- rio::import(trainearly_filenames[i])
  tmp <- tmp %>% 
    select(V5, V6, V7)
  colnames(tmp) <- str_c("early_", roi[i], "_" , train_measures)
  
  # Load late training
  tmp2 <- rio::import(trainlate_filenames[i])
  tmp2 <- tmp2 %>% 
    select(V5, V6, V7)
  colnames(tmp2) <- str_c("late_", roi[i], "_", train_measures)
  
  tmp <- cbind(tmp,tmp2)
  rsa_list[[i]] <- tmp
}

rsa <- map_dfc(rsa_list, as.tibble) %>% 
  mutate(subnum = subnum) %>% 
  select(subnum, 1:(length(rsa) - 1))

write.csv(rsa, "rsa_earlylate.csv")
