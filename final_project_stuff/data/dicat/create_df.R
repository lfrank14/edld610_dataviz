library(tidyverse)

# Define Variables
subnum <- c(201:218,220:229,231:240)
nsub <- length(subnum)

rois <- c("ahip","mofc","phip")
nroi <- length(rois)

basedir <- getwd()

# Get parameter estimates
dicat_list <- list()
for (r in 1:nroi) {
  
  roidir <- str_c(basedir, "/b_", rois[r])
  
  ## Exemplar fits
  exfits_tmp <- tibble(run1 = rep(0,nsub), run2 = rep(0,nsub), 
                        run3 = rep(0,nsub), run4 = rep(0,nsub))
  for (s in 1:nsub) {
    
    # Load mean / sd
    mn <- rio::import(str_c(roidir, "/", subnum[s], "_pe2_mean_b_", rois[r], ".txt"))
    stdev <- rio::import(str_c(roidir, "/", subnum[s], "_pe2_sd_b_", rois[r], ".txt"))

    # Calculate effect size (mean/sd) 
    exfits_tmp[s,1:4] <- mn/stdev
    
  }
  
  exfits_tmp <- exfits_tmp %>% 
    mutate(avg = (run1+run2+run3+run4)/4)
  colnames(exfits_tmp) <- str_c("exfit_", rois[r], "_", colnames(exfits_tmp))
  
  ## Prototype fits
  protfits_tmp <- tibble(run1 = rep(0,nsub), run2 = rep(0,nsub), 
                       run3 = rep(0,nsub), run4 = rep(0,nsub))
  for (s in 1:nsub) {
    
    # Load mean / sd
    mn <- rio::import(str_c(roidir, "/", subnum[s], "_pe3_mean_b_", rois[r], ".txt"))
    stdev <- rio::import(str_c(roidir, "/", subnum[s], "_pe3_sd_b_", rois[r], ".txt"))
    
    # Calculate effect size (mean/sd) 
    protfits_tmp[s,1:4] <- mn/stdev
    
  }
  
  protfits_tmp <- protfits_tmp %>% 
    mutate(avg = (run1+run2+run3+run4)/4)
  colnames(protfits_tmp) <- str_c("protfit_", rois[r], "_", colnames(protfits_tmp))
  
  # Combine and save into list
  mdlfits <- cbind(exfits_tmp, protfits_tmp)
  dicat_list[[r]] <- mdlfits
  
}

dicat <- map_dfc(dicat_list, as.data.frame) %>% 
  mutate(subnum = subnum) %>% 
  select(subnum, 1:31)

write.csv(dicat, file = "../dicat.csv")
