#--------------------------------------------------------------------#
#             Generate alarms for spirometry measurements            #
#--------------------------------------------------------------------#

library("tidyverse")

# Based on the ATS/ERS guidelines, literature and practice on Spirometry labs,
# We set some alarms to help the blows validation. These alarms are not
# alone indicative of curves with unacceptability criteria, just alarms.
# They were also based on the raw curves, before any selection.

# Read parsed tables
spiro <- readr::read_csv("output/spiro_tables.csv")


# FEV alarm ----

# For PEF, we consider a red flag whether it was 2 times the FVC
spiro <- spiro %>%
  mutate(pef_check = pef_test >= 2* fvc_test) 

# FVC alarm ----

# For FVC, we consider a red flag whether the difference between the
# largest FVC and the second largest FVC was ≥ 200 ml
spiro <- spiro %>% 
  group_by(participantID) %>%
  mutate(max_fvc = max(fvc_test, na.rm = T)) %>% 
  arrange(desc(fvc_test)) %>% 
  mutate(sec_fvc = nth(fvc_test, 2)) %>%
  ungroup()

spiro <- spiro %>% 
  mutate(fvc_check = max_fvc - sec_fvc >= 0.200)

# FEV-1 alarm ----

# For FEV-1, we consider a red flag whether the difference between the
# largest FEV-1 and the second largest FEV-1 was ≥ 200 ml
spiro <- spiro %>% 
  group_by(participantID) %>%
  mutate(max_fev1 = max(fev1_test, na.rm = T)) %>%
  arrange(desc(fev1_test)) %>% 
  mutate(sec_fev1 = nth(fev1_test, 2)) %>% 
  ungroup()
spiro <- spiro %>%
  mutate(fev1_check = max_fev1 - sec_fev1 >= 0.200)

# Save to disk ----
spiro <- select(spiro, participantID, testnum, pef_check, fvc_check, fev1_check)
write_csv(spiro, "output/alarms.csv")