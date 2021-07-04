

################################ THE CODE GRAVEYARD ###################################

# Defunct or redundant code that I am too scared to permanently delete. This is where that code with go to die for the meantime. 





####### From 25b_DATA_incorporate_2020.Rmd #####

# blue_full <- full_cp_ml %>%
#   filter(species == "BLU")
# 
# 
# blue_full_drifts <- blue_full %>%
#   select(drift)%>%
#   distinct
# 
# missing_drift <- anti_join(life_drifts, blue_full_drifts, by = "drift")
# 
# ## the 210 drifts that didn't catch any fish at all. Need to be added to blues each time
# add_drifts <- missing_drift %>%
#   mutate(species = "BLU",
#          num_caught = 0,
#          cpue = num_caught/anglerhour)%>%
#   rename(anglerhours = anglerhour,
#          site_1 = site)%>%
#    transform(site_2 = substr(drift, 3, 3))%>%
#    mutate(
#      site = case_when(
#        site_2 == "M" ~ "MPA",
#        site_2 == "R" ~ "REF"))%>%
#    select(-site_1, - site_2)
# 
# juv <- blue_full %>%
#   filter(size <= 21) %>%
#   select(-size) %>%
#   bind_rows(add_drifts)%>%
#   group_by(drift, trip, area, site, month, day, year, gridcell) %>%
#   summarise(cpue_sum = sum(cpue))%>%
#   group_by(trip, area, site, month, day, year, gridcell) %>%
#   summarise(cpue_date = mean(cpue_sum))%>%
#   group_by( area, site, year, gridcell) %>%
#   summarise(cpue_cell = mean(cpue_date))%>%
#   group_by( area, site, year) %>%
#   summarise(cpue_site = mean(cpue_cell))
# 
# ## below is the juveniles without adding the 210 drifts. Check to see how much CPUE changes. Look at each of the 0 drifts
# 
# juv2 <- blue_full %>%
#   filter(size <= 21) %>%
#   select(-size) %>%
#   #bind_rows(add_drifts)%>%
#   group_by(drift, trip, area, site, month, day, year, gridcell) %>%
#   summarise(cpue_sum = sum(cpue))%>%
#   group_by(trip, area, site, month, day, year, gridcell) %>%
#   summarise(cpue_date = mean(cpue_sum))%>%
#   group_by( area, site, year, gridcell) %>%
#   summarise(cpue_cell = mean(cpue_date))%>%
#   group_by( area, site, year) %>%
#   summarise(cpue_site = mean(cpue_cell))
# 
# 
# adult <- blue_full %>%
#   filter(size >= 32) %>%
#   select(-size)%>%
#   bind_rows(add_drifts)%>%
#   group_by(drift, trip, area, site, month, day, year, gridcell) %>%
#   summarise(cpue_sum = sum(cpue))%>%
#   group_by(trip, area, site, month, year, gridcell) %>%
#   summarise(cpue_date = mean(cpue_sum)) %>%
#   group_by( area, site, year, gridcell) %>%
#   summarise(cpue_cell = mean(cpue_date)) %>%
#   group_by( area, site, year) %>%
#   summarise(cpue_site = mean(cpue_cell))




############ from 29_sp_cross_corr.Rmd ##########





# 
# rf_list <- c("BLU", "GPR", "BLA", "OLV", "LCD", "VER", "DEA", "YTL", "KLP",
#              "CPR", "CNY", "CHN", "RSY", "BWN", "KGL", "QBK", "CBZ", "BAY", 
#              "PSD", "TRE", "SRY")
# 
# add_drift_df <- data.frame(group = character(), trip = character(), drift = character(),
#                            area = character(), site = character(), month = character(), 
#                            day = character(), year = double(), gridcell = character(),
#                            anglerhours = double())
# 
# for(i in rf_list){
#   add_drifts <- missing_drift %>%
#     mutate(species = i,
#             num_caught = 0,
#             cpue = num_caught/anglerhour) %>%
#     rename(anglerhours = anglerhour,
#            site_1 = site)%>%
#     transform(site_2 = substr(drift, 3, 3))%>%
#     mutate(site = case_when(
#        site_2 == "M" ~ "MPA",
#        site_2 == "R" ~ "REF"))%>%
#     select(-site_1, - site_2)
#   add_drift_df <- bind_rows(add_drift_df, add_drifts)
# }



#gpr juv

# gpr_juv_plot <- sp_juv_vis %>%
#   filter(species == "GPR") %>%
#   ggplot()+
#   scale_color_manual(values = c("#999999", "#009999", "#666666", "#000000"))+
#   scale_fill_manual(values = c("#999999", "#009999", "#666666", "#000000"))+
#   geom_col(position = "dodge2",aes(x = lag, y = correlation, 
#                                                     color = season, fill = season))+
#   labs(title = "Juvenile Gopher Rockfish Lag Correlation")+
#   theme_bw()+
#   theme(panel.grid = element_blank(),
#         plot.title = element_text(hjust = 0.5))+
#   facet_grid(site~area)

