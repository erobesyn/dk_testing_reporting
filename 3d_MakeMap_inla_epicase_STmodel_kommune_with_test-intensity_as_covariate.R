# 14d_makeMap_inla_epicase_STmodel_kommune_with_test-intensity_as_covariate

#### LIBRARIES ####
source("./1_load_libraries.R")

LAU <- st_read("./1 data/4 processed geodata/6 DK_LAU_for_model_visualisation/DK_LAU_for_model_visualisation.shp") 
LAU_st <- st_read("./1 data/4 processed geodata/7 DK_LAU_st_for_model_visualisation/DK_LAU_st_for_model_visualisation.shp") 
nuts3_sf_dk <- st_read("./1 data/4 processed geodata/5 DK_NUTS3_for_model_visualisation/DK_NUTS3_for_model_visualisation.shp")

bord <- ms_innerlines(nuts3_sf_dk)  
bord <-  bord |> st_as_sf() |> mutate(id = 1:10)

geo_connections <- read_rds("./1 data/4 processed geodata/3 connection arealevels_processed/zip_kommune_landsdel_region_CLEAN.rds")

## best model 
summary(fit18c)   # fit18c_without_testing. Without testing: less good fit.
summary(fit21)    # Nearly equally good fit but T.grp2 maintains better view on high testing values 

#### 3. DISPLAY RESULTS (from script c, model 18c) ####

summary(fit18c)                       

#### PARAMS ####

## INTERCEPT

fit18c$marginals.fixed$`(Intercept)`                             
as_tibble(inla.smarginal(fit18c$marginals.fixed$`(Intercept)`))  

ggplot(as_tibble(inla.smarginal(fit18c$marginals.fixed$`(Intercept)`))) +
  geom_line(aes(x = x, y = y), lwd = 1) + theme_bw() +
  ylab(expression(paste(pi, "(", "x", " | ", bold(y), ")"))) + 
  ggtitle("posterior intercept")

## RANDOM EFFECT PARAMS

# summary

fit18c$summary.random$`inla.group(T.grp)`  

fit18c$summary.random$t                    
fit18c$summary.random$ID.struct             
fit18c$summary.random$ID.unstruct           
fit18c$summary.random$ID.area.time         

fit21$summary.random$`inla.group(T.grp2)` 

# 0 Testing re

fit18c$summary.random$`inla.group(T.grp)`  

df<-data.frame(Tested_persons=fit18c$summary.random$`inla.group(T.grp)`$ID, 
               effect=fit18c$summary.random$`inla.group(T.grp)`$mean,
               high=fit18c$summary.random$`inla.group(T.grp)`$`0.975quant`,
               low=fit18c$summary.random$`inla.group(T.grp)`$`0.025quant`)
f <- ggplot(df,aes(Tested_persons,effect))                  
kommune_assoc_tested_epicase_spatiotemp <- f + 
  geom_ribbon(aes(ymin=low,ymax=high),fill= "darkgoldenrod3", alpha = 0.2)+  
  geom_line(lwd = 2)+
  geom_vline(xintercept = 1000, lty = 2, lwd = 1) +   
  ggtitle("Direct effect testing on case reporting") +   
  ylab("log(RR) cases") + 
  xlab("Testing intensity (tested persons/100 000)") +  
  theme_bw()          
kommune_assoc_tested_epicase_spatiotemp 
ggsave("./5 figs/kommune_assoc_tested_epicase_spatiotemp.png", width = 15, height = 10)  

# redo with fit21
#title("Impact test intensity")
df_v2 <- data.frame(Tested_persons=fit21$summary.random$`inla.group(T.grp2)`$ID, 
               effect=fit21$summary.random$`inla.group(T.grp2)`$mean,
               high=fit21$summary.random$`inla.group(T.grp2)`$`0.975quant`,
               low=fit21$summary.random$`inla.group(T.grp2)`$`0.025quant`)
f_v2 <- ggplot(df_v2,aes(Tested_persons,effect))                  
kommune_assoc_tested_epicase_spatiotemp_v2 <- f_v2 + 
  geom_ribbon(aes(ymin=low,ymax=high),fill= "darkgoldenrod3", alpha = 0.2) +  
  geom_line(lwd = 2)+
  geom_vline(xintercept = 1000, lty = 2, lwd = 1) +   
  ggtitle("Direct effect testing on case reporting") +  
  ylab("log(RR) cases") + 
  xlab("Testing intensity (tested persons/100 000)") + 
  theme_bw()          
kommune_assoc_tested_epicase_spatiotemp_v2 
ggsave("./5 figs/kommune_assoc_tested_epicase_spatiotemp_alt_bins_testing.png", width = 15, height = 10) 

ggarrange(kommune_assoc_tested_epicase_spatiotemp, kommune_assoc_tested_epicase_spatiotemp_v2)
ggsave("./5 figs/kommune_assoc_tested_epicase_spatiotemp_both_binnings_testing.png", width = 15, height = 10)  

df<-data.frame(Tested_persons=fit18c$summary.random$`inla.group(T.grp)`$ID, 
               effect=fit18c$summary.random$`inla.group(T.grp`$mean,
               high=fit18c$summary.random$`inla.group(T.grp)`$`0.975quant`,
               low=fit18c$summary.random$`inla.group(T.grp)`$`0.025quant`)
f<-ggplot(df,aes(Tested_persons,effect)) +             
  geom_ribbon(aes(ymin=low,ymax=high),fill= "grey80")+
  geom_line(lwd = 1)+
  geom_vline(xintercept = 1000, lty = 2, lwd = 1) +
  ggtitle("Direct effect testing on case reporting") + 
  ylab("log(RR) cases") + 
  xlab("Testintensity") +
  theme_bw()    
f

# 1. time re 
df <- data.frame(time=2014:2022,
                 effect = fit18c$summary.random$t$mean,             
                 high=fit18c$summary.random$t$`0.975quant`,      
                 low=fit18c$summary.random$t$`0.025quant`)         
f2 <- ggplot(df, aes(time, effect)) + 
  geom_ribbon(aes(ymin=low, ymax=high), fill = "darkgoldenrod3", alpha = 0.2) +  
  geom_line(lwd = 2) +
  ggtitle("Evolution case reporting over time") + 
  xlab("Time (year)") + 
  ylab("log(RR) cases") + 
  theme_bw()
f2   # Effect of time (coef of t) on log scale.  Transform with tmarg() to get on original scale.
kommune_assoc_time_epicase_spatiotemp <- f2
ggsave("./5 figs/kommune_assoc_time_epicase_spatiotemp.png", width = 15, height = 10) 

# combined plot

# first read plot from script b 
kommune_assoc_time_tested_spatiotemp <- readRDS("./5 figs/kommune_assoc_time_tested_spatiotemp.RDS") 
kommune_assoc_time_tested_spatiotemp

p <- ggarrange(kommune_assoc_time_tested_spatiotemp,       
               kommune_assoc_tested_epicase_spatiotemp,    
               kommune_assoc_time_epicase_spatiotemp,     
               ncol = 2, nrow = 2)
p
ggsave("./5 figs/assoc_time_tested_epicase.png")  

# 2. icar re  

LAU$epicase_ICAR <- fit18c$summary.random$ID.struct$mean
LAU$epicase_IID  <- fit18c$summary.random$ID.unstruct$mean

round(range(fit18c$summary.random$ID.struct$mean), 2)     # -1.24  0.77 on log scale
fivenum(range(fit18c$summary.random$ID.struct$mean))

kommune_sf_epicase_icar <- tm_shape(LAU) +    
  tm_borders(col = "grey80", lwd = 1)  +
  tm_fill("epicase_ICAR", title = "ICAR", style = "fixed",
          
          breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5),  
          # palette = brewer.pal(6,"BuPu")) +          
          palette = rev(brewer.pal(6,"RdBu")),
          midpoint = 0
  ) +
  tm_layout(main.title = "ICAR random effect, Reported Cases BYM, Denmark",
            main.title.size = 0.7,
            # legend.title.size = 1,
            legend.text.size = 0.5)   
kommune_sf_epicase_icar <- kommune_sf_epicase_icar + tm_shape(bord) + tm_lines(col = "grey60", lwd = 2) 
kommune_sf_epicase_icar 
tmap_save(kommune_sf_epicase_icar , "./5 figs/kommune_sf_epicase_icar.png")

# 3. iid re 

round(range(fit18c$summary.random$ID.unstruct$mean), 2)  
fivenum(range(fit18c$summary.random$ID.unstruct$mean))

kommune_sf_epicase_iid <- tm_shape(LAU) +    
  tm_borders(col = "grey80", lwd = 1)  +
  tm_fill("epicase_IID", title = "IID", style = "fixed",
          
          breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5),  
          palette = rev(brewer.pal(6,"RdBu")),
          midpoint = 0
  ) +         
  tm_layout(main.title = "IID random effect, Reported Cases BYM, Denmark",
            main.title.size = 0.7,
            legend.text.size = 0.5)    
kommune_sf_epicase_iid <- kommune_sf_epicase_iid + tm_shape(bord) + tm_lines(col = "grey60", lwd = 2) 
kommune_sf_epicase_iid  
tmap_save(kommune_sf_epicase_iid , "./5 figs/kommune_sf_epicase_iid.png")

kommune_sf_epicase_icar_iid <- tmap::tmap_arrange(kommune_sf_epicase_icar, kommune_sf_epicase_iid)
kommune_sf_epicase_icar_iid 
tmap_save(kommune_sf_epicase_icar_iid , "./5 figs/kommune_sf_epicase_icar_iid.png", height = 7, width = 14)

# 4. ST interaction re (map)

LAU_st$epicase_STinteraction <- fit18c$summary.random$ID.area.time$mean

round(range(LAU_st$epicase_STinteraction), 2)   
fivenum(range(LAU_st$epicase_STinteraction))

kommune_sf_epicase_STinteraction <- tm_shape(LAU_st) +  
  tm_borders(col = "grey80", lwd = 1)  +
  tm_fill("epicase_STinteraction", title = "ST interaction", style = "fixed",
          
          breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5),  
          palette = rev(brewer.pal(6,"RdBu")),
          midpoint = 0
  ) +         
  tm_layout(main.title = "ST interaction, Reported Cases BYM, Denmark",
            main.title.size = 0.7,
            legend.text.size = 0.5,
            legend.outside=FALSE, 
            legend.position = c("right", "top")) + 
  tm_facets(by = "year", nrow=3) 

kommune_sf_epicase_STinteraction + tm_shape(bord) + tm_lines(col = "grey60", lwd = 2) 
tmap_save(kommune_sf_epicase_STinteraction, "./5 figs/kommune_sf_epicase_interaction.png")

# Exceedance probability of STinteraction > 0 
LAU_st$epicase_STinteraction_exc <- unlist(lapply(fit18c$marginals.random$ID.area.time, function(X){1-inla.pmarginal(0, X) }))  # Pr(phi) > 0

kommune_sf_epicase_STinteraction_exc <- tm_shape(LAU_st) +  
  tm_borders(col = "grey80", lwd = 1.5)  +
  tm_fill("epicase_STinteraction_exc", title = "Pr(STinteraction) > 0", style = "fixed",
          breaks = c(0,0.05,0.25,0.5,0.75,0.80,0.95,0.99,1), # c(-2, -1, 0, 1, 2),                     # ADAPT DIVERGING COL
          palette = brewer.pal(n = 8, name = "Greys")) +   #"Blues"
  #tm_text("kommune", size = 0.5 )    +
  tm_layout(main.title = "Exc ST interaction, reported cases BYM, Denmark",
            main.title.size = 0.7,
            # legend.title.size = 1,
            legend.text.size = 0.5,
            legend.outside=FALSE, 
            legend.position = c("right", "top")) + 
  tm_facets(by = "year", nrow=3) 
kommune_sf_epicase_STinteraction_exc + tm_shape(bord) + tm_lines(col = "grey60", lwd = 2) 
tmap_save(kommune_sf_epicase_STinteraction_exc, "./5 figs/kommune_sf_epicase_interaction_exc.png")

## 1c hyper param

# Relative importance of variance components from 4 varying effects

# 1. Simulate unstructured variance
var.v <- inla.rmarginal(100000,inla.tmarginal(fun=function(x) 1/x, fit18c$marginals.hyperpar$`Precision for ID.unstruct`))   # (iid component)

# 2. Simulate structured variance - if not bym, I can do similar as for unstructured variance:
var.u <- inla.rmarginal(100000,inla.tmarginal(fun=function(x) 1/x, fit18c$marginals.hyperpar$`Precision for ID.struct`))   # (icar component)

# 3. Simulate structured variance 
var.t <- inla.rmarginal(100000,inla.tmarginal(fun=function(x) 1/x, fit18c$marginals.hyperpar$`Precision for t`))   # (time rw component)

# 4. Simulate structured variance 
var.STinteraction <- inla.rmarginal(100000,inla.tmarginal(fun=function(x) 1/x, fit18c$marginals.hyperpar$`Precision for ID.area.time`))   # (space time interaction component)

# percentage variance attributable to the four varying effects 
print(mean(var.u / (var.u + var.v + var.t + var.STinteraction)) *100 )                
print(mean(var.v / (var.u + var.v + var.t + var.STinteraction)) *100)           
print(mean(var.t / (var.u + var.v + var.t + var.STinteraction)) *100 )           
print(mean(var.STinteraction / (var.u + var.v + var.t + var.STinteraction)) *100)      

#### 3d. FITTED VALUES ####

# 1. summary 

# RR 
fit18c$summary.fitted.values      
LAU_st$epicase_rr <- fit18c$summary.fitted.values$mean  

round(range(LAU_st$epicase_rr), 2)    

LAU_st$epicase_rr_ll <- fit18c$summary.fitted.values$'0.025quant'
LAU_st$epicase_rr_ul <- fit18c$summary.fitted.values$'0.975quant' 

# add also RR for the unadjusted model (fit18c_without_testing)
LAU_st$epicase_rr_without_testing <- fit18c_without_testing$summary.fitted.values$mean 
round(range(LAU_st$epicase_rr_without_testing), 2) 

kommune_sf_epicase_rr <- tm_shape(LAU_st) +  
  tm_borders(col = "grey80", lwd = 1)  +           
  tm_fill("epicase_rr", title = "RR", style = "fixed",
          
          breaks = c(0, 0.5, 0.6, 0.8,  1.2, 1.5, 2, 15), 
          palette = rev(brewer.pal(7,"RdYlBu"))) +  

  #tm_text("kommune", size = 0.5 )    +
  tm_layout(main.title = "RR, Case reporting BYM, Denmark",
            main.title.size = 0.7,
            # legend.title.size = 1,
            legend.text.size = 0.5,
            legend.outside=FALSE, 
            legend.position = c("right", "top")) + 
  tm_facets(by = "year", nrow=3) 
kommune_sf_epicase_rr <- kommune_sf_epicase_rr + tm_shape(bord) + tm_lines(col = "grey60", lwd = 2) 
kommune_sf_epicase_rr
tmap_save(kommune_sf_epicase_rr, "./5 figs/kommune_sf_epicase_rr.png")

# idem map rr for model without testing (=unadjusted model)
kommune_sf_epicase_rr_without_testing <- tm_shape(LAU_st) +  
  tm_borders(col = "grey80", lwd = 1)  +           
  tm_fill("epicase_rr_without_testing", title = "RR", style = "fixed",
          
          breaks = c(0, 0.5, 0.6, 0.8,  1.2, 1.5, 2, 15),  
          palette = rev(brewer.pal(7,"RdYlBu"))) +  
  tm_layout(main.title = "RR, Case reporting BYM - total effect (unadjusted for testing), Denmark",
            main.title.size = 0.7,
            # legend.title.size = 1,
            legend.text.size = 0.5,
            legend.outside=FALSE, 
            legend.position = c("right", "top")) + 
  tm_facets(by = "year", nrow=3) 
kommune_sf_epicase_rr_without_testing <- kommune_sf_epicase_rr_without_testing + tm_shape(bord) + tm_lines(col = "grey60", lwd = 2) 
kommune_sf_epicase_rr_without_testing
tmap_save(kommune_sf_epicase_rr_without_testing, "./5 figs/kommune_sf_epicase_rr_without_testing.png")

## datasummary from plot
LAU_st          
geo_connections 

LAU_st <- LAU_st %>% 
  left_join(geo_connections)  

# RR case reporting
LAU_st_epicase_rr <- LAU_st %>% 
  select(year, kommune, landsdel, epicase_rr) %>% st_drop_geometry() |> as_tibble() 
fivenum(LAU_st_epicase_rr$epicase_rr)  

LAU_st_epicase_rr %>% 
  filter(epicase_rr >=2) %>% count(landsdel, kommune) %>% arrange(landsdel) %>% print.all() 

LAU_st_epicase_rr %>% filter(epicase_rr >=2) |> distinct(kommune, landsdel, year) |> arrange(kommune) |> print.all()
LAU_st_epicase_rr %>% filter(epicase_rr >=2) |> distinct(kommune) |> pull() 

# Estimated reporting case incidence (C.est)
LAU_st$C.est <- fit18c$summary.fitted.values$mean * data4$E / data4$pop * 100000  

# add also unadjusted (model without testing)
LAU_st$C.est_without_testing <- fit18c_without_testing$summary.fitted.values$mean * data4$E / data4$pop * 100000  

kommune_sf_epicase_C.est <- tm_shape(LAU_st) +  
  tm_borders(col = "grey80", lwd = 1)  +           
  tm_fill("C.est", title = "Rep inc Est / 100 000", style = "fixed",
          breaks = c(0,1.5,3.00,4.50,6.00,7.50,9.00,10.50,12.00),  
          palette = brewer.pal(7,"YlGn")) +     
  tm_layout(main.title = "Estimated reporting incidence, corrected for testing intensity, Denmark",
            main.title.size = 0.7,
            legend.text.size = 0.5,
            legend.outside=FALSE, 
            legend.position = c("right", "top")) + 
  tm_facets(by = "year", nrow=3) 
kommune_sf_epicase_C.est <- kommune_sf_epicase_C.est + tm_shape(bord) + tm_lines(col = "grey60", lwd = 2) 
kommune_sf_epicase_C.est
tmap_save(kommune_sf_epicase_C.est, "./5 figs/kommune_sf_epicase_R.est_corrected_for_testing.png")

# map also model without testing (unadjusted, total effect)
kommune_sf_epicase_C.est_without_testing <- tm_shape(LAU_st) +  
  tm_borders(col = "grey80", lwd = 1)  +           
  tm_fill("C.est_without_testing", title = "Rep inc Est / 100 000", style = "fixed",
          breaks = c(0,1.5,3.00,4.50,6.00,7.50,9.00,10.50,12.00), 
          palette = brewer.pal(7,"YlGn")) +     
  tm_layout(main.title = "Estimated reporting incidence, unadjusted for testing intensity (i.e. total effect), Denmark",
            main.title.size = 0.7,
            # legend.title.size = 1,
            legend.text.size = 0.5,
            legend.outside=FALSE, 
            legend.position = c("right", "top")) + 
  tm_facets(by = "year", nrow=3) 
kommune_sf_epicase_C.est_without_testing <- kommune_sf_epicase_C.est_without_testing + tm_shape(bord) + tm_lines(col = "grey60", lwd = 2) 
kommune_sf_epicase_C.est_without_testing
tmap_save(kommune_sf_epicase_C.est_without_testing, "./5 figs/kommune_sf_epicase_R.est_without_testing.png")

# RR 
LAU_st %>% ggplot(aes(id, epicase_rr)) + geom_point(cex = 1) + coord_flip() + theme_bw() +
  geom_hline(yintercept =  1, lwd = 1, col = "darkred") +
  geom_pointrange(aes(ymin = epicase_rr_ll,
                      ymax = epicase_rr_ul)) +    
  ggtitle("RR Reported Cases and 95% equal tail CrI") + 
  facet_wrap("year") 
ggsave("./5 figs/kommune_epicase_rr_pointrange.png", width = 15, height = 10) 

#########################################################################################
# COMPARISON 1. TESTED VS CASES, AND 2. CASES UNAJUSTED VS CASES ADJUSTED FOR TESTED  ###
#########################################################################################

# add testing results from script 14b
LAU_df_testing <- read_csv("./1 data/4 processed geodata/DK_LAU_results_testing/DK_LAU_results_testing.csv")
LAU_st_df_testing <- read_csv("./1 data/4 processed geodata/DK_LAU_st_results_testing/DK_LAU_st_results_testing.csv")

LAU <- LAU |> left_join(LAU_df_testing)
LAU_st <- LAU_st |> left_join(LAU_st_df_testing)

# compare point estimates: RR 
summary(LAU_st$tested_rr)       
LAU_st |> st_drop_geometry() |> 
  group_by(year) |> summarize(median_testing = median(tested_rr))

summary(LAU_st$epicase_rr)    
summary(LAU_st$epicase_rr_without_testing)

# point estimates (municipalities) RR testing vs RR cases unadjusted, quadrant  
LAU_st |> ggplot(aes(tested_rr, epicase_rr_without_testing)) +
  geom_point() +
  facet_wrap(~year) +
  ggtitle("RR testing vs RR unadjusted cases")

LAU_st |> ggplot(aes(tested_rr, epicase_rr)) +
  geom_point() +
  facet_wrap(~year) +
  ggtitle("RR testing vs RR adjusted cases")

color_above_tested <- mean(LAU_st$tested_rr) + 2*sd(LAU_st$tested_rr)
color_below_tested <- mean(LAU_st$tested_rr) - 2*sd(LAU_st$tested_rr)
color_above_epicase <- mean(LAU_st$epicase_rr) + 2*sd(LAU_st$epicase_rr)
color_below_epicase <- mean(LAU_st$epicase_rr) - 2*sd(LAU_st$epicase_rr)

LAU_st |> ggplot(aes(tested_rr, epicase_rr_without_testing)) +
  geom_point() + 
  geom_vline(xintercept = c(1)) +
  geom_hline(yintercept = c(1)) +
  geom_vline(xintercept = color_above_tested, linetype = "longdash", color = "grey") +
  geom_vline(xintercept = color_below_tested, linetype = "longdash", color = "grey") +
  geom_hline(yintercept = color_above_epicase, linetype = "longdash", color = "grey") +
  geom_hline(yintercept = color_below_epicase, linetype = "longdash", color = "grey") +
  facet_wrap(~ year) +
  xlab("RR testing") +
  ylab("RR cases unadjusted (black)") +
  facet_wrap(~year) +
  theme_bw() +
  ggtitle("RR testing vs RR unadjusted cases (black)")

color_above_tested <- 3
color_below_tested <- 0.33
color_above_epicase <- 3
color_below_epicase <- 0.33

LAU_st |> ggplot(aes(tested_rr, epicase_rr_without_testing)) +
  geom_point() + 
  #geom_
  geom_vline(xintercept = c(1)) +
  geom_hline(yintercept = c(1)) +
    geom_vline(xintercept = color_above_tested, color = "grey") +                          
    geom_vline(xintercept = color_below_tested, color = "grey") +   
    geom_hline(yintercept = color_above_epicase, color = "grey") +  
    geom_hline(yintercept = color_below_epicase, color = "grey") +  
  facet_wrap(~ year) +
  geom_point(aes(tested_rr, epicase_rr), col = "red") +
  xlab("RR testing") +
  ylab("RR case reporting") +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        ) +
  labs(title='Municipalities (n = 99), RR testing versus RR case reporting, \nunadjusted (black) - adjusted for testing (red)',
       subtitle='Gridlines at RR = 0.33, 1, and 3')

# point estimates (municipalities) RR cases unadjusted vs RR cases unadjusted
LAU_st |> ggplot(aes(epicase_rr_without_testing, epicase_rr)) +
  geom_point() +
  facet_wrap(~year) +
  ggtitle("RR unadjusted vs RR adjusted cases")

LAU_st |> st_drop_geometry() |> 
  group_by(year) |> summarize(median_testing = median(tested_rr)) 

summary(LAU_st$tested_rr)                     
summary(LAU_st$epicase_rr_without_testing)   

# 2. marginals

# Exceedance prob of RR > 1 (Pr(theta) > 1)   
LAU_st$epicase_rr_exc <-  unlist(lapply(fit18c$marginals.fitted.values, function(X){1-inla.pmarginal(1, X) })) 
kommune_sf_epicase_rr_exc <- tm_shape(LAU_st) +  
  tm_borders(col = "grey80", lwd = 1.5)  +
  tm_fill("epicase_rr_exc", title = "Pr(RR) > 1", style = "fixed",
          breaks = c(0,0.05,0.25,0.5,0.75,0.80,0.95,0.99,1), 
          palette = brewer.pal(n = 8, name = "Greys")) +     
  tm_layout(main.title = "Exc RR, Reported Cases BYM, Denmark",
            main.title.size = 0.7,
            # legend.title.size = 1,
            legend.text.size = 0.5,
            legend.outside=FALSE, 
            legend.position = c("right", "top")) + 
  tm_facets(by = "year", nrow=3) 
kommune_sf_epicase_rr_exc <- kommune_sf_epicase_rr_exc + tm_shape(bord) + tm_lines(col = "grey60", lwd = 2) 
kommune_sf_epicase_rr_exc 
tmap_save(kommune_sf_epicase_rr_exc, "./5 figs/kommune_sf_epicase_rr_exc.png")

# Deficit prob of RR < 1 (Pr(theta) < 1)    
LAU_st$epicase_rr_deficit <-  unlist(lapply(fit18c$marginals.fitted.values, function(X){inla.pmarginal(1, X) })) 
kommune_sf_epicase_rr_deficit <- tm_shape(LAU_st) +  
  tm_borders(col = "grey80", lwd = 1.5)  +
  tm_fill("epicase_rr_deficit", title = "Pr(RR) < 1", style = "fixed",
          breaks = c(0,0.05,0.25,0.5,0.75,0.80,0.95,0.99,1), # c(-2, -1, 0, 1, 2),  
          palette = brewer.pal(n = 8, name = "Greys")) +  #"Blues"
  tm_layout(main.title = "Deficit RR, Reported Cases BYM, Denmark",
            main.title.size = 0.7,
            legend.text.size = 0.5,
            legend.outside=FALSE, 
            legend.position = c("right", "top")) + 
  tm_facets(by = "year", nrow=3) 
kommune_sf_epicase_rr_deficit <- kommune_sf_epicase_rr_deficit + tm_shape(bord) + tm_lines(col = "grey60", lwd = 2) 
kommune_sf_epicase_rr_deficit
tmap_save(kommune_sf_epicase_rr_deficit, "./5 figs/kommune_sf_epicase_rr_deficit.png")

p <- tmap_arrange(kommune_sf_epicase_rr, 
                  kommune_sf_epicase_rr_exc,
                  nrow = 1, ncol = 2)
p 
tmap_save(p, "./5 figs/kommune_sf_epicase_rr_and_rr_exc.png", dpi = 600)  


#### SUMMARY ESTIMATED TESTING

df <- LAU_st |> st_drop_geometry()
df <- df |> mutate(diff_unadjusted_minus_adjusted = C.est_without_testing - C.est) 

top_5_rr_testing <- tt(df |> slice_max(tested_rr, n =  5)) 
top_5_rr_testing

# 1. C.est 

round(fivenum(df$C.est), 2)

# list all - long table with all three indicators
diff_unadj_minus_adj <- df |>  
  mutate(C.est = round(C.est, 2), 
         C.est_without_testing = round(C.est_without_testing, 2), 
         diff_unadjusted_minus_adjusted = round(diff_unadjusted_minus_adjusted, 2))|> 
  select(year, landsdel, kommune, C.est, C.est_without_testing, diff_unadjusted_minus_adjusted) |> 
  arrange(- diff_unadjusted_minus_adjusted) 

diff_unadj_minus_adj  
write_csv(diff_unadj_minus_adj, "./4 tables/t_diff_unadj_minus_adj.csv")
  
t_C_adjusted <- df |>                                   
               mutate(C.est = round(C.est, 2))|> 
               mutate(landsdel = fct_recode(landsdel,
                                            "B" = "Bornholm",
                                            "F"  = "Fyn",          
                                            "C.C" = "Koebenhavn by",
                                            "C.S" = "Koebenhavns omegn",
                                            "N-J" = "Nordjylland",
                                            "N-Z" = "Nordsjaelland",    
                                            "E-J" ="Oestjylland",           
                                            "E-Z" ="Oestsjaelland",        
                                            "S-J" = "Sydjylland",            
                                            "WS-Z" = "Vest- og Sydsjaelland",
                                            "W-J" = "Vestjylland")) |>
               arrange(landsdel, kommune) |> 
               select(year, Province = landsdel, Municipality = kommune, C.est) |> 
               pivot_wider(names_from = year, values_from = C.est)
t_C_adjusted 
write_csv(t_C_adjusted, "./4 tables/t_C_adjusted.csv")

t_C_without_testing <- df |> 
                    mutate(C.est_without_testing = round(C.est_without_testing, 2))|> 
                    mutate(landsdel = fct_recode(landsdel,
                                                 "B" = "Bornholm",
                                                 "F"  = "Fyn",          
                                                 "C.C" = "Koebenhavn by",
                                                 "C.S" = "Koebenhavns omegn",
                                                 "N-J" = "Nordjylland",
                                                 "N-Z" = "Nordsjaelland",    
                                                 "E-J" ="Oestjylland",           
                                                 "E-Z" ="Oestsjaelland",        
                                                 "S-J" = "Sydjylland",            
                                                 "WS-Z" = "Vest- og Sydsjaelland",
                                                 "W-J" = "Vestjylland")) |>
                  arrange(landsdel, kommune) |> 
                    select(year, Province = landsdel, Municipality = kommune, C.est_without_testing) |> 
                    pivot_wider(names_from = year, values_from = C.est_without_testing)
t_C_without_testing
write_csv(t_C_without_testing, "./4 tables/t_C_without_testing.csv")


t_C_unadjusted_minus_adjusted <- df |> 
                                    mutate(diff_unadjusted_minus_adjusted = round(diff_unadjusted_minus_adjusted, 2))|> 
                                    mutate(landsdel = fct_recode(landsdel,
                                                                 "B" = "Bornholm",
                                                                 "F"  = "Fyn",          
                                                                 "C.C" = "Koebenhavn by",
                                                                 "C.S" = "Koebenhavns omegn",
                                                                 "N-J" = "Nordjylland",
                                                                 "N-Z" = "Nordsjaelland",    
                                                                 "E-J" ="Oestjylland",           
                                                                 "E-Z" ="Oestsjaelland",        
                                                                 "S-J" = "Sydjylland",            
                                                                 "WS-Z" = "Vest- og Sydsjaelland",
                                                                 "W-J" = "Vestjylland")) |>
                                    arrange(landsdel, kommune) |> 
                                    select(year, Municipality = kommune, diff_unadjusted_minus_adjusted) |> 
                                    pivot_wider(names_from = year, values_from = diff_unadjusted_minus_adjusted)
t_C_unadjusted_minus_adjusted 
write_csv(t_C_unadjusted_minus_adjusted, "./4 tables/t_C_unadjusted_minus_adjusted.csv")

## datasummary 
kommune_sf_st_c <- LAU_st %>% 
  select(year, kommune, landsdel, C.est) %>% st_drop_geometry() |> as_tibble()
fivenum(kommune_sf_st_c$C.est)  
kommune_sf_st_c |> summarize( 
  min = min(C.est), 
  Q1=quantile(C.est, probs = 0.25),
  median = median(C.est),
  mean = mean(C.est),
  Q3=quantile(C.est,probs = 0.75),
  max = max(C.est))  

kommune_sf_st_c %>% 
  filter(C.est >= 10.5) %>% count(landsdel, kommune) %>% arrange(landsdel) 
kommune_sf_st_c %>% 
  filter(C.est >= 11) %>% count(landsdel, kommune) %>% arrange(landsdel)  

kommune_sf_st_c %>% 
  filter(C.est >= 9) %>% count(landsdel, kommune) %>% arrange(-n) 

kommune_sf_st_c |> filter(kommune == "Langeland") |> 
  arrange(-C.est) 
kommune_sf_st_c |> filter(kommune == "Nyborg") |> 
  arrange(-C.est) 

kommune_sf_st_c  |>  arrange(-C.est) 
kommune_sf_st_c  |>  arrange(C.est) 

### summary by kommunes (all years) 
kommune_sf_st_c |> group_by(kommune, landsdel) |> arrange(C.est)
kommune_sf_st_c |> group_by(kommune, landsdel) |> arrange(-C.est)

kommune_sf_st_c |> group_by(kommune, landsdel) |> 
  summarize(median = median(C.est), min = min(C.est), max = max(C.est)) |> 
  arrange(median)

kommune_sf_st_c |> group_by(kommune, landsdel) |> 
  summarize(median = median(C.est), min = min(C.est), max = max(C.est)) |> 
  arrange(-median)  

### summary by year (all kommunes) 
kommune_sf_st_c |> group_by(year) |> summarize(median = median(C.est))  

kommune_sf_st_c |> group_by(year) |> 
  summarize(median = median(C.est), min = min(C.est), 
            q1 = quantile(probs = 0.25, C.est),
            q3 = quantile(probs = 0.75, C.est),
            max = max(C.est)) |> 
  arrange(year) 

###########################
df <- df |> as_tibble()

# top_5 values & areas 
df |>  slice_max(C.est , n =  5) |> 
  select(year, landsdel, kommune, C.est) 

# list areas over11 
df |> filter(C.est > 11) |>                           
  select(year, landsdel,  kommune, C.est) |>
  arrange(landsdel, year) |> print.all() 

# list areas over9 
df |> filter(C.est > 9) |>                          
  select(year, landsdel,  kommune, C.est) |>
  arrange(landsdel, year) |> print.all() 

df |> filter(C.est > 9) |>                           
  select(year, landsdel,  kommune, C.est) |>
  group_by(kommune) |> count() |> arrange(-n) 

# bottom_5 values & areas 
df |>  slice_min(C.est , n =  5) |> 
  select(year, landsdel, kommune, C.est) 

# list areas under1.5
df |> filter(C.est < 1.5) |> 
  select(year, landsdel,  kommune, C.est) |>
  arrange(landsdel, year) 

# list areas under1.5
df |> filter(C.est < 2) |> 
  select(year, landsdel,  kommune, C.est) |>
  arrange(landsdel, year)  |> distinct(landsdel, kommune) 

# rr  
fivenum(df$epicase_rr)
round(df |> as_tibble() |> summarize( 
  min = min(epicase_rr), 
  Q1=quantile(epicase_rr, probs = 0.25),
  median = median(epicase_rr),
  mean = mean(epicase_rr),
  Q3=quantile(epicase_rr,probs = 0.75),
  max = max(epicase_rr)), 2)  

# top_5 values & areas 
df |>  slice_max(epicase_rr , n =  5) |> 
  select(year, landsdel, kommune, epicase_rr) 

# list areas over2.5 
df |> filter(epicase_rr > 2.5) |>   
  select(year, landsdel,  kommune, epicase_rr) |>
  arrange(landsdel, year) 

# list areas over2
df |> filter(epicase_rr > 2) |>   
  select(year, landsdel,  kommune, epicase_rr) |>
  arrange(landsdel, year)  |> distinct(landsdel, kommune) 
# > 2 = 26 areas, > 3  = 6 areas

df |> filter(epicase_rr > 2) |>   
  select(year, landsdel,  kommune, epicase_rr) |>
  arrange(landsdel, year) |> print.all()

df |> filter(epicase_rr > 2) |>                           
  select(year, landsdel,  kommune, C.est) |>
  group_by(landsdel, kommune) |> count() |> arrange(-n) 

df %>% filter(epicase_rr >=2) |> distinct(kommune, landsdel, year) |> arrange(kommune) |> print.all()
df %>% filter(epicase_rr >=2) |> distinct(kommune) |> pull() # to ease writing

# bottom_5 values & areas 
df |>  slice_min(epicase_rr , n =  10) |>                        
  select(year, landsdel, kommune, epicase_rr) 

# list areas under0.44
df |> filter(epicase_rr < 0.44) |>   
  select(year, landsdel,  kommune, epicase_rr) |>
  arrange(landsdel, year) |> print.all() 

# list areas underhalf
df |> filter(epicase_rr < 0.5) |>  
  select(year, landsdel,  kommune, epicase_rr) |>
  arrange(landsdel, year) |> distinct(kommune) |> pull()

df |> filter(epicase_rr < 0.5) |>   
  select(year, landsdel,  kommune, epicase_rr) |>
  arrange(epicase_rr) |> print.all()

df |> filter(epicase_rr < 0.5) |>                           
  select(year, landsdel,  kommune, C.est) |>
  group_by(landsdel, kommune) |> count() |> arrange(-n) 


fivenum(df$epicase_rr_exc) 

# top_5 values & areas  
df |>  slice_max(epicase_rr_exc , n =  5) |> 
  select(year, landsdel, kommune, epicase_rr_exc) 

# list areas over95 
df |> filter(epicase_rr_exc >  0.95) |> 
  select(year, landsdel,  kommune, epicase_rr_exc) |>
  arrange(landsdel, year)  |> distinct(kommune) |> pull(kommune)

# list areas over99 
df |> filter(epicase_rr_exc >  0.99) |> 
  select(year, landsdel,  kommune, epicase_rr_exc) |>
  arrange(landsdel, year)  |> distinct(kommune) |> pull(kommune)

# epicase_rr_deficit 
fivenum(df$epicase_rr_deficit)

# top_5 values & areas 
df |>  slice_max(epicase_rr_deficit , n =  5) |> 
  select(year, landsdel, kommune, epicase_rr_exc) 

# list areas over95
df |> filter(epicase_rr_deficit >  0.95) |>                  
  select(year, landsdel,  kommune, epicase_rr_deficit) |>
  arrange(landsdel, year)  |> distinct(kommune) |> pull(kommune)

# list areas over99
df |> filter(epicase_rr_deficit >  0.99) |>                 
  select(year, landsdel,  kommune, epicase_rr_deficit) |>
  arrange(landsdel, year)  |> distinct(kommune) |> pull(kommune)

# epicase_STinteraction_exc 
fivenum(df$epicase_STinteraction_exc)

# top_5 values & areas 
df |>  slice_max(epicase_STinteraction_exc , n =  5) |> 
  select(year, landsdel, kommune, epicase_STinteraction_exc)

# list areas over2 
df |> filter(epicase_STinteraction_exc >  0.95) |> 
  select(year, landsdel,  kommune, epicase_STinteraction_exc) |>
  arrange(landsdel, year)  |> distinct(kommune) |> pull(kommune)  

df |> filter(epicase_STinteraction_exc >  0.80) |> 
  select(year, landsdel,  kommune, epicase_STinteraction_exc) |>
  arrange(landsdel, year)  |> distinct(kommune) |> pull(kommune)  

df |> select(year, kommune, landsdel, epicase_STinteraction_exc) |> slice_min(epicase_STinteraction_exc, n =  5) 

# T.est
t_Testing <- df |> 
  mutate(T.est = round(T.est, 0))|> 
  mutate(landsdel = fct_recode(landsdel,
                               "B" = "Bornholm",
                               "F"  = "Fyn",          
                               "C.C" = "Koebenhavn by",
                               "C.S" = "Koebenhavns omegn",
                               "N-J" = "Nordjylland",
                               "N-Z" = "Nordsjaelland",    
                               "E-J" ="Oestjylland",           
                               "E-Z" ="Oestsjaelland",        
                               "S-J" = "Sydjylland",            
                               "WS-Z" = "Vest- og Sydsjaelland",
                               "W-J" = "Vestjylland")) |>
  arrange(landsdel, kommune) |> 
  select(year, Province = landsdel, Municipality = kommune, T.est) |> 
  pivot_wider(names_from = year, values_from = T.est)
t_Testing 
write_csv(t_Testing , "./4 tables/t_Testing.csv")

# 1. no high testing (RR<1.5), high cases (RR>2.2) 
df |> filter(tested_rr < 1.5, epicase_rr_without_testing >  2.2) |> 
  select(year, landsdel,  kommune, tested_rr, epicase_rr_without_testing, epicase_rr) |>
  arrange(landsdel, year)  

# 2. high testing (RR>2), high cases (RR>2.2) 
df |> filter(tested_rr > 2, epicase_rr_without_testing >  2.2) |> 
  select(year, landsdel,  kommune, tested_rr, epicase_rr_without_testing, epicase_rr) |>
  arrange(landsdel, year) 

# 3. very high testing (RR>3), no high cases (RR<2) 
df |> filter(tested_rr >  3, epicase_rr_without_testing < 2) |> 
  select(year, landsdel,  kommune, tested_rr, epicase_rr_without_testing, epicase_rr) |>
  arrange(landsdel, year)  




