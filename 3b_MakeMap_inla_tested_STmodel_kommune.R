# 14c_makeMap_inla_tested_STmodel_kommune

#### 1. LIBRARIES ####
source("./1_load_libraries.R")

LAU <- st_read("./1 data/4 processed geodata/6 DK_LAU_for_model_visualisation/DK_LAU_for_model_visualisation.shp") 
LAU_st <- st_read("./1 data/4 processed geodata/7 DK_LAU_st_for_model_visualisation/DK_LAU_st_for_model_visualisation.shp") 
nuts3_sf_dk <- st_read("./1 data/4 processed geodata/5 DK_NUTS3_for_model_visualisation/DK_NUTS3_for_model_visualisation.shp")

st_crs(LAU)          
st_crs(LAU_st)         
st_crs(nuts3_sf_dk)   

bord <- ms_innerlines(nuts3_sf_dk)  
bord <-  bord |> st_as_sf() |> mutate(id = 1:10)

#### 3. DISPLAY RESULTS  (from script a, model 6b)  ####

## best model 
summary(fit6b)   

#### PARAMS ####

## INTERCEPT

fit6b$marginals.fixed$`(Intercept)`                             
as_tibble(inla.smarginal(fit6b$marginals.fixed$`(Intercept)`))  

ggplot(as_tibble(inla.smarginal(fit6b$marginals.fixed$`(Intercept)`))) +
  geom_line(aes(x = x, y = y), lwd = 1) + theme_bw() +
  ylab(expression(paste(pi, "(", "x", " | ", bold(y), ")"))) + # y label to be adapted 
  ggtitle("posterior intercept")

## RANDOM EFFECT PARAMS 

# summary

fit6b$summary.random$t                    
fit6b$summary.random$ID.struct            
fit6b$summary.random$ID.unstruct           
fit6b$summary.random$ID.area.time         

# 1. time re 
df <- data.frame(time=2014:2022,
                 effect = fit6b$summary.random$t$mean,            
                 high=fit6b$summary.random$t$`0.975quant`,       
                 low=fit6b$summary.random$t$`0.025quant`)      
df

f <- ggplot(df, aes(time, effect)) + 
  geom_ribbon(aes(ymin=low, ymax=high), fill="darkgoldenrod3", alpha = 0.2) +  
  geom_line(lwd = 2) +
  xlab("Time (year)") +   
  ylab("log(RR) testing") + 
  ggtitle("Evolution testing intensity over time") +  
  theme_bw()
f   
kommune_assoc_time_tested_spatiotemp <- f
ggsave("./5 figs/kommune_assoc_time_tested_spatiotemp.png", width = 15, height = 10) 
saveRDS(kommune_assoc_time_tested_spatiotemp, file = "./5 figs/kommune_assoc_time_tested_spatiotemp.RDS") 

df  
fivenum(df$effect)

# 2. icar re 

LAU$tested_ICAR <- fit6b$summary.random$ID.struct$mean
LAU$tested_IID  <- fit6b$summary.random$ID.unstruct$mean

round(range(fit6b$summary.random$ID.struct$mean), 2)    
fivenum(range(fit6b$summary.random$ID.struct$mean))

LAU_tested_icar <- tm_shape(LAU) +  
  tm_borders(col = "grey80", lwd = 1)  +
  tm_fill("tested_ICAR", title = "ICAR", style = "fixed",
          breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5),  
          palette = rev(brewer.pal(6,"RdBu")),
          midpoint = 0
          ) +             
  tm_layout(main.title = "ICAR random effect, Testing BYM, Denmark",
            main.title.size = 0.7,
            legend.text.size = 0.5)  
LAU_tested_icar

LAU_tested_icar <- LAU_tested_icar + tm_shape(bord) + tm_lines(col = "grey70", lwd = 2)
LAU_tested_icar
tmap_save(LAU_tested_icar , "./5 figs/LAU_tested_icar.png")

# 3. iid re 

round(range(fit6b$summary.random$ID.unstruct$mean), 2)   # -0.01  0.02 on log scale
fivenum(range(fit6b$summary.random$ID.unstruct$mean))

LAU_tested_iid <- tm_shape(LAU) +  
  tm_borders(col = "grey80", lwd = 1)  +
  tm_fill("tested_IID", title = "IID", style = "fixed",
          breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5), 
          palette = rev(brewer.pal(6,"RdBu")),
          midpoint = 0
          ) +
  tm_layout(main.title = "IID random effect, Testing BYM, Denmark",
            main.title.size = 0.7,
            legend.text.size = 0.5)    
LAU_tested_iid 

LAU_tested_iid +
  tm_shape(nuts3_sf_dk) +
  tm_borders(col = "grey80", lwd = 3) + 
  tm_legend(show=FALSE) 

LAU_tested_iid <- LAU_tested_iid + tm_shape(bord) + tm_lines(col = "grey70", lwd = 2)  # 70 for not-facets map
LAU_tested_iid
tmap_save(LAU_tested_iid , "./5 figs/LAU_tested_iid.png")

LAU_tested_icar_iid <- tmap::tmap_arrange(LAU_tested_icar, LAU_tested_iid)
LAU_tested_icar_iid 
tmap_save(LAU_tested_icar_iid , "./5 figs/LAU_tested_icar_iid.png", height = 7, width = 14)

# 4. ST interaction re 

LAU_st <- LAU_st  %>% select(st_id, kommune, everything()) 
LAU_st$tested_STinteraction <- fit6b$summary.random$ID.area.time$mean 

round(range(LAU_st$tested_STinteraction), 2)    
fivenum(range(LAU_st$tested_STinteraction))

LAU_tested_STinteraction <- tm_shape(LAU_st) +  
  tm_borders(col = "grey80", lwd = 1)  +  
  tm_fill("tested_STinteraction", title = "ST interaction", style = "fixed", alpha = 1,
          breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5),  
          palette = rev(brewer.pal(6,"RdBu")),
          midpoint = 0
  ) +      
  tm_layout(main.title = "ST interaction, Testing BYM, Denmark",
            main.title.size = 0.7,
            legend.text.size = 0.5,
            legend.outside=FALSE, 
            legend.position = c("right", "top")) + 
  tm_facets(by = "year", nrow=3) 
LAU_tested_STinteraction

LAU_tested_STinteraction <- LAU_tested_STinteraction + tm_shape(bord) + tm_lines(col = "grey60", lwd = 2)  
LAU_tested_STinteraction
tmap_save(LAU_tested_STinteraction, "./5 figs/LAU_tested_interaction.png")

## 1c hyper param

# Relative importance of variance components from 4 varying effects

# 1. Simulate unstructured variance
var.v <- inla.rmarginal(100000,inla.tmarginal(fun=function(x) 1/x, fit6b$marginals.hyperpar$`Precision for ID.unstruct`))   

# 2. Simulate structured variance - if not bym, I can do similar as for unstructured variance:
var.u <- inla.rmarginal(100000,inla.tmarginal(fun=function(x) 1/x, fit6b$marginals.hyperpar$`Precision for ID.struct`))  

# 3. Simulate structured variance 
var.t <- inla.rmarginal(100000,inla.tmarginal(fun=function(x) 1/x, fit6b$marginals.hyperpar$`Precision for t`))   

# 4. Simulate structured variance 
var.STinteraction <- inla.rmarginal(100000,inla.tmarginal(fun=function(x) 1/x, fit6b$marginals.hyperpar$`Precision for ID.area.time`))  

# percentage variance attributable to the four varying effects 
print(mean(var.u / (var.u + var.v + var.t + var.STinteraction)) *100 )    
print(mean(var.v / (var.u + var.v + var.t + var.STinteraction)) *100)     
print(mean(var.t / (var.u + var.v + var.t + var.STinteraction)) *100 )    
print(mean(var.STinteraction / (var.u + var.v + var.t + var.STinteraction)) *100)

#### FITTED VALUES ####

# 1. summary 

# RR  
fit6b$summary.fitted.values      
LAU_st$tested_rr <- fit6b$summary.fitted.values$mean 

LAU_st_tested_rr <- LAU_st %>% select(tested_rr)
write_csv(LAU_st_tested_rr, "LAU_st_tested_rr.csv")

LAU_st$tested_rr_ll <- fit6b$summary.fitted.values$'0.025quant'
LAU_st$tested_rr_ul <- fit6b$summary.fitted.values$'0.975quant' 

LAU_tested_rr <- tm_shape(LAU_st) +  
  tm_borders(col = "grey60", lwd = 1)  +             
  tm_fill("tested_rr", title = "RR", style = "fixed",
          breaks = c(0, 0.5, 0.6, 0.8,  1.2, 1.5, 2, 5),  
          palette = rev(brewer.pal(7,"RdYlBu"))) +   
  tm_layout(main.title = "RR, Testing BYM, Denmark",
            main.title.size = 0.7,
            # legend.title.size = 1,
            legend.text.size = 0.5,
            legend.outside=FALSE, 
            legend.position = c("right", "top")) + 
  tm_facets(by = "year", nrow=3) 
LAU_tested_rr <- LAU_tested_rr + tm_shape(bord) + tm_lines(col = "grey40", lwd = 2)  
LAU_tested_rr 
tmap_save(LAU_tested_rr, "./5 figs/LAU_tested_rr.png")

LAU_tested_rr  
fivenum(LAU_st$rr)

# Estimated Testing incidence (T.est)
LAU_st$T.est <- fit6b$summary.fitted.values$mean * data4$E / data4$pop * 100000 

LAU_tested_T.est <- tm_shape(LAU_st) +  
  tm_borders(col = "grey60", lwd = 1)  +           
  tm_fill("T.est", title = "Est testing int / 100 000", style = "fixed",
          breaks = c(0,300,600,800,1000,1200,1400,1700,2500),  
          palette = brewer.pal(7,"BuPu")) +  
  tm_layout(main.title = "Estimated testing intensity, Denmark",
            main.title.size = 0.7,
            # legend.title.size = 1,
            legend.text.size = 0.5,
            legend.outside=FALSE, 
            legend.position = c("right", "top")) + 
  tm_facets(by = "year", nrow=3) 
LAU_tested_T.est

LAU_tested_T.est <- LAU_tested_T.est + tm_shape(bord) + tm_lines(col = "grey40", lwd = 2) 
LAU_tested_T.est
tmap_save(LAU_tested_T.est, "./5 figs/LAU_tested_T.est.png")

# RR (plot)
# plot mean and 95%CrI  -> but ll and ul not saved in dataset...

# pointrange
LAU_st %>% ggplot(aes(id, tested_rr)) + geom_point(cex = 1) + coord_flip() + theme_bw() +
  geom_hline(yintercept =  1, lwd = 1, col = "darkred") +
  geom_pointrange(aes(ymin = tested_rr_ll,
                      ymax = tested_rr_ul)) +    
  ggtitle("RR Testing and 95% equal tail CrI") + 
  facet_wrap("year") 
ggsave("./5 figs/kommune_rr_tested_pointrange.png", width = 15, height = 10) 

# 2. marginals

# Exceedance prob of RR > 1 (Pr(theta) > 1)
LAU_st$tested_rr_exc <-  unlist(lapply(fit6b$marginals.fitted.values, function(X){1-inla.pmarginal(1, X) }))
LAU_tested_rr_exc <- tm_shape(LAU_st) +  
  tm_borders(col = "grey60", lwd = 1)  +
  tm_fill("tested_rr_exc", title = "Pr(RR) > 1", style = "fixed",
          breaks = c(0,0.05,0.25,0.5,0.75,0.80,0.95,0.99,1), 
          palette = brewer.pal(n = 8, name = "Greys")) +     
  tm_layout(main.title = "Exc RR, Testing BYM, Denmark",
            main.title.size = 0.7,
            legend.text.size = 0.5,
            legend.outside=FALSE, 
            legend.position = c("right", "top")) + 
  tm_facets(by = "year", nrow=3) 
LAU_tested_rr_exc

LAU_tested_rr_exc <- LAU_tested_rr_exc + tm_shape(bord) + tm_lines(col = "grey40", lwd = 3) 
LAU_tested_rr_exc
tmap_save(LAU_tested_rr_exc, "./5 figs/LAU_tested_rr_exc.png")

# Exceedance prob of RR > 2 (Pr(theta) > 2)    
LAU_st$tested_rr_exc2 <-  unlist(lapply(fit6b$marginals.fitted.values, function(X){1-inla.pmarginal(2, X) })) 
LAU_tested_rr_exc2 <- tm_shape(LAU_st) +  
  tm_borders(col = "grey60", lwd = 1)  +
  tm_fill("tested_rr_exc2", title = "Pr(RR) > 2", style = "fixed",
          breaks = c(0,0.05,0.25,0.5,0.75,0.80,0.95,0.99,1), 
          palette = brewer.pal(n = 8, name = "Greys")) +     
  tm_layout(main.title = "Exc RR > 2, Testing BYM, Denmark",
            main.title.size = 0.7,
            legend.text.size = 0.5,
            legend.outside=FALSE, 
            legend.position = c("right", "top")) + 
  tm_facets(by = "year", nrow=3) 
LAU_tested_rr_exc2

LAU_tested_rr_exc2 <- LAU_tested_rr_exc2 + tm_shape(bord) + tm_lines(col = "grey40", lwd = 3) 
LAU_tested_rr_exc2
tmap_save(LAU_tested_rr_exc2, "./5 figs/LAU_tested_rr_exc2.png")

# Deficit prob of RR < 1 (Pr(theta) < 1)    
LAU_st$tested_rr_deficit <-  unlist(lapply(fit6b$marginals.fitted.values, function(X){inla.pmarginal(1, X) })) 
LAU_tested_rr_deficit <- tm_shape(LAU_st) +  
  tm_borders(col = "grey80", lwd = 1)  +
tm_fill("tested_rr_deficit", title = "Pr(RR) < 1", style = "fixed",
        breaks = c(0,0.05,0.25,0.5,0.75,0.80,0.95,0.99,1), 
        palette = brewer.pal(n = 8, name = "Greys")) +     
  tm_layout(main.title = "Deficit RR, Testing BYM, Denmark",
            main.title.size = 0.7,
            legend.text.size = 0.5,
            legend.outside=FALSE, 
            legend.position = c("right", "top")) + 
  tm_facets(by = "year", nrow=3) 
LAU_tested_rr_deficit

LAU_tested_rr_deficit <- LAU_tested_rr_deficit + tm_shape(bord) + tm_lines(col = "grey40", lwd = 3) 
LAU_tested_rr_deficit
tmap_save(LAU_tested_rr_deficit, "./5 figs/LAU_tested_rr_deficit.png")

#### SaveE LAU and LAU_st with results of testing, for use in script d
LAU_df_testing <- LAU |> st_drop_geometry() 
LAU_st_df_testing <- LAU_st |> st_drop_geometry() 
write_csv(LAU_df_testing, "./1 data/4 processed geodata/DK_LAU_results_testing/DK_LAU_results_testing.csv")
write_csv(LAU_st_df_testing, "./1 data/4 processed geodata/DK_LAU_st_results_testing/DK_LAU_st_results_testing.csv")


#### SUMMARY ESTIMATED TESTING
df <- LAU_st |> st_drop_geometry() |> as_tibble()
tt(df)

tt(df[1:99,] |> select("Province" = landsdel, "ID" = id, "Municipality" = kommune, "Population" = POP_2021) |> arrange(Province))
knitr::kable(df[1:99,] |> select("Province" = landsdel, "ID" = id, "Municipality" = kommune, "Population (2021)" = POP_2021) |> arrange(Province))


# 1. Population

t_poptop <- tt(df |> filter(year == 2022) |> select(year, kommune, landsdel, POP_2021) |> slice_max(POP_2021, n =  5),
   theme = "striped",
   caption = "Pop top")  
t_poptop 
t_poptop |> print("latex") 
df |> filter(year == 2022) |> select(year, kommune, landsdel, POP_2021) |> slice_max(POP_2021, n =  5) |> pull(kommune)

t_popbottom <- tt(df |> filter(year == 2022) |> select(year, kommune, landsdel, POP_2021) |> slice_min(POP_2021, n =  5),
   theme = "striped",
   caption = "Pop bottom") 
t_popbottom
df |> filter(year == 2022) |> select(year, kommune, landsdel, POP_2021) |> slice_min(POP_2021, n =  5) |> pull(kommune)

df |> filter(year == 2022) |> select(year, landsdel, POP_2021) |> summarize(sum_pop = sum(POP_2021))
df |> filter(year == 2022) |> select(year, landsdel, POP_2021) |> group_by(landsdel) |>  summarize(sum_pop = sum(POP_2021)) |> arrange(sum_pop)

# 2. T.est 
df_T.est <- df %>% 
  select(year, kommune, landsdel, T.est)

### summary all kommunes and years
df_T.est 
round(fivenum(df_T.est$T.est), 0)
df_T.est |> summarize(    min = min(T.est), 
                          Q1=quantile(T.est,probs = 0.25),
                          median = median(T.est),
                          mean = mean(T.est), 
                          Q3=quantile(T.est,probs = 0.75),
                          max = max(T.est))  

df_T.est |>  slice_min(T.est , n =  10)   
df_T.est |>  slice_max(T.est , n =  20)    
df_T.est |> 
  arrange(-T.est) |> filter(T.est > 1500) 
df_T.est |> 
  arrange(-T.est) |> filter(T.est > 1500) |> distinct(kommune, landsdel) 
df_T.est |> 
  arrange(-T.est) |> filter(T.est > 2000) 

### summary by kommunes (all years) 
df_T.est |> group_by(kommune, landsdel) |> 
  summarize(median = median(T.est), min = min(T.est), max = max(T.est)) |> 
  arrange(median)

df_T.est |> group_by(kommune, landsdel) |> 
  summarize(median = median(T.est), min = min(T.est),
            q1 = quantile(probs = 0.25, T.est),
            q3 = quantile(probs = 0.75, T.est), 
            max = max(T.est)) |> 
  arrange(-median)   

### summary by year (all kommunes)  
df_T.est |> group_by(year) |> summarize(median = median(T.est)) 

df_T.est |> group_by(year) |> 
  summarize(median = median(T.est), min = min(T.est), 
            q1 = quantile(probs = 0.25, T.est),
            q3 = quantile(probs = 0.75, T.est),
            max = max(T.est)) |> 
  arrange(year)

# top_5 values & areas
df |>  slice_max(T.est , n =  5) |> 
  select(year, landsdel, kommune, T.est) 

# list areas over1500 
df |> filter(T.est > 1400) |> 
  select(year, landsdel,  kommune, T.est) |>
  arrange(landsdel, year)  |> distinct(kommune) |> pull(kommune)

# bottom_5 values & areas 
df |>  slice_min(T.est , n =  5) |> 
  select(year, landsdel, kommune, T.est)

# list areas under150 
df |> filter(T.est < 150) |> 
  select(year, landsdel,  kommune, T.est) |>
  arrange(landsdel, year) |> distinct(kommune) |> pull()

# 3/ rr  

df_rr <- df %>% 
  select(year, kommune, landsdel, tested_rr) %>% st_drop_geometry()

### summary all kommunes and years
round(fivenum(df_rr$tested_rr), 2)
df_rr |> summarize(mean = round(mean(tested_rr), 2), 
                     min = round(min(tested_rr), 2), 
                     Q1 = round(quantile(tested_rr, probs = 0.25), 2),
                     median = round(median(tested_rr), 2),
                     Q3 = round(quantile(tested_rr, probs = 0.75), 2),
                     max = round(max(tested_rr), 2))

df_rr  |> group_by(year) |> summarize(median = median(tested_rr)) 

df_rr  |> 
  arrange(tested_rr) 
df_rr |> 
  arrange(-tested_rr) |> top_n(20) 
df_rr |> 
  arrange(-tested_rr) |> filter(tested_rr > 3)
df_rr |> 
  arrange(-tested_rr) |> filter(tested_rr > 2) 
df_rr |> 
  arrange(-tested_rr) |> filter(tested_rr > 2) |> distinct(kommune, landsdel)  |> print.all()   
df_rr  |> 
  arrange(-tested_rr) |> filter(tested_rr < 0.33) |> print.all()
df_rr  |> 
  arrange(-tested_rr) |> filter(tested_rr < 0.33)  |> distinct(kommune, landsdel)   
df_rr  |> 
  arrange(-tested_rr) |> filter(tested_rr <0.5) 
df_rr  |> 
  arrange(-tested_rr) |> filter(tested_rr <0.5) |> distinct(kommune, landsdel)   

# fivenum
fivenum(df$tested_rr)

# top_5 values & areas
df |>  slice_max(tested_rr , n =  5) |> 
  select(year, landsdel, kommune, tested_rr) 

# list areas over3 
df |> filter(tested_rr > 3) |>   
  select(year, landsdel,  kommune, tested_rr) |>
  arrange(landsdel, year) 
# > 3 = 6 areas

# list areas over2 
df |> filter(tested_rr > 2) |>   
  select(year, landsdel,  kommune, tested_rr) |>
  arrange(landsdel, year) 
# > 2 = 26 areas

# bottom_5 values & areas 
df |>  slice_min(tested_rr , n =  5) |> 
  select(year, landsdel, kommune, tested_rr) 

# list areas under half
df |> filter(tested_rr < 0.5) |>   
  select(year, landsdel,  kommune, tested_rr) |>
  arrange(landsdel, year) |> distinct(kommune) |> pull()

df |> filter(tested_rr < 0.33) |>   
  select(year, landsdel,  kommune, tested_rr) |>
  arrange(landsdel, year) |> print.all() |> distinct(kommune, landsdel) # |> pull() 

df |> filter(tested_rr < 0.25) |>   
  select(year, landsdel,  kommune, tested_rr) |>
  arrange(landsdel, year) |> distinct(kommune) # |> pull()

# 4/ tested_rr_exc 

fivenum(df$tested_rr_exc)

# top_5 values & areas 
df |>  slice_max(tested_rr_exc , n =  5) |> 
  select(year, landsdel, kommune, tested_rr_exc) 

# list areas over2 
df |> filter(tested_rr_exc >  0.95) |> 
  select(year, landsdel,  kommune, tested_rr_exc) |>
  arrange(landsdel, year)  |> distinct(kommune) |> pull(kommune)

# 5/ tested_rr_deficit 

fivenum(df$tested_rr_deficit)

# top_5 values & areas 
df |>  slice_max(tested_rr_deficit , n =  5) |> 
  select(year, landsdel, kommune, tested_rr_exc) 

# list areas over2
df |> filter(tested_rr_deficit >  0.95) |>                  
  select(year, landsdel,  kommune, tested_rr_deficit) |>
  arrange(landsdel, year)  |> distinct(kommune) |> pull(kommune)


