# 14c_inla_epicase_STmodel_kommune_with_test-intensity_as_covariate

#### LIBRARIES ####
source("./1_load_libraries.R")

#### 0. ESTIMAND ####

# DIRECT EFFECT of time and place on epicase, controlling for testing to block indirect effect through testing variation

#### 1. DATA  ####

#### 1a. READ DATA ####

# 1. data
d_kommune <- readRDS("../4 tables/kommune_model.rds") 
d_kommune <- d_kommune %>% select(-contains(c("E_tested", "pos", "sir")))  
d_kommune   
levels(d_kommune$kommune) 

levels(d_kommune$year)
d_kommune$year <- as.character(d_kommune$year) 
d_kommune$year <- as.factor(d_kommune$year) 
levels(d_kommune$year)

data.ST <- d_kommune

# 2 adj
adj_kommune_qnb3  <- read_lines("./1 data/3 raw geodata/2 Adj for INLA/adj_kommune_qnb3")  
graph <- adj_kommune_qnb3
graph  

#### 1b. PREPARE DATASETS ####

# id for areas
data.ST <- data.ST %>% arrange(year, kommune)  
data.ST$ID.struct    <- rep(1:99, 9)                           
data.ST$ID.unstruct  <- rep(1:99, 9)      

# id for year
levels(data.ST$year)
as.numeric(data.ST$year)
data.ST <- data.ST %>% mutate(t = as.numeric(year)-1)   
data.ST <- data.ST %>% mutate(t2 = t)

# id for ST interaction
data.ST$ID.area.time <- 1:891 

# y and E
data.ST <- data.ST %>% mutate(T = O_tested/pop*100000) 
data.ST <- data.ST %>% rename(y = O_epicase, E = E_epicase, T = T)  

# datasets for different models
data <- data.ST %>% select(y, E, T, t, ID.struct, ID.unstruct, pop) 
data2 <- data.ST %>% select(y, E, T, t, t2, ID.struct, ID.unstruct, pop)  
data3 <- data.ST %>% select(y, E, T, t, ID.struct, ID.area.time, pop)  
data4 <- data.ST %>% select(y, E, T, t, ID.struct, ID.unstruct, ID.area.time, pop)  

# datasets different binning 
data4$T             # T is testing, not time
data4$T.grp <- inla.group(data4$T, n = 20, method = "quantile")
data4$T.grp2 <- inla.group(data4$T, n = 20, method = "cut")

summary(data4$T)
summary(data4$T.grp)   # quantile bins 
summary(data4$T.grp2)  # equally spaced bins 

ggplot(data4) +
  geom_point(aes(x = T, y = y)) + 
  geom_point(aes(x = T.grp, y = y), col = "red") + 
  geom_point(aes(x = T.grp2, y = y), col = "blue") + 
  theme_bw() +
  ggtitle("black observed, red 'quantile' binned and blue equally spaced 'cut' data \nTesting and reported cases")  

#### 2. MODELS  ####

# MODELS WITHOUT TIME  
formula1 =  y ~ 1 + f(ID.unstruct, model="iid", param=c(0.5, 0.0005))                    
formula2 =  y ~ 1 + f(ID.struct, model = "besag", graph=graph, param=c(0.5, 0.0005)) 
formula3 =  y ~ 1 + f(ID.struct, model = "besag", graph=graph, param=c(0.5, 0.0005)) + f(ID.unstruct, model = "iid", param = c(0.5, 0.0005)) 

# MODELS WITH TIME
# assume same geographical trend for all years and linear time effect
formula4 =  y ~ 1 + t + f(ID.struct, model = "besag", graph = graph, param = c(0.5, 0.0005)) + f(ID.unstruct, model = "iid", param = c(0.5, 0.0005)) 

# assume same geographical trend for all years and non-linear time effect   (note: t acts same as id for areas, both are to make varying effects)
formula5 =  y ~ 1 + f(t,model="rw1") + f(ID.struct, model = "besag", graph=graph, param = c(0.5, 0.0005)) + f(ID.unstruct,model = "iid",param = c(0.5, 0.0005)) 
formula6 =  y ~ 1 + f(t,model="ar1") + f(ID.struct, model = "besag", graph=graph, param = c(0.5, 0.0005)) + f(ID.unstruct,model = "iid",param = c(0.5, 0.0005)) 

# linear time trend, structured spatial trend, time trend varying per region
formula7 =  y ~ 1 + t + f(ID.struct,model="bym", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, t, model="iid",param=c(0.5,0.0005)) 
formula8 =  y ~ 1 + t + f(ID.struct,model="bym", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, t, model="besag",param=c(0.5,0.0005),graph=graph) 
formula9 =  y ~ 1 + t + f(ID.struct,model="bym", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, t, model="bym",param=c(0.5,0.0005),graph=graph) 

# niet-lineare time trend, structured spatial trend, time trend varying per region
formula10 =  y ~ 1 + f(t2, model="rw1") + f(ID.struct,model="bym", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, t, model="iid",param=c(0.5,0.0005)) 

# niet-lineare time trend, structured spatial trend, unstructured interaction time-year
formula11 =  y ~ 1 + f(t, model="rw1") + 
  f(ID.struct,model="bym", graph=graph,param=c(0.5,0.0005)) + 
  f(ID.area.time, model="iid",param=c(0.5,0.0005)) 

formula13 =  y ~ 1 + T + f(t, model="rw1") + 
  f(ID.struct,model="bym", graph=graph,param=c(0.5,0.0005)) + 
  f(ID.area.time, model="iid",param=c(0.5,0.0005)) 

# inla.group() puts constraint on nr of knots (compared to method ns())
formula15 =  y ~ 1 + f(inla.group(T), model="rw1")  + f(t, model="rw1") +            
  f(ID.struct,model="besag", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, model="iid", param=c(0.5, 0.0005)) + 
  f(ID.area.time, model="iid",param=c(0.5,0.0005)) 

formula16 =  y ~ 1 + f(inla.group(T), model="rw2")  + f(t, model="rw1") + 
  f(ID.struct,model="besag", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, model="iid", param=c(0.5, 0.0005)) + 
  f(ID.area.time, model="iid",param=c(0.5,0.0005)) 

formula17 =  y ~ 1 + f(inla.group(T), model="ar1")  + f(t, model="rw1") + 
  f(ID.struct,model="besag", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, model="iid", param=c(0.5, 0.0005)) + 
  f(ID.area.time, model="iid",param=c(0.5,0.0005)) 

# T.grp
formula18 =  y ~ 1 + f(inla.group(T.grp), model="rw1")  +       
  f(t, model="rw1") +    
  f(ID.struct,model="besag", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, model="iid", param=c(0.5, 0.0005)) + 
  f(ID.area.time, model="iid",param=c(0.5,0.0005)) 

formula18b =  y ~ 1 + f(inla.group(T.grp), model="ar1")  +       
  f(t, model="ar1") +  
  f(ID.struct,model="besag", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, model="iid", param=c(0.5, 0.0005)) + 
  f(ID.area.time, model="iid",param=c(0.5,0.0005)) 

formula18c =  y ~ 1 +  f(inla.group(T.grp), model="rw1")  +       
  f(t, model="ar1") +    
  f(ID.struct, model="besag", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, model="iid", param=c(0.5, 0.0005)) + 
  f(ID.area.time, model="iid",param=c(0.5,0.0005)) 

formula18d =  y ~ 1 + f(inla.group(T.grp), model="ar1")  +       
  f(t, model="rw1") +  
  f(ID.struct,model="besag", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, model="iid", param=c(0.5, 0.0005)) + 
  f(ID.area.time, model="iid",param=c(0.5,0.0005)) 

# T.grp2 
formula19 =  y ~ 1 + f(inla.group(T.grp2), model="ar1")  +       
  f(t, model="ar1") +    
  f(ID.struct,model="besag", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, model="iid", param=c(0.5, 0.0005)) + 
  f(ID.area.time, model="iid",param=c(0.5,0.0005)) 

formula20 =  y ~ 1 + f(inla.group(T.grp2), model="ar1")  +       
  f(t, model="rw1") +   
  f(ID.struct,model="besag", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, model="iid", param=c(0.5, 0.0005)) + 
  f(ID.area.time, model="iid",param=c(0.5,0.0005)) 

formula21 =  y ~ 1 + f(inla.group(T.grp2), model="rw1")  +       
  f(t, model="ar1") +   
  f(ID.struct,model="besag", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, model="iid", param=c(0.5, 0.0005)) + 
  f(ID.area.time, model="iid",param=c(0.5,0.0005))

formula22 =  y ~ 1 + f(inla.group(T.grp2), model="rw1")  +        
  f(t, model="rw1") +   
  f(ID.struct,model="besag", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, model="iid", param=c(0.5, 0.0005)) + 
  f(ID.area.time, model="iid",param=c(0.5,0.0005))

# model similar to best, but without testing -> to compare waic, p.eff and show point.range
formula18c_without_testing =  y ~ 1 +       
  f(t, model="ar1") +   
  f(ID.struct, model="besag", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, model="iid", param=c(0.5, 0.0005)) + 
  f(ID.area.time, model="iid",param=c(0.5,0.0005)) 


# fit model
fit1 <- inla(formula1, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
             control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 
fit2 <- inla(formula2, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
             control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 
fit3 <- inla(formula3, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
             control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 

fit4 <- inla(formula4, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
             control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 
fit5 <- inla(formula5, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
             control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 
fit6 <- inla(formula6, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
             control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 

fit7 <- inla(formula7, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
             control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 
fit8 <- inla(formula8, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
             control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 
fit9 <- inla(formula9, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
             control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 

# data2
fit10 <- inla(formula10, family="poisson", E=E, data=data2, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE))   

# data3
fit11 <- inla(formula11, family="poisson", E=E, data=data3, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE,
                                                                   return.marginals.predictor=TRUE))   


fit13 <- inla(formula13, family="poisson", E=E, data=data3, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE,
                                                                   return.marginals.predictor=TRUE))   

# data5 (for T as random effect, I need extra ID)
fit15 <- inla(formula15, family="poisson", E=E, data=data4, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE,
                                                                   return.marginals.predictor=TRUE))   

fit16 <- inla(formula16, family="poisson", E=E, data=data4, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE,
                                                                   return.marginals.predictor=TRUE))   

fit17 <- inla(formula17, family="poisson", E=E, data=data4, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE,
                                                                   return.marginals.predictor=TRUE))   

fit18 <- inla(formula18, family="poisson", E=E, data=data4, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE,
                                                                   return.marginals.predictor=TRUE))   

fit18b <- inla(formula18b, family="poisson", E=E, data=data4, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE,
                                                                   return.marginals.predictor=TRUE)) 
fit18c <- inla(formula18c, family="poisson", E=E, data=data4, control.predictor=list(compute=TRUE), 
               control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE,
                                                                    return.marginals.predictor=TRUE)) 
fit18d <- inla(formula18d, family="poisson", E=E, data=data4, control.predictor=list(compute=TRUE), 
               control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE,
                                                                    return.marginals.predictor=TRUE)) 

fit19 <- inla(formula19, family="poisson", E=E, data=data4, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE,
                                                                   return.marginals.predictor=TRUE))

fit20 <- inla(formula20, family="poisson", E=E, data=data4, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE,
                                                                   return.marginals.predictor=TRUE))   

fit21 <- inla(formula21, family="poisson", E=E, data=data4, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE,
                                                                   return.marginals.predictor=TRUE)) 

fit22 <- inla(formula22, family="poisson", E=E, data=data4, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE,
                                                                   return.marginals.predictor=TRUE)) 

fit18c_without_testing <- inla(formula18c_without_testing, family="poisson", E=E, data=data4, control.predictor=list(compute=TRUE), 
                                    control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE,
                                                                                         return.marginals.predictor=TRUE)) 
                               
# resultaten van goodness-of-fit vergelijkingen:

## best model
summary(fit18c) 

# comparison with best model omitting testing
summary(fit18c_without_testing) 
                                
##################################################
# INTERMEZZO COMPARISON BEST MODEL WITH TESTING AND SAME WITHOUT TESTING
tab.modsel <- data.frame(
  Model = c("fit18c", "fit18c_without_testing"),
  WAIC  = c(fit18c$waic$waic,  fit18c_without_testing$waic$waic),
  p.eff = c(fit18c$waic$p.eff, fit18c_without_testing$waic$p.eff), 
  DIC   = c(fit18c$dic$dic,    fit18c_without_testing$dic$dic),
  logCPO = c(-mean(log(fit18c$cpo$cpo)),-mean(log(fit18c_without_testing$cpo$cpo))))

tab.modsel <- as_tibble(tab.modsel) %>% arrange(-DIC)
tab.modsel %>% arrange(-WAIC) # better in-sample fit with testing as covariate

#################################################
tab.modsel <- data.frame(                       
  Model = c("fit1" , "fit2", "fit3",
            "fit4" , "fit5", "fit6",
            "fit7" , "fit8", "fit9",
            "fit10" , "fit11", 
            "fit13" , 
            "fit15",
            "fit16" , "fit17", "fit18",
            "fit18b" , "fit18c", "fit18d",
            "fit19" , "fit20", "fit21",
            "fit22"),
  
  WAIC  = c(fit1$waic$waic,  fit2$waic$waic,  fit3$waic$waic,
            fit4$waic$waic,  fit5$waic$waic,  fit6$waic$waic,
            fit7$waic$waic,  fit8$waic$waic,  fit9$waic$waic,
            fit10$waic$waic,  fit11$waic$waic,  
            fit13$waic$waic,  
            fit15$waic$waic,
            fit16$waic$waic,  fit17$waic$waic,  fit18$waic$waic,
            fit18b$waic$waic,  fit18c$waic$waic,  fit18d$waic$waic,
            fit19$waic$waic,  fit20$waic$waic,  fit21$waic$waic,
            fit22$waic$waic),
  
  p.eff = c(fit1$waic$p.eff, fit2$waic$p.eff, fit3$waic$p.eff,
            fit4$waic$p.eff, fit5$waic$p.eff, fit6$waic$p.eff,
            fit7$waic$p.eff, fit8$waic$p.eff, fit9$waic$p.eff,
            fit10$waic$p.eff, fit11$waic$p.eff, 
            fit13$waic$p.eff, 
            fit15$waic$p.eff,
            fit16$waic$p.eff, fit17$waic$p.eff, fit18$waic$p.eff,
            fit18b$waic$p.eff, fit18c$waic$p.eff, fit18d$waic$p.eff,
            fit19$waic$p.eff, fit20$waic$p.eff, fit21$waic$p.eff,
            fit22$waic$p.eff))
tab.modsel  

tab.modsel <- as_tibble(tab.modsel) 
tab.modsel |> arrange(-WAIC) |> print.all()  

tab.modsel <- tab.modsel |> mutate(short_formula = case_when(
  Model == "fit1" ~ "y ~ 1 + f(s, iid)",                                                                                                                                                                                                   
  Model == "fit2" ~ "y ~ 1 + f(s, besag)",                                                                                                                                                                                    
  Model == "fit3" ~ "y ~ 1 + f(s, bym)",                                                                                                                           
  Model == "fit4" ~ "y ~ 1 + t + f(s, bym)",                                                                                                                           
  Model == "fit5" ~ "y ~ 1 + f(t, rw1) + f(s, bym)",                                                                                                   
  Model == "fit6" ~ "y ~ 1 + f(t, ar1) + f(s, bym)",                                                                                                   
  Model == "fit7" ~ "y ~ 1 + t + f(s, bym) + f(s, t, iid)",                                                                                                                      
  Model == "fit8" ~ "y ~ 1 + t + f(s, bym) + f(s, t, besag)",                                                                                                     
  Model == "fit9" ~ "y ~ 1 + t + f(s, bym) + f(s, t, bym)",                                                                                                       
  Model == "fit10" ~ "y ~ 1 + f(t, rw1) + f(s, bym) + f(s, t, iid)",
  Model == "fit11" ~ "y ~ 1 + f(t, rw1) + f(s, bym) + f(st, iid)",                                                                                                    
  Model == "fit13" ~ "y ~ 1 + T + f(t, rw1) + f(s, bym) + f(st, iid)",                                                                                                
  Model == "fit15" ~ "y ~ 1 + f(inla.group(T), rw1) + f(t, rw1) + f(s, bym) + f(st, iid)",     
  Model == "fit16" ~ "y ~ 1 + f(inla.group(T), rw2) + f(t, rw1) + f(s, bym) + f(st, iid)",   
  Model == "fit17" ~ "y ~ 1 + f(inla.group(T), ar1) + f(t, rw1) + f(s, bym) + f(st, iid)",
  Model == "fit18" ~ "y ~ 1 + f(inla.group(T.grp), rw1) + f(t, rw1) + f(s, bym) + f(st, iid)", 
  Model == "fit18b" ~ "y ~ 1 + f(inla.group(T.grp), ar1) + f(t, ar1) + f(s, bym) + f(st, iid)", 
  Model == "fit18c" ~ "y ~ 1 + f(inla.group(T.grp), rw1) + f(t, ar1) + f(s, bym) + f(st, iid)", 
  Model == "fit18d" ~ "y ~ 1 + f(inla.group(T.grp2), ar1) + f(t, ar1) + f(s, bym) + f(st, iid)",
  Model == "fit19" ~ "y ~ 1 + f(inla.group(T.grp2), ar1) + f(t, ar1) + f(s, bym) + f(st, iid)",
  Model == "fit20" ~ "y ~ 1 + f(inla.group(T.grp2), ar1) + f(t, rw1) + f(s, bym) + f(st, iid)",
  Model == "fit21" ~ "y ~ 1 + f(inla.group(T.grp2), rw1) + f(t, ar1) + f(s, bym) + f(st, iid)",
  Model == "fit22" ~ "y ~ 1 + f(inla.group(T.grp2), rw1) + f(t, rw1) + f(s, bym) + f(st, iid)"
))

tab.modsel <- tab.modsel |> 
  select(Model_original = Model, Formula = short_formula, WAIC, p.eff) |> 
  arrange(-WAIC) |> 
  mutate(Model = (rev(row_number()))) |>
  mutate(Model = str_c("B_", Model)) |>
  select(Model, everything())           

t_models_cases <- tab.modsel |> select(- Model_original)   
t_models_cases |> print.all() 
write_csv(t_models_cases, "../4 tables/t_models_cases.csv")
knitr::kable(t_models_cases, caption = 'All models case reporting') 






