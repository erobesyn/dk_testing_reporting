# 14a_inla_tested_STmodel_kommune

#### LIBRARIES ####
source("./1_load_libraries.R")

#### 0. ESTIMAND ####

# effect time and place on tested persons

#### 1. DATA  ####

#### 1a. READ DATA ####

# 1. data
d_kommune <- readRDS("./4 tables/kommune_model.rds") 
d_kommune <- d_kommune %>% select(-contains(c("pos", "sir", "epicase")))  
d_kommune 
levels(d_kommune$kommune) 

levels(d_kommune$year)    
d_kommune$year <- as.character(d_kommune$year) 
d_kommune$year <- as.factor(d_kommune$year) 
levels(d_kommune$year)

data.ST <- d_kommune

# 2. adj graph 

adj_kommune_qnb3  <- read_lines("./1 data/3 raw geodata/2 Adj for INLA/adj_kommune_qnb3")  
graph <- adj_kommune_qnb3  
graph  

#### 1b. PREPARE DATASETS ####

# id for areas
data.ST <- data.ST %>% arrange(year, kommune)  

data.ST$ID.struct    <- rep(1:99, 9)                           
data.ST <- data.ST %>% mutate(ID.unstruct = ID.struct)  

# id for year
levels(data.ST$year)
as.numeric(data.ST$year)
data.ST <- data.ST %>% mutate(t = as.numeric(year)-1)   
data.ST <- data.ST %>% mutate(t2 = t)

# id for ST interaction
data.ST$ID.area.time <- 1:891 

# y and E
data.ST <- data.ST %>% rename(y = O_tested, E = E_tested)

# datasets for different models
data <- data.ST %>% select(y, E, t, ID.struct, ID.unstruct, pop)                 
data2 <- data.ST %>% select(y, E, t, t2, ID.struct, ID.unstruct, pop)            
data3 <- data.ST %>% select(y, E, t, ID.struct, ID.area.time, pop)                
data4 <- data.ST %>% select(y, E, t, ID.struct, ID.unstruct, ID.area.time, pop)  

#### 2. MODELS  ####

# MODELS WITHOUT TIME  
formula1a =  y ~ 1 + f(ID.struct, model = "iid", graph=graph, param=c(0.5, 0.0005))
formula1b =  y ~ 1 + f(ID.struct, model = "besag", graph=graph, param=c(0.5, 0.0005)) 
formula1c =  y ~ 1 + f(ID.unstruct, model="bym", graph=graph, param=c(0.5, 0.0005))                    

# MODELS WITH TIME
# assume same geographical trend for all years and NON-LINEAR TIME effect   
formula2a =  y ~ 1 + f(t,model="rw1") + f(ID.struct, model = "iid", graph=graph, param = c(0.5, 0.0005))
formula2b =  y ~ 1 + f(t,model="ar1") + f(ID.struct, model = "iid", graph=graph, param = c(0.5, 0.0005))

formula2c =  y ~ 1 + f(t,model="rw1") + f(ID.struct, model = "besag", graph=graph, param = c(0.5, 0.0005))
formula2d =  y ~ 1 + f(t,model="ar1") + f(ID.struct, model = "besag", graph=graph, param = c(0.5, 0.0005))

formula2e =  y ~ 1 + f(t,model="rw1") + f(ID.struct, model = "bym", graph=graph, param = c(0.5, 0.0005))
formula2f =  y ~ 1 + f(t,model="ar1") + f(ID.struct, model = "bym", graph=graph, param = c(0.5, 0.0005))

# assume same geographical trend for all years and LINEAR TIME effect
formula3a =  y ~ t + f(ID.struct, model = "iid", graph = graph, param = c(0.5, 0.0005)) 
formula3b =  y ~ t + f(ID.struct, model = "besag", graph = graph, param = c(0.5, 0.0005)) 
formula3c =  y ~ t + f(ID.struct, model = "bym", graph = graph, param = c(0.5, 0.0005)) 

# TIME VARYING PER REGION
# LINEAR TIME trend, structured spatial trend, AND time trend VARYING BY REGION
formula4a =  y ~ 1 + t + f(ID.struct,model="iid", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, t, model="iid",param=c(0.5,0.0005)) 
formula4b =  y ~ 1 + t + f(ID.struct,model="iid", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, t, model="besag",param=c(0.5,0.0005),graph=graph) 
formula4c =  y ~ 1 + t + f(ID.struct,model="iid", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, t, model="bym",param=c(0.5,0.0005),graph=graph) 

formula4d =  y ~ 1 + t + f(ID.struct,model="besag", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, t, model="iid",param=c(0.5,0.0005)) 
formula4e =  y ~ 1 + t + f(ID.struct,model="besag", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, t, model="besag",param=c(0.5,0.0005),graph=graph) 
formula4f =  y ~ 1 + t + f(ID.struct,model="besag", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, t, model="bym",param=c(0.5,0.0005),graph=graph) 

formula4g =  y ~ 1 + t + f(ID.struct,model="bym", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, t, model="iid",param=c(0.5,0.0005)) 
formula4h =  y ~ 1 + t + f(ID.struct,model="bym", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, t, model="besag",param=c(0.5,0.0005),graph=graph) 
formula4i =  y ~ 1 + t + f(ID.struct,model="bym", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, t, model="bym",param=c(0.5,0.0005),graph=graph) 

# NON-LINEAR TIME trend, structured spatial trend, AND time trend VARYING BY REGION (but same across years, i.e. not interaction)
formula5a =  y ~ 1 + f(t, model="rw1") + f(ID.struct,model="iid", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, t2, model="bym",param=c(0.5,0.0005),graph=graph) 
formula5b =  y ~ 1 + f(t, model="ar1") + f(ID.struct,model="iid", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, t2, model="bym",param=c(0.5,0.0005),graph=graph) 

formula5c =  y ~ 1 + f(t, model="rw1") + f(ID.struct,model="bym", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, t2, model="iid",param=c(0.5,0.0005),graph=graph) 
formula5d =  y ~ 1 + f(t, model="ar1") + f(ID.struct,model="bym", graph=graph,param=c(0.5,0.0005)) + f(ID.unstruct, t2, model="iid",param=c(0.5,0.0005),graph=graph) 

# ST INTERACTION (i.e. time trend varies by region and across years)
# NON-LINEAR time trend, structured spatial trend, unstructured INTERACTION time-year
formula6 =  y ~ 1 + f(t, model="rw1") + f(ID.struct,model="bym", graph=graph,param=c(0.5,0.0005)) + f(ID.area.time, model="iid",param=c(0.5,0.0005)) 
formula6b =  y ~ 1 + f(t, model="rw1") + f(ID.struct,model="besag", graph=graph,param=c(0.5,0.0005)) + 
  f(ID.unstruct, model="iid", param=c(0.5, 0.0005)) + f(ID.area.time, model="iid",param=c(0.5,0.0005))  # same model as 6 but split bym in two components

# fit model
fit1a <- inla(formula1a, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
             control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 
fit1b <- inla(formula1b, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
             control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 
fit1c <- inla(formula1c, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
             control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 

fit2a <- inla(formula2a, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 
fit2b <- inla(formula2b, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 
fit2c <- inla(formula2c, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 
fit2d <- inla(formula2d, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 
fit2e <- inla(formula2e, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 
fit2f <- inla(formula2f, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 


fit3a <- inla(formula3a, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 
fit3b <- inla(formula3b, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 
fit3c <- inla(formula3c, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
             control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 


fit4a <- inla(formula4a, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 
fit4b <- inla(formula4b, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 
fit4c <- inla(formula4c, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 


fit4d <- inla(formula4d, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 
fit4e <- inla(formula4e, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 
fit4f <- inla(formula4f, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 


fit4g <- inla(formula4g, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 
fit4h <- inla(formula4h, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 
fit4i <- inla(formula4i, family="poisson", E=E, data=data, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 

# data2
fit5a <- inla(formula5a, family="poisson", E=E, data=data2, control.predictor=list(compute=TRUE), 
             control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 
fit5b <- inla(formula5b, family="poisson", E=E, data=data2, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 
fit5c <- inla(formula5c, family="poisson", E=E, data=data2, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 
fit5d <- inla(formula5d, family="poisson", E=E, data=data2, control.predictor=list(compute=TRUE), 
              control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 

# data3
fit6 <- inla(formula6, family="poisson", E=E, data=data3, control.predictor=list(compute=TRUE), 
             control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE)) 

# data4
fit6b <- inla(formula6b, family="poisson", E=E, data=data4, control.predictor=list(compute=TRUE), 
             control.family=list(link="log"),control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE,
                                                                  return.marginals.predictor=TRUE)) 

# comparison
tab.modsel <- data.frame(
  Model = c("fit1a" , "fit1b", "fit1c",
            "fit2a" , "fit2b", "fit2c",
            "fit2d" , "fit2e", "fit2f",
            "fit3a" , "fit3b", "fit3c",
            "fit4a" , "fit4b", "fit4c",
            "fit4d" , "fit4e", "fit4f",
            "fit4g" , "fit4h", "fit4i",
            "fit5a" , "fit5b",
            "fit5c" , "fit5d",
            "fit6"),
  WAIC  = c(fit1a$waic$waic, fit1b$waic$waic, fit1c$waic$waic,
            fit2a$waic$waic, fit2b$waic$waic, fit2c$waic$waic,
            fit2d$waic$waic, fit2e$waic$waic, fit2f$waic$waic,
            fit3a$waic$waic, fit3b$waic$waic, fit3c$waic$waic,
            fit4a$waic$waic, fit4b$waic$waic, fit4c$waic$waic,
            fit4d$waic$waic, fit4e$waic$waic, fit4f$waic$waic,
            fit4g$waic$waic, fit4h$waic$waic, fit4i$waic$waic,
            fit5a$waic$waic, fit5b$waic$waic,
            fit5c$waic$waic, fit5d$waic$waic,
            fit6$waic$waic),
  p.eff = c(fit1a$waic$p.eff, fit1b$waic$p.eff, fit1c$waic$p.eff,
            fit2a$waic$p.eff, fit2b$waic$p.eff, fit2c$waic$p.eff,
            fit2d$waic$p.eff, fit2e$waic$p.eff, fit2f$waic$p.eff,
            fit3a$waic$p.eff, fit3b$waic$p.eff, fit3c$waic$p.eff,
            fit4a$waic$p.eff, fit4b$waic$p.eff, fit4c$waic$p.eff,
            fit4d$waic$p.eff, fit4e$waic$p.eff, fit4f$waic$p.eff,
            fit4g$waic$p.eff, fit4h$waic$p.eff, fit4i$waic$p.eff,
            fit5a$waic$p.eff, fit5b$waic$p.eff,
            fit5c$waic$p.eff, fit5d$waic$p.eff,
            fit6$waic$p.eff))
tab.modsel <- as_tibble(tab.modsel) 

tab.modsel <- tab.modsel |> mutate(short_formula = case_when(
  Model == "fit1a" ~ "y ~ 1 + f(s, iid)",
  Model == "fit1b" ~ "y ~ 1 + f(s, besag)",
  Model == "fit1c" ~ "y ~ 1 + f(s, bym)",
  Model == "fit2a" ~ "y ~ 1 + f(t, rw1) + f(s, iid)",
  Model == "fit2b" ~ "y ~ 1 + f(t, ar1) + f(s, iid)",
  Model == "fit2c" ~ "y ~ 1 + f(t, rw1) + f(s, besag)",
  Model == "fit2d" ~ "y ~ 1 + f(t, ar1) + f(s, besag)",
  Model == "fit2e" ~ "y ~ 1 + f(t, rw1) + f(s, bym)",
  Model == "fit2f" ~ "y ~ 1 + f(t, ar1) + f(s, bym)",
  Model == "fit3a" ~ "y ~ 1 + t + f(s, iid)",
  Model == "fit3b" ~ "y ~ 1 + t + f(s, besag)",
  Model == "fit3c" ~ "y ~ 1 + t + f(s, bym)",
  Model == "fit4a" ~ "y ~ 1 + t + f(s, iid) + f(s, t, iid)",
  Model == "fit4b" ~ "y ~ 1 + t + f(s, iid) + f(s, t, besag)",
  Model == "fit4c" ~ "y ~ 1 + t + f(s, iid) + f(s, t, bym)",
  Model == "fit4d" ~ "y ~ 1 + t + f(s, besag) + f(s, t, iid)",
  Model == "fit4e" ~ "y ~ 1 + t + f(s, besag) + f(s, t, besag)",
  Model == "fit4f" ~ "y ~ 1 + t + f(s, besag) + f(s, t, bym)",
  Model == "fit4g" ~ "y ~ 1 + t + f(s, bym) + f(s, t, iid)",
  Model == "fit4h" ~ "y ~ 1 + t + f(s, bym) + f(s, t, besag)",
  Model == "fit4i" ~ "y ~ 1 + t + f(s, bym) + f(s, t, bym)",
  Model == "fit5a" ~ "y ~ 1 + f(t, rw1) + f(s, iid) + f(s, t, bym)", 
  Model == "fit5b" ~ "y ~ 1 + f(t, ar1) + f(s, iid) + f(s, t, bym)",
  Model == "fit5c" ~ "y ~ 1 + f(t, rw1) + f(s, bym) + f(s, t, iid)",
  Model == "fit5d" ~ "y ~ 1 + f(t, ar1) + f(s, bym) + f(s, t, iid)",
  Model == "fit6" ~ "y ~ 1 + f(t, rw1) + f(s, bym) + f(st, iid)"
))  

tab.modsel |> arrange(-WAIC) |> print.all()    

tab.modsel <- tab.modsel |> 
  select(Model_original = Model, Formula = short_formula, WAIC, p.eff) |> 
  arrange(-WAIC) |> 
  mutate(Model = (rev(row_number()))) |>
  mutate(Model = str_c("A_", Model)) |>
  select(Model, everything())             
tab.modsel 

t_models_testing <- tab.modsel |> select(- Model_original)   
t_models_testing |> print.all() 
write_csv(t_models_testing, "./4 tables/t_models_testing.csv")
knitr::kable(t_models_testing, caption = 'All models testing intensity') 

## best model
summary(fit6)   
summary(fit6b) 

