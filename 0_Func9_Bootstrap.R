


Func_Bootstrap <- function( RepTimes = 100,
         data, control_ID, 
         predictors, dependent, inv_year,
         treat_ID, match_from, match_to,
         yearVarName, IDVarName, nameVarName,
         special.predictors){
            
            
            gap_Value <- year_var <- c()
            
            for(i in 1:RepTimes){  
                        
                        control_ID_Bootstrap <- unique(sample(control_ID, length(control_ID), replace = T))
                        
                        tryInfo <- try(  
                                    synthResult <-  cmp_Func_Synth(synth_data = data , predictors = predictors,
                                                                   dependent = dependent , inv_year = inv_year,
                                                                   treat_ID = treat_ID, control_ID = control_ID_Bootstrap ,
                                                                   match_from = match_from, match_to = match_to,
                                                                   
                                                                   yearVarName = yearVarName , 
                                                                   IDVarName = IDVarName , 
                                                                   nameVarName = nameVarName,
                                                                   special.predictors = special.predictors
                                    )
                                    , silent = T) %>% class
                        
                        if ( tryInfo == "try-error") {
                                    next
                        } else {  
                                    
                                    
                                    gaps_i <- synthResult$gaps
                                    gap_Value <- c(gap_Value, gaps_i)
                                    
                                    year_i <- round(as.numeric(row.names(synthResult$Y_treat)),0)
                                    year_var <- c(year_var, year_i)
                        }
                        
            }
            
            n_row <- length(year_var)
            year_length <- length(unique(year_var))
            n_time <- n_row/year_length
            unitID <- rep(1:n_time, each = year_length)
            unitName <- as.character(unitID)
            
            
            dataOutput <- data.frame("Year" = year_var ,
                             "unitID" = unitID , 
                             "unitName" = unitName, 
                             "gapsValue" = gap_Value)
            
            return(dataOutput)
            
}

cmp_Func_Bootstrap <- cmpfun(Func_Bootstrap)

# 
# 
# 
# ### test
# # 
#  data <- read.csv("./data/data_SwedishCity.csv",comment.char = "#")
# # 
# resmy <- cmp_Func_Bootstrap(RepTimes = 3,
#             data = data , predictors = "Employee" ,dependent = "Productivity" ,
#                      treat_ID = 1780, control_ID = c(2510, 2513 ,2514 ,2518 ,2521) , match_from = 2002, match_to = 2005,
#                      yearVarName = "Year", IDVarName = "Kommun_code", nameVarName = "Kommun_name",inv_year = 2006,
#                      
#                      special.predictors = 
#                                  list(
#                                              list("Productivity", c(2001,2004), "mean"),
#                                              #list("Productivity", 2003, "mean"),
#                                              list("Employee", 2004, "mean")
#                                  )
#  )
# 


# ggplot(resmy, aes(x = Year, y = gapsValue, colour = as.factor(unitID),
#                   group = as.factor(unitID) ) ) + 
#             geom_line()

# 
