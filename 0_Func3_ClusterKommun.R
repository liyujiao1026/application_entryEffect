
library(kohonen)
# year 2000-2005

Func_Cluster_Pool <- function(data, treat_ID, comparable_Year, comparable_Var, 
                              cluster_count, yearVarName, IDVarName, unitVarName, remove_cityName, transformation = "raw") {
            
            if (missing(remove_cityName)){
                        remove_cityName = NULL
            }
            
            
            Kommun_code_th <- which(colnames(data) == as.character(IDVarName))
            
            year_th <- which(colnames(data) == as.character(yearVarName))
            
            name_th <- which(colnames(data) == as.character(unitVarName))
            
            #removeID_th <- which( as.character(data[,name_th]) %in% remove_cityName)
            removeID_th <- sapply( remove_cityName, function(x){ which(as.character(data[,name_th]) == x) })
            
            
            removeID <- unique( data[removeID_th,Kommun_code_th])
            
            if (cluster_count != 1) {
                        
                        data.i <- data[ data[,year_th] %in% comparable_Year,]
                        
                        
                        var_th <- c(Kommun_code_th,name_th,year_th, which(colnames(data.i) %in% comparable_Var))
                        
                        data.SOM0 <- data.i[, var_th]
                        data.SOM1 <- data.SOM0[order(data.SOM0[,Kommun_code_th]),]
                        data.SOM2 <- data.SOM1[,c(IDVarName, unitVarName)] %>% unique
                        
                        coll <- which(colnames(data.SOM1) %in% comparable_Var)
                        
                        for (i in  coll) {
                                    new <- by(data.SOM1[,i], data.SOM1[,IDVarName], mean) %>% as.numeric()
                                    data.SOM2 <- cbind(data.SOM2, new)
                                    colnames(data.SOM2)[colnames(data.SOM2) == "new"] <- colnames(data.SOM1)[i]
                        }
                        data.SOM <- data.SOM2
                        #rm(list = c("data.SOM0", "data.SOM1", "data.SOM2", "i", "new"))
                        
                        
                        cluster_data <- as.matrix(data.SOM[,colnames(data.SOM) %in% comparable_Var, drop = FALSE])
                        
                        
                        if (transformation == "scale"){
                                    std_data <- round(scale(cluster_data),3)
                        }else if (transformation == "log"){
                                    std_data <- round(log(cluster_data),3)
                        }else if( transformation == "raw"){
                                    std_data <- cluster_data
                        }
                                  
                        somnet <- som(std_data ,
                                      grid = somgrid(cluster_count, 1, "rectangular"))
                        
                        plot(somnet)
                        data.SOM$cluster <- map(somnet)$unit.classif 
                        
                        treat.cluster <- data.SOM[data.SOM[,IDVarName] == treat_ID,]$cluster
                        
                        clustered_UnitId <- subset(data.SOM, cluster == treat.cluster)[, IDVarName]
                        
                        
                        SC_pool <- setdiff( clustered_UnitId , c(removeID,treat_ID )  )
                        
                        
            } else{
                        SC_pool <- setdiff( unique(data[,Kommun_code_th]), c(removeID,treat_ID ) )
            }
            
            comparableUnits_count <- length(SC_pool)
            
            
            colnames(std_data) <- paste0(colnames(std_data), ".", transformation)
            data_som <- data.frame(data.SOM, std_data)
            
            
            
            return(list("SC_pool" = SC_pool, "comparableUnits_count" = comparableUnits_count, "data.som" =  data_som))
}


library(compiler)
cmp_Func_Cluster_Pool <- cmpfun(Func_Cluster_Pool)




##====================================================================================#
# 
# data <- read.csv("./data/data_IKEA_1111.csv",comment.char = "#")
# comparable_Year= c(2003,2004,2005)
# cluster_count = 7
# comparable_Var <- c("Productivity" , "Population", "Percent_University","EmployeeIndex", "SalesIndex")
# treat_ID = 2583
# yearVarName <-  "Year"
# transformation = "log"
# IDVarName = "Kommun_code"
# unitVarName = "Kommun_name"
# remove_cityName = c("Kalmar","Karlstad")
# resg2 <- cmp_Func_Cluster_Pool(data, treat_ID, comparable_Year, comparable_Var,transformation = transformation ,
#                               cluster_count, yearVarName, IDVarName, unitVarName, remove_cityName)
# resg2$SC_pool
# resg2$data.som[1,]
