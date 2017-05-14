library(grofit)




Func_GrowthCurve <- function(inv_year,Y.gaps){
            
            invTime_th <- which(rownames(Y.gaps) == as.character(inv_year))
            time <- matrix( 1:(length(Y.gaps) - invTime_th), nrow = 1)
           
            post <- inv_year < as.numeric(row.names(Y.gaps))
            
            gap.post <- as.numeric(Y.gaps[post])
            data.post <- data.frame(matrix(c(1,1,1,gap.post), nrow = 1))
           
            MyOpt <- grofit.control(smooth.gc = 0.5, interactive = FALSE)
            TEST <- grofit(time = time, data = data.post, TRUE, MyOpt)
            
            
            if (!is.null(TEST$fit.data)){
                        paraResult <- TEST$gcFit$gcFittedModels[[1]]
            } else {
                        paraResult <- NULL            
            }
            
            
            nonParaResult <- TEST$gcFit$gcFittedSplines[[1]]
            
            return(list("paraResult" = paraResult, "nonParaResult" = nonParaResult))
            
}


#res <- Func_GrowthCurve(inv_year,Y.gaps)
