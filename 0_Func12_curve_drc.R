library(drc)

#https://artax.karlin.mff.cuni.cz/r-help/library/drc/html/00Index.html

Func_GrowthCurve_drc <- function(inv_year,Y.gaps){
            
            invTime_th <- which(rownames(Y.gaps) == as.character(inv_year))
            time <- 1:(length(Y.gaps) - invTime_th)
            post <- inv_year < as.numeric(row.names(Y.gaps))
            
            gap.post <- as.numeric(Y.gaps[post])
            
            data.post <- data.frame(time, gap.post)
            
            m1 <- drm(gap.post ~ time, data = data.post, fct = LL.2())
        
            modelSelect <- mselect(m1, list(AR.2(), LL.3(),LL.4(), LL.5(), 
                                            L.3(),
                                            EXD.2(), EXD.3(),
                                            w3(), w4(),
                                            gompertz()
                                            ))
            
            modelname <- row.names(modelSelect)[1]

            fitmodel0 <-  do.call("drm", list(as.formula("gap.post ~ time"), data = as.name("data.post"), fct = call(modelname))) 
            paraName <- fitmodel0$parNames[[2]]
            paraValue <- as.numeric(round(fitmodel0$parmMat,2))
            para <- paste0("(", paste(paraName, paraValue ,sep = "=",collapse = ", "), ")")
            pred.data <- plot(fitmodel0)
            
            return(list("modelname" = modelname, "fitmodel" =  pred.data, "para" = para,"fitmodel0" =  fitmodel0))
            
}


# Y.gaps <- readRDS("Y.gaps_test")
#  inv_year <-  2007
#  ress <- Func_GrowthCurve_drc(inv_year,Y.gaps)$fitmodel0
# 
#  Func_GrowthCurve_drc(inv_year,Y.gaps)$fitmodel
