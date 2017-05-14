# server.R
source('./0_source.R', echo = F)


shinyServer(function(input, output) {
            
            # 1. loading the selected data file =================================================#
            filedata <- reactive({
                        infile <- input$datafile
                        if (is.null(infile)) {
                                    
                                    read.csv("./data/data_IKEA_1111.csv")
                        }else{
                                    read.csv(infile$datapath)     
                                    
                        }
                        
                        
            })
            
            
            output$defaultData <- renderText({
                        if (is.null(input$datafile)) {
                                    "Default dataset is using"
                        }else{NULL}
            })
            
            
            
            
            # 2. Set default valude for IKEA cases. =================================================#
            #. default value in slider bar for IKEA data (
            selected_ID <- reactive({
                        if (is.null(input$datafile)) {
                                    "Kommun_code"
                        }else{NULL}
                        
            })
            
            
            output$varUnitID <- renderUI({
                        selectInput(
                                    inputId = "var_ID",label = "Variable of units' ID:",selected = selected_ID(),
                                    choices = colnames(filedata())) 
                        
            })
            
            
            selected_var <- reactive({
                        if (is.null(input$datafile)) {
                                    "Kommun_name"
                        }else{NULL}
                        
            })
            
            output$varUnitName <- renderUI({
                        selectInput(
                                    inputId = "var_name",label = "Variable of units' name:",selected = selected_var(),
                                    choices = colnames(filedata())) 
                        
            })
            
            
            
            selected_year <- reactive({
                        if (is.null(input$datafile)) {
                                    "Year"
                        }else{NULL}
            })
            
            output$varYear <- renderUI({
                        selectInput(
                                    inputId = "var_time",label = "Variable of time:",selected = selected_year(),
                                    choices = colnames(filedata())) 
                        
            })
            
            # default value in slider bar for IKEA data 
            
            
            
            # default value for IKEA case (
            Unit_name <- reactive({
                        data <- filedata()
                        n <- which(colnames(data) == as.character(input$var_name))
                        unitNames <- unique(as.character(data[,n]))
                        names(unitNames) <- unitNames
                        unitNames 
                        
            })
            
            Unit_year <- reactive({
                        data <- filedata()
                        m <- which(colnames(data) == as.character(input$var_time))
                        unique(data[,m])
            })
            
            min_year <- reactive({
                        if (is.null(input$datafile)) {2001} else{
                                    min(as.numeric(Unit_year()))        
                        }
                                    
                                    
                        
            })
            
            max_year <- reactive({
                        if (is.null(input$datafile)) {2010} else{
                                    max(as.numeric(Unit_year()))        
                        }
            })
            
            median_year <- reactive({
                        if (is.null(input$datafile)) {
                                    2006
                                    
                        }else{
                                    max(as.numeric(Unit_year()))
                        }
            })
            
            
            choiceVar <- reactive({
                        data <- filedata()
                        colnames(data) 
            })
            
            
            treat_ID <- reactive({ 
                        data <- filedata()
                        m0 <- which( colnames(data) == as.character(input$var_name)) # column of name
                        m1 <- which(as.character(data[,m0]) == input$city_name)[1]
                        n0 <- which( colnames(data) == as.character(input$var_ID)) # column of id
                        
                        data[m1,n0]
            })
            
            
            
            
            
            selected_invYear <- reactive({
                        if (is.null(input$datafile)) {
                                    2006
                        }else{NULL}
                        
            })
            
            
            selected_removeCity <- reactive({
                        if (is.null(input$datafile)) {
                                    
                                    c("Kalmar","Karlstad")
                                    
                        }else{NULL}
                        
            })
            
            
            selected_clusterCount <- reactive({
                        if (is.null(input$datafile)) {
                                    3
                        }else{NULL}
                        
            })
            
            selected_donar_var <- reactive({
                        if (is.null(input$datafile)) {
                                    c("Percent_University")
                        }else{NULL}
                        
                        
            })
            
            
            Yearcompared <- reactive({
                        if (is.null(input$datafile)) {
                                    c(2003,2004,2005)
                        }else{NULL}
                        
            })
            
            selectedDependent <- reactive({
                        if (is.null(input$datafile)) {
                                    "Productivity"
                        }else{NULL}
                        
            })
            
            
            selectedPredictors <- reactive({
                        if (is.null(input$datafile)) {
                                    c("Percent_University")
                        }else{NULL}
                        
            })
            
            
            
            # default value for IKEA case done 
            
            
            
            
            
            # 3. UI output ========================================================
            output$city_name <- renderUI({
                        selectInput(inputId = "city_name",label = "Treated unit's name",choices = Unit_name()) 
            })
            
            output$inv_year <- renderUI({                                                                
                        selectInput(inputId = "inv_year",label = "Intervention time",
                                    choices = Unit_year() , selected = selected_invYear()
                        )
            })
            
            output$removeCityName <- renderUI({
                        
                        selectizeInput(inputId = "removeCity_name",label = "Excluded units",
                                       choices = sort(Unit_name()), selected = selected_removeCity() , multiple = T) 
                        
            })
            
            
            
            output$test_year <- renderUI({ 
                        selectizeInput(
                                    inputId = "test_year",label = "Tested Year",selected = Yearcompared(),
                                    choices = Unit_year(), multiple = F) 
            })   
            
            
            
            output$cluster_count <- renderUI({  
                        selectInput( "cluster_count", label = "Cluster count", choices = 1:15,
                                     selected_clusterCount() )
            })
            
            
            output$donar_var <- renderUI({  
                        conditionalPanel("input.cluster_count >= 2",
                                         
                                         selectizeInput( "donar_var", label = "Variables compared", 
                                                         choices = choiceVar(),  selected_donar_var() , multiple = T)
                        )
            })
            
            output$donar_year <- renderUI({ 
                        
                        conditionalPanel("input.cluster_count >= 2",
                                         selectizeInput(
                                                     inputId = "donar_year",label = "Year compared",selected = Yearcompared(),
                                                     choices = Unit_year(), multiple = T) 
                        )
                        
            })      
            
            
            
            output$dependent_var <- renderUI({
                        
                        selectInput(
                                    inputId = "dependent_var",
                                    label = "Dependent variable",selected = selectedDependent(),
                                    choices = choiceVar() )
                        
            })
            
            
            output$matchYear <- renderUI({
                        
                        conditionalPanel("input.cluster_count >= 2",
                                         sliderInput("matchYear", "", min = min_year(), max = max_year(), 
                                                     value = c(min_year(), median_year() ))
                        )
            })      
            
            
            output$potentialPredictors <- renderUI({
                        selectizeInput( "potentialPredictors", label = "Candidate predictors", 
                                        choices = choiceVar(), selected = selectedPredictors(),
                                        multiple = T)
            })
            
            
            output$predictors <- renderUI({
                        selectizeInput( "predictors", label = "Predictors", 
                                        choices = choiceVar(), selected = selectedPredictors(),
                                        multiple = T)
            })
            
            
            
            output$predictor_time_ui <- renderUI({
                        
                        if (!is.null(input$matchYear)) { 
                                    n <- length(input$predictors)
                                    lapply(1:n, function(i) {
                                                
                                                selectizeInput(paste0('TimePredictor_', i), 
                                                               label = paste0("(", i,") " ,input$predictors[i]),
                                                               choices = input$matchYear[1]:input$matchYear[2] 
                                                               , multiple = T)
                                    })
                                    
                        }else{NULL}
                        
            })
            
            ## pretest predictors -------------#
            predictorsTest <- eventReactive(input$PredictorTestButton, {
                        withProgress(message = 'Pre-testing predictors ... ', value = 0, {
                                    data <- filedata()
                                    predictors <- as.character(input$potentialPredictors)
                                    dependent <- as.character(input$dependent_var)
                                    yearVar <- as.character(input$var_time)
                                    #invYear <- as.numeric(input$inv_year)
                                    year_test <- as.numeric(input$test_year)
                                    
                                    Func_PredictorCorr(data, predictors, dependent, year = year_test, yearVar)
                                    
                        })
            })
            
            
            
            
            output$predictorsTest <- renderTable({
                        predictorsTest()
            })
            
            
            ## cluster Test
            
            
            controlID_filtered <- eventReactive(input$ClusterTestButton, {
                        
                        withProgress(message = 'Clustering ... ', value = 0, {
                                    
                                    data <- filedata()
                                    m0 <- which( colnames(data) == as.character(input$var_name)) 
                                    data[,m0] <- as.character(data[,m0])
                                    
                                    n0 <- which( colnames(data) == as.character(input$var_ID)) 
                                    
                                    
                                    treat_ID <- treat_ID()
                                    
                                    com_year <- length(input$donar_year)
                                    
                                    
                                    control_ID_res <- cmp_Func_Cluster_Pool(data = data, treat_ID = treat_ID, 
                                                                            cluster_count = input$cluster_count,
                                                                            comparable_Year = as.numeric(input$donar_year), 
                                                                            comparable_Var = input$donar_var,
                                                                            IDVarName = input$var_ID,
                                                                            yearVarName = input$var_time, 
                                                                            unitVarName = input$var_name,
                                                                            transformation = input$transformation,
                                                                            remove_cityName = as.character(input$removeCity_name)
                                                                            
                                    )
                                    
                                    
                                    control_ID_i <- control_ID_res$SC_pool
                                    SOM_data <- control_ID_res$data.som
                                    cluster_plot <- Func_clusterPlot(SOM_data, comparable_Var = input$donar_var, transformation = input$transformation)
                                    
                                    
                                    # for (i in 1:com_year) {  
                                    #             control_ID_res <- cmp_Func_Cluster_Pool(data = data, treat_ID = treat_ID, 
                                    #                                                     cluster_count = input$cluster_count,
                                    #                                                     comparable_Year = as.numeric(input$donar_year)[i], 
                                    #                                                     comparable_Var = input$donar_var,
                                    #                                                     IDVarName = input$var_ID,
                                    #                                                     yearVarName = input$var_time,
                                    #                                                     unitVarName = input$var_name,
                                    #                                                     transformation = input$transformation,
                                    #                                                     remove_cityName = as.character(input$removeCity_name)
                                    #                                                     
                                    #             )
                                    #             
                                    #             if ( i == 1 ) {
                                    #                         control_ID_i <- control_ID_res$SC_pool
                                    #             }else{
                                    #                         control_ID_i <- intersect( control_ID_res$SC_pool,control_ID_i)
                                    #             }
                                    #             
                                    # }
                                    # 
                                    
                                    q <- which( as.character(input$removeCity_name) %in% data[,m0] )
                                    
                                    remove <- unique(as.numeric(data[q,n0]))
                                    control_ID <- setdiff(control_ID_i, remove)
                                    
                                    control_name <- sort(unique(data[which(as.numeric(data[,n0]) %in% control_ID),m0]))
                                    
                                    result <- list("control_ID" = control_ID, "control_length" = length(control_ID),
                                                   "control_name" = control_name,"cluster_plot" = cluster_plot,
                                                   "SOM_data" = SOM_data)
                        })
            })
            
            
            
            
            
            
            #  Number of control units
            output$controlLength <- renderText({
                        paste0("Current donor pool include ",
                               controlID_filtered()$control_length, 
                               " units")
            })
            
            output$control_name <- renderText({
                        controlID_filtered()$control_name
                        
            })
            
            output$clusterPlot <- renderPlotly({
                        controlID_filtered()$cluster_plot
                        
            })
            
            
            ## 4. Synthetic control matching   &  Placebo testing =======================================================#        
            
            synthResult <- eventReactive(input$Submit, {
                        
                        withProgress(message = 'Synthetic control matching ... ', value = 0, {
                                    
                                    data <- filedata()
                                    m0 <- which( colnames(data) == as.character(input$var_name)) 
                                    data[,m0] <- as.character(data[,m0])
                                    
                                    nid <- which( colnames(data) == as.character(input$var_ID)) 
                                    
                                    treat_ID <- treat_ID()
                                    
                                    com_year <- length(input$donar_year)
                                    control_ID <- controlID_filtered()$control_ID
                                    control_length <- length(control_ID)
                                    
                                    special_predictors <- NULL
                                    
                                    for (i in 1:length(input$predictors)) {
                                                special_predictors[[i]] <- list(input$predictors[i], 
                                                                                as.numeric(input[[paste0("TimePredictor_",i)]]), 
                                                                                "mean")
                                                
                                    }
                                    
                                    
                                    
                                    synthResult <-  cmp_Func_Synth(synth_data = data , predictors = input$predictors  ,
                                                                   dependent = input$dependent_var , inv_year = input$inv_year,
                                                                   treat_ID = treat_ID, control_ID = control_ID ,
                                                                   match_from = input$matchYear[1], match_to = input$matchYear[2],
                                                                   
                                                                   yearVarName = input$var_time, 
                                                                   IDVarName = input$var_ID, 
                                                                   nameVarName = input$var_name,
                                                                   
                                                                   special.predictors = special_predictors
                                    )
                                    
                                    
                                    weight_vector <-  round(synthResult$synth_out$solution.w,4)
                                    weight_data0 <- data.frame(weight_vector, "KnKod" = as.numeric(rownames(weight_vector)))
                                    weight_data1 <- weight_data0[order(weight_data0$w.weight, decreasing = T),] [1:6,]
                                    
                                    weight_data <- subset(weight_data1, round(w.weight,3) > 0)
                                    
                                    Knname <- sapply(weight_data$KnKod,
                                                     function(x){
                                                                 as.character(data[which(data[,nid] == x)[1], m0])
                                                                 
                                                     }
                                    )
                                    
                                    weight_data_out <- data.frame( Knname, weight_data)
                                    
                                    
                                    
                                    result <- Unit_map(weight_data, treat_ID = treat_ID)
                                    
                                    SC_map <- result$SC_Map
                                    Treat_map <- result$TreatMap
                                    
                                    
                                    
                                    
                                    ## final output from the reactive event
                                    map <- list( "SC_map" = SC_map, "Treat_map" = Treat_map, 
                                                 "control_ID" = control_ID,
                                                 "control_length" = control_length,
                                                 "special.predictors" = special_predictors,
                                                 "weight_data_out" = weight_data_out,
                                                 "Synth" = synthResult)                        })
                        
                        
            })
            
            
            ## Placebo testing
            
            placeboResult <- eventReactive(input$placeboButton, {
                        
                        withProgress(message = 'Placebo test ... ', value = 0, {      
                                    ## === Placebo test
                                    data <- filedata()
                                    m0 <- which( colnames(data) == as.character(input$var_name)) 
                                    data[,m0] <- as.character(data[,m0])
                                    nid <- which( colnames(data) == as.character(input$var_ID)) 
                                    treat_ID <- treat_ID()
                                    com_year <- length(input$donar_year)
                                    control_ID <- controlID_filtered()$control_ID
                                    
                                    
                                    placeboResult <- cmp_Func_PlaceboTest(control_ID,
                                                                          synth_data = data, 
                                                                          predictors = input$predictors  ,
                                                                          dependent = input$dependent_var ,
                                                                          inv_year = input$inv_year,
                                                                          match_from = input$matchYear[1], 
                                                                          match_to = input$matchYear[2],
                                                                          yearVarName = input$var_time, 
                                                                          IDVarName = input$var_ID, 
                                                                          nameVarName = input$var_name)
                        })
            })
            
            ## 4. done            
            
            ## growth curve -------------#
            growthCurve <- eventReactive(input$ModelCurveButton, {
                        withProgress(message = 'fitting curve of intervention envolvement ... ', value = 0, {
                                    
                                    data.gaps <- synthResult()$Synth$gaps
                                    Func_GrowthCurve(inv_year = input$inv_year , Y.gaps = data.gaps)
                                    
                        })
            })
            
            output$curveplot <- renderPlot({
   
                        gap.Para <- growthCurve()$paraResult$fit.data
                        gap.nonPara <- growthCurve()$nonParaResult$fit.data
                        
                        gaps <- synthResult()$Synth$gaps
                        post <- as.numeric(input$inv_year) < as.numeric(row.names(gaps))
                        y.gap <-  gaps[post]
                        
                        
                        
                        time0 <- 1:length(y.gap)
                        time <- time0 + as.numeric(input$inv_year) 
                        ylimv <- c(min(y.gap, gap.nonPara) * 0.8 , max(y.gap, gap.nonPara)*1.2)
                        A.v <- round(growthCurve()$nonParaResult$parametersLowess$A,2)
                        MU.v <- round(growthCurve()$nonParaResult$parametersLowess$mu,2)
                        LAMBDA.v <- round(growthCurve()$nonParaResult$parametersLowess$lambda,2)
                                        
                        subname <- substitute(
                                    paste("(A=", A.v, ", ",mu, "=", MU.v, ", ", lambda, "=", LAMBDA.v, ")"),
                                    list(A.v = A.v, MU.v = MU.v, LAMBDA.v = LAMBDA.v)
                        )
                        
            
                        plot(x = time, y = y.gap, xlab = "time" ,
                             main = "non-parametric regression",
                             sub = subname,
                             pch = 16, ylim = ylimv)
                        lines(x = time, y = gap.nonPara, col = "blue",  lty = 2, ylim = ylimv)
                        lines(x = time, y = gap.Para, col = "red", lty = 2, ylim = ylimv)
                        
            })
            
            
            ## growth_Curve 2
            ## growth curve -------------#
            growthCurve_drc <- eventReactive(input$ModelCurve_drcButton, {
                        withProgress(message = 'fitting curve of intervention envolvement ... ', value = 0, {
                                    
                                    data.gaps <- synthResult()$Synth$gaps
                                    Func_GrowthCurve_drc(inv_year = input$inv_year , Y.gaps = data.gaps)
                                    
                        })
            })
            
            
            output$curveplot_drc <- renderPlot({
                        gaps <- synthResult()$Synth$gaps
                        post <- as.numeric(input$inv_year) < as.numeric(row.names(gaps))
                        y.gap <-  gaps[post]
                        time0 <- 1:length(y.gap)
                        time <- time0 + as.numeric(input$inv_year) 
                        
                        y.fit <- growthCurve_drc()$fitmodel
                        y.fit_time <- y.fit[,1] + as.numeric(input$inv_year)
                        
                        plot(x = time, y = y.gap, xlab = "time", main = "parametric regression",
                             ylim = c( min(y.gap)*0.8, max(y.gap)*1.2) , pch = 16,
                             sub = paste0("Model:",growthCurve_drc()$modelname," ", growthCurve_drc()$para)
                        )
                        
                        lines(x = y.fit_time, y = y.fit[,2] , col = "red", lty = 2, xlab = "", ylab="")
                        
            })
            
            
            ## 5. results extraction and output =============================================================================#
            
            output$pred_synth <- renderTable({
                        synthResult()$Synth$pred_synth
            })
            
            output$weight_data_out <- renderTable({
                        synthResult()$weight_data_out
            })
            
            
            # 5.2 SCM gap plot
            output$SCM_plot <- renderPlot({  
                        
                        Func_plot(SynthY = synthResult()$Synth$Y_synth,
                                  treatedY = synthResult()$Synth$Y_treat, 
                                  Year = round(as.numeric(row.names(synthResult()$Synth$Y_treat)),0), 
                                  inv_year = input$inv_year, treatName = input$city_name,
                                  dependVar = input$dependent_var,
                                  mainName = "SCM")
            })
            
            ## 5.3 Placebo testing
            
            ## gap data of all units (treated unit and control units)
            placeboData_gaps <- reactive({
                        
                        plotData_control <- placeboResult()
                        plotData_control$unitID <- as.factor(plotData_control$unitID)
                        
                        plotData_treat0 <- synthResult()$Synth$gaps
                        placebo_gaps_value <- as.vector(plotData_treat0)
                        Year <- round(as.numeric(row.names(plotData_treat0)),0)
                        unitID <- as.numeric(treat_ID())
                        plotData_treat <- data.frame("Year" = Year, "unitID" = unitID,
                                                     "unitName" = input$city_name,
                                                     "gapsValue" = placebo_gaps_value )
                        
                        rbind(plotData_treat, plotData_control )
                        
            })
            
            
            
            # plot of placebo units
            output$placeboPlot <- 
                        renderPlotly({
                                    data1 <- data.frame(placeboData_gaps())
                                    
                                    treat_th <- which(as.numeric(data1[,2]) == as.numeric(treat_ID()))
                                    
                                    plotData_control <- data1[-treat_th,]
                                    
                                    plotData_treat <-  data1[treat_th,]
                                    
                                    
                                    p1 <- ggplot(plotData_control , aes(x = Year, y = gapsValue, 
                                                                        colour = unitName)) +
                                                geom_line(lwd = 0.4, alpha = 0.3,linetype = "solid") +
                                                
                                                geom_line(
                                                            data = plotData_treat,
                                                            aes(x = Year, y = gapsValue), colour = "black", 
                                                            lwd = 0.5, linetype = "solid" ) + 
                                                
                                                geom_hline(aes(yintercept = 0), colour = "black" , 
                                                           lwd = 0.4, linetype = "dashed") +
                                                
                                                geom_vline(aes(xintercept = as.numeric(input$inv_year)), 
                                                           lwd = 0.3,  linetype = "dotted") +
                                                theme_bw() +
                                                
                                                theme(legend.position = "none",
                                                      
                                                      panel.grid.major = element_blank(),
                                                      panel.grid.minor = element_blank(),
                                                      panel.background = element_blank()) +
                                                
                                                ggtitle(paste0("placebo test plot for all units" ))
                                    
                                    
                                    ggplotly(p1)
                                    
                                    
                        })
            
            
            
            
            
            
            ### 5.4 gap values for output ============================#
            
            
            placeboResult_Table <- reactive({
                        Func_placeboOut(placeboData_gaps = placeboData_gaps(), bootstrap =  FALSE,
                                        invYear = input$inv_year
                        )
                        
                        
            })
            
            
            
            
            ### 6. bootstrap interval
            Bootstrap <- eventReactive(input$Bootstrap_button, {
                        
                        withProgress( message = 'Bootstraping ... ', value = 0, {
                                    
                                    Bootstrap_Result  <-  cmp_Func_Bootstrap(
                                                
                                                RepTimes = 50,
                                                data = filedata() , predictors = input$predictors  ,
                                                dependent = input$dependent_var , inv_year = input$inv_year,
                                                treat_ID = treat_ID(), control_ID = synthResult()$control_ID ,
                                                match_from = input$matchYear[1], match_to = input$matchYear[2],
                                                
                                                yearVarName = input$var_time, 
                                                IDVarName = input$var_ID, 
                                                nameVarName = input$var_name,
                                                
                                                special.predictors = synthResult()$special.predictors
                                    )     
                                    
                                    
                                    year <- unique(Bootstrap_Result$Year)
                                    
                                    gapsValue <- as.numeric(synthResult()$Synth$gaps)
                                    treat_id <- as.numeric(Bootstrap_Result$unitID[1]) - 1
                                    treat_name <- as.factor(treat_id)
                                    
                                    treat_raw <- data.frame("Year" = year,
                                                            "unitID" = treat_id, 
                                                            "unitName" = treat_name, 
                                                            "gapsValue" = gapsValue)
                                    
                                    Bootstrap_Result$unitID <- as.numeric(Bootstrap_Result$unitID)
                                    data <- rbind(treat_raw, Bootstrap_Result)
                                    
                                    
                        })
                        
            })
            
            
            
            bootstrap_Interval <- reactive({
                        bootstrap_interval <- as.vector(Func_placeboOut(placeboData_gaps = Bootstrap(), bootstrap =  TRUE,
                                                                        invYear = input$inv_year)$outputData)
                        
                        names(bootstrap_interval) <- "Bootstrap_interval"
                        data.frame(bootstrap_interval)
                        
                        
            })
            
            
            output$placeboResult_Table <- renderTable(
                        if ( input$Bootstrap_button == 0 ) {
                                    placeboResult_Table()$outputData
                        }else{
                                    
                                    cbind(placeboResult_Table()$outputData, bootstrap_Interval())
                        }
            )
            
            
            
            dot_Data <- reactive({
                        
                        
                        dataPlot <- placeboResult_Table()$placeboDotPlotData
                        
                        
                        MSPEdata <- dataPlot[order(dataPlot$MPSE_pre),]
                        numberQt <- round(quantile(1:nrow(MSPEdata), 0.9),0)
                        if (numberQt == 0) {numberQt <- 1}
                        
                        MSPEdata$color <- c(rep("black" , numberQt), rep("blue", nrow(MSPEdata) - numberQt))
                        MSPEdata$color[which(MSPEdata$unitID == treat_ID())] <- "red"
                        
                        
                        
                        RatioData <- dataPlot[order(dataPlot$MPSE_ratio),]
                        RatioData$color <- c(rep("black" , numberQt), rep("blue", nrow(MSPEdata) - numberQt))
                        RatioData$color[which(RatioData$unitID == treat_ID())] <- "red"
                        
                        return(list("MSPEdata" = MSPEdata,"RatioData" = RatioData))
                        
            })
            
            output$FitIndex <- renderText(placeboResult_Table()$FitIndex)
            
            
            conclusion <- reactive({
                        
                        # 1. Treated one : availability of SCM
                        treat_th <- which(dot_Data()$MSPEdata$unitName == input$city_name)
                        treat_mspe <- dot_Data()$MSPEdata$MPSE_pre[treat_th]
                        mspe_quantile <- quantile( dot_Data()$MSPEdata$MPSE_pre , 0.9 )
                        if (treat_mspe > mspe_quantile) {
                                    SCM_availability <- "No"
                        }else{
                                    SCM_availability <- "Yes"
                        }
                        
                        
                        # 2. Cluster number
                        
                        controlunitLength <- synthResult()$control_length
                        if (controlunitLength > 60) { 
                                    cluster_suggestion = "should decrease number of cluster"
                        } else if (controlunitLength < 15) {
                                    cluster_suggestion = "should increase number of cluster"            
                        } else {
                                    cluster_suggestion = "No"
                        }
                        
                        
                        
                        
                        # 3. preMSPE Outliers
                        
                        PreMspe <- dot_Data()$MSPEdata$MPSE_pre
                        sd_PreMspe <- sd(PreMspe) * 4
                        outlier_PreMspe_th <- which( PreMspe > sd_PreMspe  )
                        
                        if (length(outlier_PreMspe_th) == 0) {
                                    outlier_PreMspe <- "none" 
                        }else{
                                    outlier_PreMspe0 <- sapply( outlier_PreMspe_th , function(x){ dot_Data()$MSPEdata$unitName[x]})
                                    outlier_PreMspe <- setdiff(as.character(outlier_PreMspe0), as.character(input$city_name))
                                    if (length(outlier_PreMspe) == 0 ) { 
                                                outlier_PreMspe <- "None"    
                                    }
                        }
                        
                        
                        
                        # 4. RatioMSPE Outliers
                        
                        RatioMspe <- dot_Data()$RatioData$MPSE_ratio
                        sd_RatioMspe <- sd(RatioMspe) * 4
                        
                        outlier_RatioMspe_th <- which( RatioMspe > sd_RatioMspe  )
                        
                        if (length(outlier_RatioMspe_th) == 0) {
                                    outlier_RatioMspe <- "None"  
                        }else{
                                    outlier_RatioMspe0 <- sapply( outlier_RatioMspe_th , function(x){ dot_Data()$RatioData$unitName[x]})
                                    outlier_RatioMspe <- setdiff(as.character(outlier_RatioMspe0), as.character(input$city_name))
                                    
                                    if (length(outlier_RatioMspe) == 0 ) {
                                                outlier_RatioMspe <- "None"    
                                    }
                                    
                        }
                        
                        return(list("SCM_availability" = SCM_availability,
                                    "cluster_suggestion" = cluster_suggestion,
                                    "outlier_PreMspe" = outlier_PreMspe,
                                    "outlier_RatioMspe" = outlier_RatioMspe
                        ))
                        
                        
            })
            
            
            output$SCM_Availability <- renderText({
                        
                        conclusion()$SCM_availability})
            
            output$Clustering_status <- renderText({            
                        
                        conclusion()$cluster_suggestion})
            
            output$outlier_mspe <- renderText({  
                        as.character(conclusion()$outlier_PreMspe)
            })
            
            output$outlier_RatioMspe <- renderText({             
                        as.character(conclusion()$outlier_RatioMspe)
            })
            
            
            
            
            
            
            
            
            
            
            ## plot output
            output$plot_Dot <- renderPlot({
                        par(mfrow = c(1,2))
                        dotchart(dot_Data()$MSPEdata$MPSE_pre,labels = dot_Data()$MSPEdata$unitName, pch = 20, cex = 0.65,
                                 main = "Pre MSPE", color = as.character(dot_Data()$MSPEdata$color),
                                 xlab = "MSPE")
                        
                        dotchart(dot_Data()$RatioData$MPSE_ratio,labels = dot_Data()$RatioData$unitName, pch = 20, cex = 0.65,
                                 main = "Post-RMSPE/Pre-RMSPE", color = as.character(dot_Data()$RatioData$color),
                                 xlab = "Ratio of MSPE")
                        
            })
            
            
            
            
            
            
            output$bootPlot <- 
                        renderPlotly({
                                    data2 <- data.frame(Bootstrap())
                                    k <- length(unique(data2[,1]))
                                    treatdata <- data2[1:k,]
                                    
                                    p2 <- ggplot(data2 , aes(x = Year, y = gapsValue,
                                                             colour = unitName) ) +
                                                
                                                geom_line(lwd = 0.4, alpha = 0.3,linetype = "solid") +
                                                
                                                geom_line(
                                                            data = treatdata,
                                                            aes(x = Year, y = gapsValue), colour = "black", 
                                                            lwd = 0.5, linetype = "solid" ) + 
                                                
                                                
                                                geom_hline(aes(yintercept = 0), colour = "black" , 
                                                           lwd = 0.4, linetype = "dashed") +
                                                
                                                geom_vline(aes(xintercept = as.numeric(input$inv_year)), 
                                                           lwd = 0.3,  linetype = "dotted") +
                                                theme_bw() +
                                                
                                                theme(legend.position = "none",
                                                      
                                                      panel.grid.major = element_blank(),
                                                      panel.grid.minor = element_blank(),
                                                      panel.background = element_blank()) +
                                                
                                                ggtitle(paste0("Bootstrap of donor pool" ))
                                    
                                    ggplotly(p2)
                        })
            
            
            
            
            # ELM result output ============================================#
            # elm_result <- eventReactive(input$Submit, {
            #             
            #             elm_result <- cmp_Func_ELM(treatName = input$city_name,inv_year = input$inv_year, 
            #                                        data = filedata(), 
            #                                        varYear = input$var_time, varUnitName = input$var_name, 
            #                                        varOutput = input$dependent_var, varUnitID = input$var_ID)
            #             
            # })
            # 
            # 
            # output$ELM_plot <- renderPlot({
            #             Func_plot(SynthY = elm_result()$SynthY, treatedY = elm_result()$treatedY, 
            #                       Year = elm_result()$Year, input$inv_year, main = "ELM")
            #             
            #             
            # }) 
            
            
            
            
            
            # Map ====================================================================================#
            
            output$map <- renderLeaflet(
                        
                        if (class(try(synthResult() ,silent = F)) == "try-error") {
                                    NULL
                        }else{
                                    finalMap(synthResult()$SC_map,synthResult()$Treat_map,synthResult()$control_ID) }
            )
            
            
            
            # Generating report ====================================================================================#           
            output$report = downloadHandler(
                        filename = 'myreport.pdf',
                        
                        content = function(file) {
                                    out = knit2pdf('input.Rnw', clean = TRUE)
                                    file.rename(out, file) # move pdf to file for downloading
                        },
                        
                        contentType = 'application/pdf'
            )
            
            
            
            
            output$dataTable <- renderDataTable({filedata()})
            output$placeboData_gaps <- renderDataTable({placeboData_gaps()})
})

