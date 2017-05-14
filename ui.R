#ui.R

source('./0_source.R', echo = F)

## 1. header --------------------------------------------------------------------------##
header <- dashboardHeader(title = "Intervention effect study", titleWidth = 230)


## 2. siderbar --------------------------------------------------------------------------##
siderbar <- dashboardSidebar(
            
            width = 230,
            
            sidebarMenu(
                        menuItem("Step1. Refining Donor Pool", tabName = "DonorPoolRefining", icon = icon("play-circle")) ,
                        menuItem("Step2. SCM Estimation", tabName = "SCM", icon = icon("play-circle")) ,
                        menuItem("Step3. SCM Inference", tabName = "SCM_inference", icon = icon("play-circle")) ,
                        
                        #menuItem("ELM", tabName = "ELM", icon = icon("play-circle")) ,
                        
                        menuItem("Data", tabName = "data", icon = icon("table")),
                        
                        menuItem("Code", tabName = "source code", icon = icon("code")),
                        
                        menuItem("Mannual", tabName = "mannual", icon = icon("book")),
                        
                        menuItem("Appendix", tabName = "appendix", icon = icon("info-circle")),
                        
                        br(),
                        
                        column(width = 10, offset = 0.05, textOutput("defaultData")),
                        fileInput("datafile", label = "Upload data"),
                        br(),
                        uiOutput("varUnitID"),
                        uiOutput("varUnitName") ,
                        uiOutput("varYear")
            )
)


## 3. body --------------------------------------------------------------------------##
body <- dashboardBody(
            
            tabItems(  
                        # tab 1
                        tabItem(
                                    tabName = "DonorPoolRefining",    
                                    
                                    fluidRow(  
                                                # First 
                                                box(width  = 12, solidHeader = TRUE, status = "warning",title = "Step1. Refining Donor Pool",
                                                    column(width = 3, 
                                                           div(head("(1) Treated Unit"), 
                                                               style = "color:darkorange;font-size:16px;font-weight: 600;"),
                                                           br(),
                                                           
                                                           uiOutput("city_name"),      
                                                           uiOutput("inv_year"),
                                                           uiOutput("dependent_var")),
                                                    
                                                    column(width = 3, offset = 1,
                                                           div(head("(2) Predictors validation"), 
                                                               style = "color:darkorange;font-size:16px;font-weight: 600;"),
                                                           br(),
                                                           uiOutput("potentialPredictors"),
                                                           uiOutput("test_year"),
                                                           actionButton("PredictorTestButton", label = "Predictor Test",icon("paper-plane"), 
                                                                        style = "color: #fff; background-color:#FFA500;border-color:#2e6da4; width:150px")
                                                    ),
                                                    
                                                    column(width = 3, offset = 1,
                                                           div(head("(3) Donar pool refining"), 
                                                               style = "color:darkorange;font-size:16px;font-weight: 600;"),
                                                           br(),
                                                           
                                                           uiOutput("cluster_count"),
                                                           uiOutput("donar_var"),
                                                           radioButtons(inputId = "transformation", label = "Variables' transformation", 
                                                                        inline = T, width = 400, 
                                                                        choices = c("raw" = "raw",
                                                                                    "log" = "log",
                                                                                    "scale" = "scale")),
                                                           
                                                           
                                                           uiOutput("donar_year"),
                                                           uiOutput("removeCityName"),
                                                           actionButton("ClusterTestButton", label = "Cluster",icon("paper-plane"), 
                                                                        style = "color: #fff; background-color:#FFA500;border-color:#2e6da4; width:150px"),
                                                           fluidRow(br()),
                                                           div(textOutput("controlLength"), 
                                                               style = "color:darkorange;font-size:15px;font-weight: 600;")
                                                           
                                                    )
                                                    
                                                    
                                                )),
                                    
                                    fluidRow( 
                                                box(width = 12, solidHeader = T, status = "success", title = "Refining results",
                                                    tableOutput("predictorsTest"),
                                                    plotlyOutput("clusterPlot")
                                                    
                                                )
                                                
                                    )
                        ),
                        
                        
                        
                        #tab 2
                        tabItem(
                                    tabName = "SCM",    
                                    fluidRow(  
                                                box(width  = 12, solidHeader = TRUE, status = "warning",title = "Step2. SCM Estimation",   
                                                    div(head("Synthetic control matching"), 
                                                        style = "color:darkorange;font-size:16px;font-weight: 600;"),
                                                    
                                                    column(width = 4, 
                                                           uiOutput("predictors")),
                                                    column(width = 4, 
                                                           div("Matching time:", style = "font-weight:bold"),
                                                           uiOutput("matchYear")),
                                                    column(width = 4, 
                                                           strong("Speical predictors"),
                                                           uiOutput("predictor_time_ui"),
                                                           
                                                           #textOutput("test"),
                                                           actionButton("Submit", label = "Submit_SCM",icon("paper-plane"), 
                                                                        style = "color: #fff; background-color:#FFA500;border-color:#2e6da4; width:150px")        
                                                    ))),
                                    br(),
                                    
                                    fluidRow(
                                                box(width = 12, solidHeader = T, status = "success", title = "Estimation Results",
                                                    div(head("Treated unit vs. Synthetic control unit"), 
                                                        style = "color:darkgreen;font-size:16px;font-weight: 600;"),
                                                    tableOutput("pred_synth"),
                                                    plotOutput("SCM_plot",width = "100%", height = "450px"),
                                                    strong("Mapping the SCM results"),
                                                    leafletOutput("map")
                                                ))),
                        
                        
                        
                        #tab 3
                        tabItem(tabName = "SCM_inference",
                                box(width  = 12, solidHeader = TRUE, status = "warning",title = "Step3. SCM Inference",   
                                    
                                    column(width = 3, offset = 1,
                                           
                                           actionButton("placeboButton", label = "placebo test",
                                                        icon("paper-plane"), 
                                                        style = "color: #fff; background-color:#FFA500;border-color:#2e6da4; width:200px")     
                                           
                                    ),
                                    
                                    
                                    
                                    column(width = 3, offset = 1, 
                                           actionButton("Bootstrap_button", label = "Bootstrap interval",icon("paper-plane"), 
                                                        style = "color: #fff; background-color:#FFA500;border-color:#2e6da4; width:150px")
                                    ),
                                    
                                    
                                    column(width = 3, offset = 1, 
                                           actionButton("fit_growth_model", label = "fit growth model",icon("paper-plane"), 
                                                        style = "color: #fff; background-color:#FFA500;border-color:#2e6da4; width:150px")
                                    )
                                ),
                                
                                
                                
                                box(width = 12, solidHeader = T, status = "success", title = "Inference Results",
                                    column(width = 7, 
                                           div(head("Model Output"), 
                                               style = "color:darkgreen;font-size:16px;font-weight: 600;"),
                                           br(),
                                           
                                           tableOutput("placeboResult_Table")
                                    ),
                                    
                                    
                                    column(width = 5, 
                                           div(head("Model Diagnosis"), 
                                               style = "color:darkgreen;font-size:16px;font-weight: 600;"),
                                           
                                           br(),
                                           strong("Reliability of SCM: "),
                                           textOutput("SCM_Availability"),
                                           br(),
                                           
                                           strong("Should re-Cluster:"),
                                           textOutput("Clustering_status"),
                                           br(),
                                           
                                           strong("Should excluded following unit(s) due to pre-MSPE :"),
                                           textOutput("outlier_mspe"),
                                           br(),
                                           
                                           strong("Should excluded following unit(s) due to ratio-MSPE :"),
                                           textOutput("outlier_RatioMspe"),
                                           br(),
                                           
                                           strong("Pre-MSPE of units in donor pool:"),
                                           textOutput("FitIndex")
                                           
                                    )
                                ),
                                
                                
                                box(width = 12, solidHeader = T, status = "success", title = "Inference Results figures",
                                    fluidRow(
                                                column(width = 12,  
                                                plotOutput("plot_Dot",width = "100%", height = "450px"))
                                                ),
                                    
                                    fluidRow(column(width = 5,  offset = 1, 
                                                    plotlyOutput("placeboPlot",width = "100%", height = "500px") ),
                                             
                                             column(width = 5,  offset = 1, 
                                                    plotlyOutput("bootPlot",width = "100%", height = "500px") )
                                             
                                    )),
                                
                                
                                fluidRow(
                                            column(width = 4, offset = 2,
                                                   actionButton("ModelCurveButton", label = "fit non-parametric regression",
                                                                #icon("paper-plane"), 
                                                                style = "color: #fff; background-color:#FFA500;border-color:#2e6da4; width:200px")),
                                            
                                            column(width = 4, offset = 2,       
                                                   actionButton("ModelCurve_drcButton", label = "fit parametric regression",
                                                                #icon("paper-plane"), 
                                                                style = "color: #fff; background-color:#FFA500;border-color:#2e6da4; width:200px")
                                                   
                                            )
                                ),
                                
                                fluidRow(
                                            column(width = 5, offset = 1,
                                                   plotOutput("curveplot",width = "100%", height = "450px")),
                                            column(width = 5, offset = 1,
                                                   plotOutput("curveplot_drc",width = "100%", height = "450px")))
                                
                        ),
                        
                        
                        
                        
                        
                        #tab 4
                        # tabItem(tabName = "ELM",
                        #         column(width = 5 ,offset = 2, 
                        #                plotOutput("ELM_plot",width = "100%", height = "700px"))
                        # ),
                        
                        # tab 5
                        tabItem(tabName = "data",
                                dataTableOutput("dataTable")
                        ),
                        
                        # tab 6
                        tabItem(tabName = "appendix",
                                textOutput("control_name"),
                                tableOutput("weight_data_out"),
                                dataTableOutput("placeboData_gaps")
                                
                        )
            )
)


dashboardPage(header,siderbar,body)          

