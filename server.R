#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  output$var1_name_detrend_text<- renderText({ 
    paste(input$var1_name,sep="")
  })
  
  output$var2_name_detrend_text<- renderText({ 
    paste(input$var2_name,sep="") 
  })
  
  output$var1_name_decl_text<- renderText({ 
    paste(input$var1_name,sep="")
  })
  
  output$var2_name_decl_text<- renderText({ 
    paste(input$var2_name,sep="") 
  })
  
  
  output$sample_conditioned_on_var1_dep_str <- renderText({ 
    paste("Sample conditioned on ", input$var1_name, sep="") 
  })
  
  output$sample_conditioned_on_var2_dep_str <- renderText({ 
    paste("Sample conditioned on ", input$var2_name, sep="") 
  })
  
  output$sample_conditioned_on_var1_mar_dist <- renderText({ 
    paste("Sample conditioned on ", input$var1_name, sep="") 
  })
  
  output$sample_conditioned_on_var2_mar_dist <- renderText({ 
    paste("Sample conditioned on ", input$var2_name, sep="") 
  })
  
  output$dep_str_value_var1 <- renderText({ 
    paste("Please choose the thresholds for drawing samples conditioned on ", input$var1_name, sep="") 
  })
  
  output$dep_str_value_var2 <- renderText({    
    paste("Please choose the thresholds for drawing samples conditioned on ", input$var2_name,sep="") 
  })
  
  output$gpd_var1_text <- renderText({    
    paste(input$var1_name," (GPD)",sep="") 
  })
  
  output$gpd_var2_text <- renderText({    
    paste(input$var2_name," (GPD)",sep="") 
  })
  
  output$non_extreme_var1_text <- renderText({    
    paste(input$var1_name) 
  })
  
  output$non_extreme_var2_text <- renderText({    
    paste(input$var2_name) 
  })
  
  output$sample_conditioned_on_var1_iso <- renderText({ 
    paste("Sample conditioned on ", input$var1_name, sep="") 
  })
  
  output$sample_conditioned_on_var2_iso <- renderText({ 
    paste("Sample conditioned on ", input$var2_name, sep="") 
  })
  
  output$var1_text_iso <- renderText({ 
    paste(input$var1_name) 
  })
  
  output$var2_text_iso <- renderText({ 
    paste(input$var2_name) 
  })
  
  output$var1_text_iso_2 <- renderText({ 
    paste(input$var1_name) 
  })
  
  output$var2_text_iso_2 <- renderText({ 
    paste(input$var2_name) 
  })
  
  output$contents_var1 <- renderTable({
    req(input$file_var_1) 
    file <- input$file_var_1
    
    dataframe = read.csv(file$datapath)
    
    head(dataframe)
  })
  
  output$contents_var2 <- renderTable({
    req(input$file_var_2)
    file <- input$file_var_2
    
    dataframe = read.csv(file$datapath)
    
    head(dataframe)
  })
  
  df_var_1 <- reactive({
    req(input$file_var_1)
    file <- input$file_var_1
    
    read.csv(file$datapath)[,-1]
  })
  
  df_var_2 <- reactive({
    req(input$file_var_2)
    file <- input$file_var_2
    
    read.csv(file$datapath)[,-1]
  })
  
  
  output$dataPlotvar_1 <- renderPlot({
    req(df_var_1())
    plot(as.Date(df_var_1()[,1]),df_var_1()[,2],xlab="Date",ylab=paste(input$var1_name,' [',input$var1_units,']',sep=""))
    
  })
  
  output$dataPlotvar_2 <- renderPlot({
    req(df_var_2())
    plot(as.Date(df_var_2()[,1]),df_var_2()[,2],xlab="Date",ylab=paste(input$var2_name,' [',input$var2_units,']',sep=""))
    
  })
  
  #Detrend tab
  output$detrendPlot_var_1 <- renderPlot({
    x = df_var_1()
    x[,1] = as.Date(x[,1])
    Detrend(Data=x,Method=input$detrendmethodvar1,Window_Width= input$detrendwindowwidthvar1, End_Length = 5, PLOT=TRUE,x_lab="Date",y_lab=paste(input$var1_name,' [',input$var1_units,']',sep=""))
  })
  
  detrend_var_1 <- reactive({
    x = df_var_1()
    x[,1] = as.Date(x[,1])
    if (input$detvar1 == "yes") {
      detrend_var1 = Detrend(Data=x,Method=input$detrendmethodvar1,Window_Width= input$detrendwindowwidthvar1, End_Length = 5, PLOT=FALSE)
      df  = data.frame(x[,1],detrend_var1)
    }
    if (input$detvar1 == "no") {
      df = x
    }
    colnames(df) = c("Date",input$var1_name)
    return(df)
  })
  
  output$detrendPlot_var_2 <- renderPlot({
    x = df_var_2()
    x[,1] = as.Date(x[,1])
    Detrend(Data=x,Method=input$detrendmethodvar2,Window_Width= input$detrendwindowwidthvar2, End_Length = 5, PLOT=TRUE,x_lab="Date",y_lab=paste(input$var2_name,' [',input$var2_units,']',sep=""))
  })
  
  detrend_var_2 <- reactive({
    x = df_var_2()
    x[,1] = as.Date(x[,1])
    if (input$detvar2 == "yes") {
      detrend_var2 = Detrend(Data=x,Method=input$detrendmethodvar2,Window_Width= input$detrendwindowwidthvar2, End_Length = 5, PLOT=FALSE)
      df = data.frame("Date" = x[,1],detrend_var2)
    }
    if (input$detvar2 == "no") {
      df = x 
    }
    df[,1] = as.Date(df[,1])
    colnames(df) = c("Date",input$var2_name)
    return(df)
  })
  
  output$detrend_table_var1 <- renderTable({
    head(detrend_var_1())
  })
  
  output$detrend_table_var2 <- renderTable({
    head(detrend_var_2())
  })
  
  output$dec_plot_var1 <- renderPlot({
    if (input$decvar1 == "Solari") {
      decl = Decluster_SW(Data=detrend_var_1()[,1:2], Window_Width = input$decwindowwidthvar1)
      plot(as.Date(detrend_var_1()[,1]),detrend_var_1()[,2],pch=16,xlab="Date",ylab=paste(input$var1_name,' [',input$var1_units,']',sep=""))
      points(as.Date(detrend_var_1()[,1])[decl$EventID],detrend_var_1()[decl$EventID,2],col=2,pch=16)
    }
    if (input$decvar1 == "Standard") {
      decl = Decluster(Data=detrend_var_1()[,2], u = input$declrunsthresholdvar1, SepCrit = input$declrunssepcritvar1, mu = input$declrunsratevar1)
      plot(as.Date(detrend_var_1()[,1]),detrend_var_1()[,2],pch=16,xlab="Date",ylab=paste(input$var1_name,' [',input$var1_units,']',sep=""))
      points(as.Date(detrend_var_1()[,1])[decl$EventsMax],detrend_var_1()[,2][decl$EventsMax],col=2,pch=16)
    }
  })
  
  declust_var_1 <- reactive({
    if (input$decvar1 == "Solari") {
      decl = Decluster_SW(Data=detrend_var_1()[,1:2], Window_Width = input$decwindowwidthvar1)
    }
    if (input$decvar1 == "Standard") {
      decl = Decluster(Data=detrend_var_1()[,2], u = input$declrunsthresholdvar1, SepCrit = input$declrunssepcritvar1, mu = input$declrunsratevar1)
    } 
    df = data.frame(as.Date(detrend_var_1()[,1]),decl$Declustered)
    colnames(df) = c("Date",input$var1_name)
    return(df)
  })
  
  output$dec_text_var1 <- renderTable({
    x = declust_var_1()
    x[,1] = as.Date(x[,1])
    head(x)
  })
  
  output$dec_plot_var2 <- renderPlot({
    if (input$decvar2 == "Solari") {
      decl = Decluster_SW(Data=detrend_var_2()[,1:2], Window_Width = input$decwindowwidthvar2)
      plot(as.Date(detrend_var_2()[,1]),detrend_var_2()[,2],pch=16,,xlab="Date",ylab=paste(input$var2_name,' [',input$var2_units,']',sep=""))
      points(as.Date(detrend_var_2()[,1])[decl$EventID],detrend_var_2()[decl$EventID,2],col=2,pch=16)
    }
    if (input$decvar2 == "Standard") {
      decl = Decluster(Data=detrend_var_2()[,2], u = input$declrunsthresholdvar2, SepCrit = input$declrunssepcritvar2, mu = input$declrunsratevar2)
      plot(as.Date(detrend_var_2()[,1]),detrend_var_2()[,2],pch=16,,xlab="Date",ylab=paste(input$var2_name,' [',input$var2_units,']',sep=""))
      points(as.Date(detrend_var_2()[,1])[decl$EventsMax],detrend_var_2()[decl$EventsMax,2],col=2,pch=16)
    }
  })
  
  declust_var_2 <- reactive({
    if (input$decvar2 == "Solari") {
      decl = Decluster_SW(Data=detrend_var_2()[,1:2], Window_Width = input$decwindowwidthvar2)
    }
    if (input$decvar2 == "Standard") {
      decl = Decluster(Data=detrend_var_2()[,2], u = input$declrunsthresholdvar2, SepCrit = input$declrunssepcritvar2, mu = input$declrunsratevar2)
    } 
    df = data.frame(as.Date(detrend_var_2()[,1]), decl$Declustered)
    colnames(df) = c("Date",input$var2_name)
    return(df)
  })
  
  output$dec_text_var2 <- renderTable({
    x = declust_var_2()
    x[,1] = as.Date(x[,1])
    head(x)
  })
  
  #Dependence Analysis
  data_detrend_df <- reactive({
    df = data.frame("Date"=seq.Date(as.Date(min(df_var_1()$Date,df_var_2()$Date)),as.Date(max(df_var_1()$Date,df_var_2()$Date)),by="day"))
    df_1 = left_join(df,detrend_var_1(),by="Date")
    df = left_join(df_1,detrend_var_2(),by="Date")
    colnames(df) = c("Date",input$var1_name,input$var2_name)
    return(df)
  })  
  
  data_decl_df <- reactive({
    df = data.frame("Date"=seq.Date(as.Date(min(df_var_1()$Date,df_var_2()$Date)),as.Date(max(df_var_1()$Date,df_var_2()$Date)),by="day"))
    df_1 = left_join(df,declust_var_1(),by="Date")
    df = left_join(df_1,declust_var_2(),by="Date")
    colnames(df) = c("Date",input$var1_name,input$var2_name)
    return(df)
  })
  
  #Copula analysis
  output$cop_threshold_plot_var1 <- renderPlot({
    print(head(data_detrend_df()))
    print(head(data_decl_df()))
    Copula_Threshold_2D_Lag(
      Data_Detrend = data_detrend_df()[,-1], Data_Declust = data_decl_df()[,-1],
      u1 = seq(input$min_quantile_var1, input$max_quantile_var1, input$interval_quantile_var1), u2 = NA, PLOT = TRUE,
      Lag_Backward_Var1 = input$lag_backward_var1, Lag_Forward_Var1 = input$lag_forward_var1,
      Lag_Backward_Var2 = NA, Lag_Forward_Var2 = NA,
      x_lim_min = input$x_lim_min_var1,x_lim_max = input$x_lim_max_var1,
      y_lim_min = input$y_lim_min_var1,y_lim_max = input$y_lim_max_var1,
      GAP = 0.1)
    
  })
  
  output$cop_threshold_plot_var2 <- renderPlot({
    Copula_Threshold_2D_Lag(
      Data_Detrend = data_detrend_df()[,-1], Data_Declust = data_decl_df()[,-1],
      u1 = NA, u2 = seq(input$min_quantile_var2, input$max_quantile_var2, input$interval_quantile_var2), PLOT = TRUE,
      Lag_Backward_Var1 = NA, Lag_Forward_Var1 = NA,
      Lag_Backward_Var2 = input$lag_backward_var2, Lag_Forward_Var2 = input$lag_forward_var2,
      x_lim_min = input$x_lim_min_var2,x_lim_max = input$x_lim_max_var2,
      y_lim_min = input$y_lim_min_var2,y_lim_max = input$y_lim_max_var2,
      GAP=0.1)
  })
  
  #Marginal distributions
  output$gpd.method1 = renderUI({
    selectInput("gpd_var1_method", NULL, c(Window = "Solari", Runs = "Standard"), selected = input$decvar1)
    
  })
  
  output$gpd.method2 = renderUI({
    selectInput("gpd_var2_method", NULL, c(Window = "Solari", Runs = "Standard"), selected = input$decvar2)
    
  })
  
  output$iso.gpd1 = renderUI({
    numericInput("iso_gpd_var1_threshold", NULL, input$gpd_var1_threshold) 
  })
  
  output$iso.gpd2 = renderUI({
    numericInput("iso_gpd_var2_threshold", NULL, input$gpd_var2_threshold) 
  })
  
  output$iso.method1 = renderUI({
    selectInput("iso_gpd_var1_method", NULL, c(Window = "Solari", Runs = "Standard"), selected = input$decvar1)
  })
  
  output$iso.method2 = renderUI({
    selectInput("iso_gpd_var2_method", NULL, c(Window = "Solari", Runs = "Standard"), selected = input$decvar2)
  })
  
  output$gpd_plot_var1 <- renderPlot({
    #Ensure all necessary inputs and data are available 
    req(declust_var_1(), detrend_var_1(), input$gpd_var1_threshold, input$gpd_var1_rate, input$gpd_var1_method, input$var1_name)
    GPD_Fit(Data=declust_var_1()[,2], Data_Full=detrend_var_1()[,2], u = input$gpd_var1_threshold, Thres = NA, mu = input$gpd_var1_rate, 
            GPD_Bayes = TRUE, Method = input$gpd_var1_method, min.RI = 1, max.RI = 100, 
            PLOT = TRUE, xlab_hist = input$var1_name, y_lab = input$var1_name)
  })
  
  output$gpd_plot_var2 <- renderPlot({
    #Ensure all necessary inputs and data are available 
    req(declust_var_2(), detrend_var_2(), input$gpd_var2_threshold, input$gpd_var2_rate, input$gpd_var2_method, input$var2_name)
    GPD_Fit(Data=declust_var_2()[,2], Data_Full=detrend_var_2()[,2], u = input$gpd_var2_threshold, Thres = NA, mu = input$gpd_var2_rate, 
            GPD_Bayes = TRUE, Method = input$gpd_var2_method, min.RI = 1, max.RI = 100, 
            PLOT = TRUE, xlab_hist = input$var2_name, y_lab = input$var2_name)
  })
  
  
  
  output$gpd_text_var1 <- renderUI({
    gpd_var1 = GPD_Fit(Data=declust_var_1()[,2], Data_Full=detrend_var_1()[,2], u = input$gpd_var1_threshold, Thres = NA, mu = input$gpd_var1_rate, 
                       GPD_Bayes = TRUE, Method = input$gpd_var1_method, min.RI = 1, max.RI = 100, 
                       PLOT = FALSE, xlab_hist = input$var1_name, y_lab = input$var1_name)
    
    str1 = paste("Threshold", round(gpd_var1$Threshold,2))
    str2 = paste("sigma:", round(gpd_var1$sigma,2))
    str3 = paste("xi:", round(gpd_var1$xi,2))
    str4 = paste("sigma SE:", round(gpd_var1$sigma.SE,2))
    str5 = paste("xi SE:", round(gpd_var1$xi.SE,2))
    HTML(paste(str1, str2, str3, str4, str5, sep = '<br/>'))
    
  })
  
  
  output$trunc_var2 <- renderUI({
    tagList(
      helpText("this will show to input two numerics to be added"),
      checkboxGroupInput(
        selected = c("BS","Exp","Gam(2)","Gam(3)","GamMix(2)","GamMix(3)","LNorm","TNorm","Twe","Weib"),
        inputId = "var2_trunc",
        choices = c("Birnbaum Saunders" = "BS","Exponential" = "Exp", "Two-parameter gamma" = "Gam(2)",
                    "Three-parameter gamma" = "Gam(3)","Mixed two-parameter gamma" = "GamMix(2)", "Mixed three-parameter gamma" = "GamMix(3)",
                    "Lognormal" = "LNorm","Truncated normal" = "TNorm","Tweedie" = "Twe","Weibull" = "Weib"),
        label = NULL)
    )
  })
  
  
  output$nontrunc_var2 <- renderUI({
    tagList(
      helpText("this will show to input two numerics to be added"),
      checkboxGroupInput(
        selected = c("Gaus","Gum","Lapl","Logis","RGum"),
        inputId = "var2_nontrunc",
        label = NULL,
        choices = c("Gaussian" = "Gaus","Gumbel" = "Gum","Laplace" = "Lapl","Logistic" = "Logis","Reverse Gumbel" = "RGum")
      )
    )
  })
  
  output$diag_non_con_Plot_var2 <- renderPlot({
    
    x = Con_Sampling_2D_Lag(Data_Detrend = data_detrend_df()[,-1], Data_Declust = data_decl_df()[,-1],
                            Con_Variable = input$var1_name,
                            u = input$gpd_var1_threshold, Thres = NA, 
                            Lag_Backward = input$lag_backward_var1, Lag_Forward = input$lag_forward_var1)$Data[,2]
    
    var = (1:5)[-match(input$var2_nontrunc,c("Gaus","Gum","Lapl","Logis","RGum"))]
    Diag_Non_Con(Data=x,
                 Omit=c("Gaus","Gum","Lapl","Logis","RGum")[var],x_lab="Data",
                 y_lim_min=0,y_lim_max=1)
  })
  
  diag_non_con_best_fit_var2 <- reactive({
    
    x = Con_Sampling_2D_Lag(Data_Detrend = data_detrend_df()[,-1], Data_Declust = data_decl_df()[,-1],
                            Con_Variable = input$var1_name,
                            u = input$gpd_var1_threshold, Thres = NA, 
                            Lag_Backward = input$lag_backward_var1, Lag_Forward = input$lag_forward_var1)$Data[,2]
    
    var = (1:5)[-match(input$var2_nontrunc,c("Gaus","Gum","Lapl","Logis","RGum"))]
    result = Diag_Non_Con(Data=x,
                          Omit=c("Gaus","Gum","Lapl","Logis","RGum")[var],x_lab="Data",
                          y_lim_min=0,y_lim_max=1)$Best_fit
    return(result)
  })
  
  output$diag_con_trunc_Plot_var2 <- renderPlot({
    
    x = Con_Sampling_2D_Lag(Data_Detrend = data_detrend_df()[,-1], Data_Declust = data_decl_df()[,-1],
                            Con_Variable = input$var1_name,
                            u = input$gpd_var1_threshold, Thres= NA,
                            Lag_Backward = input$lag_backward_var1, Lag_Forward= input$lag_forward_var1)$Data[,2]
    
    var = (1:10)[-match(input$var2_trunc,c("BS","Exp","Gam(2)","Gam(3)","GamMix(2)","GamMix(3)","LNorm","TNorm","Twe","Weib"))]
    Diag_Non_Con_Trunc(Data=x+0.0001,
                       Omit=c("BS","Exp","Gam(2)","Gam(3)","GamMix(2)","GamMix(3)","LNorm","TNorm","Twe","Weib")[var],x_lab=paste(input$var2_name,' [',input$var2_units,']', sep=""),
                       y_lim_min=0,y_lim_max=1)
  })
  
  diag_con_trunc_best_fit_var2 <- reactive({
    
    x = Con_Sampling_2D_Lag(Data_Detrend = data_detrend_df()[,-1], Data_Declust = data_decl_df()[,-1],
                            Con_Variable = input$var1_name,
                            u = input$gpd_var1_threshold, Thres= NA,
                            Lag_Backward = input$lag_backward_var1, Lag_Forward= input$lag_forward_var1)$Data[,2]
    
    var = (1:10)[-match(input$var2_trunc,c("BS","Exp","Gam(2)","Gam(3)","GamMix(2)","GamMix(3)","LNorm","TNorm","Twe","Weib"))]
    result = Diag_Non_Con_Trunc(Data=x+0.0001,
                                Omit=c("BS","Exp","Gam(2)","Gam(3)","GamMix(2)","GamMix(3)","LNorm","TNorm","Twe","Weib")[var],x_lab=paste(input$var2_name,' [',input$var2_units,']', sep=""),
                                y_lim_min=0,y_lim_max=1)$Best_fit
    return(result)
  })
  
  output$trunc_var1 <- renderUI({
    tagList(
      helpText("this will show to input two numerics to be added"),
      checkboxGroupInput(
        selected = c("BS","Exp","Gam(2)","Gam(3)","GamMix(2)","GamMix(3)","LNorm","TNorm","Twe","Weib"),
        inputId = "var1_trunc",
        choices = c("Birnbaum Saunders" = "BS","Exponential" = "Exp", "Two-parameter gamma" = "Gam(2)",
                    "Three-parameter gamma" = "Gam(3)","Mixed two-parameter gamma" = "GamMix(2)", "Mixed three-parameter gamma" = "GamMix(3)",
                    "Lognormal" = "LNorm","Truncated normal" = "TNorm","Tweedie" = "Twe", "Weibull" = "Weib"),
        label = NULL)
    )
  })
  
  
  output$nontrunc_var1 <- renderUI({
    tagList(
      helpText("this will show to input two numerics to be added"),
      checkboxGroupInput(
        selected = c("Gaus","Gum","Lapl","Logis","RGum"),
        inputId = "var1_nontrunc",
        label = NULL,
        choices = c("Gaussian" = "Gaus","Gumbel" = "Gum","Laplace" = "Lapl","Logistic" = "Logis","Reverse Gumbel" = "RGum")
      )
    )
  })
  
  output$diag_non_con_Plot_var1 <- renderPlot({
    
    x = Con_Sampling_2D_Lag(Data_Detrend = data_detrend_df()[,-1], Data_Declust = data_decl_df()[,-1],
                            Con_Variable = input$var2_name,
                            u = input$gpd_var2_threshold, Thres= NA, 
                            Lag_Backward = input$lag_backward_var2, Lag_Forward = input$lag_forward_var2)$Data[,1]
    
    var = (1:5)[-match(input$var1_nontrunc,c("Gaus","Gum","Lapl","Logis","RGum"))]
    Diag_Non_Con(Data=x,
                 Omit=c("Gaus","Gum","Lapl","Logis","RGum")[var],x_lab="Data",
                 y_lim_min=0,y_lim_max=1)
  })
  
  diag_non_con_best_fit_var1 <- reactive({
    
    x = Con_Sampling_2D_Lag(Data_Detrend = data_detrend_df()[,-1], Data_Declust = data_decl_df()[,-1],
                            Con_Variable = input$var2_name,
                            u = input$gpd_var2_threshold, Thres= NA, 
                            Lag_Backward = input$lag_backward_var2, Lag_Forward = input$lag_forward_var2)$Data[,1]
    
    var = (1:5)[-match(input$var1_nontrunc,c("Gaus","Gum","Lapl","Logis","RGum"))]
    result = Diag_Non_Con(Data=x,
                          Omit=c("Gaus","Gum","Lapl","Logis","RGum")[var],x_lab="Data",
                          y_lim_min=0,y_lim_max=1)$Best_fit
    return(result)
  })
  
  output$diag_con_trunc_Plot_var1 <- renderPlot({
    
    x = Con_Sampling_2D_Lag(Data_Detrend = data_detrend_df()[,-1], Data_Declust = data_decl_df()[,-1],
                            Con_Variable = input$var2_name, 
                            u = input$gpd_var2_threshold, Thres = NA, 
                            Lag_Backward = input$lag_backward_var2, Lag_Forward = input$lag_forward_var2)$Data[,1]
    
    var = (1:10)[-match(input$var1_trunc,c("BS","Exp","Gam(2)","Gam(3)","GamMix(2)","GamMix(3)","LNorm","TNorm","Twe","Weib"))]
    Diag_Non_Con_Trunc(Data=x+0.0001,
                       Omit=c("BS","Exp","Gam(2)","Gam(3)","GamMix(2)","GamMix(3)","LNorm","TNorm","Twe","Weib")[var],x_lab=paste(input$var2_name,' [',input$var2_units,']', sep=""),
                       y_lim_min=0,y_lim_max=1)
  })
  
  diag_con_trunc_best_fit_var1 <- reactive({
    
    x = Con_Sampling_2D_Lag(Data_Detrend = data_detrend_df()[,-1], Data_Declust = data_decl_df()[,-1],
                            Con_Variable = input$var2_name, 
                            u = input$gpd_var2_threshold, Thres = NA, 
                            Lag_Backward = input$lag_backward_var2, Lag_Forward = input$lag_forward_var2)$Data[,1]
    
    var = (1:10)[-match(input$var1_trunc,c("BS","Exp","Gam(2)","Gam(3)","GamMix(2)","GamMix(3)","LNorm","TNorm","Twe","Weib"))]
    result = Diag_Non_Con_Trunc(Data=x+0.0001,
                                Omit=c("BS","Exp","Gam(2)","Gam(3)","GamMix(2)","GamMix(3)","LNorm","TNorm","Twe","Weib")[var],x_lab=paste(input$var2_name,' [',input$var2_units,']', sep=""),
                                y_lim_min=0,y_lim_max=1)$Best_fit
    
    return(result)
  })
  
  
  #Isolines
  output$iso_trunc_var2 <- renderUI({
    checkboxGroupInput(
      selected = diag_con_trunc_best_fit_var2(),
      inputId = "iso_var2_non_ex_dist",
      choices = c("Birnbaum Saunders" = "BS","Exponential" = "Exp", "Two-parameter gamma" = "Gam(2)",
                  "Three-parameter gamma" = "Gam(3)","Mixed two-parameter gamma" = "GamMix(2)", "Mixed three-parameter gamma" = "GamMix(3)",
                  "Lognormal" = "LNorm","Truncated normal" = "TNorm","Tweedie" = "Twe", "Weibull" = "Weib"),
      label = NULL
    )
  })
  
  
  output$iso_nontrunc_var2 <- renderUI({
    checkboxGroupInput(
      selected = diag_non_con_best_fit_var2(),
      inputId = "iso_var2_non_ex_dist",
      label = NULL,
      choices = c("Gaussian" = "Gaus","Gumbel" = "Gum","Laplace" = "Lapl","Logistic" = "Logis","Reverse Gumbel" = "RGum")
    )
  })
  
  
  output$iso_trunc_var1 <- renderUI({
    checkboxGroupInput(
      selected = diag_con_trunc_best_fit_var1(),
      inputId = "iso_var1_non_ex_dist",
      choices = c("Birnbaum Saunders" = "BS","Exponential" = "Exp", "Two-parameter gamma" = "Gam(2)",
                  "Three-parameter gamma" = "Gam(3)","Mixed two-parameter gamma" = "GamMix(2)", "Mixed three-parameter gamma" = "GamMix(3)",
                  "Lognormal" = "LNorm","Truncated normal" = "TNorm","Tweedie" = "Twe", "Weibull" = "Weib"),
      label = NULL
    )
  })
  
  
  output$iso_nontrunc_var1 <- renderUI({
    checkboxGroupInput(
      selected = diag_non_con_best_fit_var1(),
      inputId = "iso_var1_non_ex_dist",
      label = NULL,
      choices = c("Gaussian" = "Gaus","Gumbel" = "Gum","Laplace" = "Lapl","Logistic" = "Logis","Reverse Gumbel" = "RGum")
    )
  })
  
  output$isoline <- renderPlot({
    #Conditional samples + copula
    cop_var1 = Copula_Threshold_2D_Lag(Data_Detrend = data_detrend_df()[,-1], Data_Declust = data_decl_df()[,-1],
                                       u1 = input$iso_gpd_var1_threshold, u2= NA,PLOT=FALSE, 
                                       Lag_Backward_Var1 = input$lag_backward_var1, Lag_Forward_Var1 = input$lag_forward_var1,
                                       Lag_Backward_Var2 = NA, Lag_Forward_Var2 = NA)$Copula_Family_Var1
    
    cop_var2 = Copula_Threshold_2D_Lag(Data_Detrend = data_detrend_df()[,-1], Data_Declust = data_decl_df()[,-1],
                                       u1 = NA, u2 = input$iso_gpd_var2_threshold, PLOT=FALSE,
                                       Lag_Backward_Var1 = NA, Lag_Forward_Var1 = NA,
                                       Lag_Backward_Var2 = input$lag_backward_var2, Lag_Forward_Var2 = input$lag_forward_var2)$Copula_Family_Var2
    
    con_samp_var1 = Con_Sampling_2D_Lag(Data_Detrend = data_detrend_df()[,-1], Data_Declust = data_decl_df()[,-1],
                                        Con_Variable = input$var1_name,
                                        u = input$iso_gpd_var1_threshold, Thres = NA,
                                        Lag_Backward = input$lag_backward_var1, Lag_Forward = input$lag_forward_var1)$Data
    
    con_samp_var2 = Con_Sampling_2D_Lag(Data_Detrend = data_detrend_df()[,-1], Data_Declust = data_decl_df()[,-1],
                                        Con_Variable = input$var2_name,
                                        u = input$iso_gpd_var2_threshold, Thres = NA,
                                        Lag_Backward = input$lag_backward_var2, Lag_Forward = input$lag_forward_var2)$Data
    
    con_samp_var1 = con_samp_var1 + 0.001
    con_samp_var2 = con_samp_var2 + 0.001
    
    #Find events in both samples 
    x.con_samp_var1 = Con_Sampling_2D_Lag(Data_Detrend = data_detrend_df()[,-1], Data_Declust = data_decl_df()[,-1],
                                          Con_Variable = input$var1_name,
                                          u = input$iso_gpd_var1_threshold, Thres = NA,
                                          Lag_Backward = input$lag_backward_var1, Lag_Forward = input$lag_forward_var1)$x.con
    
    x.con_samp_var2 = Con_Sampling_2D_Lag(Data_Detrend = data_detrend_df()[,-1], Data_Declust = data_decl_df()[,-1],
                                          Con_Variable = input$var2_name,
                                          u = input$iso_gpd_var2_threshold, Thres = NA,
                                          Lag_Backward = input$lag_backward_var2, Lag_Forward = input$lag_forward_var2)$x.con
    
    z1 = numeric(length(x.con_samp_var2))
    z = numeric(length(x.con_samp_var1))
    for(i in 1:length(x.con_samp_var1)){
      for(j in 1:length(x.con_samp_var2)){
        z1[j] <- ifelse(x.con_samp_var1[i] > (x.con_samp_var2[j]-input$lag_backward_var2) & x.con_samp_var1[i] < (x.con_samp_var2[j]+input$lag_forward_var2),1,0)
      }
      z[i] = ifelse(sum(z1)>0,1,0)
    }
    
    N_both = sum(z)
    
    
    #Marginal GPD distributions
    var1_th = quantile(data_detrend_df()[,2],input$iso_gpd_var1_threshold,na.rm=T)
    var1_declust = data_detrend_df()[,2][which(data_decl_df()[,2]>=var1_th)]
    var1_gpd= GPD_Fit(Data = var1_declust, Data_Full = na.omit(data_detrend_df()[,2]), u=NA, Thres =var1_th , Method = input$gpd_var1_method)
    var1_rate = length(var1_declust)/(length(na.omit(data_detrend_df()[,2]))/input$all_rate)
    var1_gpd$Rate = var1_rate
    
    var2_th = quantile(data_detrend_df()[,3],input$iso_gpd_var2_threshold,na.rm=T)
    var2_declust = data_decl_df()[,3][which(data_decl_df()[,3]>=var2_th)]
    var2_gpd = GPD_Fit(Data = var2_declust, Data_Full = na.omit(data_detrend_df()[,3]), u=NA, Thres =var2_th, Method = input$gpd_var2_method)
    var2_rate = length(var2_declust)/(length(na.omit(data_detrend_df()[,3]))/input$all_rate)
    var2_gpd$Rate = var2_rate
    
    req(data_detrend_df(), con_samp_var1, con_samp_var2, input$iso_gpd_var1_threshold, input$iso_gpd_var2_threshold, N_both, cop_var1, cop_var2, input$iso_var2_non_ex_dist, input$iso_var1_non_ex_dist, input$var1_name, input$var2_name, var1_gpd, var1_rate, var2_gpd, var2_rate, input$iso_grid_x_min, input$iso_grid_x_max, input$iso_grid_y_min, input$iso_grid_y_max, input$iso_grid_x_res, input$iso_grid_y_res, input$all_rate, input$iso_x_min, input$iso_x_max, input$iso_y_min, input$iso_y_max, input$iso_rp, input$iso_N, input$N_ensemble, input$var1_units, input$var2_units)
    
    Design.event<-Design_Event_2D_Grid(Data=data_detrend_df()[,-1],
                                       Data_Con1=con_samp_var1,Data_Con2=con_samp_var2, 
                                       u1=input$iso_gpd_var1_threshold, u2=input$iso_gpd_var2_threshold, 
                                       N_Both = N_both,
                                       Copula_Family1=cop_var1,Copula_Family2=cop_var2,
                                       Marginal_Dist1=input$iso_var2_non_ex_dist, Marginal_Dist2=input$iso_var1_non_ex_dist, 
                                       Con1 = input$var1_name, Con2 = input$var2_name, 
                                       GPD1 = var1_gpd,
                                       Rate_Con1 = var1_rate,
                                       GPD2 = var2_gpd,
                                       Rate_Con2 = var2_rate,
                                       Grid_x_min = input$iso_grid_x_min,
                                       Grid_x_max = input$iso_grid_x_max,
                                       Grid_y_min = input$iso_grid_y_min,
                                       Grid_y_max = input$iso_grid_y_max,
                                       Grid_x_interval = input$iso_grid_x_res,
                                       Grid_y_interval = input$iso_grid_y_res,
                                       mu = input$all_rate, 
                                       x_lim_min = input$iso_x_min,
                                       x_lim_max = input$iso_x_max, 
                                       y_lim_min = input$iso_y_min,
                                       y_lim_max = input$iso_y_max,
                                       RP=input$iso_rp,
                                       Plot_Quantile_Isoline = FALSE,
                                       N=input$iso_N,
                                       N_Ensemble = input$N_ensemble,
                                       End = T,
                                       x_lab = paste(input$var1_name,' [',input$var1_units,']',sep=""),
                                       y_lab = paste(input$var2_name,' [',input$var2_units,']',sep=""))
  })
  
  observeEvent(input$to_detrend_tab, {
    updateNavbarPage(session, "bfa",selected = "Detrend")
  })
  
  observeEvent(input$to_detrend_tab2, {
    updateTabsetPanel(session, "Detrend_tabs",selected = "detrend_var_2")
  })
  
  
  observeEvent(input$to_decluster_tab, {
    updateNavbarPage(session, "bfa", selected = "Decluster")
  })
  
  observeEvent(input$to_declust_tab2, {
    updateTabsetPanel(session, "Decluster_tabs",selected = "decluster_var_2")
  })
  
  observeEvent(input$to_margin_tab, {
    updateNavbarPage(session, "bfa",selected = "Marginal distributions")
  })
  
  observeEvent(input$to_non_extreme_var2_tab, {
    updateTabsetPanel(session, "Sample_conditioned_on_var_1",selected = "non_extreme_var2")
  })
  
  observeEvent(input$to_con_samp_2_tab, {
    updateTabsetPanel(session, "Conditioned_samples",selected = "Sample_conditioned_on_var2_mar_dist")
  })
  
  observeEvent(input$to_non_extreme_var1_tab, {
    updateTabsetPanel(session, "Sample_conditioned_on_var2",selected = "non_extreme_var1")
  })
  
  observeEvent(input$to_dependence_structure_tab, {
    updateNavbarPage(session, "bfa",selected = "Dependence structure")
  })
  
  observeEvent(input$sample_conditioned_on_var2_tab, {
    updateTabsetPanel(session, "Sample_conditioned_on_rainfall", selected = "copula_tab_2")
  })
  
  observeEvent(input$to_isolines_tab, {
    updateNavbarPage(session, "bfa" ,selected = "Isolines")
  })
}