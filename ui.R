#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinythemes)
# library(RColorBrewer)
library(dplyr)
# library(VGAM)
# library(texmex)
library(fitdistrplus)
# library(ks)
# library(copula)
# library(VineCopula)
library(MultiHazard)
library(gamlss)
library(gamlss.mx)



# Define UI for application that draws a histogram
fluidPage(
    theme = shinytheme("cerulean"),
    div(style = "width: 50px; margin: 10px;", shinythemes::themeSelector()),
    tags$head(
        tags$style(HTML("
      /* Make the navbar blue */
      .navbar-default {
        background-color: #1976d2;
        border-color: #165c96;
      }
      .navbar-default .navbar-brand,
      .navbar-default .navbar-text,
      .navbar-default .navbar-nav > li > a {
        color: #fff;
        font-weight: 600;
      }
    "))
    ),
    navbarPage(
        title = strong(span(icon("earth"), "MultiHazard Analysis")),
        id = "bfa",
        tabPanel(
            title = span(icon("database"), "Data"),
            id = "data",
            # Application title
            titlePanel("Data"),

            # Sidebar with a slider input for number of bins
            sidebarLayout(
                sidebarPanel(
                    # select plot type
                    h4(strong("Variable 1")),
                    fileInput("file_var_1", "Choose CSV File", accept = ".csv"),
                    textInput("var1_name", "Name:", "Rainfall"),
                    textInput("var1_units", "Units:", "Inches"),
                    br(),
                    br(),
                    h4(strong("Variable 2")),
                    fileInput("file_var_2", "Choose CSV File", accept = ".csv"),
                    textInput("var2_name", "Name:", "Storm surge"),
                    textInput("var2_units", "Units:", "ft NAVD88")
                ),

                # Show a plot of the generated distribution
                mainPanel(
                    tabsetPanel(
                        tabPanel(
                            "Summary",
                            tableOutput("contents_var1"),
                            tableOutput("contents_var2")
                        ),
                        tabPanel(
                            "Plots",
                            plotOutput("dataPlotvar_1"),
                            plotOutput("dataPlotvar_2")
                        )
                    )
                )
            ),
            actionButton("to_detrend_tab", "Continue")
        ),
        tabPanel(
            title = span(icon("chart-line"), "Detrend"),
            id = "detrend",
            # Application title
            titlePanel("Detrending the time series"),
            tabsetPanel(
                id = "Detrend_tabs",
                tabPanel(
                    title = textOutput("var1_name_detrend_text"),
                    id = "detrend_tab_1",
                    value = "detrend_var_1",
                    sidebarLayout(
                        sidebarPanel(
                            h4(strong("Detrend")),
                            helpText("Detrend the time series?"),
                            selectInput("detvar1", NULL,
                                c(Yes = "yes", No = "no"),
                                selected = "no"
                            ),
                            conditionalPanel(
                                "input.detvar1=='yes'",
                                tagList(
                                    h4(strong("Method")),
                                    helpText("Please choose the detrending method"),
                                    selectInput(
                                        "detrendmethodvar1", NULL,
                                        c(Linear = "linear", Window = "window")
                                    )
                                )
                            ),
                            conditionalPanel(
                                "input.detrendmethodvar1=='window'",
                                h4(strong("Window width")),
                                helpText("Please choose width of moving window"),
                                numericInput("detrendwindowwidthvar2", NULL, 10)
                            )
                        ),
                        # Show a plot of the generated distribution
                        mainPanel(
                            conditionalPanel(
                                condition = "input.detvar1 == 'yes'",
                                plotOutput("detrendPlot_var_1"),
                                tableOutput("detrend_table_var1")
                            )
                        )
                    ),
                    actionButton("to_detrend_tab2", "Continue")
                ),
                tabPanel(
                    title = textOutput("var2_name_detrend_text"),
                    value = "detrend_var_2",
                    sidebarLayout(
                        sidebarPanel(
                            h4(strong("Detrend")),
                            helpText("Detrend the time series?"),
                            selectInput("detvar2", NULL,
                                c(Yes = "yes", No = "no"),
                                selected = "yes"
                            ),
                            # select plot type
                            conditionalPanel(
                                "input.detvar2=='yes'",
                                tagList(
                                    h4(strong("Method")),
                                    helpText("Please choose the detrending method"),
                                    selectInput(
                                        "detrendmethodvar2", NULL,
                                        c(Linear = "linear", Window = "window")
                                    )
                                ),
                                conditionalPanel(
                                    "input.detrendmethodvar2=='window'",
                                    h4(strong("Window width")),
                                    helpText("Please choose width of moving window"),
                                    numericInput("detrendwindowwidthvar2", NULL, 10)
                                )
                            )
                        ),
                        # Show a plot of the generated distribution
                        mainPanel(
                            conditionalPanel(
                                condition = "input.detvar2 == 'yes'",
                                plotOutput("detrendPlot_var_2"),
                                tableOutput("detrend_table_var2")
                            )
                        )
                    ),
                    actionButton("to_decluster_tab", "Continue")
                )
            )
        ),
        tabPanel(
            title = span(icon("scissors"), "Decluster"),
            id = "decluster",
            titlePanel("Declustering the time series"),
            tabsetPanel(
                id = "Decluster_tabs",
                tabPanel(
                    title = textOutput("var1_name_decl_text"),
                    value = "decluster_var_1",
                    sidebarPanel(
                        h4(strong("Method")),
                        helpText("Please choose the declustering method"),
                        selectInput("decvar1", NULL,
                            selected = "",
                            c(Window = "Solari", Runs = "Standard")
                        ),
                        conditionalPanel(
                            condition = "input.decvar1 == 'Solari'",
                            tagList(
                                h4(strong("Window width")),
                                helpText("Please choose declustering window width"),
                                numericInput("decwindowwidthvar1", NULL, "10")
                            )
                        ),
                        conditionalPanel(
                            condition = "input.decvar1 == 'Standard'",
                            tagList(
                                h4(strong("Threshold")),
                                helpText("Please choose the quantile threshold"),
                                sliderInput("declrunsthresholdvar1",
                                    NULL,
                                    min = 0.5,
                                    max = 0.99,
                                    value = 0.95
                                ),
                                h4(strong("Separation criterion")),
                                helpText("Please choose the separation criterion"),
                                numericInput("declrunssepcritvar1", NULL, "3"),
                                h4(strong("Rate")),
                                helpText("Please specify the temporal resolution of the data in terms of years"),
                                numericInput("declrunsratevar1", NULL, 365.25)
                            )
                        )
                    ),
                    mainPanel(
                        plotOutput("dec_plot_var1"),
                        tableOutput("dec_text_var1")
                    ),
                    actionButton("to_declust_tab2", "Continue")
                ),
                tabPanel(
                    title = textOutput("var2_name_decl_text"),
                    value = "decluster_var_2",
                    sidebarPanel(
                        h4(strong("Method")),
                        helpText("Please choose the declustering method"),
                        selectInput("decvar2", NULL,
                            selected = "",
                            c(Window = "Solari", Runs = "Standard")
                        ),
                        conditionalPanel(
                            condition = "input.decvar2 == 'Solari'",
                            tagList(
                                h4(strong("Window width")),
                                helpText("Please choose declustering window width"),
                                numericInput("decwindowwidthvar2", NULL, "10")
                            )
                        ),
                        conditionalPanel(
                            condition = "input.decvar2 == 'Standard'",
                            tagList(
                                h4(strong("Threshold")),
                                helpText("Please choose the quantile threshold"),
                                sliderInput("declrunsthresholdvar2",
                                    NULL,
                                    min = 0.5,
                                    max = 0.99,
                                    value = 0.95
                                ),
                                h4(strong("Separation criterion")),
                                helpText("Please choose the separation criterion"),
                                numericInput("declrunssepcritvar2", NULL, "3"),
                                h4(strong("Rate")),
                                helpText("Please specify the temporal resolution of the data in terms of years"),
                                numericInput("declrunsratevar2", NULL, 365.25)
                            )
                        )
                    ),
                    mainPanel(
                        plotOutput("dec_plot_var2"),
                        tableOutput("dec_text_var2")
                    ),
                    actionButton("to_dependence_structure_tab", "Continue")
                )
            )
        ),
        tabPanel(
            title = span(icon("project-diagram"), "Dependence structure"),
            tabsetPanel(
                id = "Sample_conditioned_on_rainfall",
                tabPanel(
                    title = textOutput("sample_conditioned_on_var1_dep_str"),
                    value = "copula_tab_1",
                    sidebarLayout(
                        sidebarPanel(
                            h4(strong("Quantile threshold")),
                            helpText(textOutput("dep_str_value_var1")),
                            numericInput("min_quantile_var1", "Miniumum", 0.85),
                            numericInput("max_quantile_var1", "Maximum", 0.99),
                            numericInput("interval_quantile_var1", "Interval", 0.01),
                            h4(strong("Time lag")),
                            helpText("Please choose a time lag for the conditional sampling"),
                            numericInput("lag_backward_var1", "Negative (backward)", 3),
                            numericInput("lag_forward_var1", "Positive (forward)", 3),
                            h4(strong("Axes")),
                            helpText("Please choose axis limits (optional)"),
                            h5(strong("x-axis")),
                            numericInput("x_lim_min_var1", "Minimum", ""),
                            numericInput("x_lim_max_var1", "Maximum", ""),
                            h5(strong("y-axis")),
                            numericInput("y_lim_min_var1", "Minimum", -1),
                            numericInput("y_lim_max_var1", "Maximum", 1)
                        ),
                        # Show a plot of the generated distribution
                        mainPanel(
                            plotOutput("cop_threshold_plot_var1")
                        )
                    ),
                    actionButton("sample_conditioned_on_var2_tab", "Continue")
                ),
                tabPanel(
                    title = textOutput("sample_conditioned_on_var2_dep_str"),
                    value = "copula_tab_2",
                    sidebarLayout(
                        sidebarPanel(
                            h4(strong("Quantile threshold")),
                            helpText(textOutput("dep_str_value_var2")),
                            numericInput("min_quantile_var2", "Miniumum", 0.85),
                            numericInput("max_quantile_var2", "Maximum", 0.99),
                            numericInput("interval_quantile_var2", "Interval", 0.01),
                            h4(strong("Time lag")),
                            helpText("Please choose a time lag for the conditional sampling"),
                            numericInput("lag_backward_var2", "Negative (backward)", 3),
                            numericInput("lag_forward_var2", "Positive (Forward)", 3),
                            h4(strong("Axes")),
                            helpText("Please choose axis limits (optional)"),
                            h5(strong("x-axis")),
                            numericInput("x_lim_min_var2", "Minimum", ""),
                            numericInput("x_lim_max_var2", "Maximum", ""),
                            h5(strong("y-axis")),
                            numericInput("y_lim_min_var2", "Minimum", -1),
                            numericInput("y_lim_max_var2", "Maximum", 1)
                        ),
                        # Show a plot of the generated distribution
                        mainPanel(
                            plotOutput("cop_threshold_plot_var2")
                        )
                    ),
                    actionButton("to_margin_tab", "Continue")
                )
            ),
        ),
        tabPanel(
            title = span(icon("wave-square"), "Marginal distributions"),
            tabsetPanel(
                id = "Conditioned_samples",
                tabPanel(
                    textOutput("sample_conditioned_on_var1_mar_dist"),
                    tabsetPanel(
                        id = "Sample_conditioned_on_var_1",
                        tabPanel(
                            title = textOutput("gpd_var1_text"),
                            value = "extreme_var1",
                            sidebarLayout(
                                sidebarPanel(
                                    h4(strong("Method")),
                                    helpText("Please specify the method used to decluster the time series"),
                                    uiOutput("gpd.method1"),
                                    h4(strong("Quantile threshold")),
                                    helpText("Please choose the threshold above which observations are fit to a GPD. Threshold should be higher than any threshold specified in the declustering step."),
                                    sliderInput("gpd_var1_threshold",
                                        NULL,
                                        min = 0.5,
                                        max = 0.99,
                                        value = 0.95
                                    ),
                                    h4(strong("Rate")),
                                    helpText("Please specify the temporal resolution of the data in terms of years"),
                                    numericInput("gpd_var1_rate", NULL, 365.25),
                                ),
                                mainPanel(
                                    tabsetPanel(
                                        tabPanel(
                                            title = "Plot",
                                            plotOutput("gpd_plot_var1")
                                        ),
                                        tabPanel(
                                            title = "Parameters",
                                            htmlOutput("gpd_text_var1")
                                        )
                                    )
                                )
                            ),
                            actionButton("to_non_extreme_var2_tab", "Continue")
                        ),
                        tabPanel(
                            title = textOutput("non_extreme_var2_text"),
                            value = "non_extreme_var2",
                            sidebarLayout(
                                sidebarPanel(
                                    h4(strong("Distribution type")),
                                    helpText("Please specify the type of distributions to test. For samples with no negative values (e.g. rainfall totals) choose truncated. For unbounded samples choose non-truncated."),
                                    selectInput("dist_type_non_extreme_var2", NULL, choices = c("Truncated", "Non-truncated"), selected = "Non-truncated"),
                                    helpText("Please choose which distributions of the specified type to test"),
                                    conditionalPanel("input.dist_type_non_extreme_var2=='Truncated'", uiOutput("trunc_var2")),
                                    conditionalPanel("input.dist_type_non_extreme_var2=='Non-truncated'", uiOutput("nontrunc_var2"))
                                ),
                                # Show a plot of the generated distribution
                                mainPanel(
                                    conditionalPanel(
                                        condition = "input.dist_type_non_extreme_var2 == 'Non-truncated'",
                                        plotOutput("diag_non_con_Plot_var2")
                                    ),
                                    conditionalPanel(
                                        condition = "input.dist_type_non_extreme_var2 == 'Truncated'",
                                        plotOutput("diag_con_trunc_Plot_var2")
                                    )
                                )
                            ),
                            actionButton("to_con_samp_2_tab", "Continue")
                        )
                    )
                ),
                tabPanel(textOutput("sample_conditioned_on_var2_mar_dist"),
                    value = "Sample_conditioned_on_var2_mar_dist",
                    tabsetPanel(
                        id = "Sample_conditioned_on_var2",
                        tabPanel(
                            title = textOutput("gpd_var2_text"),
                            sidebarPanel(
                                h4(strong("Method")),
                                helpText("Please specify the method used to decluster the time series"),
                                uiOutput("gpd.method2"),
                                h4(strong("Quantile threshold")),
                                helpText("Please choose the threshold above which observations are fit to a GPD. Threshold should be higher than any threshold specified in the declustering step."),
                                sliderInput("gpd_var2_threshold",
                                    NULL,
                                    min = 0.5,
                                    max = 0.99,
                                    value = 0.95
                                ),
                                h4(strong("Rate")),
                                helpText("Please specify the temporal resolution of the data in terms of years"),
                                numericInput("gpd_var2_rate", NULL, 365.25)
                            ),
                            mainPanel(
                                # tabsetPanel(
                                tabPanel(
                                    title = "Plot",
                                    plotOutput("gpd_plot_var2")
                                ),
                                # tabPanel(title = "Parameters",
                                #          htmlOutput("gpd_text_var2"))
                                # )
                            ),
                            actionButton("to_non_extreme_var1_tab", "Continue")
                        ),
                        tabPanel(
                            title = textOutput("non_extreme_var1_text"),
                            value = "non_extreme_var1",
                            sidebarLayout(
                                sidebarPanel(
                                    h4(strong("Distribution type")),
                                    helpText("Please specify the type of distributions to test. For samples with no negative values (e.g. rainfall totals) choose truncated. For unbounded samples choose non-truncated."),
                                    selectInput("dist_type_non_extreme_var1", NULL, choices = c("Truncated", "Non-truncated"), selected = "Truncated"),
                                    helpText("Please choose which distributions of the specified type to test"),
                                    conditionalPanel("input.dist_type_non_extreme_var1=='Truncated'", uiOutput("trunc_var1")),
                                    conditionalPanel("input.dist_type_non_extreme_var1=='Non-truncated'", uiOutput("nontrunc_var1"))
                                ),
                                # Show a plot of the generated distribution
                                mainPanel(
                                    conditionalPanel(
                                        condition = "input.dist_type_non_extreme_var1 == 'Non-truncated'",
                                        plotOutput("diag_non_con_Plot_var1")
                                    ),
                                    conditionalPanel(
                                        condition = "input.dist_type_non_extreme_var1 == 'Truncated'",
                                        plotOutput("diag_con_trunc_Plot_var1")
                                    )
                                )
                            ),
                            actionButton("to_isolines_tab", "Continue")
                        )
                    )
                )
            )
        ),
        tabPanel(
            title = span(icon("chart-area"), "Isolines"),
            sidebarLayout(
                sidebarPanel(
                    h3(textOutput("sample_conditioned_on_var1_iso")),
                    h5(strong("Quantile threshold")),
                    helpText("Please choose the quantile threshold for the conditional sampling (Default is GPD threshold)"),
                    uiOutput("iso.gpd1"),
                    h4(textOutput("var1_text_iso")),
                    h5(strong("Method")),
                    helpText("Please specify the method used for declustering the time series"),
                    uiOutput("iso.method1"),
                    h4(textOutput("var2_text_iso")),
                    h5(strong("Distribution")),
                    helpText("Please choose a distribution of conditioned storm surge values"),
                    conditionalPanel("input.dist_type_non_extreme_var2=='Truncated'", uiOutput("iso_trunc_var2")),
                    conditionalPanel("input.dist_type_non_extreme_var2=='Non-truncated'", uiOutput("iso_nontrunc_var2")),
                    br(),
                    h3(textOutput("sample_conditioned_on_var2_iso")),
                    h5(strong("Quantile threshold")),
                    helpText("Please choose the quantile threshold for the conditional sampling (Default is GPD threshold)"),
                    uiOutput("iso.gpd2"),
                    h4(textOutput("var2_text_iso_2")),
                    h5(strong("Method")),
                    helpText("Please specify the method used for declustering the time series"),
                    uiOutput("iso.method2"),
                    h4(textOutput("var1_text_iso_2")),
                    h5(strong("Distribution")),
                    helpText("Please choose a distribution of conditioned rainfall values"),
                    conditionalPanel("input.dist_type_non_extreme_var1=='Truncated'", uiOutput("iso_trunc_var1")),
                    conditionalPanel("input.dist_type_non_extreme_var1=='Non-truncated'", uiOutput("iso_nontrunc_var1")),
                    br(),
                    h4(strong("Rate")),
                    helpText("Please specify the temporal resolution of the data in terms of years"),
                    numericInput("all_rate", NULL, 365.25),
                    h4(strong("Return period (years)")),
                    helpText("Please specify a joint return period"),
                    numericInput("iso_rp", NULL, "100"),
                    h4(strong("Contour on the isoline")),
                    helpText("Size of the sample from the fitted joint distributions used to estimate the density along an isoline"),
                    numericInput("iso_N", NULL, "100000"),
                    h4(strong("Event ensemble")),
                    helpText("Number of events to be sampled along the isoline"),
                    numericInput("N_ensemble", NULL, "100"),
                    h4(strong("Axes")),
                    helpText("Please specify axis limits (optional)"),
                    h5(strong("x-axis")),
                    numericInput("iso_x_min", "Minimum", 0),
                    numericInput("iso_x_max", "Maximum", 20),
                    h5(strong("y-axis")),
                    numericInput("iso_y_min", "Minimum", 0),
                    numericInput("iso_y_max", "Maximum", 10),
                    h4(strong("Grid")),
                    helpText("Please specify the grid over which the probabilites are estimates"),
                    h5(strong("x-axis")),
                    numericInput("iso_grid_x_min", "Minimum", 0),
                    numericInput("iso_grid_x_max", "Maximum", 20),
                    numericInput("iso_grid_x_res", "Resolution", 0.1),
                    h5(strong("y-axis")),
                    numericInput("iso_grid_y_min", "Minimum", 0),
                    numericInput("iso_grid_y_max", "Maximum", 10),
                    numericInput("iso_grid_y_res", "Resolution", 0.1)
                ),
                mainPanel(
                    plotOutput("isoline")
                ),
            )
        ),
        tabPanel(
            title = span(icon("info-circle"), "About"),
            h3(span(icon("star"), "About the App")),
            hr(),
            tags$img(
                src = "https://www.frontiersin.org/files/Articles/682759/frym-10-682759-HTML-r1/image_m/figure-1.jpg",
                # width = "400px",
                style = "border:1px solid #ccc; margin-bottom:20px;"
            ),
            helpText("The MultiHazard-Shiny App is an interactive web application for conducting bivariate joint probability analyses. Powered by the MultiHazard R package, this application provides a user-friendly interface for conducting advanced statistical analyses on time series data, supporting researchers and practitioners to explore the relationships between hydrological and meteorological variables, aiding in the assessment of multi-hazard risks such as compound flooding."),
            br(),
            h4(span(icon("bullseye"), "Purpose")),
            hr(),
            helpText("The app helps users visualize and model different hazard scenarios to make informed decisions."),
            br(),
            h4(span(icon("book"), "MultiHazard Package")),
            hr(),
            helpText("This app is built on the MultiHazard package available on GitHub."),
            helpText(a("View on GitHub", href = "https://github.com/rjaneUCF/MultiHazard", target = "_blank")),
            br(),
            h4(span(icon("graduation-cap"), "References")),
            hr(),
            helpText("1. Jane, R., Cadavid, L., Obeysekera, J., and Wahl, T. (2020). Multivariate statistical modelling of the drivers of compound flood events in South Florida, Nat. Hazards Earth Syst. Sci., 20, 2681–2699, https://doi.org/10.5194/nhess-20-2681-2020."),
            br(),
            h4(span(icon("users"), "Developers")),
            hr(),
            helpText("• Robert Jane – r.jane@ucf.edu"),
            helpText("• Javed Ali – javed.ali@ucf.edu"),
            br(),
            h4(span(icon("envelope"), "Contact")),
            hr(),
            helpText("For inquiries or issues, please reach out to developer.one@example.com or submit a ticket on GitHub."),
            br(),
            br(),
        )
    )
)
