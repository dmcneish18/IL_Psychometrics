library(devtools)
devtools::install_github("daattali/shinycssloaders")

###packages
library(shiny)
library(dplyr)
library(tidyr)
library(misty)
library(shinycssloaders)
library(ggplot2)
library(shinyjs)
library(lavaan)
library(lme4)
library(purrr)

######################## Shiny Code

## Code for Interface
ui <- fluidPage(
  
  shinyjs::useShinyjs(),

    # Application title
  titlePanel(h1(HTML("Intensive Longitudinal Psychometrics"), align="center", style="background-color: #005EB8	 ; color: white; padding-top:10px;padding-bottom:10px;"),
             windowTitle = "Intensive Longitudinal Psychometrics"
  ),

    # sidebar for data upload and option selection
      sidebarLayout(
        
        sidebarPanel(
          
          #make labels font color white
          tags$style('label {color:white;}'),
          
          #Slider value font
          tags$style('.irs-grid-text{color:white;background:none; text-shadow:none;}'),
          tags$style('.irs-min {color:white;}'),
          tags$style('.irs-max {color:white;}'),
          
          #make help text font color white;
          tags$style('.help-block {color:white;}'),
          
          #change progress bar to black
          tags$head(tags$style(".progress-bar{background-color:#000000;}")),
          
          #create object that is horizontal line
          tags$head(tags$style(HTML("hr {border-top: 1px solid color=white;}"))), 
          
            #instructions heading
            h3(HTML("<b>Instructions</b>"), align="center", style="color: white; margin-top:0px;"),
            
            #instructions;
            helpText("1. Upload a Dataset in .csv format"),
            helpText("2. The first 5 rows will appear to verify the data loaded correctly."),
            helpText("3. Enter your missing data indicator, scale items, Person ID, and Time variable"), 
            helpText("4. Click Submit one time to begin calculation"),
            helpText("5. Results will appear to the right when calculations are complete"),
            
            #add horizontal line to separate instructions from options
            hr(),
          
            #label for file upload
            h5(HTML("<b>File Upload</b>"), style="color: white; margin-top:25px; margin-bottom:10px;"),
          
            #box to upload data
            fileInput("upload", NULL, accept = c(".csv")),
          
            #reduce space between upload box and ratio buttons
            div(style = "margin-top:-15px"),
          
            #box to input missing data indicator
            textInput("missing", "Missing Data Indicator", "NA"),
          div(style = "margin-bottom:-15px"),
          # Helptext underneath box to select items
          helpText(HTML("<i>Leave at default if no missing data</i>")),
          
          #box to select scale items
          #adapts once  data are uploaded
          selectizeInput(
            "vec1"
            , "Select Scale Items"
            , choices = "Upload Data to Populate"
            , multiple = TRUE),
          #reduce space between upload box and ratio buttons
          div(style = "margin-bottom:-15px"),
          # Helptext underneath box to select items
          helpText(HTML("<i>Press backspace to remove variables</i>")),
          
          selectizeInput(
            "ID"
            , "Select Person ID Variable"
            , choices = "Upload Data to Populate"
            , multiple=TRUE, options=list(maxItems=1)),
      
         
          selectizeInput(
            "Time"
            , "Select Time Variable"
            , choices = "Upload Data to Populate"
            , multiple=TRUE, options=list(maxItems=1)),
          
          div(style="display:inline-block; width:100%; text-align: center;", 
              checkboxInput("fix", label = "Fix between-person  residual variances to 0 in factor analysis", value = FALSE)),
          div(style = "margin-bottom:-10px"),
          helpText(HTML("<i>-Can address convergence issues caused by strong factorial invariance</i>")),
          div(style = "margin-bottom:-10px"),
          helpText(HTML("<i>-Strong factorial invariance is common and implies 0 between-person residual variance</i>")),
         
          div(style = "margin-bottom:50px"),
        
          
            #activation button to begin calculations
            #center "submit" within column
            div(style="display:inline-block; width:100%; text-align: center;", actionButton("go", "Submit")),
            
            #make background blue
            style="background-color: #005EB8	 ;"
        ),
  
   #print first 5 rows of data after it is uploaded
   mainPanel(  tags$style(HTML("
           .tabbable > .nav > li[class=active]    > a {background-color: #005EB8	 ; color:white}")),
   
     #create panels for output
     tabsetPanel(id="Tabs",type="pill",
       
       tabPanel(title="Welcome",
                h4("Welcome to the Intensive Longitudinal Psychometrics Shiny Application!"),
                p(HTML("<br/>")),
                h4(HTML("<ul><li>Intensive longitudinal data are increasingly common")),
                h4(HTML("<ul><li>However, evaluation of psychometric properties of outcome variables is somewhat uncommon")),
                h4(HTML("<ul><li>This software is intended to facilitate application of <b>foundational psychometric methods</b>
                   to intensive longituidnal data.")),
                h4(HTML("<ul><li> This application focuses on methods <b>multiple-item scales </b>")),
                h4(HTML("<ul><li> This application applies methods that are available in open-source software and that can be easily automated")),
                h4(HTML("<ul><li> Other methods exist and this app is not comprehensive")),
                h4(HTML("<ul><li> It is intended to be a <b>starting point</b> to facilitate basic reporting")),
                p(HTML("<br/>")),
                p(HTML("<br/>")),
                h4(HTML("<ul><li>To begin, have your .csv data in the long format (one row per timepoint) with each item response in a different column")),
                ),
       
       tabPanel(value="Data Preview", title=div(style="text-align: center;",HTML(paste("Data", "Preview", sep = "<br/>"))),
                tableOutput("head")
       ),
       
       #tab for multilevel reliability
       tabPanel(value="Multilevel",title =div(style="text-align: center;",HTML(paste("Multilevel", "Reliability", sep = "<br/>"))),
                 fluidRow(wellPanel(h4(HTML("Multilevel Omega <br>(From Multilevel Factor Analysis):")),
                 tableOutput("Table1"),align="center")),
                 
                 fluidRow(wellPanel(h4(HTML("Multilevel Alpha <br>(From Mulitlevel Variance Component Model):")),
                 tableOutput("Table3"),align="center"),
                 tags$details(tags$summary("Note", style = "display: list-item;font-size:14px; ; margin-bottom:8px; font-weight:bold;"),
                              p(HTML("The multilevel alpha method requires that Person ID and Time uniquely identify an observation. <br>
                              <br>
                              For instance, if there are multiple observations per day and 'Day' is used as the time variable, the method will only use the first
                                      observation for each day and delete all duplicate entries with the same Person ID and Time. <br>
                              <br>This is not a requirement of the method in the top panel, so results may differ if a less granular Time variable is selected.
                                      "), style="width:60%;")))
            ), 
       
       #div code formats the location of the download button to be below the plot, centered with the x-axis label
       tabPanel(title = div(style="text-align: center;",HTML(paste("Person-Specific", "Reliability", sep = "<br/>"))),
                fluidRow(column(wellPanel(h5(HTML("<b><u>People with 50+ Timepoints</b></u>")),plotOutput("ptechPlot", width="3in", height="4in")), width=7, align="center"),
                         column(wellPanel(h5(HTML("<b><u>People with 50+ Timepoints</b></u>")), tableOutput("ptechTable")), width=4,align="center",
                                
                                tags$details(tags$summary("Interpretation", style = "display: list-item;font-size:14px; ; margin-bottom:8px; font-weight:bold;"),
                                             p(HTML("Results are from <i>p</i>-technique factor analysis, which fits a seaparate one-factor model to each person's
                                             data. <br>
                                             <br>The histogram for each person's reliability is shown along with descriptive statistics. <br>
                                             <br>This top panel only shows people with 50 or more observed timepoints, which is one 
                                             recommendation to ensure that there is enough data to trust a factor analysis
                                      "), style="width:60%;")),
                                uiOutput("RelPlot50"))
                ),
                fluidRow(column(wellPanel(h5(HTML("<b><u>All People</b></u>")), plotOutput("ptechPlot_all", width="3in", height="4in")), width=7, align="center"),
                         column(wellPanel(h5(HTML("<b><u>All People</b></u>")), tableOutput("ptechTable_all")), width=4, align="center",
                                
                                tags$details(tags$summary("Interpretation", style = "display: list-item;font-size:14px; ; margin-bottom:8px; font-weight:bold;"),
                                             p(HTML("Results are from <i>p</i>-technique factor analysis, which fits a seaparate one-factor model to each person's
                                             data. <br>
                                             <br>The histogram for each person's reliability is shown along with descriptive statistics. <br>
                                             <br>This bottom panel only shows results from the entire sample regardless of the number of observed timepoints.<br>
                                             <br>The sample sizein the table may not exactly match the full sample size in the data if factor analysis for some people could not converge (these
                                             cases are deleted and are not included in plots or descriptive statistics)
                                      "), style="width:60%;")),
                                uiOutput("RelPlot"))
                )
                
         ),
          
      
         
       #tab for ML-CFA output
         tabPanel( title = div(style="text-align: center;",HTML(paste("Construct Validity", "(Internal Structure)", sep = "<br/>"))),
                  # div(style="display:inline-block; width:350px; text-align:center; margin-bottom:15px; margin-top:15px;" ,uiOutput("RelTable")),
                   h4(HTML("Multilevel Factor Analysis Fit:")),
                   tableOutput("Table2"),
                  tags$details(tags$summary("Interpretation", style = "display: list-item;font-size:14px; ; margin-bottom:8px; font-weight:bold;"),
                               p(HTML("The table follows the progression of models suggested by Stapleton & Johnson (2019) for a scale that aims to 
                                        capture the same construct within-person and between-person (i.e., a Configural Construct).<br>
                                      <br>
                                      The model in the <b>Partially Saturated</b> column fits a one-factor model within-person and saturates the
                                      between-person model with all pairwise covariances. The fit metrics in this column represent
                                      the fit of the factor structure within-person only.<br>
                                      <br>
                                      The model in the <b>Configural</b> column fit a unidimensional model within-person and between-person and
                                      constrains the loadings to be equal at each level. The fit metrics in this column represent the fit of a model
                                      posing that the scale measures a single factor at both the within-person and between-person levels. <br>
                                      <br>
                                      Ideally, the <i>p</i>-value for the chi-square test would be not significant, which indicates that the model exactly
                                      reproduces the observed covariance matrix. However, this is a strict test and the model may still be useful even
                                      if the test is significant.<br>
                                      <br>
                                      Broadly, 'good' values are represented by CFI close to 1, RMSEA close to 0, and SRMR close to 0. There is limited 
                                      guidance for interpretation of multilevel factor analysis fit indices, so it is somewhat unclear how far from 1 or 0
                                      these indices can deviate and still indicate good fit. In single-level models, suggestions are RMSEA < .05, CFI > .95, and
                                      SRMR < .08. However, these suggestions may not generalize to multilevel factor analysis and a rudimentary at best. 
                                      "), style="width:60%;"))
                   ),
       
       #tab for ML-CFA output
       tabPanel( div(style = "margin-top:50px"), title = div(style="text-align: center;",HTML(paste("Multilevel Factor", "Analysis Estimates", sep = "<br/>"))),
                 tabsetPanel(
                   tabPanel("Partially Saturated Model",
                            tags$details(tags$summary("Within-Person Loadings", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                                         tableOutput("Loadm1wp")
                                         ),
                            
                            tags$details(tags$summary("Between-Person Covariances", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                                         tableOutput("covm1bp")
                            ),
                            
                            tags$details(tags$summary("Variances", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                                         h5("Within-Person:"),
                                         tableOutput("Varm1wp"),
                                         h5("Between-Person:"),
                                         tableOutput("Varm1bp"))
                            ),
                   
                   tabPanel("Configural Model",
                            tags$details(tags$summary("Loadings", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                                         h5("Within-Person:"),
                                         tableOutput("Loadm2wp"),
                                         h5("Between-Person:"),
                                         tableOutput("Loadm2bp")),
                            
                            tags$details(tags$summary("Variances", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                                         h5("Within-Person:"),
                                         tableOutput("Varm2wp"),
                                         h5("Between-Person:"),
                                         tableOutput("Varm2bp"))
                   )
                 
              )
       ),
       
       #tab for ML-CFA output
       tabPanel(style="width:70%;",title = "References",
                 h4("This Application:"),
                 p(HTML("McNeish, D. (2025). Intensive Longitudinal Psychometrics R Shiny application [Software], version 0.0.0.")),
                 p(HTML("<br>")),
                 h4("Methodological Ideas Underlying this Applications:"),
                 p(HTML("<br>")),
                 h5(HTML("<u>Reliaiblity</u>:")),
                 p(HTML("Castro-Alvarez, S., Bringmann, L. F., Back, J., & Liu, S. (2024). The many reliabilities of psychological dynamics: 
                          An overview of statistical approaches to estimate the internal consistency reliability of intensive longitudinal data.
                        <i>PsyArXiv</i>, https://doi.org/10.31234/osf.io/qyk2r")), 
                 p(HTML("Geldhof, G. J., Preacher, K. J., & Zyphur, M. J. (2014). Reliability estimation in a multilevel confirmatory factor analysis framework. 
                        <i>Psychological Methods, 19</i>(1), 72-91.")),    p(HTML("Hu, Y., Nesselroade, J. R., Erbacher, M. K., Boker, S. M., Burt, S. A., Keel, P. K., ... & Klump, K. (2016). 
                        Test reliability at the individual level. <i>Structural Equation Modeling, 23</i>(4), 532-543.")),
                 p(HTML("Lai, M. H. (2021). Composite reliability of multilevel data: It’s about observed scores and construct meanings. 
                        <i>Psychological Methods, 26</i>(1), 90-102.")),
                 p(HTML("Nezlek, J. B. (2017). A practical guide to understanding reliability in studies of within-person variability. 
                        <i>Journal of Research in Personality, 69</i>, 149-155.")),
                 p(HTML("<br>")),
                 h5(HTML("<u>Construct Validity</u>:")),
                 p(HTML("Stapleton, L. M., Yang, J. S., & Hancock, G. R. (2016). Construct meaning in multilevel settings. 
                        <i>Journal of Educational and Behavioral Statistics, 41</i>(5), 481-520.")),
                 p(HTML("Stapleton, L. M., & Johnson, T. L. (2019). Models to examine the validity of cluster-level factor structure using individual-level data.
                        <i>Advances in Methods and Practices in Psychological Science, 2</i>(3), 312-329.")),
                 p(HTML("Jak, S. (2019). Cross-level invariance in multilevel factor models. 
                        <i>Structural Equation Modeling</i>, 26(4), 607-622.")),
                 p(HTML("<br>")),
                 h4("Computationally, this application relies on:"),
                 p(HTML("Bates, D., Mächler, M., Bolker, B., & Walker, S. (2015). Fitting linear mixed-effects models using lme4. <i>Journal of Statistical Software, 67</i>, 1-48.")),
                 p(HTML("Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. <i>Journal of Statistical Software, 48</i>(2), 1-36.")),
                 p(HTML("Wickham, H., Henry, L., & Posit Software. (2025). purrr: Functional Programming Tools. R package version 1.0.4. https://CRAN.R-project.org/package=purrr")),
                 p(HTML("Yanagida,T. (2021). misty: Miscellaneous functions. R package version 0.6.8. https://CRAN.R-project.org/package=misty")),
                 p(HTML("<br>")),
                 h4("Aesthetically, this application relies on:"),
                 p(HTML("Chang, W., Cheng, J., Allaire, J., Xie, Y., & McPherson, J. (2020). shiny: Web Application Framework for R. R package version 1.4.0.2.")),
                 p(HTML("Attali, D. (2021). shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds. R package version 2.1.0.")),
                 p(HTML("Attali, D & Sali, A. (2023). shinycssloaders: Add loading animations to a ‘shiny’ output while it’s recalculating. R package version 1.1.0.")),
                 
       ),
       
        )
     )
   )
)

#R code to execute in the background
server <- function(input, output,session) {
  
  shinyjs::disable("go")
  
  #create object for data once it is uploaded  
  data <- reactive({
        req(input$upload)
        
        ext <- tools::file_ext(input$upload$name)
        switch(ext,
               csv = vroom::vroom(input$upload$datapath, delim = ","),
               validate("Invalid file; Please upload a .csv file")
        )
    })
  
  csv<-reactive({
    input$upload
  })
  
  observeEvent(csv(),{updateTabsetPanel(session, inputId = "Tabs", selected = 'Data Preview')})
  
    #Once data are uploaded, update the dropdown menu with the columns names of the uploaded data
    observeEvent(data(), updateSelectizeInput(session,"vec1", choices=names(data())))
    observeEvent(data(), updateSelectizeInput(session,"ID", choices=names(data())))
    observeEvent(data(), updateSelectizeInput(session,"Time", choices=names(data())))
    
    
    #If prespecified interval width, then show slider with width options
    output$Width<-renderUI({
      req(input$interval=="width")
              sliderInput(inputId="W", 
                       label="Enter width (from coefficient to bound):",
                      min=0, max=.20, step=.005, value=.05, ticks=FALSE)
    })
    
    #if raw interval width, show slider bar for interval lower bound
    output$RawLow<-renderUI({
      req(input$interval=="raw")
      sliderInput(inputId="rawLow", 
                    label="Enter Interval Lower Bound:",
                    min=0, max=1, step=.01, value=.50, ticks=FALSE)
    })
    
    #if raw interval wdith, show slider bar for interval upper bound
    output$RawHigh<-renderUI({
      req(input$interval=="raw")
      sliderInput(inputId="rawHigh", 
                  label="Enter Interval Upper Bound",
                  min=0, max=1, step=.01, value=.50, ticks=FALSE)
    })
  
  #print the first five rows of the uploaded data
    output$head <- renderTable({
        req(input$upload)
        head(data(), 5)
    })
    
    observeEvent(c(
                   req(input$vec1),
                   req(input$ID)),
                 {shinyjs::enable("go")})
    
    
    #once the submit button is clicked (go =1), run the conditional reliabliity function using the selected options
    observeEvent(input$go,{
     
      #show spinner while calculating
      shinycssloaders::showPageSpinner(caption = "Calculating, results will appear in tabs when calculations are complete")
      
      M<-function(data=NULL, m=NULL){
        data[data == m] <- NA
        return(data)
      }
      
      data1<-reactive({
        req(input$upload)
        if(input$missing!="NA"){
          x<-M(data=data(),m=input$missing)
        }
        
        if(input$missing=="NA"){
          x<-data()
        }
        return(x)
      })
      
      dat<-data1()
      
      ###########################################
      # P-technique with 50+ timepoints #########
      ###########################################
      
      ID<-input$ID
      ##if restrict to 50 or more;
      data1<-dat
      #how many NA rows
      data1$na<-rowSums(is.na(data1[,input$vec1]))
      #remove row with all missing
      data11<-data1%>%filter(na!=length(input$vec1))
      #count number of non-missing timepoints
      data12<-data11%>%group_by(ID)%>%tally()
      #keep only those with 50 or more timepoints
      data13<-data12%>%filter(n >=50)
      #keep IDs with 50+ timepoints in original data 
      data14<-data11%>%filter(ID %in% data13$ID)%>%dplyr::select(-na)
      ####
      
      l<-(input$vec1)
      
      #left side is factor name
      lhs<-paste0("f","=~")
      #begin right side with first item
      rhs<-paste0("NA","*",l[1])
      
      #loop over number of items
      for(i in 1:(length(l)-1)){
        #plus sign between item names
        rhs<-paste(rhs,"+", l[i+1])
      }
      
      line2<-paste0("f","~~","1","*","f")
      
      #combine left and right hand side
      line1<-paste(lhs,rhs)
      m<-list(line1,line2)
      model<-unlist(m)
      
      Q<-list()
      for(i in (unique(data14$ID))){
        d<-dat%>%
          filter(ID==i)
        Q[[i]]<-d
      }
      
      Q<-purrr::compact(Q)
      
      possible=purrr::possibly(.f=cfa, otherwise=NULL)
      a<-purrr::map(Q, function (x) possible(model=model, data=x, bounds="pos.ov.var"))
      a2<-purrr::compact(a)
      
      N<-purrr::map(a2, function(x) sum(lavaan::inspect(x, what="est")$lambda)^2 )
      T<-purrr::map(a2, function(x) sum(lavaan::inspect(x, what="est")$theta) )
      
      O<-list()
      for(i in 1:length(a2)){
        O[[i]]<-N[[i]]/(N[[i]]+T[[i]])  
      }
      
      O2<-unlist(O)
      names(O2)<-"omega"
      
      ptech_plot<-ggplot()+aes(O2)+
        geom_histogram(aes(y =after_stat(count)),color ="black",fill ="lightgrey",bins =15)+
        xlab("Within-Person Omega")+
        ylab("Count")+
        xlim(0,1.05)+
        #scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, 2))+
        labs(title="P-Technique")+
        labs(subtitle="50+ Timepoints")+
        theme_classic()
      
      t50_1<-matrix(c("Mean", "Median", "Min", "Max", "N"), nrow=5, ncol=1)
      t50_2<-matrix(c(round(mean(O2),3),round(median(O2),3),round(min(O2),3),round(max(O2),3),length(O2)), nrow=5, ncol=1)
      
      colnames(t50_1)<-c("Statistic")
      colnames(t50_2)<-c("Value")
      
      t50<-(cbind(t50_1,t50_2))
      
      output$ptechPlot<-renderPlot(ptech_plot, res=125)
      output$ptechTable<-renderTable({t50})
      
      ###########################################
      # P-technique with All timepoints #########
      ###########################################
      
      Q_all<-list()
      for(i in (unique(dat$ID))){
        d<-dat%>%
          filter(ID==i)
        Q_all[[i]]<-d
      }
      
      Q_all<-purrr::compact(Q_all)
      
      possible=purrr::possibly(.f=cfa, otherwise=NULL)
      a_all<-purrr::map(Q_all, function (x) possible(model=model, data=x, bounds="pos.ov.var"))
      a2_all<-purrr::compact(a_all)
      
      N_all<-purrr::map(a2_all, function(x) sum(lavaan::inspect(x, what="est")$lambda)^2 )
      T_all<-purrr::map(a2_all, function(x) sum(lavaan::inspect(x, what="est")$theta) )
      
      O_all<-list()
      for(i in 1:length(a2_all)){
        O_all[[i]]<-N_all[[i]]/(N_all[[i]]+T_all[[i]])  
      }
      
      O2_all<-unlist(O_all)
      names(O2_all)<-"omega"
      
      ptech_plot_all<-ggplot()+aes(O2_all)+
        geom_histogram(aes(y =after_stat(count)),color ="black",fill ="lightgrey",bins =15)+
        xlab("Within-Person Omega")+
        ylab("Count")+
        xlim(0,1.05)+
        #scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, 2))+
        labs(title="P-Technique")+
        labs(subtitle="All People")+
        theme_classic()
      
      all_1<-matrix(c("Mean", "Median", "Min", "Max", "N"), nrow=5, ncol=1)
      all_2<-matrix(c(round(mean(O2_all),3),round(median(O2_all),3),round(min(O2_all),3),round(max(O2_all),3),length(O2_all)), nrow=5, ncol=1)
      
      colnames(all_1)<-c("Statistic")
      colnames(all_2)<-c("Value")
      
      all<-(cbind(all_1,all_2))
      
      output$ptechPlot_all<-renderPlot(ptech_plot_all, res=125)
      output$ptechTable_all<-renderTable({all})
      
      
      ###############################
      # ML-CFA###
      ###############################
        
      if (input$fix == TRUE){
        fixed="all"
      }  
      
      if (input$fix == FALSE){
        fixed=NULL
      }  
      
      #fit1<-misty::multilevel.omega(data()[,input$vec1], cluster=data()[,input$vec1], missing="listwise", const = "within")
      fit2<-misty::multilevel.omega(dat[,input$vec1], cluster=dat[,input$ID], missing="listwise",fix.resid = fixed)
      
      rel<-fit2$result$omega[1:2,]
      colnames(rel)<-c("Level","Items", "Omega", "95% CI Lower", "95% CI Higher")
      rel[1,1]<-"Within-Person"
      rel[2,1]<-"Between-Person"
      
      fit3<-  misty::multilevel.cfa(dat[,input$vec1], cluster=dat[,input$ID], model=input$vec1, const="within", missing="listwise", fix.resid = fixed)
      fit4<-  misty::multilevel.cfa(dat[,input$vec1], cluster=dat[,input$ID], model=input$vec1, const="config", missing="listwise", fix.resid = fixed)
      
      ###### ML-CFA Estimate Tables #########
      
      ####Partially Saturated####
      wpPS<-fit3$result$param$within
      
      names(wpPS)[names(wpPS) == 'est'] <- 'Estimate'
      names(wpPS)[names(wpPS) == 'se'] <- 'SE'
      names(wpPS)[names(wpPS) == 'z'] <- 'Z'
      names(wpPS)[names(wpPS) == 'pvalue'] <- 'p'
      names(wpPS)[names(wpPS) == 'stdyx'] <- 'Standardized'
      
      bpPS<-fit3$result$param$between
      
      names(bpPS)[names(bpPS) == 'est'] <- 'Estimate'
      names(bpPS)[names(bpPS) == 'se'] <- 'SE'
      names(bpPS)[names(bpPS) == 'z'] <- 'Z'
      names(bpPS)[names(bpPS) == 'pvalue'] <- 'p'
      names(bpPS)[names(bpPS) == 'stdyx'] <- 'Standardized'
      
      ##within-person loadings
      wpPSload<-wpPS %>%  dplyr::filter(op=="=~")
      wpPSload<-wpPSload[,-c(1,3)]
      names(wpPSload)[names(wpPSload) == 'lhs'] <- 'Factor'
      names(wpPSload)[names(wpPSload) == 'rhs'] <- 'Item'
      wpPSload$Factor<-"Within"
      
      #within-person variances
      wpPSVar<-wpPS %>%
        dplyr::filter(lhs==rhs & op=="~~")
      wpPSVar<-wpPSVar[,-c(1,2,3,9)]
      names(wpPSVar)[names(wpPSVar) == 'rhs'] <- 'Variable'
      wpPSVar<-wpPSVar%>%mutate(Variable = replace(Variable,Variable=="wf", "Factor"))
      
      
      ##between-person Covariances
      bpPScov<-bpPS %>%
        dplyr::filter(lhs!=rhs & op=="~~")
      bpPScov<-bpPScov[,-c(1,3)]
      names(bpPScov)[names(bpPScov) == 'lhs'] <- 'Item 1'
      names(bpPScov)[names(bpPScov) == 'rhs'] <- 'Item 2'
      names(bpPScov)[names(bpPScov) == 'Estimate'] <- 'Covariance'
      names(bpPScov)[names(bpPScov) == 'Standardized'] <- 'Correlation'
      
    
      #between-person variances
      bpPSVar<-bpPS %>%
        dplyr::filter(lhs==rhs & op=="~~")
      bpPSVar<-bpPSVar[,-c(1,2,3,9)]
      names(bpPSVar)[names(bpPSVar) == 'rhs'] <- 'Variable'
      bpPSVar<-bpPSVar%>%mutate(Variable = replace(Variable,Variable=="bf", "Factor"))
      
      
      ####Configural####
      wpC<-fit4$result$param$within
      
      names(wpC)[names(wpC) == 'est'] <- 'Estimate'
      names(wpC)[names(wpC) == 'se'] <- 'SE'
      names(wpC)[names(wpC) == 'z'] <- 'Z'
      names(wpC)[names(wpC) == 'pvalue'] <- 'p'
      names(wpC)[names(wpC) == 'stdyx'] <- 'Standardized'
      
      bpC<-fit4$result$param$between
      
      names(bpC)[names(bpC) == 'est'] <- 'Estimate'
      names(bpC)[names(bpC) == 'se'] <- 'SE'
      names(bpC)[names(bpC) == 'z'] <- 'Z'
      names(bpC)[names(bpC) == 'pvalue'] <- 'p'
      names(bpC)[names(bpC) == 'stdyx'] <- 'Standardized'
      
      ##within-person loadings
      wpCload<-wpC %>%  dplyr::filter(op=="=~")
      wpCload<-wpCload[,-c(1,3)]
      names(wpCload)[names(wpCload) == 'lhs'] <- 'Factor'
      names(wpCload)[names(wpCload) == 'rhs'] <- 'Item'
      wpCload$Factor<-"Within"
      
      #within-person variances
      wpCVar<-wpC %>%
        dplyr::filter(lhs==rhs & op=="~~")
      wpCVar<-wpCVar[,-c(1,2,3,9)]
      names(wpCVar)[names(wpCVar) == 'rhs'] <- 'Variable'
      wpCVar<-wpCVar%>%mutate(Variable = replace(Variable,Variable=="wf", "Factor"))
      
      
      ##between-person loadings
      bpCload<-bpC %>%  dplyr::filter(op=="=~")
      bpCload<-bpCload[,-c(1,3)]
      names(bpCload)[names(bpCload) == 'lhs'] <- 'Factor'
      names(bpCload)[names(bpCload) == 'rhs'] <- 'Item'
      bpCload$Factor<-"Between"
      
      #between-person variances
      bpCVar<-bpC %>%
        dplyr::filter(lhs==rhs & op=="~~")
      bpCVar<-bpCVar[,-c(1,2,3,9)]
      names(bpCVar)[names(bpCVar) == 'rhs'] <- 'Variable'
      bpCVar<-bpCVar%>%mutate(Variable = replace(Variable,Variable=="bf", "Factor"))
      
      
      output$Loadm1wp<-renderTable({wpPSload})
      output$covm1bp<-renderTable({bpPScov})
      
      output$Varm1wp<-renderTable({wpPSVar})
      output$Varm1bp<-renderTable({bpPSVar})
      
      output$Loadm2wp<-renderTable({wpCload})
      output$Loadm2bp<-renderTable({bpCload})
      
      output$Varm2wp<-renderTable({wpCVar})
      output$Varm2bp<-renderTable({bpCVar})
      
      #Hide Spinner when calculation is complete
      shinycssloaders::hidePageSpinner()
      
      #open results tab once calculations are complete
      updateTabsetPanel(session, inputId = "Tabs", selected = 'Multilevel')

    #ML-CFA Code

      a<-fit3$result$fit[c(13:16,19,23),c(1,3)]
      b<-fit3$result$fit[c(29,30),c(1,2)]
      colnames(a)<-c("Fit Metric","Partially Saturated")
      colnames(b)<-c("Fit Metric","Partially Saturated")
      c<-rbind(a,b)
      
      d<-fit4$result$fit[c(13:16,19,23),c(3)]
      e<-fit4$result$fit[c(29,30),c(2)]
      names(d)<-c("Configural")
      names(e)<-c("Configural")
      f<-c(d,e)
      
      g<-cbind(c,f)
      colnames(g)<-c("Fit Metric","Partially Saturated","Configural")
      g[,1]<-c("Chi-Square", "Degrees of Freedom", "p-value",
               "Scaling Factor", "CFI", "RMSEA",
               "Within-Person SRMR", "Between-Person SRMR")
    
    #ML-CFA Reliability
    output$Table1<-renderTable({rel}, digits=3)
    output$Table2<-renderTable({g}, digits=3)
    #output$Table3<-renderTable({fit3$result$omega[1:2,]})
    
    
    #######################
    ## Multilevel Method ##
    #######################
    
    dat_wide<-dat[!duplicated(dat[c(input$ID,input$Time)]),]
    
    long<-reshape(dat_wide,
                  direction ="long",
                  idvar     =c(input$ID,input$Time),
                  timevar   ="item",
                  varying   =list(input$vec1),
                  v.names   ="resp")
    
    #number of items
    K<-length(input$vec1)
    
    #max number of timepoints
    t<-max(data12$n)
    
    #Variance Components
    mlm<-lme4::lmer(as.formula(paste0("resp~(1|",input$ID,"/",input$Time,")")), data=long)
    vc<-as.data.frame(lme4::VarCorr(mlm))
    
    #Reliability Calculation
    between<-vc[2,"vcov"]/(vc[2,"vcov"]+vc[1,"vcov"]/t+vc[3,"vcov"]/(K*t))
    within<-vc[1,"vcov"]/(vc[1,"vcov"]+vc[3,"vcov"]/K)
    
    w1<-c("Within-Person",round(within,3))
    b1<-c("Between-Person",round(between,3))
    
    mlm_rel<-rbind(w1,b1)
    
    colnames(mlm_rel)<-c("Level","Alpha")
    
    output$Table3<-renderTable({mlm_rel})
    
    ## PNG: Create Download Button for Reliability Plot after the Submit Button is clicked
    output$RelPlot50<-renderUI(downloadButton("downloadPlot50", "Download as High Resolution PNG"))
    output$downloadPlot50 <- downloadHandler(
      filename = function() {
        # create default file name
        paste0("Reliability_Plot_50", ".png")
      },
      content = function(file) {
        # create a hi-res png file of the plot to be downloaded
        ggsave(file, 
               ptech_plot,
               height=6,
               width=4.5,
               dpi=1200,
               units="in",
               device="png")
      })
    
    ## PNG: Create Download Button for Reliability Plot after the Submit Button is clicked
    output$RelPlot<-renderUI(downloadButton("downloadPlot", "Download as High Resolution PNG"))
    output$downloadPlot <- downloadHandler(
      filename = function() {
        # create default file name
        paste0("Reliability_Plot_All", ".png")
      },
      content = function(file) {
        # create a hi-res png file of the plot to be downloaded
        ggsave(file, 
               ptech_plot_all,
               height=6,
               width=4.5,
               dpi=1200,
               units="in",
               device="png")
      })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
