
ui = fluidPage(
  
  tabsetPanel(
    
    
    tabPanel(title="correcting admissions",
      h5(paste(
        "Update: since 14th of May, SSI's has changed their method to report admissions. ",
        "The latest reports now has a backwards correction from ~2100 to ~2400 total admissions. ",
        "It appears admissions numbers by the new method are already stable after 1day. This makes this analysis obsolete.",
        "Anyone can now compute R values just using the lastest data (except release day)")),
      br(),
      h3(" daily SSI's admission (r)eports by date(yymmdd) "),
      plotly::plotlyOutput("showTS"),
      shiny::checkboxInput("showTS_logY","click to use log10 scale on y-axis"),
      p(paste0(
        "Daily observed admisssions are reported by SSI. Day 0 (same day as report is released) is always zero. ",
        "Patients are not defined as having COVID-19 before confirmed with a test. It can takes 1-5 days before ",
        "all tests have returned to LPR. Patients not yet tested COVID-19 poistive are in this sense regarded negative. ",
        "SSI's claims to drop most 'recent latest 4 days', whether that is days 0,1,2,3,4 or only 0,1,2,3 is ambigous to me at the moment. ",
        "Thereby SSI handles that latest admission numbers should not be taken as facevalue. As there could be a regular pattern in missingness of test results, ",
        "it could the latest admission numbers carry the information and just need correction. No report on admissions was released before 16th of April."
        
      )),
      a(
        "source: SSI's pdf's and zip's",
        href="https://www.ssi.dk/aktuelt/sygdomsudbrud/coronavirus/covid-19-i-danmark-epidemiologisk-overvaagningsrapport"
      ),
      br(),
      br(),
      br(),
      h2("Looking into patterns of missingness"),
      p(paste0(
        "The reports have been combined to a trainingset, where each row is admission for a given day. Columns x0, x1, ...,x6 ",
        " show the observed value on day 0,1, up to 6. Latest six days cannot be included as wwe have not yet observed what will ",
        "happen 6 days thereafter. It is still the future."
      )),
      verbatimTextOutput("trainingX"),
      br(),
      br(),
      h3("eyeballing correlations...."),
      plotOutput("scatterX"),
      p(paste0(
        "A good start is to make a multi scatter plot. We plot every column(lag of day) against eachother and see if any patterns arises.",
        "We see that x5 and x6 are quite similar as all results have returned. If we can predict x6 with respectively x1, x2 ... ",
        "then we can compute R (reproduction number) days earlier. x0 is always zero and thus carries now information."
      )),
      br(),
      h3("corlation matix plot (R^2 pearson sqaured)"),
      plotOutput("corrX"),
      p(paste0(
        "Ok nice, x1 is slightly, x2 quite, and the rest a lot!  linearly correlated to day x6. ",
        "It seems simple linear models could do the job"
      )),
      br(),
      br(),
      p(paste0(
        "After some thought I came to think that standard linear regression y=b0*b1*x " ,
        "is not ideal. If the is predictor e.g. day zero (x0) or day one (x1), the model will besically",
        "only concist of an offset which is the average daily admission observed at day (x6) over the traing period.",
        "Such an offset could be useful for forecasting. As corrected admissions value is to be used ",
        "as inputs in predictions of R, I think it is better not to encode corrected admissions values with ",
        "beliefs of future R. Otherwise the reasoning becomes unnecessarily recursive. I choose to use series ",
        " of simple constants instead. E2(x6) = x2 * c2. Where c2 is mean(x6)/mean(x2)."
      )),
      plotOutput("boxX"),
      
      "print ",
      plotOutput("linfitX"),
      
      verbatimTextOutput("modelX")
    ),
      
    tabPanel(title="show/download/code files",
      shiny::fluidRow(tagList(
       "see code and submit issues here:", 
       shiny::a("github.com/covid19dk/fastRestimation",href="https://github.com/covid19dk/fastRestimation")
      )),
      shiny::fluidRow(
        h6("or show/download directly here:"),
        shinyFilesButton('files', label='File select', title='download data files', multiple=FALSE),
        shiny::downloadButton("downloadFiles","download")
      ),
      uiOutput("showfile")
    )
    
    
  
  
  )
)
