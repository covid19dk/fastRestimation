server = function(input,output,session) {
  roots = c(wd='../')
  
  shinyFileChoose(input, 'files', root=roots, filetypes=c('csv',"R","r", 'txt'))

  observe({
    print(input$files)
    print(parseFilePaths(roots, input$files))
    dfiles <<- input$files
    })
  
  r_fullName = reactive(as.character(parseFilePaths(roots, input$files)$datapath))
  r_fileName = reactive( as.character(parseFilePaths(roots, input$files)$name))
  
  output$downloadFiles <- downloadHandler(
    filename = function() r_fileName(),
    content = function(file) file.copy(r_fullName(), file)
  )
  
  output$showfile = renderUI({
    out=NULL
    fullName = r_fullName() 
    if(is_string(fullName) && nchar(fullName)) {
      
      if(tools::file_ext(fullName)=="csv") {
        out = DT::dataTableOutput("showDT")
      }
      if(tools::file_ext(fullName)%in%c("","txt","r","R")) {
        rawText <- readLines(fullName) # get raw text
        splitText <- stringi::stri_split(str = rawText, regex = '\\n')
        replacedText <- lapply(splitText, p)
        out = c(list(p(h3("content of text file:"))),replacedText)
      }

    }
    out
  })
  
  output$showDT = renderDT({
    fullName = r_fullName()
    out = NULL
    if(is_string(fullName) && nchar(fullName)) {
      dt = data.table::fread(fullName)
      out = DT::datatable(dt,options = list(pageLength=500))
    }
    out
  })
  
  
  output$showText = renderPrint({
    fullName = r_fullName()
    out = NULL
    if(is_string(fullName) && nchar(fullName)) {
      dt = data.table::fread(fullName)
      out = DT::datatable(dt,options = list(pageLength=500))
    }
    out
  })
  
  output$showTS = renderPlotly({
    
    make_ts_plot(admissions,input$showTS_logY)
    
  })
  
  output$trainingX = renderPrint({print(X)})
  output$scatterX = renderPlot({plot(X[,-1])})
  output$corrX = renderPlot({
    
    xcor = cor(X[,-1])^2
    diag(xcor) = NA
    ggcorrplot(xcor,type="lower",
               outline.col = "white",
               ggtheme = ggplot2::theme_gray,
               colors = c("#6D9EC1", "white", "#E46726"),lab=TRUE,
    ) + scale_fill_gradient(limit = c(0,1), low = "blue", high =  "red")
  })
  
  output$boxX = renderPlot({
    dd = X[,-c(1)] / X$x6
    ggplot(data = melt(dd), aes(x=variable, y=value)) + geom_boxplot(aes(fill=variable))

  })
  
  
  output$linfitX = renderPlot({
    
    
    models_lin_mad = list(
      m1 = OLR(X$x1,X$x6),
      m2 = OLR(X$x2,X$x6),
      m3 = OLR(X$x3,X$x6),
      m4 = OLR(X$x4,X$x6),
      m5 = OLR(X$x5,X$x6)
    ) %>% add_class("model_list")
    
    lines_lin_mad = lapply(models_lin_mad,predict,c(-100,100))
    par(mfrow=2:3)
    plot(X$x0,X$x6,ylim=c(0,max(X$x6)),xlim=c(0,max(X$x0)));segments(-5,mean(X$x6),5)
    plot(X$x1,X$x6,ylim=c(0,max(X$x6)),xlim=c(0,max(X$x1)));abline(lm(x6~x1,X));lines(c(-100,100),lines_lin_mad$m1,col="red")
    plot(X$x2,X$x6,ylim=c(0,max(X$x6)),xlim=c(0,max(X$x2)));abline(lm(x6~x2,X));lines(c(-100,100),lines_lin_mad$m2,col="red")
    plot(X$x3,X$x6,ylim=c(0,max(X$x6)),xlim=c(0,max(X$x3)));abline(lm(x6~x3,X));lines(c(-100,100),lines_lin_mad$m3,col="red")
    plot(X$x4,X$x6,ylim=c(0,max(X$x6)),xlim=c(0,max(X$x4)));abline(lm(x6~x4,X));lines(c(-100,100),lines_lin_mad$m4,col="red")
    plot(X$x5,X$x6,ylim=c(0,max(X$x6)),xlim=c(0,max(X$x5)));abline(lm(x6~x5,X));lines(c(-100,100),lines_lin_mad$m5,col="red")
    legend(x="topleft",legend=c("lm","olr"),col=1:2,lwd=1)
    
  })
  
  output$modelX = renderPrint({

    
    print("linear regression models")
    print(reg_models)
    print("");print("")
    print("linear Ordinary least roots")
    print(mad_models)
    
  })
  
}