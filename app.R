suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(glue)
  library(ibdsim2)
  library(ribd)
})


#analysis.form-group { margin-top: 15px; margin-bottom: 3px;}
#nsims.form-group {margin-bottom: 0px;}


VERSION = list(shinyapp = "1.2.1", 
               ibdsim2 = packageVersion("ibdsim2"))

.MODELS = c(Haldane = "haldane", chi2 = "chi")
.MAPS = c("Decode (1-22)" = "decode19", "Single (26M/42M)" = "onechrom")

# User interface
ui = fluidPage(
  
  useShinyjs(),  # Set up shinyjs
  
  tags$head(
    tags$style(type = "text/css", "
      .body {font-size: small}
      .well {padding-top: 10px;}
      .selectize-dropdown {width: 250px !important;}
      .fa-arrow-left { font-size:xx-large; color:#E87722;}
      .fa-check { font-size:xx-large; color:Lime}
  ")),
  
  # Application title
  h2(id = "title-h2", "IBD sharing by family members"),
  tags$style(HTML("#title-h2 {background-color: gray; color: white; padding: 15px}")),
  
  p(bold("Purpose: "),
    "Estimate and visualise distributions of genomic segments shared identical-by-descent (IBD) between related individuals, or within inbred individuals (autozygosity). This is done by simulating the recombination process through the pedigree."),
  
  p(bold("More information: "),
    "This program is a frontend for the R package ", link("ibdsim2", "https://github.com/magnusdv/ibdsim2"), 
    ", which is part of the ", link("ped suite", "https://magnusdv.github.io/pedsuite"), " ecosystem for pedigree analysis.", 
    "Details about the simulations and the various parameters can be found in the documentation of ibdsim2 (and also in the book ",
    link("Pedigree analysis in R", "https://www.elsevier.com/books/pedigree-analysis-in-r/vigeland/978-0-12-824430-2"), ")."), 
    
  p(bold("Tip: "),
    "If you want to load a custom pedigree, you can use ", link("QuickPed", "https://magnusdv.shinyapps.io/ibdsim2-shiny/"), 
    " to create the required ped file."),

# Widgets --------------------------------------------------------------
fluidRow(
  
  # Left sidebar
  sidebarPanel(width = 2, style = "min-width: 185px",
               
    h4("Pedigree 1"),
    selectizeInput("builtin1", "Built-in pedigree", selected = "Siblings", choices = BUILTIN_PEDS, size = 10),
    fileInput("loadped1", "Load ped file", buttonLabel = icon("folder-open"),
              accept = c(".ped", ".txt"), width = "100%", placeholder = NULL),
    textInput("ids1", "Individuals", value = "", width = "100%"),
    textInput("label1", "Label", value = "Ped 1", width = "100%"),
  
    # Model
    radioButtons("model1", "Model", choices = .MODELS, selected = "chi", inline = TRUE),
    
    # Map
    selectInput("map1", "Map", choices = .MAPS, selected = "decode19"),
    checkboxInput("sexspec1", "Sex specific", value = TRUE),
    numericInput("cutoff1", "Cutoff (cM)", value = 0, min = 0, step = 1),
    br(),
    fluidRow(
      column(8, style = "font-size: larger;", actionButton("simulate1", "Simulate!", width = "100%", class = "btn btn-primary")),
      column(4, style = "padding-left:10px;", uiOutput("icon1"))
    ),
  ),
  
  # Middle region: Plots
  mainPanel(width = 8, 
    fluidRow(
      column(6, align = "center", plotOutput("pedplot1", width = "300px", height = "300px")),
      column(6, align = "center", plotOutput("pedplot2", width = "300px", height = "300px"))
    ),
    plotOutput("ibdplot", width = "100%"),
  ),
  
  # Right sidebar
  sidebarPanel(width = 2, style = "min-width: 185px",
    h4("Pedigree 2"),
    selectizeInput("builtin2", "Built-in pedigree", selected = "", choices = BUILTIN_PEDS, size = 10),
    fileInput("loadped2", "Load ped file", buttonLabel = icon("folder-open"),
              accept = c(".ped", ".txt"), width = "100%", placeholder = NULL),
    textInput("ids2", "Individuals", value = "", width = "100%"),
    textInput("label2", "Label", value = "Ped 2", width = "100%"),
    
    # Model
    radioButtons("model2", "Model", choices = .MODELS, selected = "chi", inline = TRUE),
    
    # Map
    selectInput("map2", "Map", choices = .MAPS, selected = "decode19"),
    checkboxInput("sexspec2", "Sex specific", value = TRUE),
    numericInput("cutoff2", "Cutoff (cM)", value = 0, min = 0, step = 1),
    br(),
    fluidRow(
      column(8, style = "font-size: larger;", actionButton("simulate2", "Simulate!", width = "100%", class = "btn btn-primary")),
      column(4, style = "padding-left:10px;", uiOutput("icon2"))
    ),
  ),
  
),
  
fluidRow(column(12, wellPanel(
  fluidRow(
    column(3, radioButtons("analysis", "Analysis:", selected = "Sharing", inline = TRUE,
                 choices = c("Sharing", "Autozygosity"))),
    column(3, numericInput("nsims", "Sims:", value = 50, min = 1, max = 10000, width = "100px")),
    column(3, style="margin-top: 25px", downloadButton("download", "Download data", class="btn btn"))
  )))),

p(style = "font-size:small", "This is version", VERSION$shinyapp, "of ibdsim2-shiny (",
link("changelog", "https://github.com/magnusdv/ibdsim2-shiny/blob/master/NEWS.md"), ").",
"Bug reports, feature requests and other comments are most welcome, for instance by filing an issue ", 
link("here", "https://github.com/magnusdv/ibdsim2-shiny/issues"), "."),
)


# Server logic
server = function(input, output, session) {

  SEED = 1234
  
  ped1 = reactiveVal(NULL)
  ped2 = reactiveVal(NULL)
  
  ids1 = reactive(setdiff(trimws(strsplit(input$ids1, ",")[[1]]), ""))
  ids2 = reactive(setdiff(trimws(strsplit(input$ids2, ",")[[1]]), ""))
  
  observeEvent(input$builtin1, {
    ped1(loadBuiltin(req(input$builtin1)))
    updateTextInput(session, "ids1", value = toString(leaves(ped1())))
  })
  
  observeEvent(input$builtin2, {
    ped2(loadBuiltin(req(input$builtin2)))
    updateTextInput(session, "ids2", value = toString(leaves(ped2())))
  })
  
  observeEvent(input$loadped1, {
    ped = tryCatch(loadPed(input$loadped1$datapath),
                   error = function(e) errModal(conditionMessage(e)),
                   warning = function(e) errModal(conditionMessage(e)))
    ped1(req(ped))
    updateSelectizeInput(session, "builtin1", selected = "")
    updateTextInput(session, "ids1", value = "")
  })
  
  observeEvent(input$loadped2, {
    ped = tryCatch(loadPed(input$loadped2$datapath),
                   error = function(e) errModal(conditionMessage(e)),
                   warning = function(e) errModal(conditionMessage(e)))
    ped2(req(ped))
    updateSelectizeInput(session, "builtin2", selected = "")
    updateTextInput(session, "ids2", value = "")
  })
  
  sim1 = reactiveVal(NULL)
  sim2 = reactiveVal(NULL)
      
  map1 = reactive({
    dMap = loadMap("decode19", uniform = TRUE, sexAverage = !input$sexspec1)
    switch(input$map1, decode19 = dMap, onechrom = uniformMap(Mb = physRange(dMap), cM = mapLen(dMap)))
  })
  map2 = reactive({
    dMap = loadMap("decode19", uniform = TRUE, sexAverage = !input$sexspec2)
    switch(input$map2, decode19 = dMap, onechrom = uniformMap(Mb = physRange(dMap), cM = mapLen(dMap)))
  })
  

# Simulations -------------------------------------------------------------

  # Reset if anything changes
  observe({ped1(); ids1(); map1(); input$model1; input$nsims; input$analysis; sim1(NULL); enable("simulate1")})
  observe({ped2(); ids2(); map2(); input$model2; input$nsims; input$analysis; sim2(NULL); enable("simulate2")})
  
  # Icons
  output$icon1 = renderUI(icon(name = if(is.null(sim1())) "arrow-left" else "check"))
  output$icon2 = renderUI(icon(name = if(is.null(sim2())) "arrow-left" else "check"))
  
  # Simulate!
  observeEvent(input$simulate1, {
    chk = checkSimInput(ped1(), ids1(), input$analysis)
    if(chk != "ok")
      return(errModal(chk))
    disable("simulate1")
    sim1( ibdsim(ped1(), N = input$nsims, ids = ids1(), map = map1(), model = input$model1, seed = SEED, verbose = FALSE) )
  })
  
  # Simulate!
  observeEvent(input$simulate2, {
    chk = checkSimInput(ped2(), ids2(), input$analysis)
    if(chk != "ok")
      return(errModal(chk))
    disable("simulate2")
    sim2( ibdsim(ped2(), N = input$nsims, ids = ids2(), map = map2(), model = input$model2, seed = SEED, verbose = FALSE) )
  })
  
  segmentData1 = reactive(getSegmentData(sim1(), analysis = input$analysis, cutoff = input$cutoff1))
  segmentData2 = reactive(getSegmentData(sim2(), analysis = input$analysis, cutoff = input$cutoff2))
  
  
# Plots ----------------------------------------------------------

  COLS = c(2, 4)
  
  ploterrs = reactiveValues(err1 = NULL, err2 = NULL)
  
  output$pedplot1 = renderPlot({
    ped = req(ped1())
    tryCatch(
      plotped(ped, ids = ids1(), col = COLS[1], title = input$label1, pedname = input$builtin1),
      error = function(e) {
        plot.new(); box(which = "outer", col = 1); title(input$label1); 
        msg = if(grepl("reduce cex", conditionMessage(e))) "(Too big for plot region)" else conditionMessage(e)
        mtext(msg, line = 0, col = 2)
      })
  })

  output$pedplot2 = renderPlot({
    ped = req(ped2())
    tryCatch(
      plotped(ped, ids = ids2(), col = COLS[2], title = input$label2, pedname = input$builtin2),
      error = function(e) {
        plot.new(); box(which = "outer", col = 1); title(input$label2); 
        msg = if(grepl("reduce cex", conditionMessage(e))) "(Too big for plot region)" else conditionMessage(e)
        mtext(msg, line = 0, col = 2)
      })
  })
  
  output$ibdplot = renderPlot({
    segData = list(segmentData1(), segmentData2())
    nonnull = req(!sapply(segData, is.null))
    
    cols = COLS
    names(segData) = names(cols) = c(input$label1, input$label2)
    
    g = generateIbdPlot(segData[nonnull], input$analysis, cols = cols[nonnull])
    suppressWarnings(print(g))
  })
  

# Download data -----------------------------------------------------------

  allParams1 = reactive(list(ped = ped1(), label = input$label1, builtin = input$builtin1, loadped = input$loadped1$name, ids = ids1(), 
                             model = .MODELS[.MODELS == input$model1], map = .MAPS[.MAPS == input$map1], sexspec = input$sexspec1, 
                             cutoff = input$cutoff1, analysis = input$analysis, nsims = input$nsims, seed = SEED))
  
  allParams2 = reactive(list(ped = ped2(), label = input$label2, builtin = input$builtin2, loadped = input$loadped2$name, ids = ids2(),
                             model = .MODELS[.MODELS == input$model2], map = .MAPS[.MAPS == input$map2], sexspec = input$sexspec2, 
                             cutoff = input$cutoff2, analysis = input$analysis, nsims = input$nsims, seed = SEED))
  
  output$download = downloadHandler(
    filename = "ibdsim2-output.zip",
    content = function(con) {
      files = saveData(segmentData1(), segmentData2(), params1 = allParams1(), params2 = allParams2(), version = VERSION)
      if(!length(files)) return(errModal("No data to save"))
      zip(con, files, flags = "-jq9X") # j = junkpaths; q = quiet
    }, 
    contentType = "application/zip"
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
