require(shiny)
require(car)
require(DT)
require(caret)
require(maptools)
require(rgdal)
require(raster)
require(sp)
require(rgeos)
require(leaflet)
require(shinyBS)
require(RColorBrewer)

## TODO:
## Monitoring pop-ups, outliers

shinyServer(function(input, output, session) {
  source("functions.R")
  source("spatial.R")
  source("global.R")
  source("variableTable.R")

  ##########################
  ## TAB CHANGE EVENTS
  ##########################
  
  observeEvent(input$tabs, {
    if (input$tabs == "predictions") {
      ## SHOW/ZOOM TO PREDICTION DATA EXTENTS
      leafletProxy("mymap") %>%
        removeShape(layerId = "bb.cor") %>%
        removeShape(layerId = "bb.rds") %>%
        removeShape(layerId = "bb.pop")
      bbox.cor.extent <- NULL
      bbox.rds.extent <- NULL
      bbox.pop.extent <- NULL
      
      if (!is.null(dat$corine)) {
        bbox.cor <- get.bbox(dat$corine, TRUE, isolate(input$epsg))
        bbox.cor.extent <- slot(bbox.cor, "bbox")
        leafletProxy("mymap", data = dat$corine) %>%
          addPolygons(
            data = bbox.cor, weight = 3, fillOpacity = 0, color = "green", layerId = "bb.cor"
          )
      }
      if (!is.null(dat$roads)) {
        bbox.rds <- get.bbox(dat$roads, TRUE, isolate(input$epsg))
        bbox.rds.extent <- slot(bbox.rds, "bbox")
        leafletProxy("mymap", data = dat$roads) %>%
          addPolygons(
            data = bbox.rds, weight = 3, fillOpacity = 0, color = "red", layerId = "bb.rds"
          )
      }
      if (!is.null(dat$population)) {
        bbox.pop <- get.bbox(dat$population, TRUE, isolate(input$epsg))
        bbox.pop.extent <- slot(bbox.pop, "bbox")
        leafletProxy("mymap", data = dat$population) %>%
          addPolygons(
            data = bbox.pop, weight = 3, fillOpacity = 0, color = "purple", layerId = "bb.pop"
          )
      }
      
      ## COMMON EXTENT OF INPUT DATA
      if (!is.null(dat$corine) |
          !is.null(dat$roads) | !is.null(dat$population)) {
        ext$min.long <-
          min(bbox.cor.extent[1, 1], bbox.rds.extent[1, 1], bbox.pop.extent[1, 1])
        ext$min.lat <-
          min(bbox.cor.extent[2, 1], bbox.rds.extent[2, 1], bbox.pop.extent[2, 1])
        ext$max.long <-
          min(bbox.cor.extent[1, 2], bbox.rds.extent[1, 2], bbox.pop.extent[1, 2])
        ext$max.lat <-
          min(bbox.cor.extent[2, 2], bbox.rds.extent[2, 2], bbox.pop.extent[2, 2])
        leafletProxy("mymap") %>%
          fitBounds(ext$min.long, ext$min.lat, ext$max.long, ext$max.lat)
      }
    }
    else if (input$tabs == "training") {
      ## SHOW ZOOM TRAINING SET
      if (!is.null(dat$monitor)) {
        tmp <- spTransform(dat$monitor , CRS("+proj=longlat +datum=WGS84"))
        training.map(tmp)
        training.map.extent(tmp)
        
        ## get outliers
 ##  tmp.out <- subset(tmp, rownames(tmp@data) %in% "TODO: table selection")
        ##Pop-up row names
        ##Show outliers
      }
    }
  })
  
  ############################################################################################################
  ############################################################################################################
  #### VARIABLE CREATION TAB #################################################################################
  
  ##########################
  ## INPUT DATA FOR TRAINING
  ##########################
  
  ## TRIGGER GENERATE BOXES
  gis$corine <- FALSE
  gis$road <- FALSE
  gis$popn <- FALSE
  
  ## CRS
  observe({
    epsg$x <- as.integer(input$epsg)
  })
  
  ## GET ALL THE PARTS OF A SHAPEFILE
  get.shp <- function(x) {
    prevWD <- getwd()
    uploadDirectory <- dirname(x$datapath[1])
    setwd(uploadDirectory)
    for (i in 1:nrow(x)) {
      file.rename(x$datapath[i], x$name[i])
    }
    shpName <- x$name[grep(x = x$name, pattern = "*.shp")]
    shpPath <- paste(uploadDirectory, shpName, sep = "/")
    setwd(prevWD)
    return(shpPath)
  }
  
  ## LOAD MONITORING
  observeEvent(input$file.monitor, {
    filename.monitor$f <- input$file.monitor
    err.code <- get.monitor.shp()
    if (err.code != 0) {
      createAlert(
        session, "alert0", "Alert", title = "UPLOAD ERROR",
        content = paste0("Message: ", error.message(err.code)), append = FALSE
      )
    }
  })
  
  get.monitor.shp <- function(x) {
    err.code <- 0
    if (is.na(epsg$x) || !is.integer(epsg$x)) {
      return(1)
    }
    tryCatch({
      dat$monitor <- readShapePoints(get.shp(filename.monitor$f))
      prj <- paste0("+init=epsg:", isolate(epsg$x))
      projection(dat$monitor) <- prj
      inFile$ts <- NULL
      in.data$val <- slot(dat$monitor, "data")
      return(err.code)
    },
    error = function(e) {
      err.code <- 2
    }, finally = function(e) {
      return(err.code)
    })
  }
  
  ## LOAD ROADS
  observeEvent(input$file.roads, {
    filename.roads$f <- input$file.roads
    err.code <- get.roads.shp()
    if (err.code != 0) {
      createAlert(
        session, "alert0", "Alert", title = "UPLOAD ERROR",
        content = paste0("Message: ", error.message(err.code)), append = FALSE
      )
    }
  })
  
  get.roads.shp <- function(x) {
    err.code <- 0
    if (is.na(epsg$x) || !is.integer(epsg$x)) {
      return(1)
    }
    tryCatch({
      dat$roads <- readShapeLines(get.shp(filename.roads$f))
      prj <- paste0("+init=epsg:", isolate(epsg$x))
      projection(dat$roads) <- prj
      gis$road <- FALSE
      return(err.code)
    },
    error = function(e) {
      err.code <- 4
    }, finally = function(e) {
      return(err.code)
    })
  }
  
  ## LOAD LANDCOVER (CORINE)
  observeEvent(input$file.landcover, {
    filename.corine$f <- input$file.landcover
    err.code <- get.landcover.shp()
    if (err.code != 0) {
      createAlert(
        session, "alert0", "Alert", title = "UPLOAD ERROR",
        content = paste0("Message: ", error.message(err.code)), append = FALSE
      )
    }
  })
  
  get.landcover.shp <- function(x) {
    err.code <- 0
    if (is.na(epsg$x) || !is.integer(epsg$x)) {
      return(1)
    }
    tryCatch({
      dat$corine <- readShapePoly(get.shp(filename.corine$f))
      prj <- paste0("+init=epsg:", isolate(epsg$x))
      projection(dat$corine) <- prj
      gis$corine <- FALSE
      return(err.code)
    },
    error = function(e) {
      err.code <- 3
    }, finally = function(e) {
      return(err.code)
    })
  }
  
  ## LOAD POPULATION
  observeEvent(input$file.population, {
    filename.popn$f <- input$file.population
    err.code <- get.popn.shp()
    if (err.code != 0) {
      createAlert(
        session, "alert0", "Alert", title = "UPLOAD ERROR",
        content = paste0("Message: ", error.message(err.code)), append = FALSE
      )
    }
  })
  
  get.popn.shp <- function(x) {
    err.code <- 0
    if (is.na(epsg$x) || !is.integer(epsg$x)) {
      return(1)
    }
    tryCatch({
      dat$population <- readShapePoints(get.shp(filename.popn$f))
      prj <- paste0("+init=epsg:", isolate(epsg$x))
      projection(dat$population) <- prj
      gis$popn <- FALSE
      return(err.code)
    },
    error = function(e) {
      err.code <- 2
    }, finally = function(e) {
      return(err.code)
    })
  }
  
  ## LOAD EXISTING CSV
  observeEvent(input$filecsv, {
    filename.exist$f <- input$filecsv
    err.code <- get.exist.csv()
    if (err.code != 0) {
      createAlert(
        session, "alert0", "Alert", title = "UPLOAD ERROR",
        content = paste0("Message: ", error.message(err.code)), append = FALSE
      )
    }
  })
  
  get.exist.csv <- function(x) {
    err.code <- 0
    tryCatch({
      inFile$ts <- NULL
      in.data$val <- read.csv(filename.exist$f$datapath)
      in.data$val[is.na(in.data$val)] <- 0
      in.data$val <- in.data$val[, sapply(in.data$val, is.numeric)]
      return(err.code)
    },
    warning = function(e) {
      err.code <- 5
    }, finally = function(e) {
      return(err.code)
    })
  }
  
  ##########################
  ## SPATIAL OPERATIONS
  ##########################
  
  ## PROCESS CORINE BUFFERS
  observeEvent(input$action.corine, {
    progress <- shiny::Progress$new(min = 0, max = length(buffers.1) * length(dat$monitor))
    progress$set(message = "Computing", value = 0)
    on.exit(progress$close())
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + 1
      }
      progress$set(value = value, detail = detail)
    }
    tryCatch({
      in.data$val <-
        do.corine(
          updateProgress, dat$monitor, dat$corine, corine.vars, buffers.1, isolate(input$corinecode)
        )
      gis$corine <- TRUE
    },
    error = function(e) {
      createAlert(
        session, "alert0", "Alert", title = "SPATIAL OPERATION ERROR",
        content = paste0("Message: ", error.message(16)), append = FALSE
      )
      progress$close()
    })
  })
  
  ## PROCESS ROADS
  observeEvent(input$action.roads, {
    progress <-
      shiny::Progress$new(min = 0, max = nrow(dat$monitor) * 2)
    progress$set(message = "Computing", value = 0)
    on.exit(progress$close())
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + 1
      }
      progress$set(value = value, detail = detail)
    }
    tryCatch({
      in.data$val <-
        do.roads.dists(
          updateProgress, dat$monitor, dat$roads, roads.vars.nn.maj,
          roads.vars.nn.all, isolate(input$roadcodetotal),
          isolate(input$roadcodeHGV), isolate(input$roadmajor)
        )
      progress$close()
    },
    error = function(e) {
      progress$close()
      createAlert(
        session, "alert0", "Alert", title = "SPATIAL OPERATION ERROR",
        content = paste0("Message: ", error.message(15)), append = FALSE
      )
    })
    
    progress <- shiny::Progress$new(min = 0, max = length(buffers.2) * length(dat$monitor))
    progress$set(message = "Computing", value = 0)
    on.exit(progress$close())
    tryCatch({
      in.data$val <- cbind(
        in.data$val,
        do.roads.buffer(
          updateProgress, dat$monitor, dat$roads, roads.vars.length.maj,
          roads.vars.length.all, buffers.2, buffers.2, isolate(input$roadcodetotal),
          isolate(input$roadcodeHGV), isolate(input$roadmajor)
        )
      )
      gis$road <- TRUE
    },
    error = function(e) {
      progress$close()
      createAlert(
        session, "alert0", "Alert", title = "SPATIAL OPERATION ERROR",
        content = paste0("Message: ", error.message(17)), append = FALSE
      )
    })
  })
  
  ##PROCESS POPN BUFFER
  observeEvent(input$action.popn, {
    progress <- shiny::Progress$new(min = 0, max = length(buffers.1))
    progress$set(message = "Computing", value = 0)
    on.exit(progress$close())
    
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + 1
      }
      progress$set(value = value, detail = detail)
    }
    tryCatch({
      in.data$val <-
        do.popn(
          updateProgress, dat$monitor, dat$population, popn.vars,
          buffers.1, isolate(input$popnpop), isolate(input$popnhhold)
        )
      gis$popn <- TRUE
    },
    error = function(e) {
      progress$close()
      createAlert(
        session, "alert0", "Alert", title = "SPATIAL OPERATION ERROR",
        content = paste0("Message: ", error.message(18)), append = FALSE
      )
    })
  })
  
  ##########################
  ## OPTIONS FOR VARIABLE GENERATION
  ##########################
  
  ## CORINE
  corine.attrs <- reactive({
    names(dat$corine)
  })
  output$corinecode <- renderUI({
    selectInput(
      "corinecode", "CORINE Code", choices = corine.attrs(), selected = "CODE_00n", multiple = FALSE
    )
  })
  
  ## TRAFFIC
  road.attrs <- reactive({
    names(dat$roads)
  })
  output$roadcodetotal <- renderUI({
    selectInput(
      "roadcodetotal", "Daily total flow", choices = road.attrs(), selected = "ALLMV", multiple = FALSE
    )
  })
  output$roadcodeHGV <- renderUI({
    selectInput(
      "roadcodeHGV", "Daily HGV flow", choices = road.attrs(), selected = "HEAVY", multiple = FALSE
    )
  })
  output$roadmajor <- renderUI({
    numericInput("roadmajor", "Major Road AADT >", value = 5000)
  })
  
  ## POPULATION
  popn.attrs <- reactive({
    names(dat$population)
  })
  output$popnpop <- renderUI({
    selectInput(
      "popnpop", "Population", choices = popn.attrs(), selected = "POPULATION", multiple = FALSE
    )
  })
  output$popnhhold <- renderUI({
    selectInput(
      "popnhhold", "Households", choices = popn.attrs(), selected = "HOUSEHOLDS", multiple = FALSE
    )
  })
  
  ##########################
  ## GENERATE BUTTONS COLOUR
  ##########################
  
  output$landcoverGenerate <- renderUI({
    if (gis$corine) {
      box(
        title = NULL, status = "success", background = "green", width = 12, solidHeader = FALSE,
        actionButton("action.corine", label = "Generate")
      )
    }
    else {
      box(
        title = NULL, status = "danger", background = "red", width = 12, solidHeader = FALSE,
        actionButton("action.corine", label = "Generate")
      )
    }
  })
  
  output$roadGenerate <- renderUI({
    if (gis$road) {
      box(
        title = NULL, status = "success", background = "green", width = 12, solidHeader = FALSE,
        actionButton("action.roads", label = "Generate")
      )
    }
    else {
      box(
        title = NULL, status = "danger", background = "red", width = 12, solidHeader = FALSE,
        actionButton("action.roads", label = "Generate")
      )
    }
  })
  
  output$popnGenerate <- renderUI({
    if (gis$popn) {
      box(
        title = NULL, status = "success", background = "green", width = 12, solidHeader = FALSE,
        actionButton("action.popn", label = "Generate")
      )
    }
    else {
      box(
        title = NULL, status = "danger", background = "red", width = 12, solidHeader = FALSE,
        actionButton("action.popn", label = "Generate")
      )
    }
  })
  
  ############################################################################################################
  ############################################################################################################
  #### TRAINING SET TAB ######################################################################################
  
  ## MAKE OUTPUT TABLE
  get.inFile <- function(x) {
    if (is.null(inFile$ts)) {
      return(x)
    }
    else
    {
      existing.names <- names(inFile$ts)
      new.names <- names(x)
      temp <- inFile$ts
      for (i in 1:length(new.names)) {
        if (new.names[i] %in% existing.names) {
          temp[, new.names[i]] <- x[, new.names[i]]
        }
        else {
          temp <- cbind(temp, x[, new.names[i]])
          colnames(temp)[ncol(temp)] <- new.names[i]
        }
      }
      return (temp)
    }
  }
  
  ## DISPLAY OUTPUT TABLE
  observe ({
    inFile$ts <- get.inFile(in.data$val)
  })
  
  output$table.trainingset <- DT::renderDataTable({
    input$action.clear.out
    if (!is.null(inFile$ts)) {
      DT::datatable(
        inFile$ts, options = list(
          scrollX = TRUE,
          ordering = FALSE,
          searching = FALSE,
          lengthMenu = NULL,
          pageLength = 25,
          extensions = 'Responsive'
        )
      )
    }
  })
  
  ## TAKE OUT HIGHLIGHTED OUTLIER ROWS
  remove.outliers <- function(x) {
    sel <- input$table.trainingset_rows_selected
    sel.inv <- rownames(inFile$ts)[!rownames(inFile$ts) %in% sel]
    sel.data <- inFile$ts[sel.inv,]
    return(sel.data)
  }
  
  ## COUNT OUTLIERS
  output$out.count <- renderText({
    n <- 0
    if (!is.null(inFile$ts)) {
      n <- nrow(inFile$ts)
    }
    paste0(length(input$table.trainingset_rows_selected), " out of ", n," points removed")
  })
  
  ## LEAFLET BASE MAP
  output$monitormap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      fitBounds(-31, 49, 21, 60)
  })
  
  ## CHANGE SYMBOL SIZE
  observeEvent(input$pointsize, {
    if (!is.null(dat$monitor)) {
      tmp <- spTransform(dat$monitor , CRS("+proj=longlat +datum=WGS84"))
      training.map(tmp)
    }
  })
  
  ## PLOT TRAINING SET POINTS
  training.map <- function(x) {
    if (!is.null(isolate(input$response))) {
      if (isolate(input$response) %in% names(dat$monitor)) {
        attr <- x@data[, isolate(input$response)]
        resp <- isolate(input$response)
      }
      else {
        attr <- x@data[, 1]
        resp <- names(x@data)[1]
      }
    }
    else {
      attr <- x@data[, 1]
      resp <- names(x@data)[1]
    }
    leafletProxy("monitormap", data = x) %>%
      clearGroup("monitor") %>%
      clearGroup("outlier") %>% #TODO:
      removeControl("mlegend") %>%
      addCircleMarkers(
        lng = x@coords[, 1], lat = x@coords[, 2], group = "monitor",
        radius = isolate(input$pointsize), stroke = FALSE, fillOpacity = 0.7, fill = TRUE,
        fillColor = ~ colorNumeric(pal, c(min(attr), max(attr)))(attr)
      ) %>%
      addLegend(
        position = "bottomleft", pal = colorNumeric(pal, c(min(attr), max(attr))),
        title = resp, values = attr, layerId = "mlegend"
      ) 
  }
  
  ## ZOOM TO EXTENT
  training.map.extent <- function(x) {
    if (!is.null(dat$monitor)) {
      tmp <- spTransform(dat$monitor , CRS("+proj=longlat +datum=WGS84"))
      bbox.monitor.extent <- slot(tmp, "bbox")
      leafletProxy("monitormap") %>%
        fitBounds(
          bbox.monitor.extent[1, 1], bbox.monitor.extent[2, 1],
          bbox.monitor.extent[1, 2], bbox.monitor.extent[2, 2]
        )
    }
  }
  
  observeEvent(input$pointextent, {
    training.map.extent()
  })
  
  ############################################################################################################
  ############################################################################################################
  #### MODEL BUILDER TAB #####################################################################################
  
  ##VARIABLE DATATABLE
  output$training.set <- DT::renderDataTable({
    input$action.clear
    validate(need(!is.null(inFile$ts), ""))
    DT::datatable(selectedData(), options = list(
      scrollX = TRUE,
      searching = TRUE,
      pageLength = nrow(selectedData()),
      lengthMenu = NULL,
      extensions = 'Responsive'
    ))
  })
  
  selectedData <- reactive({
    covars <- inFile$ts[, which(names(inFile$ts) != input$response)]
    pmcc <-
      round(cor(covars[,], inFile$ts[, which(names(inFile$ts) == input$response)], use = "pairwise.complete.obs"), 4)
    nonzero <-
      round(apply(covars, 2, function(x)
        length(which(x != 0))) / (nrow(covars)) * 100, 0)
    options(scipen = 999)
    vsd <- round(apply(covars, 2, sd), 2)
    data.frame(PPMC = pmcc, nonzero = nonzero, SD = vsd)
  })
  
  ## MODEL DATATABLE
  output$model.stats <- DT::renderDataTable({
    validate(need(!is.null(inFile$ts), ""))
    DT::datatable(
      selectedModel(), options = list(
        scrollX = TRUE,
        ordering = FALSE,
        searching = FALSE,
        lengthMenu = NULL,
        paging = FALSE,
        extensions = 'Responsive'
      )
    )
  })
  
  ## RESPONSE VARIABLE
  outVar <- reactive({
    names(inFile$ts)
  })
  output$response <- renderUI({
    selectInput("response", "Response", choices = outVar(), multiple = FALSE)
  })
  
  ## SELECTED MODEL
  selectedModel <- reactive({
    tryCatch(
      selected.vars <-
        rownames(selectedData()[input$training.set_rows_selected,]),
      sel.data <- remove.outliers(),
      error = function(e) {
        selected.vars <- NULL
      }
    )
    if (is.null(selected.vars)) {
      lur.model$lm <- NULL
    }
    else if (nrow(sel.data) < 3) {
      lur.model$lm <- NULL
    }
    else {
      if (length(selected.vars) == 0) {
        LUR <- as.formula((paste(input$response, "~1")))
      }
      else {
        LUR <-
          as.formula(paste(input$response, paste(names(inFile$ts)[which(names(inFile$ts) %in% selected.vars)], collapse = " + "), sep = "~"))
      }
      
      ## MODEL
      lm.lur <- lm(LUR, data = sel.data)
      
      ## STEPWISE
      var.list.step <- vector()
      this.lm.step.r2 <- c(NA)
      this.lm.step.aic <- c(NA)
      this.lm.step.rmse <- c(NA)
      
      for (v in names(lm.lur$coefficients)) {
        if (v != "(Intercept)") {
          var.list.step <- append(var.list.step, v)
          equ <-
            as.formula(paste(
              input$response, paste(var.list.step, collapse = " + "), sep = "~"
            ))
          lm.step <- lm(equ, data = sel.data)
          this.lm.step.r2 <-
            c(this.lm.step.r2, round(summary(lm.step)$r.squared, 4))
          this.lm.step.aic <-
            c(this.lm.step.aic, round(AIC(lm.step), 4))
          this.lm.step.rmse <-
            c(this.lm.step.rmse, (summary(lm.step)$sigma))
        }
      }
      
      ## VIF
      if (length(lm.lur$coefficients) > 2) {
        this.lm.lur.vif <- vif(lm.lur)
        this.lm.lur.vif <- append(this.lm.lur.vif, 0, after = 0)
      }
      else {
        this.lm.lur.vif <- 0
      }
      
      ## REGRESSION
      dt1 <-
        data.frame(
          Estimate = round((summary(lm.lur))$coefficients[, 1], 4),
          Std.Error = round((summary(lm.lur))$coefficients[, 2], 4),
          t.value = round((summary(lm.lur))$coefficients[, 3], 4),
          Prob. = round((summary(lm.lur))$coefficients[, 4], 4),
          Sig = unlist(lapply((
            summary(lm.lur)
          )$coefficients[, 4], FUN = get.sig.star))
        )
      rownames(dt1) <- names(lm.lur$coefficients)
      dt1 <-
        dt1[match(c("(Intercept)", input$training.set_rows_selected), rownames(dt1)),]
      dt1$VIF <- this.lm.lur.vif
      dt1$RSq <- this.lm.step.r2
      dt1$AIC <- this.lm.step.aic
      dt1$RMSE <- round(this.lm.step.rmse, 4)
      
      lur.model$lm <- lm.lur
      lur.model$r2 <- this.lm.step.r2
      lur.model$rmse <- this.lm.step.rmse
      lur.model$aic <- this.lm.step.aic
      
      ## CLEAR ANY PREVIOUS PREDICTION
      leafletProxy("mymap") %>%
        removeShape(layerId = "raster") %>%
        removeImage(layerId = "raster")
      r <<- NULL
      if (!input$cbrecept) {
        d$draw <- TRUE
      }
      
      dt1
    }
  })
  
  ## PLOTS: CORRELATION MATRIX
  output$pairs <- renderPlot({
    validate(need(!is.null(inFile$ts), ""))
    sel.data <- remove.outliers()
    selected.vars <-
      rownames(selectedData()[input$training.set_rows_selected,])
    if (length(selected.vars) == 0) {
      plot(sel.data[, input$response], sel.data[, input$response], xlab = input$response, ylab = input$response)
    }
    else {
      pairs(
        as.formula(paste0(
          "~", input$response, "+", paste(selected.vars, collapse = " + ")
        )),
        data = inFile$ts, lower.panel = panel.smooth, upper.panel =
          panel.cor
      )
    }
  })
  
  ## PLOTS: OBSERVED AND PREDICTED
  output$lm.obs.exp <- renderPlot({
    validate(need(!is.null(inFile$ts), ""))
    obs <- lur.model$lm$fitted.values + lur.model$lm$residuals
    prd <- lur.model$lm$fitted.values
    plot(obs, prd, xlab = "Observed", ylab = "Predicted")
    abline(0, 1, lty = 2)
    abline(lm(prd ~ obs))
    if (input$labels) {
      text(
        obs, prd, labels = names(lur.model$lm$fitted.values), cex = 0.7, pos = 4
      )
    }
  })
  
  ## PLOTS: CHANGE IN R2, AIC, RMSE
  output$lm.aic.r2 <- renderPlot({
    validate(need(!is.null(inFile$ts), ""))
    par(mfrow = c(1, 3))
    if (length(lur.model$r2) > 1) {
      plot(
        lur.model$r2[-1], main = "R-Squared", xlab = "Variable", ylab = "R-Squared", xaxt = "n"
      )
      lines(lur.model$r2[-1])
      axis(1, at = 1:length(lur.model$r2[-1]))
      
      plot(
        lur.model$rmse[-1], main = "RMSE", xlab = "Variable", ylab = "RMSE", xaxt = "n"
      )
      lines(lur.model$rmse[-1])
      axis(1, at = 1:length(lur.model$rmse[-1]))
      
      plot(
        lur.model$aic[-1], main = "AIC", xlab = "Variable", ylab = "AIC", xaxt = "n"
      )
      lines(lur.model$aic[-1])
      axis(1, at = 1:length(lur.model$aic[-1]))
    }
  })
  
  ## PLOTS: DIAGNOSTICS 1, "Residuals vs Fitted"
  output$lm.plot.1 <- renderPlot({
    validate(need(!is.null(inFile$ts), ""))
    plot(lur.model$lm, which = 1)
  })
  
  ## PLOTS: DIAGNOSTICS 2, "Normal Q-Q"
  output$lm.plot.2 <- renderPlot({
    validate(need(!is.null(inFile$ts), ""))
    plot(lur.model$lm, which = 2)
  })
  
  ## PLOTS: DIAGNOSTICS 3, "Scale-Location"
  output$lm.plot.3 <- renderPlot({
    validate(need(!is.null(inFile$ts), ""))
    plot(lur.model$lm, which = 3)
  })
  
  ## PLOTS: DIAGNOSTICS 4, "Cook's distance"
  output$lm.plot.4 <- renderPlot({
    validate(need(!is.null(inFile$ts), ""))
    plot(lur.model$lm, which = 4)
  })
  
  ## PLOTS: DIAGNOSTICS 5, "Residuals vs Leverage"
  output$lm.plot.5 <- renderPlot({
    validate(need(!is.null(inFile$ts), ""))
    plot(lur.model$lm, which = 5)
  })
  
  output$model.summary <- renderText({
    if (length(input$training.set_rows_selected) > 0) {
      selected.vars <-
        rownames(selectedData()[input$training.set_rows_selected,])
      equ <-
        paste(input$response, "~", paste(selected.vars, collapse = " + "), sep = " ")
      train_control <- trainControl(method = "LOOCV")
      loocv <-
        train(
          as.formula(equ), data = inFile$ts, trControl = train_control, method = "lm"
        )
      lur.loocv <<- paste(
        equ, "\n\nLOOCV RMSE: ", loocv$results[2], "\nLOOCV R-Squared: ", loocv$results[3], sep = ""
      )
      lur.loocv
    }
  })
  
  ############################################################################################################
  ############################################################################################################
  #### PREDICTIONS TAB #######################################################################################
  
  ##########################
  ## LEAFLET BASE MAP
  ##########################
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      fitBounds(-31, 49, 21, 60)
  })
 
  ##########################
  ## EXTENT POLYGON DRAWING
  ##########################
  
  ## ALLOW USER TO DRAW
  d <- reactiveValues()
  d$draw <- TRUE
  observeEvent(input$cbrecept, {
    clear.draw()
    if (input$cbrecept == TRUE) {
      d$draw <- FALSE
    }
    else {
      d$draw <- TRUE
    }
  })
  
  ## MAP CLICK EVENT
  observe({
    click <- input$mymap_click
    isolate(vertices$xy <-
              rbind(vertices$xy, c(click$lng, click$lat)))
  })
  
  ## EXISTING BBOX CLICK EVENT
  observe({
    click <- input$mymap_shape_click
    isolate(vertices$xy <-
              rbind(vertices$xy, c(click$lng, click$lat)))
  })
  
  ## START MARKER CLICK EVENT
  observe({
    click <- input$mymap_marker_click
    if (!is.null(click) &&
        !is.null(click$id) && click$id == paste0("start", x)) {
      isolate(vertices$xy <-
                rbind(vertices$xy, c(vertices$xy[1, 1], vertices$xy[1, 2])))
    }
  })
  
  ## DRAW USER DEFINED BBOX
  observe({
    ## DRAW STARTING CIRCLE
    if (d$draw) {
      if (nrow(vertices$xy) == 1) {
        x <<- x + 1
        leafletProxy("mymap") %>%
          addCircleMarkers(
            vertices$xy[1, 1], vertices$xy[1, 2],
            radius = 15, color = "green", layerId = paste0("start", x)
          )
      }
      else {
        ## DRAW FINAL POLYGON
        if (nrow(vertices$xy) > 1 &&
            vertices$xy[1,] == vertices$xy[nrow(vertices$xy),]) {
          ln <- Polygon(vertices$xy)
          leafletProxy("mymap") %>%
            addPolygons(
              data = ln, weight = 3, fillOpacity = 0.5, color = "blue", layerId = "bbox"
            ) %>%
            removeMarker(layerId = paste0("start", x))
          d$draw <- FALSE
        }
        ## DRAW EDGES
        else {
          ln <- Line(vertices$xy)
          leafletProxy("mymap") %>%
            addPolylines(
              data = ln, weight = 3, fillOpacity = 0.5, color = "blue", layerId = "bbox"
            )
        }
      }
    }
  })
  
  ## DRAW EXTENT BASED ON INPUTS
  observeEvent(input$cb.extent, {
    validate(need(!is.null(ext$min.long), ""))
    validate(need(is.finite(ext$min.long), ""))
    clear.draw()
    if (input$cb.extent) {
      xy <- matrix(0, 5, 2)
      xy[1, 1] <- ext$min.long
      xy[1, 2] <- ext$max.lat
      xy[2, 1] <- ext$max.long
      xy[2, 2] <- ext$max.lat
      xy[3, 1] <- ext$max.long
      xy[3, 2] <- ext$min.lat
      xy[4, 1] <- ext$min.long
      xy[4, 2] <- ext$min.lat
      xy[5, 1] <- ext$min.long
      xy[5, 2] <- ext$max.lat
      d$draw <- TRUE
      vertices$xy <- xy
    }
    else {
      d$draw <- TRUE
    }
  })
  
  ## CLEAR EXTENT POLYGON
  observeEvent(input$action.clear.box, {
    leafletProxy("mymap") %>%
      clearGroup("pp")
    if (!input$cb.extent) {
      clear.draw()
      d$draw <- TRUE
    }
  })
  clear.draw <- function() {
    if (length(vertices$xy) != 0) {
      leafletProxy("mymap") %>%
        removeShape(layerId = "bbox") %>%
        removeMarker(layerId = paste0("start", x)) %>%
        clearGroup("pp") %>%
        clearControls() %>%
        removeShape(layerId = "raster")
      vertices$xy <- matrix(0, 0, 2)
    }
  }
  
  ## CLEAR EVERYTHING
  clear.all <- function() {
    r <<- NULL
    leafletProxy("mymap") %>%
      clearControls() %>%
      clearMarkers() %>%
      clearShapes()
  }
  
  ########################
  ## PREDICTIONS FROM USER POINTS
  ########################
  
  ## LOAD RECEPTORS
  observeEvent(input$file.recept, {
    filename.recept$f <- input$file.recept
    err.code <- get.recept.shp()
    if (err.code != 0) {
      createAlert(
        session, "alert", "Alert", title = "ERROR",
        content = paste0("Message: ", error.message(err.code)), append = FALSE
      )
    }
  })
  
  get.recept.shp <- function(x) {
    err.code <- 0
    if (is.na(epsg$x) || !is.integer(epsg$x)) {
      return(1)
    }
    
    if (!is.null(lur.model$lm)) {
      pred.vars <-
        unlist(strsplit(names(lur.model$lm$coefficients)[-1], "_"))
    }
    
    ## ERROR HANDLING
    if (is.null(lur.model$lm)) {
      err.code <- 6
    }
    else if (is.null(dat$corine) &
             any(pred.vars %in% corine.vars)) {
      err.code <- 8
    }
    else if (is.null(dat$roads) &
             any(
               pred.vars %in% c(
                 roads.vars.length.maj, roads.vars.length.all,
                 roads.vars.nn.maj, roads.vars.nn.all
               )
             )) {
      err.code <- 9
    }
    else if (is.null(dat$population) &
             any(pred.vars %in% popn.vars)) {
      err.code <- 10
    }
    
    if (err.code != 0) {
      return(err.code)
    }
    
    tryCatch({
      ## CLEAR ANY PREVIOUS GRID
      leafletProxy("mymap") %>%
        clearGroup("pp")
      
      prj <- paste0("+init=epsg:", isolate(epsg$x))
      pred.grid <<-
        SpatialPoints(coords = slot(readShapePoints(get.shp(
          filename.recept$f
        )), "coords"), proj4string = CRS(prj))
      
      ## CLIP TO DATA SOURCES
      tryCatch({
        clip.grid(pred.vars)
      }, error = function(e) {
        pred.grid <<- NULL
      }, finally = function(e) {
        if (length(pred.grid) == 0) {
          err.code <- 11
        }
      })
      
      if (err.code != 0) {
        return(err.code)
      }
      
      ## SHOW POINTS ON MAP
      clear.draw()
      ext <- get.bbox(pred.grid, TRUE, isolate(epsg$x))
      leafletProxy("mymap") %>%
        addCircles(
          data = spTransform(pred.grid , CRS("+proj=longlat +datum=WGS84")),
          fill = TRUE, fillColor = "red", color = "red",
          radius = 2, weight = 10, fillOpacity = 1, group = "pp"
        ) %>%
        fitBounds(ext@bbox[1,1], ext@bbox[2,1], ext@bbox[1,2], ext@bbox[2,2])
      
      rownames(slot(pred.grid, "coords")) <<-
        seq(1:length(pred.grid))
      return(err.code)
    },
    error = function(e) {
      err.code <- 2
    }, finally = function(e) {
      return(err.code)
    })
  }
  
  ########################
  ## MAKE PREDICTIONS GRID
  ########################
  
  ## RESOLUTION
  res <- reactiveValues()
  observe({
    res$x <- as.integer(input$gres)
  })
  
  observeEvent(input$action.grid, {
    ## GET VARIABLES
    if (!is.null(lur.model$lm)) {
      pred.vars <-
        unlist(strsplit(names(lur.model$lm$coefficients)[-1], "_"))
    }
    
    ## ERROR HANDLING
    err.code <- 0
    if (input$cbrecept) {
      err.code <- 21
    }
    else if (is.na(res$x) || !is.integer(res$x)) {
      err.code <- 19
    }
    else if (is.null(lur.model$lm)) {
      err.code <- 6
    }
    else if (length(vertices$xy) == 0) {
      err.code <- 7
    }
    else if (is.null(dat$corine) &
             any(pred.vars %in% corine.vars)) {
      err.code <- 8
    }
    else if (is.null(dat$roads) &
             any(
               pred.vars %in% c(
                 roads.vars.length.maj, roads.vars.length.all,
                 roads.vars.nn.maj, roads.vars.nn.all
               )
             )) {
      err.code <- 9
    }
    else if (is.null(dat$population) &
             any(pred.vars %in% popn.vars)) {
      err.code <- 10
    }
    
    if (err.code == 0) {
      progress <- shiny::Progress$new(min = 0, max = 0)
      on.exit(progress$close())
      progress$set(value = 0, detail = "Making grid")
      
      ## CLEAR ANY PREVIOUS GRID
      leafletProxy("mymap") %>%
        clearGroup("pp") %>%
        removeShape(layerId = "raster")
      
      pred.grid <<- NULL
      
      ## GET DEFINED EXTENT POLYGON
      pred.ext <-
        SpatialPolygons(list(Polygons(list(
          Polygon(vertices$xy)
        ), 1)))
      projection(pred.ext) <- "+proj=longlat +datum=WGS84"
      pred.ext.trans <-
        spTransform(pred.ext, CRS(paste0("+init=epsg:", isolate(epsg$x))))
      
      ## MAKE POINT GRID
      tryCatch({
        pred.grid <<-
          spsample(pred.ext.trans, cellsize = isolate(input$gres), type = "regular")
      },
      ## IF NO POINTS, TAKE ONLY CENTROID (IF RESOLUTION SPECIFIED TOO BIG)
      error = function(e) {
        pred.grid <<- gCentroid(pred.ext.trans, byid = TRUE)
      })
      
      ## CLIP TO DATA SOURCES
      tryCatch({
        clip.grid(pred.vars)
      }, error = function(e) {
        pred.grid <<- NULL
      }, finally = function(e) {
        if (length(pred.grid) == 0) {
          err.code <- 11
        }
      })
    }
    
    if (err.code == 0) {
      ## CONVERT TO SPDF
      dimnames(slot(pred.grid, "coords")) <-
        list(seq(1:length(pred.grid)), c("x1", "x2"))
      
      pred.grid <<-
        SpatialPointsDataFrame(
          coords = slot(pred.grid, "coords"), proj4string = CRS(paste0("+init=epsg:", isolate(epsg$x))),
          data = data.frame(ID = dimnames(slot(
            pred.grid, "coords"
          ))[[1]])
        )
      
      ## SHOW POINTS ON MAP
      leafletProxy("mymap") %>%
        addCircles(
          data = spTransform(pred.grid , CRS("+proj=longlat +datum=WGS84")),
          fill = TRUE, fillColor = "red", color = "red",
          radius = 0.25, weight = 4, fillOpacity = 0, group = "pp"
        )
    }
    else {
      createAlert(
        session, "alert", "Alert", title = "GRID ERROR",
        content = paste0("Message: ", error.message(err.code)), append = FALSE
      )
    }
  })
  
  ## MAKE SURE IS MAXIMUM OF ALL LAYERS INCLUDING ALLOWING FOR BUFFERS
  clip.grid <- function(pred.vars) {
    corine.pred.vars <- pred.vars[which(pred.vars %in% corine.vars)]
    if (length(corine.pred.vars) != 0) {
      if (!is.null(dat$corine) & length(pred.grid) != 0) {
        this.bb <-
          get.bbox(dat$corine, FALSE, isolate(input$epsg))
        max.buffer <-
          as.numeric(max(pred.vars[which(pred.vars %in% corine.vars) + 1]))
        this.bb <-
          gBuffer(this.bb, byid = TRUE, width = -1 * (max.buffer / 2))
        pred.grid <<- gIntersection(pred.grid, this.bb, byid = TRUE)
      }
    }
    
    roads.pred.var.maj <-
      pred.vars[which(pred.vars %in% roads.vars.length.maj)]
    roads.pred.var.all <-
      pred.vars[which(pred.vars %in% roads.vars.length.all)]
    nn.pred.var.maj <-
      pred.vars[which(pred.vars %in% roads.vars.nn.maj)]
    nn.pred.var.all <-
      pred.vars[which(pred.vars %in% roads.vars.nn.all)]
    if (length(roads.pred.var.maj) + length(roads.pred.var.all) != 0) {
      this.bb <- get.bbox(dat$roads, FALSE, isolate(input$epsg))
      max.buffer <-
        as.numeric(max(c(pred.vars[which(pred.vars %in% roads.vars.length.maj) + 1]),
                       pred.vars[which(pred.vars %in% roads.vars.length.all) + 1]))
      this.bb <-
        gBuffer(this.bb, byid = TRUE, width = as.numeric(max.buffer) / 2)
      pred.grid <<- gIntersection(pred.grid, this.bb, byid = TRUE)
    }
    else if (length(nn.pred.var.maj) + length(nn.pred.var.all) != 0) {
      this.bb <- get.bbox(dat$roads, FALSE, isolate(input$epsg))
      pred.grid <<- gIntersection(pred.grid, this.bb, byid = TRUE)
    }
    
    popn.pred.vars <- pred.vars[which(pred.vars %in% popn.vars)]
    if (length(popn.pred.vars) != 0) {
      this.bb <-
        get.bbox(dat$population, FALSE, isolate(input$epsg))
      max.buffer <-
        as.numeric(max(pred.vars[which(pred.vars %in% popn.vars) + 1]))
      this.bb <-
        gBuffer(this.bb, byid = TRUE, width = max.buffer / 2)
      pred.grid <<- gIntersection(pred.grid, this.bb, byid = TRUE)
    }
  }
  
  ########################
  ## RUN PREDICTIONS
  ########################
  
  observeEvent(input$action.pred, {
    err.code <- 0
    if (is.null(pred.grid)) {
      err.code <- 12
    }
    else if (length(pred.grid) == 0) {
      err.code <- 11
    }
    else if (is.null(lur.model$lm)) {
      err.code <- 6
    }
    else if (length(names(lur.model$lm$coefficients)[-1]) == 0) {
      err.code <- 13
    }
    ## CHECK FOR UNKNOWN VARIABLES
    else if (any(!(
      unlist(lapply(names(lur.model$lm$coefficients)[-1], function(x)
        strsplit(x, "_")[[1]][1])) %in% c(
          corine.vars, roads.vars.length.maj, roads.vars.length.all,
          roads.vars.nn.maj, roads.vars.nn.all, popn.vars
        )
    )))
    {
      err.code <- 14
    }
    
    if (err.code == 0) {
      ## PREDICTION VARIABLES
      pred.vars <-
        unlist(strsplit(names(lur.model$lm$coefficients)[-1], "_"))
      
      ## PREDICTION NEW DATA LOCATIONS
      new.data <- coordinates(pred.grid)
      
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + 1
        }
        progress$set(value = value, detail = detail)
      }
      
      ## CORINE NEW DATA
      corine.pred.vars <-
        unique(pred.vars[which(pred.vars %in% corine.vars)])
      if (length(corine.pred.vars) != 0) {
        corine.buffers <-
          unique(pred.vars[which(pred.vars %in% corine.vars) + 1])
        
        progress <-
          shiny::Progress$new(min = 0, max = length(corine.buffers) * length(pred.grid))
        progress$set(message = "Computing", value = 0)
        
        new.data <-
          cbind(
            new.data, do.corine(
              updateProgress, pred.grid, dat$corine, corine.pred.vars, corine.buffers,
              isolate(input$corinecode)
            )
          )
        progress$close()
      }
      
      ## ROAD BUFFER NEW DATA
      roads.pred.var.maj <-
        pred.vars[which(pred.vars %in% roads.vars.length.maj)]
      roads.pred.var.all <-
        pred.vars[which(pred.vars %in% roads.vars.length.all)]
      if (length(roads.pred.var.maj) + length(roads.pred.var.all) != 0) {
        road.buffers.maj <-
          pred.vars[which(pred.vars %in% roads.vars.length.maj) + 1]
        road.buffers.all <-
          pred.vars[which(pred.vars %in% roads.vars.length.all) + 1]
        
        progress <-
          shiny::Progress$new(
            min = 0, max = length(unique(c(road.buffers.maj, road.buffers.all))) * length(pred.grid)     
          )
        progress$set(message = "Computing", value = 0)
        
        new.data <-
          cbind(
            new.data, do.roads.buffer(
              updateProgress, pred.grid, dat$roads, roads.pred.var.maj,
              roads.pred.var.all, road.buffers.maj, road.buffers.all,
              isolate(input$roadcodetotal), isolate(input$roadcodeHGV),
              isolate(input$roadmajor)
            )
          )
        progress$close()
      }
      
      ## ROAD NN NEW DATA
      nn.pred.var.maj <-
        pred.vars[which(pred.vars %in% roads.vars.nn.maj)]
      nn.pred.var.all <-
        pred.vars[which(pred.vars %in% roads.vars.nn.all)]
      if (length(nn.pred.var.maj) + length(nn.pred.var.all) != 0) {
        pmax <- length(pred.grid)
        if (length(nn.pred.var.maj) != 0 &
            length(nn.pred.var.all) != 0) {
          pmax <- 2 * pmax
        }
        progress <- shiny::Progress$new(min = 0, max = pmax)
        progress$set(message = "Computing", value = 0)
        new.data <-
          cbind(
            new.data, do.roads.dists(
              updateProgress, pred.grid, dat$roads, nn.pred.var.maj, nn.pred.var.all,
              isolate(input$roadcodetotal), isolate(input$roadcodeHGV),
              isolate(input$roadmajor)
            )
          )
        progress$close()
      }
      
      ## POPN NEW DATA
      popn.pred.vars <- pred.vars[which(pred.vars %in% popn.vars)]
      if (length(popn.pred.vars) != 0) {
        popn.buffers <- pred.vars[which(pred.vars %in% popn.vars) + 1]
        
        progress <-
          shiny::Progress$new(min = 0, max = 2 * length(pred.grid))
        progress$set(message = "Computing", value = 0)
        
        new.data <-
          cbind(
            new.data, do.popn(
              updateProgress, pred.grid, dat$population, popn.pred.vars, popn.buffers,
              isolate(input$popnpop), isolate(input$popnhhold)
            )
          )
        progress$close()
      }
      
      ## TRUNCATE TO TRAINING SET MIN-MAX
      if (input$cbtrunc == TRUE) {
        for (i in names(new.data)[3:length(names(new.data))]) {
          mx <- max(inFile$ts[, i])
          mn <- min(inFile$ts[, i])
          temp <- matrix(new.data[, i])
          for (j in 1:nrow(new.data)) {
            if(temp[j] < mn) {
              temp[j] <- mn
            }
            else if(temp[j] > mx) {
              temp[j] <- mx
            }
          }
          new.data[, i] <- as.numeric(temp)
        }
      }
      
      ## MAKE PREDICTION
      lur.pred <- predict(lur.model$lm, newdata = new.data)
      
      ## PLOT RASTER FOR A GRID OR POINTS FOR USER SHP
      clear.draw()
      clear.all()
      if (input$cbrecept == FALSE) {
        r <<- rasterFromXYZ(SpatialPointsDataFrame(
          coordinates(pred.grid), data.frame("response" = lur.pred)
        ))
        projection(r) <<- paste0("+init=epsg:", isolate(epsg$x))
        r <<- projectRaster(r, crs = "+proj=longlat +datum=WGS84")
        dn <- values(r)[!is.na(values(r))]
        
        ## IF LARGE RASTER (>2MB, but Leaflet could handle 4MB)
        if (object.size(r)[1] > 2000000) {
          shrink <- as.integer(object.size(r)[1] / 2000000)
          r.samp <- aggregate(r, fact = shrink, fun = mean)
        }
        else {
          r.samp <- r
        }
        
        leafletProxy("mymap") %>%
          clearGroup("pp") %>%
          addRasterImage(
            r.samp, colors = pal, opacity = 0.6, layerId = "raster", project = FALSE
          ) %>%
          addLegend(
            position = "bottomleft", pal = colorNumeric(pal, c(min(dn), max(dn))),
            title = isolate(input$response), values = dn
          )
      }
      else {
        r <<-
          SpatialPointsDataFrame(coordinates(pred.grid), data.frame("response" = lur.pred), match.ID = FALSE)
        projection(r) <<- paste0("+init=epsg:", isolate(epsg$x))
        v <- r@data$response
        leafletProxy("mymap") %>%
          clearGroup("pp") %>%
          addCircleMarkers(
            data = spTransform(r , CRS("+proj=longlat +datum=WGS84")),
            stroke = FALSE, fillOpacity = 0.8, fill = TRUE,
            fillColor = ~ colorNumeric(pal, r$response)(response), radius = 7, group = "raster"
          ) %>%
          addLegend(
            position = "bottomleft", pal = colorNumeric(pal, c(min(v), max(v))),
            title = isolate(input$response), values = v
          )
      }
      pred.grid <<- NULL
      d$draw <- TRUE
    }
    else {
      createAlert(
        session, "alert", "Alert", title = "PREDICTION ERROR",
        content = paste0("Message: ", error.message(err.code)), append = FALSE
      )
    }
  })
  
  ########################
  ## SAVE OUTPUT
  ########################
  
  output$downloadData <- downloadHandler(
    filename = function() {
      if (input$downloadItem == 1) {
        if (is.null(r)) {
          stop()
        }
        else if (input$cbrecept) {
          "lur_prediction.shp"
        }
        else {
          "lur_prediction.tif"
        }
      }
      else if (input$downloadItem == 2) {
        if (is.null(inFile$ts)) {
          stop()
        }
        else {
          "lur_trainingset.csv"
        }
      }
      else if (input$downloadItem == 3) {
        if (is.null(lur.model$lm)) {
          stop()
        }
        else {
          "lur_model.txt"
        }
      }
      else if (input$downloadItem == 4) {
        "lur_workspace.Rdata"
      }
    },
    content = function(file) {
      if (input$downloadItem == 1) {
        if (input$cbrecept) {
          writePointsShape(r, fn = file)
        }
        else {
          writeRaster(r, filename = file, format = "GTiff", overwrite = TRUE)
        }
      }
      else if (input$downloadItem == 2) {
        write.csv(sel.data <- remove.outliers(), file = file)
      }
      else if (input$downloadItem == 3) {
        tmp <- selectedModel()
        write.table(data.frame(tmp), file = file)
        write.table(data.frame(lur.loocv), file = file, append = TRUE)
      }
      else if (input$downloadItem == 4) {
        save.image(file = file)
      }
    }
  )
  
  ############################################################################################################
  ############################################################################################################
  #### DESCRIPTIONS TAB ######################################################################################
  
  output$table.escape <- DT::renderDataTable({
    DT::datatable(
      escape, options = list(
        scrollX = TRUE,
        rownames = FALSE,
        ordering = TRUE,
        searching = TRUE,
        lengthMenu = NULL,
        selection = "none",
        pageLength = 15,
        extensions = 'Responsive'
      )
    )
  })
  
  ############################################################################################################
  ############################################################################################################
  #### HELP TAB ##############################################################################################
  
  output$instructions <- renderText({
    readLines("help/help1.htm")
  })
  observeEvent(input$help1, {
    output$instructions <- renderText({
      readLines("help/help1.htm")
    })
  })
  observeEvent(input$help2, {
    output$instructions <- renderText({
      readLines("help/help2.htm")
    })
  })
  observeEvent(input$help3, {
    output$instructions <- renderText({
      readLines("help/help3.htm")
    })
  })
  observeEvent(input$help4, {
    output$instructions <- renderText({
      readLines("help/help4.htm")
    })
  })
  observeEvent(input$help5, {
    output$instructions <- renderText({
      readLines("help/help5.htm")
    })
  })
  observeEvent(input$help5a, {
    output$instructions <- renderText({
      readLines("help/help5a.htm")
    })
  })
  observeEvent(input$help6, {
    output$instructions <- renderText({
      readLines("help/help6.htm")
    })
  })
  observeEvent(input$help7, {
    output$instructions <- renderText({
      readLines("help/help7.htm")
    })
  })
})

###################### END ######################
