################################################
## ESCAPE variables
## Road NN and traffic loads
################################################
do.roads.dists <-
  function(updateProgress = NULL, monitors, roads, var.names.maj, var.names.all,
           attr.all, attr.hgv, major.thresh) {
    ## Create empty data frame for results
    attrs.full <- c(var.names.maj, var.names.all)
    traffic.dists <-
      data.frame(matrix(
        vector(), length(monitors), length(attrs.full), dimnames = list(c(), attrs.full)
      ),
      stringsAsFactors = F)
    
    ## All roads
    if (length(var.names.all != 0)) {
      ## Find nearest neighbour and distance
      dist.table <- c()
      for (p in 1:length(monitors)) {
        if (is.function(updateProgress)) {
          prog.message <- paste("Measuring", p, sep = " ")
          updateProgress(detail = prog.message)
        }
        d <- gDistance(monitors[p,], roads, byid = TRUE)
        nn.dist <- min(d)
        nn.id <- which(d == nn.dist)
        dist.table <-
          rbind(dist.table, cbind(as.integer(row.names(monitors[p,])) + 1, nn.dist, nn.id - 1))
      }

      for (p in 1:length(monitors)) {
        this.att <-
          slot(roads[which(row.names(roads) == dist.table[p, 3]),], "data")
        this.dist <- dist.table[p, 2]
        if ("TRAFNEAR" %in% var.names.all)
          traffic.dists[p, "TRAFNEAR"]  <- this.att[, attr.all]
        if ("DISTINVNEAR1" %in% var.names.all)
          traffic.dists[p, "DISTINVNEAR1"] <- 1 / this.dist[1]
        if ("DISTINVNEAR2" %in% var.names.all)
          traffic.dists[p, "DISTINVNEAR2"] <- 1 / (this.dist[1] ^ 2)
        if ("INTINVDIST" %in% var.names.all)
          traffic.dists[p, "INTINVDIST"] <-
          this.att[, attr.all] * (1 / this.dist[1])
        if ("INTINVDIST2" %in% var.names.all)
          traffic.dists[p, "INTINVDIST2"] <-
          this.att[, attr.all] * (1 / (this.dist[1] ^ 2))
        if ("HEAVYTRAFNEAR" %in% var.names.all)
          traffic.dists[p, "HEAVYTRAFNEAR"] <- this.att[, attr.hgv]
      }
    }

    ## Major roads
    if (length(var.names.maj != 0)) {
      major <- roads[roads@data[, attr.all] > major.thresh,]

      ## Find nearest neighbour and distance
      dist.table <- c()
      for (p in 1:length(monitors)) {
        if (is.function(updateProgress)) {
          prog.message <- paste("Measuring", p, sep = " ")
          updateProgress(detail = prog.message)
        }
        d <- gDistance(monitors[p,], major, byid = TRUE)
        nn.dist <- min(d)
        nn.id <- which(d == nn.dist)
        dist.table <-
          rbind(dist.table, cbind(as.integer(row.names(monitors[p,])) + 1, nn.dist, nn.id - 1))
      }
      
      for (p in 1:length(monitors)) {
        this.att <-
          slot(major[which(row.names(roads) == dist.table[p, 3]),], "data")
        this.dist <- dist.table[p, 2]
        if ("TRAFMAJOR" %in% var.names.maj)
          traffic.dists[p, "TRAFMAJOR"] <- this.att[, attr.all]
        if ("HEAVYTRAFMAJOR" %in% var.names.maj)
          traffic.dists[p, "HEAVYTRAFMAJOR"] <- this.att[, attr.hgv]
        if ("DISTINVMAJOR1" %in% var.names.maj)
          traffic.dists[p, "DISTINVMAJOR1"] <- 1 / this.dist[1]
        if ("DISTINVMAJOR2" %in% var.names.maj)
          traffic.dists[p, "DISTINVMAJOR2"] <-
          1 / (this.dist[1] ^ 2)
        if ("INTMAJORINVDIST" %in% var.names.maj)
          traffic.dists[p, "INTMAJORINVDIST"] <-
          this.att[, attr.all] * (1 / this.dist[1])
        if ("INTMAJORINVDIST2" %in% var.names.maj)
          traffic.dists[p, "INTMAJORINVDIST2"] <-
          this.att[, attr.all] * (1 / (this.dist[1] ^ 2))
      }
    }
    traffic.dists
  }

################################################
## ESCAPE variables
## Road buffers and traffic loads
################################################
do.roads.buffer <-
  function(updateProgress = NULL, monitors, roads, var.names.maj,
           var.names.all, buffer.sizes.maj, buffer.sizes.all,
           attr.all, attr.hgv, major.thresh) {
    ## Create empty data frame for results
    attrs.full <-
      unlist(lapply(var.names.maj, function(x)
        paste(x, buffer.sizes.maj, sep = "_")))
    attrs.full <-
      append(attrs.full, unlist(lapply(var.names.all, function(x)
        paste(x, buffer.sizes.all, sep = "_"))))
    traffic.loads <-
      data.frame(matrix(
        vector(), length(monitors), length(attrs.full),
        dimnames = list(c(), attrs.full)
      ),stringsAsFactors = F)
    
    major <- roads[roads@data[, attr.all] > major.thresh,]
    buffers <- unique(c(buffer.sizes.maj, buffer.sizes.all))
    
    ## Do intersection
    for (k in 1:length(buffers)) {
      ## Progress bar
      if (is.function(updateProgress)) {
        prog.message <- paste("Buffering", buffers[k], sep = " ")
        updateProgress(detail = prog.message)
      }
      
      b <- gBuffer(monitors, byid = TRUE, width = buffers[k])
      
      ## MAJOR ROADS ONLY
      if (buffers[k] %in% buffer.sizes.maj) {
        i <- gIntersection(b, major, byid = TRUE)
        lengths <- gLength(i, byid = TRUE)
        line.ids <-
          unlist(lapply(names(lengths), function(x)
            unlist(strsplit(
              as.character(x), " "
            ))[2]))
        rec.ids <-
          unlist(lapply(names(lengths), function(x)
            unlist(strsplit(
              as.character(x), " "
            ))[1]))
        line.total <-
          slot(roads[as.integer(line.ids) + 1,], "data")[, attr.all]
        line.hgv <-
          slot(roads[as.integer(line.ids) + 1,], "data")[, attr.hgv]
        length.table <-
          cbind(as.integer(rec.ids) + 1, as.integer(line.ids), lengths, line.total, line.hgv)
        colnames(length.table) <-
          c("RecID", "LineID", "Length", "Total", "HGV")
        for (p in 1:length(monitors)) {
          length.p <-
            length.table[which(length.table[,"RecID"] == p), ,drop = FALSE]
          total.roads <- sum(length.p[, 3])
          traffic.loads[p, paste("MAJORROADLENGTH", buffers[k], sep = "_")] <-
            total.roads
          traffic.loads[p, paste("TRAFMAJORLOAD", buffers[k], sep = "_")] <-
            sum(length.p[, "Length"] * length.p[, "Total"])
          traffic.loads[p, paste("HEAVYTRAFMAJORLOAD", buffers[k], sep = "_")] <-
            sum(length.p[, "Length"] * length.p[, "HGV"])
        }
      }
      
      ## ALL ROADS
      if (buffers[k] %in% buffer.sizes.all) {
        i <- gIntersection(b, roads, byid = TRUE)
        lengths <- gLength(i, byid = TRUE)
        line.ids <-
          unlist(lapply(names(lengths), function(x)
            unlist(strsplit(
              as.character(x), " "
            ))[2]))
        rec.ids <-
          unlist(lapply(names(lengths), function(x)
            unlist(strsplit(
              as.character(x), " "
            ))[1]))
        line.total <-
          slot(roads[as.integer(line.ids) + 1,], "data")[, attr.all]
        line.hgv <-
          slot(roads[as.integer(line.ids) + 1,], "data")[, attr.hgv]
        length.table <-
          cbind(as.integer(rec.ids) + 1, as.integer(line.ids), lengths, line.total, line.hgv)
        colnames(length.table) <-
          c("RecID", "LineID", "Length", "Total", "HGV")
        for (p in 1:length(monitors)) {
          length.p <-
            length.table[which(length.table[,"RecID"] == p), ,drop = FALSE]
          total.roads <- sum(length.p[, 3])
          traffic.loads[p, paste("ROADLENGTH", buffers[k], sep = "_")] <-
            total.roads
          traffic.loads[p, paste("TRAFLOAD", buffers[k], sep = "_")] <-
            sum(length.p[, "Length"] * length.p[, "Total"])
          traffic.loads[p, paste("HEAVYTRAFLOAD", buffers[k], sep = "_")] <-
            sum(length.p[, "Length"] * length.p[, "HGV"])
        }
      }
    }
    traffic.loads
  }

################################################
## ESCAPE variables
## CORINE landcover buffers
################################################
do.corine <-
  function(updateProgress = NULL, monitors, landcover, var.names, buffer.sizes, attr.code) {
    ## Create empty data frame for results
    attrs.full <-
      unlist(lapply(var.names, function(x)
        paste(x, buffer.sizes, sep = "_")))
    landcover.areas <-
      data.frame(matrix(
        vector(), length(monitors), length(attrs.full),
        dimnames = list(c(), attrs.full)
      ),stringsAsFactors = F)
    
    ## Do intersection
    for (k in 1:length(buffer.sizes)) {
      ## Progress bar
      if (is.function(updateProgress)) {
        prog.message <- paste("Buffering", buffer.sizes[k], sep = " ")
        updateProgress(detail = prog.message)
      }
      b <- gBuffer(monitors, byid = TRUE, width = buffer.sizes[k])
      i <- gIntersection(b, landcover, byid = TRUE)
      area <- gArea(i, byid = TRUE)
      ## Format table
      poly.ids <-
        unlist(lapply(names(area), function(x)
          unlist(strsplit(
            as.character(x), " "
          ))[2]))
      rec.ids <-
        unlist(lapply(names(area), function(x)
          unlist(strsplit(
            as.character(x), " "
          ))[1]))
      poly.code <-
        unlist(lapply(poly.ids, function(x)
          slot(landcover[which(row.names(landcover) == x),], "data")[, attr.code]))
      area.table <-
        cbind(as.integer(rec.ids) + 1, as.integer(poly.ids), as.integer(poly.code), area)
      colnames(area.table) <-
        c("RecID", "PolyID", attr.code, "Area")
      
      ## Sum up the areas
      for (p in 1:length(monitors)) {
        ## subset for this point
        area.p <-
          area.table[which(area.table[,"RecID"] == p), ,drop = FALSE]
        ## sum categories
        if ("HDRES" %in% var.names)
          landcover.areas[p, paste("HDRES", buffer.sizes[k], sep = "_")] <-
            sum(area.p[which(area.p[, attr.code] == 111), "Area"])
        if ("LDRES" %in% var.names)
          landcover.areas[p, paste("LDRES", buffer.sizes[k], sep = "_")] <-
            sum(area.p[which(area.p[, attr.code] == 112), "Area"])
        if ("INDUSTRY" %in% var.names)
          landcover.areas[p, paste("INDUSTRY", buffer.sizes[k], sep = "_")] <-
            sum(area.p[which(area.p[, attr.code] == 121), "Area"])
        if ("PORT" %in% var.names)
          landcover.areas[p, paste("PORT", buffer.sizes[k], sep = "_")] <-
            sum(area.p[which(area.p[, attr.code] == 123), "Area"])
        if ("URBGREEN" %in% var.names)
          landcover.areas[p, paste("URBGREEN", buffer.sizes[k], sep = "_")] <-
            sum(area.p[which(area.p[, attr.code] == 141), "Area"] +
                  sum(area.p[which(area.p[, attr.code] == 142), "Area"]))
        if ("NATURAL" %in% var.names)
          landcover.areas[p, paste("NATURAL", buffer.sizes[k], sep = "_")] <-
            sum(area.p[which(area.p[, attr.code] == 311), "Area"]) +
            sum(area.p[which(area.p[, attr.code] == 312), "Area"]) +
            sum(area.p[which(area.p[, attr.code] == 313), "Area"]) +
            sum(area.p[which(area.p[, attr.code] == 321), "Area"]) +
            sum(area.p[which(area.p[, attr.code] == 322), "Area"]) +
            sum(area.p[which(area.p[, attr.code] == 323), "Area"]) +
            sum(area.p[which(area.p[, attr.code] == 324), "Area"]) +
            sum(area.p[which(area.p[, attr.code] == 331), "Area"]) +
            sum(area.p[which(area.p[, attr.code] == 523), "Area"]) +
            sum(area.p[which(area.p[, attr.code] == 332), "Area"]) +
            sum(area.p[which(area.p[, attr.code] == 333), "Area"]) +
            sum(area.p[which(area.p[, attr.code] == 334), "Area"]) +
            sum(area.p[which(area.p[, attr.code] == 335), "Area"]) +
            sum(area.p[which(area.p[, attr.code] == 411), "Area"]) +
            sum(area.p[which(area.p[, attr.code] == 412), "Area"]) +
            sum(area.p[which(area.p[, attr.code] == 421), "Area"]) +
            sum(area.p[which(area.p[, attr.code] == 422), "Area"]) +
            sum(area.p[which(area.p[, attr.code] == 423), "Area"]) +
            sum(area.p[which(area.p[, attr.code] == 512), "Area"]) +
            sum(area.p[which(area.p[, attr.code] == 521), "Area"]) +
            sum(area.p[which(area.p[, attr.code] == 522), "Area"])
      }
    }
    landcover.areas
  }

################################################
## ESCAPE variables
## Population/Household buffers
################################################
do.popn <-
  function(updateProgress = NULL, monitors, popn, var.names, buffer.sizes,
           attr.pop, attr.hhold) {
    ## Create empty data frame for results
    attrs.full <-
      unlist(lapply(var.names, function(x)
        paste(x, buffer.sizes, sep = "_")))
    postcode.sums <-
      data.frame(matrix(
        vector(), length(monitors), length(attrs.full),
        dimnames = list(c(), attrs.full)
      ),stringsAsFactors = F)
    
    ## Do overlay
    for (k in 1:length(buffer.sizes)) {
      ## Progress bar
      if (is.function(updateProgress)) {
        prog.message <- paste("Buffering", buffer.sizes[k], sep = " ")
        updateProgress(detail = prog.message)
      }
      
      b <- gBuffer(monitors, byid = TRUE, width = buffer.sizes[k])
      if ("POP" %in% var.names)
        postcode.sums[paste("POP", buffer.sizes[k], sep = "_")] <-
        over(b, popn[attr.pop], fn = sum)
      if ("HHOLD" %in% var.names)
        postcode.sums[paste("HHOLD", buffer.sizes[k], sep = "_")] <-
        over(b, popn[attr.hhold], fn = sum)
    }
    postcode.sums
  }