escape <-
  structure(
    list(
      Dataset = structure(
        c(
          1L, 1L, 1L, 1L, 1L, 1L,
          2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
          3L, 3L, 3L, 3L, 3L, 3L
        ), .Label = c("CORINE", "Population", "Road network"), class = "factor"
      ), Predictor.variable = structure(
        c(
          5L, 9L,
          6L, 12L, 28L, 21L, 11L, 10L, 27L, 8L, 7L, 18L, 17L, 26L, 2L,
          1L, 16L, 15L, 25L, 24L, 4L, 14L, 13L, 3L, 23L, 22L, 19L, 20L
        ), .Label = c(
          "Inverse distance squared to the nearest major road",
          "Inverse distance to the nearest major road", "Heavy-duty traffic intensity on nearest major road",
          "Heavy-duty traffic intensity on nearest road", "High density residential land",
          "Industry", "Inverse distance squared to the nearest road", "Inverse distance to the nearest road",
          "Low density residential land", "Number of households", "Number of inhabitants",
          "Port", "Product of heavy vehicle traffic intensity on nearest major road and inverse of distance squared to the nearest road",
          "Product of heavy vehicle traffic intensity on nearest major road and inverse of distance to the nearest road",
          "Product of traffic intensity on nearest major road and inverse of distance squared to the nearest road",
          "Product of traffic intensity on nearest major road and inverse of distance to the nearest road",
          "Product of traffic intensity on nearest road and inverse of distance squared to the nearest road",
          "Product of traffic intensity on nearest road and inverse of distance to the nearest road",
          "Road length of all roads in a buffer", "Road length of major roads in a buffer",
          "Semi-natural and forested areas", "Total heavy-duty traffic load of all roads in a buffer (sum of (heavy-duty traffic intensity * length of all segments))",
          "Total heavy-duty traffic load of major roads in a buffer (sum of (heavy-duty traffic intensity * length of all segments))",
          "Total traffic load of all roads in a buffer (sum of (traffic intensity * length of all segments))",
          "Total traffic load of major roads in a buffer (sum of (traffic intensity * length of all segments))",
          "Traffic intensity on nearest major road", "Traffic intensity on nearest road",
          "Urban green"
        ), class = "factor"
      ), Name.variable1 = structure(
        c(
          5L,
          18L, 13L, 22L, 28L, 20L, 21L, 12L, 27L, 3L, 4L, 14L, 15L, 25L,
          1L, 2L, 16L, 17L, 26L, 24L, 11L, 6L, 7L, 9L, 10L, 8L, 23L, 19L
        ), .Label = c(
          "DISTINVMAJOR1", "DISTINVMAJOR2", "DISTINVNEAR1",
          "DISTINVNEAR2", "HDRES", "HEAVYINTINVDIST", "HEAVYINTINVDIST2",
          "HEAVYTRAFLOAD", "HEAVYTRAFMAJOR", "HEAVYTRAFMAJORLOAD", "HEAVYTRAFNEAR",
          "HHOLD", "INDUSTRY", "INTINVDIST", "INTINVDIST2", "INTMAJORINVDIST",
          "INTMAJORINVDIST2", "LDRES", "MAJORROADLENGTH", "NATURAL", "POP",
          "PORT", "ROADLENGTH", "TRAFLOAD", "TRAFMAJOR", "TRAFMAJORLOAD",
          "TRAFNEAR", "URBGREEN"
        ), class = "factor"
      ), Unit = structure(
        c(
          4L,
          4L, 4L, 4L, 4L, 4L, 5L, 5L, 6L, 2L, 3L, 8L, 9L, 6L, 2L, 3L, 8L,
          9L, 7L, 7L, 6L, 8L, 9L, 6L, 7L, 7L, 1L, 1L
        ), .Label = c(
          "m",
          "m-1", "m-2", "m2", "N(umber)", "Veh.day-1", "Veh.day-1m", "Veh.day-1m-1",
          "Veh.day-1m-2"
        ), class = "factor"
      ), Buffer.size..radius.of.buffer.in.meter. = structure(
        c(
          1L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, 2L, 2L, NA, NA, NA, NA, 2L, 2L, 2L, 2L
        ), .Label = c("100,  300, 500, 1000, 5000",
                      "25, 50, 100,  300, 500, 1000"), class = "factor"
      ), Direction.of.effect = structure(
        c(
          2L,
          2L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
          2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L
        ), .Label = c("-",
                      "+"), class = "factor"
      )
    ), .Names = c(
      "Dataset", "Predictor.variable",
      "Name.variable1", "Unit", "Buffer.size..radius.of.buffer.in.meter.",
      "Direction.of.effect"
    ), class = "data.frame", row.names = c(NA,-28L)
  )

colnames(escape) <- c("Dataset", "Variable", "Description", "Unit", "Buffers", "Direction")
