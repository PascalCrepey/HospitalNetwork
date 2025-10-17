plot_hist_degree_fnct <- function(hist_degrees, degrees) {
      hd_long <- melt(hist_degrees, id.vars = "degree")
      p <- ggplot(hd_long[!is.na(value)], aes(x = degree, y = value, fill = variable)) +
        geom_col(position = "dodge") +
        scale_fill_discrete(name = NULL) +
        scale_x_continuous(name = "Degree", limits = c(min(degrees[, -1]) - 1, NA)) +
        scale_y_continuous(name = "Number of facilities") +
        theme_bw() +
        theme(legend.position = "bottom")
      p
}


plot_matrix_fnct <- function(n_facilities, edgelist) {
      n_h <- n_facilities
      ggplot(edgelist, aes(x = target, y = origin)) +
        geom_raster(aes(fill = N)) +
        scale_fill_gradient(low = "grey90", high = "red") +
        scale_x_discrete(
          name = "Target",
          # take the breaks from origin to get the same order
          breaks = edgelist[, unique(origin)][seq(1, n_h, length.out = 10)]
        ) +
        scale_y_discrete(
          name = "Origin",
          breaks = edgelist[, unique(origin)][seq(1, n_h, length.out = 10)]
        ) +
        labs(x = "Origin", y = "Target", title = "Matrix") +
        theme_bw() +
        theme(
          axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3),
          axis.text.y = element_text(size = 8),
          plot.title = element_text(size = 12),
          panel.grid = element_blank()
        )
}


plot_spaghetti_fnct <- function(matrix, cluster_infomap, plotLinks = 5000, alphaSet = 0.1, edgeColourNode = FALSE, facilityColours = NULL, firstCircleCol = NULL, secondCircleCol = NULL) {


      # function to average the color of two nodes for the edge colour. Currently takes
      meanColor <- function(col1, col2) {
        paste0(
          "#",
          stringr::str_pad(sprintf("%x", floor((as.integer(as.hexmode(stringr::str_sub(col1, 2, 3))) +
            as.integer(as.hexmode(stringr::str_sub(col2, 2, 3)))) / 2)), 2, side = "left", pad = "0"),
          stringr::str_pad(sprintf("%x", floor((as.integer(as.hexmode(stringr::str_sub(col1, 4, 5))) +
            as.integer(as.hexmode(stringr::str_sub(col2, 4, 5)))) / 2)), 2, side = "left", pad = "0"),
          stringr::str_pad(sprintf("%x", floor((as.integer(as.hexmode(stringr::str_sub(col1, 6, 7))) +
            as.integer(as.hexmode(stringr::str_sub(col2, 6, 7)))) / 2)), 2, side = "left", pad = "0")
        )
      }

      # checking the parameters
      if (alphaSet > 1 || alphaSet < 0) {
        warning("alphaSet should be between 0 and 1, set to 1.0")
        alphaSet <- 1
      }
      if (plotLinks > length(matrix)) plotLinks <- length(matrix)

      # Creating underlying hierarchy
      myleaves <- colnames(matrix)
      comms <- split(cluster_infomap, by = "cluster_infomap")
      getComRow <- function(y) {
        unlist(lapply(1:length(comms), function(x) {
          sum(matrix[(c(comms[[x]][, "node"]))[[1]], (c(comms[[y]][, "node"]))[[1]]])
        }))
      }
      absMat <- matrix(unlist(lapply(1:length(comms), getComRow)), nrow = length(comms), ncol = length(comms))

      newDendro <- hclust(as.dist(1 / (absMat + 0.001)), method = "average")

      levelData <- data.frame(
        firstLevel = cutree(newDendro, ceiling(length(newDendro$order) / 4)),
        secondLevel = cutree(newDendro, ceiling(length(newDendro$order) / 6)),
        thirdlevel = cutree(newDendro, ceiling(length(newDendro$order) / 12))
      )
      hierarchy <- rbind(
        (data.frame(from = rep("origin", max(levelData[, 3])), to = unique(paste0("level3-", levelData[, 3])))),
        unique(data.frame(from = paste0("level3-", levelData[, 3]), to = paste0("level2-", levelData[, 2]))),
        unique(data.frame(from = paste0("level2-", levelData[, 2]), to = paste0("level1-", levelData[, 1]))),
        data.frame(from = paste0("level1-", levelData[, 1]), to = paste0("comms-", names(comms))),
        data.frame(from = paste0("comms-", data.frame(cluster_infomap)[, "cluster_infomap"]), to = (cluster_infomap[, "node"])[[1]])
      )
      vertices <- data.frame(name = unique(c(as.character(hierarchy$from), as.character(hierarchy$to))))

      # Create the graph and extract the layout
      mygraph <- igraph::graph_from_data_frame(hierarchy, vertices = vertices)
      lay <- ggraph::create_layout(mygraph, layout = "dendrogram", circular = TRUE)

      # Set the node colours, standard node colour is black
      # lay$nodecol=rgb(0,0,0)
      # if(!is.null(facilityColours)){
      #  for(ii in 1:length(facilityColours)) lay$nodecol[lay$name %in% facilityColours[[ii]]$facility]<-facilityColours[[ii]]$colour
      # }
      lay$nodecol <- rgb(0, 0, 0)
      lay$firstCircle <- rgb(0, 0, 0)
      lay$secondCircle <- rgb(0, 0, 0)

      if (!is.null(facilityColours)) {
        for (ii in 1:length(facilityColours)) lay$nodecol[lay$name %in% facilityColours[[ii]]$facility] <- facilityColours[[ii]]$colour
      }
      if (!is.null(firstCircleCol)) {
        for (ii in 1:length(firstCircleCol)) lay$firstCircle[lay$name %in% firstCircleCol[[ii]]$facility] <- firstCircleCol[[ii]]$colour
      }
      if (!is.null(secondCircleCol)) {
        for (ii in 1:length(secondCircleCol)) lay$secondCircle[lay$name %in% secondCircleCol[[ii]]$facility] <- secondCircleCol[[ii]]$colour
      }

      # create the connections
      # set the minimum number of shared patients for an edge to be plotted
      minShares <- matrix[order(-matrix)][plotLinks]
      strongConnections <- data.frame(
        to = myleaves[which(matrix > minShares, arr.ind = TRUE)[, 1]],
        from = myleaves[which(matrix > minShares, arr.ind = TRUE)[, 2]],
        weight = (matrix[matrix > minShares])
      )

      # artificially duplicate strong connections so that they appear darker on the plot
      # strongConnections<-do.call("rbind", lapply(seq(minStr,maxStr,by=steps),function(x){data.frame(from=myleaves[which(matrix>x,arr.ind = TRUE)[,1]],to=myleaves[which(matrix>x,arr.ind = TRUE)[,2]])}))
      fromVert <- match(strongConnections$from, vertices$name)
      toVert <- match(strongConnections$to, vertices$name)
      weights <- strongConnections$weight
      # Create the edge colouring, which is a combination of bnode colours and alpha for number of shares.
      if (edgeColourNode) {
        strWeights <- paste0(
          meanColor(lay[toVert, ]$nodecol, lay[fromVert, ]$nodecol),
          stringr::str_pad(as.hexmode(floor(255.0 * alphaSet * weights / max(weights))), 2, side = "left", pad = "0")
        )
      } else {
        strWeights <- paste0(
          "#000000",
          stringr::str_pad(as.hexmode(floor(255.0 * alphaSet * weights / max(weights))), 2, side = "left", pad = "0")
        )
      }

      # calculate the coordiantes for the outer rings
      nLeafs <- sum(lay$leaf)
      lay$angle <- atan2(lay$x, lay$y)
      lay$circleIndex <- 0
      lay$arcstart <- 0
      lay$arcend <- 0
      lay[lay$leaf == TRUE, ]$circleIndex <- order(order(lay[lay$leaf == TRUE, ]$angle))
      lay[lay$leaf == TRUE, ]$arcstart <- ((2 * pi * (lay[lay$leaf == TRUE, ]$circleIndex - 1.25)) / nLeafs) + pi
      lay[lay$leaf == TRUE, ]$arcend <- ((2 * pi * (lay[lay$leaf == TRUE, ]$circleIndex - 0.25)) / nLeafs) + pi

      # Create the full plot
      spaghetti <-
        ggraph::ggraph(lay) +
        ggraph::geom_conn_bundle(
          data = ggraph::get_con(to = toVert, from = fromVert, valueF = strWeights),
          aes(color = valueF),
          tension = 0.80, n = 50
        ) +
        ggraph::scale_edge_color_manual(limits = strWeights, values = strWeights) +
        ggraph::geom_node_point(aes(filter = leaf, x = x * 1.00, y = y * 1.00, color = nodecol), stroke = 0.5, pch = 16, size = 2) +
        ggplot2::scale_colour_manual(limits = unique(lay$nodecol), values = unique(lay$nodecol)) +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::scale_fill_manual(limits = unique(c(lay$firstCircle, lay$secondCircle)), values = unique(c(lay$firstCircle, lay$secondCircle)))


      if (!is.null(firstCircleCol)) {
        spaghetti <- spaghetti + ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r = 1.05, r0 = 1.1, start = arcstart, end = arcend, fill = firstCircle), linetype = 0)
      }
      if (!is.null(secondCircleCol)) {
        spaghetti <- spaghetti + ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r = 1.11, r0 = 1.16, start = arcstart, end = arcend, fill = secondCircle), linetype = 0)
      }

      spaghetti
}


plot_clustered_matrix_fnct = function(igraph, edgelist, n_facilities) {
      # get the clustering
      cl <- cluster_infomap(igraph, modularity = FALSE)

      dt_cl <- data.table(cl$names, cl$membership)
      # put it in a copy of the edgelist
      el <- copy(edgelist)
      el[dt_cl, origin_c := V2, on = c("origin" = "V1")]
      el[dt_cl, target_c := V2, on = c("target" = "V1")]
      el[, col := ifelse(origin_c == target_c, origin_c, 0)]
      el[, col := factor(col)]

      # reorder the axis to make the cluster appear
      el$origin <- factor(el$origin, levels = unique(el[order(origin_c), origin]))
      el$target <- factor(el$target, levels = unique(el[order(target_c), target]))

      n_h <- n_facilities
      # plot it
      ggplot(el, aes(x = origin, y = target)) +
        geom_raster(aes(fill = col)) +
        scale_fill_manual(name = NULL, values = c("grey", rainbow(max(el$origin_c)))) +
        scale_x_discrete(
          name = "Origin",
          breaks = levels(el$origin)[round(seq(1, n_h, length.out = 10), 0)]
        ) +
        scale_y_discrete(
          name = "Target",
          breaks = levels(el$target)[round(seq(1, n_h, length.out = 10), 0)]
        ) +
        labs(x = "Origin", y = "Target", title = "Clustered matrix") +
        theme_bw() +
        theme(
          axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3),
          axis.text.y = element_text(size = 8),
          plot.title = element_text(size = 12),
          panel.grid = element_blank()
        )
}
  