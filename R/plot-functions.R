library(ggplot2)
library(patchwork)

PLOTHINTS = list(
  "3/4-siblings" = list(order = 1:7, spouse = rbind(c(3,5,0), c(5,4,0))),
  "3/4-siblings + child" = list(order = 1:8, spouse = rbind(c(3,5,0), c(5,4,0)))
)

plotped = function(ped, ids, col, title, pedname = NULL) {
  colvec = ifelse(labels(ped) %in% ids, col, 1)
  hints = PLOTHINTS[[pedname]]
  suppressWarnings(
    plot(ped, hatched = ids, col = colvec, title = title, cex.main = 1.5, cex = 1.25, 
         margin = c(.5, .5, 3.5, .5), hints = hints)
  )
}

parsePlotError = function(e) {
  msg = conditionMessage(e)
  if(grepl("reduce cex", msg))
    msg = "Pedigree is too big for plotting.\n(You may still perform simulations.)"
  msg
}



getSegmentData = function(sim, analysis, cutoff) {
  if(is.null(sim))
    return(NULL)
  
  ids = extractIds(sim)
  
  # Extract segments according to analysis type
  pattern = setNames(list(ids), switch(analysis, Sharing = "carriers", Autozygosity = "autozygous"))
  segs = findPattern(sim, pattern, cutoff = cutoff)
  
  # Return stats
  segmentStats(segs, returnAll = TRUE)
}

generateIbdPlot = function(segData, analysis, cols) {
  # assume no entry or segData empty!
  labs = names(segData)
  
  perSimList = lapply(labs, function(lb) cbind(segData[[lb]]$perSim, Relationship = lb))
  perSim = do.call(rbind, perSimList)
  
  allSegsList = lapply(labs, function(lb) segData[[lb]]$allSegs)
  allSegs = data.frame(Length = unlist(allSegsList, use.names = FALSE), 
                       Relationship = rep(labs, lengths(allSegsList)))
  
  # Plot variables
  base = 14
  legendInside = TRUE
  
  
  # Plot1: Average length vs count
  g1 = ggplot(perSim, aes_(~Count, ~Average, color = ~Relationship)) + 
    geom_jitter(width = 0.35, alpha = 0.5, shape = 1, show.legend = FALSE) +
    labs(x = "Number of segments", y = "Average segment (cM)", col = NULL) +
    suppressWarnings(stat_ellipse(size = 1.2)) +
    theme_classic(base_size = base)
  
  if(legendInside)
    g1 = g1 + theme(legend.position = c(.99, .99),
                    legend.justification = c("right", "top"),
                    legend.key.width = unit(1, "cm"),
                    legend.text = element_text(size = 14))
  
  # Plot 2: Distr of total sharing
  g2 = ggplot(perSim, aes_(~Total, color = ~Relationship)) + 
    geom_density(aes_(fill = ~Relationship), alpha = 0.1, size = 1.2, show.legend = FALSE) +
    labs(x = switch(analysis, Sharing = "Total shared cM", Autozygosity = "Total autozygosity (cM)")) + 
    theme_classic(base_size = base) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_text(size = 12))
  
  
  # Plot 3: Count distribution
  g3 = ggplot(perSim, aes_(~Count, color = ~Relationship)) + 
    geom_density(aes_(fill = ~Relationship), alpha = 0.1, size = 1.2, show.legend = FALSE) +
    labs(x = "Number of segments") +
    theme_classic(base_size = base) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_text(size = 12))
  
  # Plot 4: Length distribution
  g4 = ggplot(allSegs, aes_(~Length, color = ~Relationship)) + 
  geom_density(aes_(fill = ~Relationship), alpha = 0.1, size = 1.2, show.legend = FALSE) +
  labs(x = "Segment length") +
  theme_classic(base_size = base) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 12))

  (g1 | (g2 / g3 / g4)) & # plot_layout(guides = 'collect') & 
  scale_color_manual(values = cols) & 
  theme(plot.margin = margin(10,10,10,10))
}

