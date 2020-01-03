# ------------------------------------------------------------------------
# -----------------GroupBootPlot------------------------------------------
# ------------------------------------------------------------------------



# -----------------GroupBootPlot------------------------------------------



GroupBootPlot <- function(
      BootOut, ## Output from GroupNetworkBoot
      GroupNames, ## Set group names (if not set, default is network names). New names should be sorted according to the alphabetical order of old names. If unsure, set GroupNamesCheck = TRUE to get matches printed to console.
      edges.x, # Define a subset of edges for inclusion in plot
      edges.y, # Define a subset of edges for inclusion in plot
      labels = TRUE, ## Should labels be set for the plot?
      transparency = 0.15, # Set alpha for confidence interval ribbon in plot. 
      point.size = 1.5, # Set point size. 
      line.size = 1, # Set line size.
      legend.position = "none", # Define legend position to indicate colour for sample and bootstrap means. See ?theme in ggplot2.
      GroupNamesCheck = FALSE # Option to print match of indicated GroupNames to console. Only prints if GroupNames is specified
)     {
      
      
      ## Use BootTable to prepare a data.frame for plotting from GroupNetworkBoot output
      boot_dat <- BootTable(BootOut)
      
      
      ## rbind edge data to separate sample and boot.mean for plotting
      boot_dat <- rbind(boot_dat, boot_dat)
      boot_dat$mean.type <- rep(c("Sample", "Bootstrap Mean"), each = nrow(boot_dat) / 2) 
      boot_dat$mean <- boot_dat$sample
      boot_dat[boot_dat$mean.type == "Bootstrap Mean",]$mean <- boot_dat[boot_dat$mean.type == "Bootstrap Mean",]$boot.mean
      
      
      
      ## If GroupNamesCheck == FALSE, print name matching to console
      if(!missing(GroupNames) & GroupNamesCheck == TRUE)  {
            GroupMatch <- cbind.data.frame(data.list.names = sort(names(BootOut$sample$network)), 
                                           GroupNames = GroupNames)
            for(i in 1:nrow(GroupMatch))  {cat("data.list ", as.character(GroupMatch[i, 1]), 
                                               " matches GroupNames ", as.character(GroupMatch[i, 2]), "\n")}
      } 
      
      ## Define GroupNames if not indicated
      if(missing(GroupNames)) {GroupNames <- names(BootOut$sample$network)}
      
      # Create GroupNames variable in boot_dat
      boot_dat$gnames <- NA
      
      for(i in 1:length(unique(boot_dat$g)))  {
            
            boot_dat[boot_dat$g == unique(boot_dat$g)[i], "gnames"] <- GroupNames[i]
            
      }
      
      
      ## Rank edges by their summed size across groups
      
      # Create rank data
      edge_rank <- boot_dat[boot_dat$mean.type == "Sample", c("edges", "mean")] %>%
            group_by(edges) %>%
            summarise_all(sum) %>%
            arrange(mean)
      
      # use rank data to inform factor levels
      boot_dat$edges <- factor(boot_dat$edges, levels = edge_rank$edges)
      
      
      
      ## If edges.x & edges.y are defined, subset to unique edge combinations
      
      if(!missing(edges.x) & !missing(edges.y))      {
            
            # get unique edge combinations
            edges <- c(as.vector(outer(edges.x, edges.y, FUN = "paste", sep = "-")),
                       as.vector(outer(edges.y, edges.x, FUN = "paste", sep = "-")))
            
            # Subset to relevant edges only
            boot_dat <- boot_dat[boot_dat$edges %in% edges,]
            
      }
      
      
      ## Create plot
      boot_plot <- ggplot(boot_dat, aes(y = mean, x = edges, group = mean.type))  +
         geom_point(aes(col = mean.type), size = point.size) +
         geom_line(aes(col = mean.type), size = line.size) +
         geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = transparency) +
         coord_flip() +
         scale_color_manual(values = c("darkred", "black")) +
         geom_hline(yintercept = 0, col = "black") +
         facet_grid(. ~ gnames) +
         theme_bw() +
         {if(!labels) theme(axis.text.y = element_blank())} +
         theme(legend.position = legend.position,
               legend.title = element_blank(),
               axis.title = element_blank()) 
      
      # Return plot as output
      return(boot_plot)
}



# -----------------BootTable-------------------------------

BootTable <- function(
   BootOut  ## Output of GroupNetworkBoot
) {
   
   ## Define Parameters
   nodes <- colnames(BootOut$sample[[1]][[1]])
   Gn <- length(BootOut$sample[[1]])
   Gnames <- names(BootOut$sample[[1]])
   nboots <- length(BootOut$boot)
   
   ## Create data.frame with all edge combinations
   edges <- do.call(expand.grid, rep(list(nodes), 2))
   edges <- mutate_all(edges, as.character)
   edges <- edges[edges$Var1 != edges$Var2,]
   edges$edges <- apply(cbind(edges$Var1, edges$Var2), 1, function(x) paste(sort(x), collapse="-"))
   edges <- edges[!duplicated(edges$edges),]
   
   ## Create empty vectors to fill
   edges$sample <- NA
   edges$boot.mean <- NA
   edges$ci.lb <- NA
   edges$ci.ub <- NA
   edges$boot.zero <- NA
   
   ## Expand edges data.frame Gn times and save group variable
   edges <- do.call("rbind", replicate(Gn, edges, simplify = FALSE))
   edges$g <- rep(Gnames, each = nrow(edges) / Gn)
   
   ## Run loops to save values
   for(i in 1:nrow(edges)) {
      
      ## Sample Mean
      edges[i, "sample"] = BootOut$sample$network[[edges$g[i]]][edges$Var1[i], edges$Var2[i]]
      
      ## Get Bootstrap vector
      boot_vec <- c()
      
      for(j in 1:nboots)   {
         
         boot_vec[j] <- BootOut$boot[[paste("b", j, sep = "")]][[edges$g[i]]][edges$Var1[i], edges$Var2[i]]
         
      }
      
      ## Get bootstrap values
      edges[i, "boot.mean"] = mean(boot_vec)
      edges[i, c("ci.lb", "ci.ub")] = quantile(boot_vec, probs = c(0.025, 0.975))
      edges[i, "boot.zero"] = sum(boot_vec == 0) / nboots
      
      ## Remove temporary variables
      rm(boot_vec)
      
   }
   
   ## Compute the number of time bootstrap sample was 0
   
   ## Sort edge data by sample value
   edges <- arrange(edges, g, sample)

   ## Return output
   return(edges)
   
   
}


