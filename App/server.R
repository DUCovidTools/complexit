# Server.R for Complex-It model

## I want this to plot the output for a particular location (NE only) and eventually to include uncertainty

# input = list(NEregion = "Calderdale", proxy_range = c(0.9, 1.25), showPast = TRUE, addNumbers = TRUE)

shinyServer <- function(input, output, session){
  
  # function to find the Italian proxies for a NE region
  # IE those from the same quadrant, where the value on the last actual day 
  # is within the range given by proxy_range
    proxy_proj <- reactive({
      thisName = input$NEregion
      lastDayValue <- as.numeric(filter(inData, areaName == thisName)[idxLastActualDay])
      thisCluster = inData$quadrant[inData$areaName == thisName]
    
      df_cluster = inData[inData$quadrant == thisCluster, ]
      italyCluster = fullItaly[fullItaly$quadrant == thisCluster, ]
      if(nrow(italyCluster) > 0){
        lastDay_range = input$proxy_range*lastDayValue
        itClusLD = italyCluster[
          (italyCluster[idxLastActualDay]>=lastDay_range[1])&
            (italyCluster[idxLastActualDay]<=lastDay_range[2]),
          ]
      } else {
        itClusLD = italyCluster # same empty data.frame
      }
      # validate(
      #   need(nrow(italyCluster)>0, "There are no Italian proxy trends for this region.")
      # )
      # validate(
      #   need(nrow(itClusLD)>0, "There are no Italian proxy trends within this range for this region.")
      # )
    
    n.itCluster = nrow(itClusLD)
    dayCols = idxLastActualDay + (-6:10)
    
    dayNames = names(inData)[dayCols]
    #thisRowNE = inData[inData$areaName==thisName, names(inData) %in% dayNames]
    thisRowNE = inData[inData$areaName==thisName, all_of(dayNames)]
    
    dfProxy = data.frame(matrix(NA, nrow=n.itCluster+1, ncol=3+length(dayCols)))    
    names(dfProxy) = c("areaName", "type", "source", dayNames)
    if(n.itCluster > 0){
      for (i in 1:n.itCluster){
        dfProxy[i, ] = c(as.character(itClusLD$areaName[i]), "Proxy", "Italy",as.numeric(itClusLD[i, dayCols]))
      }
    }
    dfProxy[n.itCluster+1, ] = c(thisName, "Actual", "UK", thisRowNE)
    if(input$addNumbers & (n.itCluster > 0 )){
      dfNumbers = data.frame(matrix(NA, nrow=2, ncol=3+length(dayCols))) 
      names(dfNumbers) = c("areaName", "type", "source", dayNames)
      upperNums = sapply(
        dayCols,
        function(dayj){
          max(itClusLD[,dayj])
        }
      )
      lowerNums = sapply(
        dayCols,
        function(dayj){
          min(itClusLD[,dayj])
        }
      )
      dfNumbers[1, ] = c("Bound", "Upper", "Italy", upperNums)
      dfNumbers[2, ] = c("Bound", "Lower", "Italy", lowerNums)
      dfProxy = rbind(dfProxy, dfNumbers)
    }
    dfProxy_long =  na.omit(gather(dfProxy, Day, Cases, dayNames, factor_key=TRUE))
    
    dfProxy_long$Day <- date_format(dfProxy_long$Day)
    dfProxy_long$Cases <- as.numeric(dfProxy_long$Cases)
    dfProxy_long$type[dfProxy_long$areaName == thisName] = "Actual"
    
    return(dfProxy_long)
    
  })
  
  ## function to calculate the 'hack' estimate, based on the past week of thisName data
  estimate <- reactive({
    thisName = input$NEregion
    thisRow <- inData %>% 
      filter(areaName == thisName) %>% 
      select(areaName:cluster, (idxLastActualDay-6):idxLastActualDay)
    lastDayValue <- as.numeric(filter(inData, areaName == thisName)[idxLastActualDay])
    
    minWeek <- min(thisRow[5:11])
    maxWeek <- max(thisRow[5:11])
    daysGap <- which.max(thisRow[5:11]) - which.min(thisRow[5:11])
    dayIncrement <- (maxWeek - minWeek) / daysGap # will be negative if negative gradient
    
    # if gradient is negative, we want the Upper gradient to be less steep
    grad_lower = ifelse(dayIncrement > 0, 0.9, 1.1)
    grad_upper = ifelse(dayIncrement > 0, 1.1, 0.9)
    

    daysSeq = (-6):10
    dayNames = names(inData)[(idxLastActualDay-6):(idxLastActualDay+10)]
    lowerHackIn <- floor((lastDayValue + grad_lower*daysSeq * dayIncrement)) # rounded down
    names(lowerHackIn) <- dayNames
    
    # change the names for pltting reasons, since the lines are grouped by areaName
    lowerHack <- cbind(
      data.frame(areaName =  sprintf("%s_min", thisName),
                 type = "Estimate",
                 source = "min",
                 stringsAsFactors = FALSE),
      t(lowerHackIn))
    
    upperHackIn <- ceiling(1.1 * lastDayValue + grad_upper * daysSeq * dayIncrement)
    names(upperHackIn) <- dayNames
    upperHack <- cbind(
      data.frame(areaName = sprintf("%s_max", thisName),
                 type = "Estimate",
                 source = "max",
                 stringsAsFactors = FALSE),
      t(upperHackIn))

    projections = data.frame(matrix(NA, nrow=2, ncol = 3+length(daysSeq)))
    names(projections) = c("areaName", "type", "source", dayNames)
    projections[1, ] = lowerHack
    projections[2, ] = upperHack
    est_long = gather(projections, Day, Cases, dayNames, factor_key=TRUE)
    
    #    proj_long$Day <- as.numeric(gsub("day", "", proj_long$Day, ignore.case = T))
    est_long$Day = date_format(est_long$Day)
    est_long$Cases = as.numeric(est_long$Cases)
    
    return(est_long)
    
  })
  
  UK_cluster <- reactive({
    thisName = input$NEregion
    lastDayValue <- as.numeric(filter(inData, areaName == thisName)[idxLastActualDay])
    thisCluster = inData$quadrant[inData$areaName == thisName]
    dayCols = idxLastActualDay + (-6:0)

    dayNames = names(inData)[dayCols]
    UK_clus = rbind(
      fullUKother[fullUKother$quadrant == thisCluster, c("areaName", dayNames)],
      fullNE[fullNE$quadrant == thisCluster, c("areaName", dayNames)]
    )
    lastDay_range = input$proxy_range*lastDayValue
    UK_clus = UK_clus[
      (UK_clus[,lastActualDay]<lastDay_range[2]) & (UK_clus[,lastActualDay]>lastDay_range[1]),
    ]
    UK_clus$type = "UK Cluster"
    UK_clus$source = sprintf("UK-cluster%g", thisCluster)
    UK_clus_long = gather(UK_clus, Day, Cases, dayNames, factor_key = TRUE)
    UK_clus_long$Day <- date_format(UK_clus_long$Day)    
 #   UK_clus_long$Day <- as.numeric(gsub("day", "", UK_clus_long$Day, ignore.case = T))
    UK_clus_long$Cases <- as.numeric(UK_clus_long$Cases)
    UK_clus_long
  })


  ## Gives all Italian regions in this cluster  
  cluster = reactive({
    thisName = input$NEregion
    thisCluster = inData[inData$areaName == thisName,]$quadrant
    italyCluster = fullItaly[fullItaly$quadrant == thisCluster,]
    n.cluster = nrow(italyCluster)
    dayCols = idxLastActualDay + (-6:10)
    if(n.cluster > 0){
      areaNamesClus = sprintf("area%g", seq(n.cluster))
      italyCluster = italyCluster[ , dayCols]
      dayNames = names(fullItaly)[dayCols]
      dfCluster <- as.data.frame(matrix(NA, nrow=n.cluster, ncol=(3+length(dayCols))))
      names(dfCluster) = c("areaName", "type", "source", dayNames)
      for(i in 1:n.cluster){
        dfCluster[i, ] = c(areaNamesClus[i], "Italy Cluster", "Italy", as.numeric(italyCluster[i,]))
      }
      dfCluster_long = gather(dfCluster, Day, Cases, dayNames, factor_key=TRUE)
#      dfCluster_long$Day = as.numeric(gsub("day", "", dfCluster_long$Day, ignore.case =T))
      dfCluster_long$Day= date_format(dfCluster_long$Day)
      dfCluster_long$Cases = as.numeric(dfCluster_long$Cases)
      dfCluster_long = dfCluster_long[order(dfCluster_long$Day),]
      return(dfCluster_long)
    } else {
      return(italyCluster)
    }
    
    })
    
  output$NEname_proxyplot = renderPlot({
    proxy_NE = proxy_proj() # now contains Actual as type for thisName
    
    legend_master = list( # select which bits of legend we need
      name = "Type",
      labels = c("Actual",
                 "Estimate",
                 "Italy Cluster",
                 "Proxy",
                 "UK Cluster"
      ),
      values = c("Actual" = "black",
                 "Estimate" = "slateblue",
                 "Italy Cluster" = "Gray70",
                 "Proxy" = "orangered2",
                 "UK Cluster" = "seagreen3"
      )
    )
    
    plot_df = proxy_NE
    if(input$showCluster){
      clus_IT = cluster()
      plot_df = rbind(plot_df, clus_IT)
    }
    if(input$showUK){
      UK_clus = UK_cluster()
      plot_df = rbind(plot_df, UK_clus)
    }
    if(input$showEst){
      est_NE = estimate()
      plot_df = rbind(plot_df, est_NE)
    }
    
    ## Work out which elements of the legend are required
    plot_types = unique(plot_df$type)
    legend_select = (1:5)[
      sapply(1:5,
      function(i){
        if(any(legend_master$labels[i] == plot_types))
          TRUE
        else
          FALSE
      })
    ]
    legend_master$labels = legend_master$labels[legend_select]
    legend_master$values = legend_master$values[legend_select]
    
    plot_dates = unique(plot_df$Day)
    plot = ggplot(data = plot_df[plot_df$areaName != "Bound",], aes(x=Day, y=Cases, group = areaName)) +
      geom_path(aes(col = type)) + theme_bw() + 
      scale_x_date(name = "Date", date_breaks = "2 days", date_labels = "%b %d") +
      geom_vline(xintercept=date_format(lastActualDay) , col=2, lty=2)+
      geom_point(data = plot_df[plot_df$type == "Actual", ], aes(x=Day, y=Cases, group = areaName)) +
      geom_path(data = plot_df[plot_df$type == "Actual", ], aes(x=Day, y=Cases, group = areaName), size=1)
    
    if(sum(proxy_NE$type == "Proxy") ==0){
      plot = plot + labs(title = "There is no Italian proxy for this region with these constraints.") +
       theme(plot.title = element_text(colour = "red"))
    }


    if(input$addNumbers){
      if(sum(proxy_NE$type == "Proxy") > 0){
        plot = plot + 
          geom_text(data=plot_df[plot_df$type=="Upper",], aes(x=Day, y=Cases, label = Cases), vjust = -1)+
          geom_text(data=plot_df[plot_df$type=="Lower",], aes(x=Day, y=Cases, label = Cases), vjust = 1)
      } else if (input$showEst){
        plot = plot + 
          geom_text(data=plot_df[plot_df$type=="max",], aes(x=Day, y=Cases, label = Cases), vjust = -1)+
          geom_text(data=plot_df[plot_df$type=="min",], aes(x=Day, y=Cases, label = Cases), vjust = 1)
      }
      ylim_shownum = c(
        min(plot_df$Cases)-1,
        max(plot_df$Cases)+1
      )
      plot = plot + ylim(ylim_shownum)  

    }  
    if(input$showUK){
      UK_clus_names = UK_clus[UK_clus$Day == min(UK_clus$Day),]
      UK_clus_names$name_form = format(UK_clus_names$areaName, )
      UK_clus_names$Day = UK_clus_names$Day #- 2
      plot = plot +
        geom_path(data = UK_clus, aes(x=Day, y=Cases, group=areaName, col=type), alpha = 0.6) +
        geom_text_repel(data = UK_clus_names, aes(x=Day, y=Cases, label=areaName), nudge_x=-1.5, col = "seagreen4")
    }


    plot + scale_color_manual(name = legend_master$name,
                                      labels = legend_master$labels,
                                      values = legend_master$values)
    
      
  })
  session$onSessionEnded(function() {
    stopApp()
  })
}



