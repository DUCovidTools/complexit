# Server.R for Complex-It model

## I want this to plot the output for a particular location (NE only) and eventually to include uncertainty

# input = list(NEregion = "Calderdale", ModelType = "log_lm", n_RMSE = 5, first_reg = last_week, max_RMSE = 40, usepast = TRUE, addNumbers = TRUE)

# input = list(NEregion = "Barnsley", ModelType = "log_lm", n_RMSE = 5, first_reg = last_week, max_RMSE = 40, usepast = FALSE, addNumbers = TRUE)

shinyServer <- function(input, output, session){
  
  # function to find the Italian proxies for a NE region
  # Based on RMSE over a number of days
  
    rmse_proxy = reactive({
      thisName = input$NEregion
      thisCluster = inData$quadrant[inData$areaName == thisName]
      thisRow = inData[inData$areaName == thisName, ]
      
      # reg_first_date = input$first_reg
      # first_date_name = date_names_past[dates_past == reg_first_date]
      # id_start = which(names(inData)==first_date_name)
      # id_end = idxLastActualDay + 9 # might have to change this depending on what others want
      
      
      df_cluster = inData[inData$quadrant == thisCluster, ]
      italyCluster = fullItaly[fullItaly$quadrant == thisCluster, ]
      
      if(nrow(italyCluster) > 0){
        RMSE_range = (idxLastActualDay - input$n_RMSE+1):idxLastActualDay # check this gives the right no.
        
        this_vec = as.numeric(thisRow[ ,RMSE_range])
        It_vecs = italyCluster[ ,RMSE_range]
        
        n_it = nrow(italyCluster)
        RMSE_vec = sapply(1:n_it,
                          function(j){
                            it_vec_j = as.numeric(It_vecs[j,])
                            rmse_j = sqrt( mean( (this_vec - it_vec_j)^2) )
                            rmse_j
                          })
        itClusRMSE = italyCluster[RMSE_vec <= input$max_RMSE, ]
      } else {
        itClusRMSE = italyCluster # same empty data.frame
      }

      ## BIT ROPEY FROM HERE
      n.itCluster = nrow(itClusRMSE)
      ## choose earliest of regression and RMSE days - seems quite convoluted!
    
      first_col = idxLastActualDay - 13
      
       dayCols = first_col:(idxLastActualDay+9)
      #
       dayNames = names(inData)[dayCols]
      # #thisRowNE = inData[inData$areaName==thisName, names(inData) %in% dayNames]
       thisRowNE = inData[inData$areaName==thisName, all_of(dayNames)]
       
        dfProxy = data.frame(matrix(NA, nrow=n.itCluster+1, ncol=3+length(dayCols)))
       names(dfProxy) = c("areaName", "type", "source", dayNames)
       if(n.itCluster > 0){
         for (i in 1:n.itCluster){
           dfProxy[i, ] = c(as.character(itClusRMSE$areaName[i]), "Proxy", "Italy",as.numeric(itClusRMSE[i, dayCols]))
         }
       }
       dfProxy[n.itCluster+1, ] = c(thisName, "Actual", "UK", thisRowNE)
       if((n.itCluster > 0 )){
         dfNumbers = data.frame(matrix(NA, nrow=2, ncol=3+length(dayCols)))
         names(dfNumbers) = c("areaName", "type", "source", dayNames)
         upperNums = sapply(
           dayCols,
           function(dayj){
             max(itClusRMSE[,dayj])
           }
         )
         lowerNums = sapply(
           dayCols,
           function(dayj){
             min(itClusRMSE[,dayj])
           }
         )
         dfNumbers[1, ] = c("Bound", "Upper", "Italy", upperNums)
         dfNumbers[2, ] = c("Bound", "Lower", "Italy", lowerNums)
         dfProxy = rbind(dfProxy, dfNumbers)
       }
       dfProxy_long =  na.omit(gather(dfProxy, Day, Cases, dayNames, factor_key=TRUE))
     #  
       dfProxy_long$Day <- date_format(dfProxy_long$Day)
       dfProxy_long$Cases <- as.numeric(dfProxy_long$Cases)
       dfProxy_long$type[dfProxy_long$areaName == thisName] = "Actual"

       dfProxy_long$type = as.factor(dfProxy_long$type)
       dfProxy_long$areaName = as.factor(dfProxy_long$areaName)
       dfProxy_long$source = as.factor(dfProxy_long$source)

      return(dfProxy_long)
      
  
      
    })
    
    UK_cluster <- reactive({
      thisName = input$NEregion
      thisRow = inData[inData$areaName == thisName, ]
      
      lastDayValue <- as.numeric(filter(inData, areaName == thisName)[idxLastActualDay])
      thisCluster = inData$quadrant[inData$areaName == thisName]

      UK_clus = rbind(
        fullUKother[fullUKother$quadrant == thisCluster, ],
        fullNE[fullNE$quadrant == thisCluster, ]
      )
      
      if(nrow(UK_clus) > 0){
        RMSE_range = (idxLastActualDay - input$n_RMSE+1):idxLastActualDay # check this gives the right no.
        
        this_vec = as.numeric(thisRow[ ,RMSE_range])
        UK_vecs = UK_clus[ ,RMSE_range]
        
        n_uk = nrow(UK_clus)
        RMSE_vec = sapply(1:n_uk,
                          function(j){
                            UK_vec_j = as.numeric(UK_vecs[j,])
                            rmse_j = sqrt( mean( (this_vec - UK_vec_j)^2) )
                            rmse_j
                          })
        UKClusRMSE = UK_clus[RMSE_vec <= input$max_RMSE, ]
      } else {
        UKClusRMSE = UK_clus # same empty data.frame
      }
      n_uk_rmse = nrow(UKClusRMSE)
      first_col = idxLastActualDay - 13
      dayCols = first_col:idxLastActualDay
      dayNames = names(inData)[dayCols]

      df_UK = data.frame(matrix(NA, nrow=n_uk_rmse, ncol=3+length(dayCols)))
      names(df_UK) = c("areaName", "type", "source", dayNames)
      if(n_uk_rmse > 0){
        for (i in 1:n_uk_rmse){
          df_UK[i, ] = c(as.character(UKClusRMSE$areaName[i]), "UK Cluster", "UK",as.numeric(UKClusRMSE[i, dayCols]))
        }
      }

      UK_clus_long = gather(df_UK, Day, Cases, dayNames, factor_key = TRUE)
      UK_clus_long$Day <- date_format(UK_clus_long$Day)
      UK_clus_long$Cases <- as.numeric(UK_clus_long$Cases)
      UK_clus_long
    })
    
  
    ## Given a data set, builds a regression model
    ## Model should be better than this. But at least it can now be changed quite easily
    
    reg_proj <- reactive({
      rmseData = rmse_proxy()
      if(!input$useproxy){
        rmseData = rmseData[rmseData$type == "Actual", ]
      }
      
      if(input$usepast){
        reg_data = rmseData[rmseData$Day >= input$first_reg,]
      } else {
        reg_data = rmseData[rmseData$Day >= date_format(lastActualDay),]
      }
       if(input$ModelType == "lm"){
        model_fit = lm(Cases ~ Day, data=reg_data)
      } else if (input$ModelType == "lmvar"){
        X = model.matrix( ~ Day - 1, reg_data)
        model_fit = try(lmvar(reg_data$Cases, X_mu = X, X_sigma = X), silent=TRUE)
      } else if (input$ModelType == "glm"){
        model_fit = glm(Cases ~ Day, data = reg_data, family = gaussian(link= "log"))
        ## use sampling and transform sample to get prediction interval.
      } else if (input$ModelType == "log_lm"){
        model_fit = lm(log(Cases) ~ Day, data = reg_data)
      }
      
      model_fit

    })
    
    pred_int = reactive({
      lm_fit = reg_proj()
      rmse_df = rmse_proxy()
      
      first_day_col = idxLastActualDay 
      lastDay_col = idxLastActualDay + 9
      dayCols = first_day_col:lastDay_col
      full_dates = date_format(names(inData)[dayCols])
      # pred_df is a data frame with 'Fit', 'Upper' and 'Lower', giving
      # mean and 95% prediction interval
      if(input$ModelType == "lm"){
        pred_interval = predict(lm_fit, newdata = data.frame(Day = full_dates), interval = "prediction",
                                level = 0.95)
        pred_df = as.data.frame(pred_interval)
      } else if (input$ModelType == "lmvar"){
        if(class(lm_fit) == "try-error"){
          pred_df_long = NULL
        } else {
          newData = data.frame(Day = full_dates)
          newX = model.matrix(~Day-1, newData)
          pred_interval = predict(lm_fit, X_mu = newX, X_sigma= newX, interval = "prediction", level = 0.95)
          pred_df = as.data.frame(pred_interval[,c(1,3,4)]) # removes 'sigma' column
        }
      } else if (input$ModelType == "glm"){
        pred_glm_link = predict(lm_fit, newdata = data.frame(Day = full_dates), type = "link", se.fit = TRUE)
        #    pred_glm_response = predict(lm_fit, newdata = data.frame(Day = full_dates), type = "response", se.fit = TRUE)
        
        sam_logCases = rmvnorm(1000, mean = pred_glm_link$fit, sigma = diag(pred_glm_link$se.fit^2))
        sam_Cases = exp(sam_logCases)
        pred_mat = matrix(NA, ncol=3, nrow = length(full_dates))
        for (i in 1:length(full_dates)){
          row_i_quants = quantile(as.numeric(sam_Cases[ ,i]), c(0.025, 0.5, 0.975))
          pred_mat[i, ] = row_i_quants[c(2,1,3)]
        }
        
        pred_df = as.data.frame(pred_mat)
      } else if (input$ModelType == "log_lm"){
        pred_interval = predict(lm_fit, newdata = data.frame(Day = full_dates), interval = "prediction",
                                level = 0.95, se.fit=TRUE)
        ## this is a bit dodgy, using the prediction interval to estimate the predictive SE
        se_est = (pred_interval$fit[,3] - pred_interval$fit[,1])/1.96
        sam_logCases = rmvnorm(1000, mean = pred_interval$fit[,1], sigma = diag(se_est^2))
        sam_Cases = exp(sam_logCases)
        pred_mat = matrix(NA, ncol=3, nrow = length(full_dates))
        for (i in 1:length(full_dates)){
          row_i_quants = quantile(as.numeric(sam_Cases[ ,i]), c(0.025, 0.5, 0.975))
          pred_mat[i, ] = row_i_quants[c(2,1,3)]
        }
        pred_df = as.data.frame(pred_mat)
      }
      if(length(lm_fit) > 1){ ## if it isn't a try-error
        names(pred_df) = c("Fit", "Lower", "Upper")
        pred_df$Day = full_dates
        pred_df$type = "Model"
        pred_mat_long = matrix(NA, ncol=5, nrow=3*length(full_dates))
        pred_df_long = data.frame(pred_mat_long)
        names(pred_df_long) = c("areaName", "type", "source", "Day", "Cases")
        pred_df_long$areaName = rep(c("Mean", "Lower", "Upper"), rep(length(full_dates), 3))
        pred_df_long$type = "Model"
        pred_df_long$source = "Model"
        pred_df_long$Day = rep(full_dates, 3)
        pred_df_long$Cases = c(pred_df$Fit, pred_df$Lower, pred_df$Upper)
      }
      
      pred_df_long
    })
    
    plot_generator = reactive({
      rmse_df = rmse_proxy()
      model_fit = reg_proj()
      if(input$ModelType == "lmvar"){
        validate(
          need(length(model_fit)>1,                     # it will only be 1 if it's a try-error
               "This leaves no data with which to fit a model - please include past and/or proxy data or change the RMSE constraints.")
        )
      } else {
        validate(
          need(!is.na(model_fit$coefficients[2]),
               "This leaves no data with which to fit a model - please include past and/or proxy data or change the RMSE constraints.")
        )
      }
      pred_df = pred_int()
      
      ribbon_df = spread(pred_df, areaName, Cases)
      
      plot_df = rbind(rmse_df, pred_df)
      ## this is the key!  
      
      if(input$showUK){
        UK_clus = UK_cluster()
        plot_df = rbind(plot_df, UK_clus)
      }
      
      
      first_reg_day = ifelse(input$usepast, input$first_reg, date_format(lastActualDay))
      
      legend_master = list( # select which bits of legend we need
        name = "",
        labels = c("Actual",
                   "Italy Cluster",
                   "Model",
                   "Proxy",
                   "UK Cluster"
        ),
        values = c("Actual" = "#68246D",
                   "Italy Cluster" = "#B6AAA7",
                   "Model" = "#68246D",
                   "Proxy" = "#B3BDB1",
                   "UK Cluster" = "#CBA8B1"
        ),
        linetype = c("blank",
                     "solid",
                     "solid",
                     "solid",
                     "solid"),
        shape = c(16, NA, NA, NA, NA)
      )
      
      plot = ggplot() + 
        geom_path(data = plot_df[plot_df$type == "Proxy",], aes(x=Day, y=Cases, group = areaName, col=type))+
        geom_point(data = plot_df[plot_df$type == "Actual",], aes(x=Day, y=Cases, group = areaName, col=type)) +
        geom_path(data = plot_df[plot_df$areaName == "Mean", ], 
                  aes(x=Day, y=Cases, group = areaName, col = type), size=1) +
        geom_path(data=ribbon_df,aes(x=Day,y=Lower),col="#CBA8B1",size=1)+
        geom_path(data=ribbon_df,aes(x=Day,y=Upper),col="#CBA8B1",size=1)+
        geom_ribbon(data = ribbon_df, aes(x=Day, ymin=Lower, ymax=Upper), fill="#CBA8B1", alpha=0.2)+
        geom_vline(xintercept = date_format(lastActualDay), col="#B6AAA7", lty=2)  +
        scale_x_date(name = "Date", date_breaks = "2 days", date_labels = "%b %d") +
        theme_bw()
      
      
      if(input$usepast){
        plot = plot + geom_vline(xintercept = input$first_reg, col="#00AEEF", lty=2) # doesn't like this!
      } else {
        plot = plot + geom_vline(xintercept = date_format(lastActualDay), col="#BE1E2D", lty=2)
      }
      ## Add line to show which days RMSE is being used over
      first_day_rmse = names(inData)[idxLastActualDay - input$n_RMSE + 1]
      plot = plot + geom_vline(xintercept = date_format(first_day_rmse), col="orangered2", lty=2)
      if(sum(rmse_df$type == "Proxy") ==0){
        plot = plot + labs(title = "There is no Italian proxy for this region with these constraints.") +
          theme(plot.title = element_text(size=14, face="bold", colour = "darkred"))
      
      }
      
      if(input$addNumbers){
        plot = plot + 
          geom_text(data = ribbon_df, aes(x=Day, y=Upper, label = ceiling(Upper)), vjust = -1) +
          geom_text(data = ribbon_df, aes(x=Day, y=Lower, label = floor(Lower)), vjust = 1)
      }
      
      if(input$showUK){
        UK_clus_names = UK_clus[UK_clus$Day == min(UK_clus$Day),]
        #    UK_clus_names$name_form = format(UK_clus_names$areaName, )
        UK_clus_names$Day = UK_clus_names$Day #- 2
        plot = plot +
          geom_path(data = UK_clus, aes(x=Day, y=Cases, group=areaName, col=type), alpha = 0.6) +
          geom_text_repel(data = UK_clus_names, aes(x=Day, y=Cases, label=areaName), nudge_x=-1.5, col = "seagreen4")
      }
      #
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
      legend_master$linetype = legend_master$linetype[legend_select]
      legend_master$shape = legend_master$shape[legend_select]
      
      plot + 
        scale_color_manual(name = legend_master$name,
                           labels = legend_master$labels,
                           values = legend_master$values,
                           guide = guide_legend(override.aes = list(
                             linetype = legend_master$linetype,
                             shape = legend_master$shape))
                           ) +
        theme(axis.text = element_text(size=12),
              text=element_text(size=12),
              legend.position = "bottom",
              legend.text = element_text(size=12))
      
      
    })
 
output$reg_plot = renderPlot({
  plot_generator()
})

plotName = reactive({
  paste(
    gsub(" ", "", input$NEregion), '_',
    gsub("_", "", input$ModelType), '_',
    gsub("_", "", gsub("X", "", lastActualDay)),
    '.png',
    sep = '')
})

output$downloadPlot <- downloadHandler(
  filename = function(){plotName()},
  content = function(file){
    ggsave(file, plot = plot_generator(), device = 'png', width = 12.8, height=8)
  }
)

  session$onSessionEnded(function() {
    stopApp()
  })
}



