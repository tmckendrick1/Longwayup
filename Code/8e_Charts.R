
# Ensures these are ordered
lookup_table_stats_and_records$blog<-
  factor(lookup_table_stats_and_records$blog,
         levels=unique(lookup_table_stats_and_records$blog))
lookup_table_stats_and_records$country<-
  factor(lookup_table_stats_and_records$country,
         levels=unique(lookup_table_stats_and_records$country))

variables_chart <- 
  data.frame(
    variable=c("distance",
               "elemax",
               "elemin",
               "elegain",
               "tempmax",
               "tempmin",
               "speedmax",
               "avg_ascent_gradient",
               "avg_descent_gradient",
               "avg_gradient",
               "avg_distance",
               "avg_elegain",
               "daydistmax",
               "dayelegainmax",
               "wa_ele",
               "wa_temp"),
    desc=c("Total Distance (Km)",
           "Highest Altitude (m)",
           "Lowest Altitude (m)",
           "Total Elevation Gain (m)",
           "Hottest Temparature (C)",
           "Coldest Temperature (C)",
           "Maximum Speed (m/s)",
           "Average Climbing Gradient (%)",
           "Average Descending Gradient (%)",
           "Average Overall Gradient (%)",
           "Average Daily Distance (km)",
           "Average Daily Elevation Gain (m)",
           "Highest Daily Distance (km)",
           "Biggest Daily Elevation Gain (m)",
           "Average Altitude (m)",
           "Average Temperature (C)"),
    stringsAsFactors = FALSE
  )

# Cheat to make colouring easier:
for (i in variables_chart$variable) {
  lookup_table_stats_and_records[[i]][is.na(lookup_table_stats_and_records[[i]])]<-0
}
plots<-list()
### TURN PLOT NAMES INTO A VECTOR THEN THIS IS DONE - EXCEPT FOR HIGHLIGHTING MULTIPLE BLOGS IN ONE CASE
plot_names<-data.frame(comparison=NA,
                       activity=NA,
                       across=NA,
                       variable=NA,
                       countryorblog=NA)
plot_colours<-list()
plot<-0


# COMPARE COUNTRIES

for (comparison in c("country","blog")) { # What is being compared
  
  # Selects the x-axis variable needed. Fine.
  variablescondition <- c(comparison,
                          "activity",
                          variables_chart$variable)
  
  # Selects the data condition.
  if (comparison=="country") { # Extracts all data relating to individual countries at their aggregate level
    datacondition_comparison <- 
      lookup_table_stats_and_records$country!="Overall" & 
      lookup_table_stats_and_records$blog=="Overall"
    
  } else if (comparison=="blog") { # Extracts all data relating to individual blogs at their aggregate level
    datacondition_comparison <-
      lookup_table_stats_and_records$country!="Overall" & 
      lookup_table_stats_and_records$blog!="Overall" &
      lookup_table_stats_and_records$date=="Overall"
  }
  
  for (activity in c("Biking","Hiking")) { ### Temp and Speed shouldn't be generated for hiking
    datacondition_activity <-
      datacondition_comparison &
      lookup_table_stats_and_records$activity==activity
    
    for (across in c("Overall",countrylevels)) {
      if (across!="Overall" & comparison=="blog") { # Only rendering this for blog, intra-country comp
        datacondition_across <- 
          datacondition_activity &
          lookup_table_stats_and_records$country==across
      } else { # Note this means we render some charts multiple times, but overwrite the name. Makes coding neater but inefficient.
        datacondition_across <- 
          datacondition_activity
      }
      
      for (variable in variables_chart$variable) {
        
        if (
          !(activity=="Hiking" &
            (variable=="tempmax" |
             variable=="tempmin" |
             variable=="speedmax") | # Don't make these plots
            comparison=="country" &
            across!="Overall") # Don't make these plots
        ) { 
          
          plot<-plot+1
          
          # List of comparisons
          ### TRY PUTTING ALL OF THESE IN AN ARRAY THEN CALLING AT SAME TIME???
          
          plot_colours[[plot]] <-
            as.vector(
              sapply(as.vector(
                sapply(unique(lookup_table_stats_and_records[datacondition_across,
                                                             variablescondition])[[comparison]],
                       function(x) {
                         unique(lookup_table_stats_and_records$country[
                           lookup_table_stats_and_records[[comparison]]==x
                           ])
                       }
                )),
                function(x) {
                  which(unique(lookup_table_stats_and_records$country)==x)
                }
              )
            )
          
          plot_unhighlighted<- # Make the plot
            ggplot(data=unique(lookup_table_stats_and_records[datacondition_across,
                                                              variablescondition]),
                   aes_string(x=comparison,
                              y=variable,
                              fill=comparison)) +
            geom_bar(stat="identity", colour="black") +
            guides(fill=FALSE) +
            theme(axis.text.x =
                    element_text(size  = 10,
                                 angle = 45,
                                 hjust = 1,
                                 vjust = 1)) +
            ggtitle(variables_chart$desc[variables_chart$variable==variable]) + 
            theme(axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  plot.title = element_text(hjust = 0.5))
          
          # Save chart to plots list
          plots[[plot]]<-plot_unhighlighted
          
          # Record chart details in plot_name dataframe
          plot_names[plot,"comparison"]<-comparison
          plot_names[plot,"activity"]<-activity
          plot_names[plot,"across"]<-across
          plot_names[plot,"variable"]<-variable
          plot_names[plot,"countryorblog"]<-"Overall"
          
          # Highlighting
          if (comparison=="country"&
              across=="Overall" ) { # Highlight particular country - one for each country
            
            for (country in countrylevels) {
              datacondition_countries <- 
                datacondition_across &
                lookup_table_stats_and_records$country==country
              
              plot<-plot+1
              plot_colours[[plot]] <- plot_colours[[plot-1]]
              plot_highlighted <-
                plot_unhighlighted +
                geom_bar(data=unique(lookup_table_stats_and_records[datacondition_countries,
                                                                    variablescondition]),
                         aes_string(x=comparison,
                                    y=variable),
                         stat="identity",
                         alpha=0,
                         size=1.5,
                         color="black") +
                ggtitle(variables_chart$desc[variables_chart$variable==variable])+ 
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      plot.title = element_text(hjust = 0.5))
              
              # Save chart to plots list
              plots[[plot]]<-plot_highlighted
              # Record chart details in plot_name dataframe
              plot_names[plot,"comparison"]<-comparison
              plot_names[plot,"activity"]<-activity
              plot_names[plot,"across"]<-across
              plot_names[plot,"variable"]<-variable
              plot_names[plot,"countryorblog"]<-country
              
            }
          } else if (comparison=="blog"&
                     across=="Overall" ) { 
            
            # Highlight particular blog in cross-country comparison
            for (blog in unique(geodfcomprwg$blog)) {
              datacondition_blog_cross <- 
                datacondition_across &
                lookup_table_stats_and_records$blog==blog
              
              plot<-plot+1
              plot_colours[[plot]] <- plot_colours[[plot-1]]
              plot_highlighted <-
                plot_unhighlighted +
                geom_bar(data=unique(lookup_table_stats_and_records[datacondition_blog_cross,
                                                                    variablescondition]),
                         aes_string(x=comparison,
                                    y=variable),
                         stat="identity",
                         alpha=0,
                         size=1.5,
                         color="black") +
                ggtitle(variables_chart$desc[variables_chart$variable==variable])+ 
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      plot.title = element_text(hjust = 0.5))
              
              # Save chart to plots list
              plots[[plot]]<-plot_highlighted
              # Record chart details in plot_name dataframe
              plot_names[plot,"comparison"]<-comparison
              plot_names[plot,"activity"]<-activity
              plot_names[plot,"across"]<-across
              plot_names[plot,"variable"]<-variable
              plot_names[plot,"countryorblog"]<-blog 
              
            }
            
            # Highlight sets of blogs in cross-country comparison
            for (country in unique(geodfcomprwg$country)) {
              datacondition_blogset_cross <- 
                datacondition_across &
                lookup_table_stats_and_records$country==country
              
              plot<-plot+1
              plot_colours[[plot]] <- plot_colours[[plot-1]]
              plot_highlighted <-
                plot_unhighlighted +
                geom_bar(data=unique(lookup_table_stats_and_records[datacondition_blogset_cross,
                                                                    variablescondition]),
                         aes_string(x=comparison,
                                    y=variable),
                         stat="identity",
                         alpha=0,
                         size=1.5,
                         color="black") +
                ggtitle(variables_chart$desc[variables_chart$variable==variable])+ 
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      plot.title = element_text(hjust = 0.5))
              
              # Save chart to plots list
              plots[[plot]]<-plot_highlighted
              # Record chart details in plot_name dataframe
              plot_names[plot,"comparison"]<-comparison
              plot_names[plot,"activity"]<-activity
              plot_names[plot,"across"]<-across
              plot_names[plot,"variable"]<-variable
              plot_names[plot,"countryorblog"]<-country
              
            }
          } else if (comparison=="blog"&
                     across!="Overall" ) { # Highlight particular blog in intra-country comparison
            for (blog in unique(geodfcomprwg$blog[geodfcomprwg$country==across])) { # Stops 'Overall' chart being made
              datacondition_countries_blogs <- 
                datacondition_across &
                lookup_table_stats_and_records$blog==blog
              
              plot<-plot+1
              plot_colours[[plot]] <- plot_colours[[plot-1]]
              plot_highlighted <-
                plot_unhighlighted +
                geom_bar(data=unique(lookup_table_stats_and_records[datacondition_countries_blogs,
                                                                    variablescondition]),
                         aes_string(x=comparison,
                                    y=variable),
                         stat="identity",
                         alpha=0,
                         size=1.5,
                         color="black") +
                ggtitle(variables_chart$desc[variables_chart$variable==variable])+ 
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      plot.title = element_text(hjust = 0.5))
              
              # Save chart to plots list
              plots[[plot]]<-plot_highlighted
              # Record chart details in plot_name dataframe
              plot_names[plot,"comparison"]<-comparison
              plot_names[plot,"activity"]<-activity
              plot_names[plot,"across"]<-across
              plot_names[plot,"variable"]<-variable
              plot_names[plot,"countryorblog"]<-blog
              
            }
          }
        }
      }
    }
  }
}
