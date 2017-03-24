plot_base<- # Make the plot
  ggplot()+
  guides(fill=FALSE)+
         theme(axis.text.x = element_text(size  = 10,
                                          angle = 45,
                                          hjust = 1,
                                          vjust = 1))
    
         


plot_function <- function(data_condition,
                          size,
                          countryorblog,
                          base_plot=plot_unhighlighted,
                          save_plot="plot_highlighted"
                          ) {
  base_plot +
    geom_bar(data=unique(lookup_table_stats_and_records[data_condition,
                                                        variablescondition]),
             aes_string(x=comparison,
                        y=variable,
                        fill=comparison),
             stat="identity", 
             alpha=0,
             size=size,
             colour="black")
  
  # Save chart to plots list
  plots[[plot]]<-get(save_plot)
  # Record chart details in plot_name dataframe
  plot_names$comparison[plot]<-comparison 
  plot_names$activity[plot]<-activity
  plot_names$across[plot]<-across 
  plot$names$variable[plot]<-variable
  plot$names$countryorblog[plot]<-countryorblog
  
}


### SHOULD BE EVEN EASIER TO ASSIGN THESE STRAIGHT TO PLOTS_PLOT[[X]]
### AND THEN DON'T NEED THE SAVE_PLOT - ONLY FOR UNHIGHLIGHTED
# MAIN
plot_unhighlighted <-
  plot_function(datacondition_across,
                0,
                "none",
                base_plot="plot_base",
                save_plot="plot_unhighlighted")


# HIHGLIGHTS
#FIRST ONE

plot_highlighted <-
  plot_function(datacondition_countries,
                1.5,
                country)
  
#SECOND ONE

plot_highlighted <-
  plot_function(datacondition_blogs,
                1.5,
                blog)

# THIRD ONE
plot_highlighted <-
  plot_function(datacondition_countries_blogs,
                1.5,
                blog)


