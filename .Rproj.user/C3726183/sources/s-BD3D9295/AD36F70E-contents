server <- function(input, output, session) {
  
  # Uganda reactive data set
  Uganda.df <- reactive({
    df <- Uganda %>% 
      filter(Scenarios %in% input$ug_scenarios) %>% 
      filter(Targets %in% input$ug_targets)
  })
  
  # Rwanda reactive data set
  Rwanda.df <- reactive({
    df <- Rwanda %>% 
      filter(Scenarios %in% input$rw_scenarios) %>% 
      filter(Targets %in% input$rw_targets)
  })
  
  
  # Uganda Institutional costs
  output$ug_inst <- renderPlotly({
    
    ug.inst <- Uganda.df() %>%
      filter(`Investment category` == "Institutional costs")
      
      ug_inst_costs <- ggplot(ug.inst, aes(Year, Amount, group = `Type of investment`)) +
      geom_line(aes(color = `Type of investment`), size = 1) +
      geom_point(aes(col = `Type of investment`), size = 2) +
      scale_color_manual(values = c("red", "steelblue", "green")) +
      theme_bw() +
      #   scale_y_continuous(labels = comma) +
      # geom_text(aes(label = Amount),
      #           position = position_dodge(width = 1), size = 3) +
      labs (y = "Amount", title = paste("Institutional costs :", "Uganda |", input$ug_targets, "|", input$ug_scenarios)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.background = element_rect(
              color="black", fill="#F4FC00", size=1.5, linetype="solid"))
    
      ug_inst_costs %>%
      ggplotly %>%
      layout(legend = list(orientation = "h",
                           x = 0.1,
                           y = -0.2,
                           title = list(text =""))) #%>%
      # style(text = format(ug.inst$Amount, big.mark = ","), textposition = "top")
    
  }) 
  
  # Uganda Individual farmer costs
  output$ug_indv <- renderPlotly({
    
    ug.indv <- Uganda.df() %>%
      filter(`Type of investment` %in% Individual_farmer_costs) %>% 
      filter(Year != "BAU")
    
    ug_indv_costs <- ggplot(ug.indv, aes(Year, Amount, group = `Type of investment`)) +
      geom_line(aes(color = `Type of investment`), size = 1) +
      geom_point(aes(color = `Type of investment`),size = 2) +
      scale_color_manual(values = c("red", "steelblue", "green")) +
      theme_bw() +
      # geom_text(aes(label = Amount),
      #           position = position_dodge(width = 1), size = 3) +
      labs (y = "Amount", title = paste("Individual farmer costs :", "Uganda |", input$ug_targets, "|", input$ug_scenarios)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.background = element_rect(
              color="black", fill="#F4FC00", size=1.5, linetype="solid"))
    
    ug_indv_costs %>%
      ggplotly %>%
      layout(legend = list(orientation = "h",
                           x = 0.1,
                           y = -0.2,
                           title = list(text =""))) #%>% 
      # style(text = format(ug.indv$Amount, big.mark = ","), textposition = "top")
    
  }) 
  
  
  # Uganda incremental farmer costs
  output$ug_incremntal <- renderPlotly({
    
    if (input$ug_scenarios == "BAU") {
      validate("Sorry : There is no data for the selected scenario. Please select a different scenario to visualize data")
    }
    
    
    ug.incremental <- Uganda.df() %>%
      filter(`Type of investment` %in% Incremental_farmer_costs) 
    
    ug_incremental_costs <- ggplot(ug.incremental, aes(Year, Amount, group = `Type of investment`)) +
      geom_line(aes(color = `Type of investment`), size = 1) +
      geom_point(aes(color = `Type of investment`),size =2) +
      scale_color_manual(values = c("red", "steelblue", "green")) +
      theme_bw() +
      # geom_text(aes(label = Amount),
      #           position = position_dodge(width = 1), size = 3) +
      labs (y = "Amount", title = paste("Incremental farmer costs :", "Uganda |", input$ug_targets, "|", input$ug_scenarios)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.background = element_rect(
              color="black", fill="#F4FC00", size=1.5, linetype="solid"))
    
    ug_incremental_costs %>%
      ggplotly %>%
      layout(legend = list(orientation = "h",
                           x = 0.1,
                           y = -0.2,
                           title = list(text =""))) #%>% 
      # style(text = format(ug.incremental$Amount, big.mark = ","), textposition = "top")
    
  }) 
  
  
  # Uganda Percent increase farmer costs
  output$ug_percent_increase <- renderPlotly({
    
    if (input$ug_scenarios == "BAU") {
      validate("Sorry : There is no data for the selected scenario. Please select a different scenario to visualize data")
    }
    
    ug.percent.increase <- Uganda.df() %>%
      filter(`Type of investment` %in% Incremental_percent_costs)
    
    ug_percent_increase <- ggplot(ug.percent.increase, aes(Year, Amount, group = `Type of investment`)) +
      geom_line(aes(color = `Type of investment`), size = 1) +
      geom_point(aes(color = `Type of investment`), size =2) +
      scale_color_manual(values = c("red", "steelblue", "green")) +
      theme_bw() +
      # geom_text(aes(label = Amount),
      #           position = position_dodge(width = 1), size = 3) +
      labs (y = "Amount", title = paste("Incremental costs in (%) :", "Uganda |", input$ug_targets, "|", input$ug_scenarios)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.background = element_rect(
              color="black", fill="#F4FC00", size=1.5, linetype="solid"))
    
    ug_percent_increase %>%
      ggplotly %>%
      layout(legend = list(orientation = "h",
                           x = 0.1,
                           y = -0.2,
                           title = list(text =""))) #%>% 
      # style(text = format(ug.percent.increase$Amount, big.mark = ","), textposition = "top")
    
  }) 
  
  
  # Uganda Gross benefits to individual farmers 
  output$ug_gross <- renderPlotly({
    
    if (input$ug_scenarios == "Average (Scen2 & Scen3)") {
      validate("Sorry : There is no data for the selected scenario. Please select a different scenario to visualize data")
    }
    
    
    ug.gross <- Uganda.df() %>%
      filter(`Type of investment` %in% Gross_benefits) %>% 
      filter(Year != "BAU")
    
    ug_gross_benefits <- ggplot(ug.gross, aes(Year, Amount, fill = `Type of investment`)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = alpha(rainbow(4), .7)) +
      theme_bw() +
      # scale_y_continuous(labels = comma) +
      # geom_text(aes(label = Amount),
      #           position = position_dodge(width = 1),size = 3) +
      labs (y = "Amount", title = paste("Gross benfits to farmers :", "Uganda |", input$ug_targets, "|", input$ug_scenarios)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ug_gross_benefits %>%
      ggplotly %>%
      layout(legend = list(orientation = "h",
                           x = 0.1,
                           y = -0.2,
                           title = list(text =""))) 
    
    
  }) 
  
  
  # Uganda Net Present values 
  output$ug_net <- renderPlotly({
    
    if (input$ug_scenarios == "Average (Scen2 & Scen3)") {
      validate("Sorry : There is no data for the selected scenario. Please select a different scenario to visualize data")
    }
    
    ug.net <- Uganda.df() %>%
      filter(`Type of investment` %in% Net_values) %>% 
      filter(Year != "BAU")
    
    ug_net_values <- ggplot(ug.net, aes(Year, Amount, group = `Type of investment`)) +
      geom_line(aes(color = `Type of investment`), size = 1) +
      geom_point(aes(color = `Type of investment`),size =2) +
      scale_color_manual(values = c("red", "steelblue", "green")) +
      theme_bw() +
      # geom_text(aes(label = Amount),
      #           position = position_dodge(width = 1), size = 3) +
      labs (y = "Amount", title = paste("Net present values :", "Uganda |", input$ug_targets, "|", input$ug_scenarios)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.background = element_rect(
              color="black", fill="#F4FC00", size=1.5, linetype="solid"))
    
    ug_net_values %>%
      ggplotly %>%
      layout(legend = list(orientation = "h",
                           x = 0.1,
                           y = -0.2,
                           title = list(text =""))) #%>% 
      # style(text = format(ug.net$Amount, big.mark = ","), textposition = "top")
    
  }) 
  
  
  # Uganda Benefit cost ratio
  output$ug_bcr <- renderPlotly({
    
    if (input$ug_scenarios == "Average (Scen2 & Scen3)") {
      validate("Sorry : There is no data for the selected scenario. Please select a different scenario to visualize data")
    }
    
    ug.bcr <- Uganda.df() %>%
      filter(`Type of investment` %in% BCR) %>% 
      filter(Year != "BAU")
    
    ug_bcr <- ggplot(ug.bcr, aes(Year, Amount, group = `Type of investment`)) +
      geom_line(aes(color = `Type of investment`), size = 1) +
      geom_point(aes(color = `Type of investment`),size =2) +
      scale_color_manual(values = c("red", "steelblue", "green")) +
      theme_bw() +
      geom_text(aes(label = Amount),
                position = position_dodge(width = 1), size = 3) +
      labs (y = "Amount", title = paste("Benefit cost ratio :", "Uganda |", input$ug_targets, "|", input$ug_scenarios)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.background = element_rect(
              color="black", fill="#F4FC00", size=1.5, linetype="solid"))
    
    ug_bcr %>%
      ggplotly %>%
      layout(legend = list(orientation = "h",
                           x = 0.1,
                           y = -0.2,
                           title = list(text =""))) %>% 
      style(text = format(ug.bcr$Amount, big.mark = ","), textposition = "top")
    
  }) 
  
  
  
  # Rwanda Institutional costs
  output$rw_inst <- renderPlotly({
    
    rw.inst <- Rwanda.df() %>%
      filter(`Investment category` == "Institutional costs")
    
    rw_inst_costs <- ggplot(rw.inst, aes(Year, Amount, group = `Type of investment`)) +
      geom_line(aes(color = `Type of investment`), size = 1) +
      geom_point(aes(col = `Type of investment`), size = 2) +
      scale_color_manual(values = c("red", "steelblue", "green")) +
      theme_bw() +
      #   scale_y_continuous(labels = comma) +
      # geom_text(aes(label = Amount),
      #           position = position_dodge(width = 1), size = 3) +
      labs (y = "Amount", title = paste("Institutional costs :", "Rwanda |", input$rw_targets, "|", input$rw_scenarios)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.background = element_rect(
              color="black", fill="#F4FC00", size=1.5, linetype="solid"))
    
    rw_inst_costs %>%
      ggplotly %>%
      layout(legend = list(orientation = "h",
                           x = 0.1,
                           y = -0.2,
                           title = list(text =""))) #%>%
    # style(text = format(rw.inst$Amount, big.mark = ","), textposition = "top")
    
  }) 
  
  # Rwanda Individual farmer costs
  output$rw_indv <- renderPlotly({
    
    rw.indv <- Rwanda.df() %>%
      filter(`Type of investment` %in% Individual_farmer_costs) %>% 
      filter(Year != "BAU")
    
    rw_indv_costs <- ggplot(rw.indv, aes(Year, Amount, group = `Type of investment`)) +
      geom_line(aes(color = `Type of investment`), size = 1) +
      geom_point(aes(color = `Type of investment`),size = 2) +
      scale_color_manual(values = c("red", "steelblue", "green")) +
      theme_bw() +
      # geom_text(aes(label = Amount),
      #           position = position_dodge(width = 1), size = 3) +
      labs (y = "Amount", title = paste("Individual farmer costs :", "Rwanda |", input$rw_targets, "|", input$rw_scenarios)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.background = element_rect(
              color="black", fill="#F4FC00", size=1.5, linetype="solid"))
    
    rw_indv_costs %>%
      ggplotly %>%
      layout(legend = list(orientation = "h",
                           x = 0.1,
                           y = -0.2,
                           title = list(text =""))) #%>% 
    # style(text = format(rw.indv$Amount, big.mark = ","), textposition = "top")
    
  }) 
  
  
  # Rwanda incremental farmer costs
  output$rw_incremntal <- renderPlotly({
    
    if (input$rw_scenarios == "BAU") {
      validate("Sorry : There is no data for the selected scenario. Please select a different scenario to visualize data")
    }
    
    
    rw.incremental <- Rwanda.df() %>%
      filter(`Type of investment` %in% Incremental_farmer_costs) 
    
    rw_incremental_costs <- ggplot(rw.incremental, aes(Year, Amount, group = `Type of investment`)) +
      geom_line(aes(color = `Type of investment`), size = 1) +
      geom_point(aes(color = `Type of investment`),size =2) +
      scale_color_manual(values = c("red", "steelblue", "green")) +
      theme_bw() +
      # geom_text(aes(label = Amount),
      #           position = position_dodge(width = 1), size = 3) +
      labs (y = "Amount", title = paste("Incremental farmer costs :", "Rwanda |", input$rw_targets, "|", input$rw_scenarios)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.background = element_rect(
              color="black", fill="#F4FC00", size=1.5, linetype="solid"))
    
    rw_incremental_costs %>%
      ggplotly %>%
      layout(legend = list(orientation = "h",
                           x = 0.1,
                           y = -0.2,
                           title = list(text =""))) #%>% 
    # style(text = format(rw.incremental$Amount, big.mark = ","), textposition = "top")
    
  }) 
  
  
  # Rwanda Percent increase farmer costs
  output$rw_percent_increase <- renderPlotly({
    
    if (input$rw_scenarios == "BAU") {
      validate("Sorry : There is no data for the selected scenario. Please select a different scenario to visualize data")
    }
    
    rw.percent.increase <- Rwanda.df() %>%
      filter(`Type of investment` %in% Incremental_percent_costs)
    
    rw_percent_increase <- ggplot(rw.percent.increase, aes(Year, Amount, group = `Type of investment`)) +
      geom_line(aes(color = `Type of investment`), size = 1) +
      geom_point(aes(color = `Type of investment`), size =2) +
      scale_color_manual(values = c("red", "steelblue", "green")) +
      theme_bw() +
      # geom_text(aes(label = Amount),
      #           position = position_dodge(width = 1), size = 3) +
      labs (y = "Amount", title = paste("Incremental costs in (%) :", "Rwanda |", input$rw_targets, "|", input$rw_scenarios)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.background = element_rect(
              color="black", fill="#F4FC00", size=1.5, linetype="solid"))
    
    rw_percent_increase %>%
      ggplotly %>%
      layout(legend = list(orientation = "h",
                           x = 0.1,
                           y = -0.2,
                           title = list(text =""))) #%>% 
    # style(text = format(rw.percent.increase$Amount, big.mark = ","), textposition = "top")
    
  }) 
  
  
  # Rwanda Gross benefits to individual farmers 
  output$rw_gross <- renderPlotly({
    
    if (input$rw_scenarios == "Average (Scen2 & Scen3)") {
      validate("Sorry : There is no data for the selected scenario. Please select a different scenario to visualize data")
    }
    
    
    rw.gross <- Rwanda.df() %>%
      filter(`Type of investment` %in% Gross_benefits) %>% 
      filter(Year != "BAU")
    
    rw_gross_benefits <- ggplot(rw.gross, aes(Year, Amount, fill = `Type of investment`)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = alpha(rainbow(4), .7)) +
      theme_bw() +
      # scale_y_continuous(labels = comma) +
      # geom_text(aes(label = Amount),
      #           position = position_dodge(width = 1),size = 3) +
      labs (y = "Amount", title = paste("Gross benfits to farmers :", "Rwanda |", input$rw_targets, "|", input$rw_scenarios)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    rw_gross_benefits %>%
      ggplotly %>%
      layout(legend = list(orientation = "h",
                           x = 0.1,
                           y = -0.2,
                           title = list(text =""))) 
    
    
  }) 
  
  
  # Rwanda Net Present values 
  output$rw_net <- renderPlotly({
    
    if (input$rw_scenarios == "Average (Scen2 & Scen3)") {
      validate("Sorry : There is no data for the selected scenario. Please select a different scenario to visualize data")
    }
    
    rw.net <- Rwanda.df() %>%
      filter(`Type of investment` %in% Net_values) %>% 
      filter(Year != "BAU")
    
    rw_net_values <- ggplot(rw.net, aes(Year, Amount, group = `Type of investment`)) +
      geom_line(aes(color = `Type of investment`), size = 1) +
      geom_point(aes(color = `Type of investment`),size =2) +
      scale_color_manual(values = c("red", "steelblue", "green")) +
      theme_bw() +
      # geom_text(aes(label = Amount),
      #           position = position_dodge(width = 1), size = 3) +
      labs (y = "Amount", title = paste("Net present values :", "Rwanda |", input$rw_targets, "|", input$rw_scenarios)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.background = element_rect(
              color="black", fill="#F4FC00", size=1.5, linetype="solid"))
    
    rw_net_values %>%
      ggplotly %>%
      layout(legend = list(orientation = "h",
                           x = 0.1,
                           y = -0.2,
                           title = list(text =""))) #%>% 
    # style(text = format(rw.net$Amount, big.mark = ","), textposition = "top")
    
  }) 
  
  
  # Rwanda Benefit cost ratio
  output$rw_bcr <- renderPlotly({
    
    if (input$rw_scenarios == "Average (Scen2 & Scen3)") {
      validate("Sorry : There is no data for the selected scenario. Please select a different scenario to visualize data")
    }
    
    rw.bcr <- Rwanda.df() %>%
      filter(`Type of investment` %in% BCR) %>% 
      filter(Year != "BAU")
    
    rw_bcr <- ggplot(rw.bcr, aes(Year, Amount, group = `Type of investment`)) +
      geom_line(aes(color = `Type of investment`), size = 1) +
      geom_point(aes(color = `Type of investment`),size =2) +
      scale_color_manual(values = c("red", "steelblue", "green")) +
      theme_bw() +
      geom_text(aes(label = Amount),
                position = position_dodge(width = 1), size = 3) +
      labs (y = "Amount", title = paste("Benefit cost ratio :", "Rwanda |", input$rw_targets, "|", input$rw_scenarios)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.background = element_rect(
              color="black", fill="#F4FC00", size=1.5, linetype="solid"))
    
    rw_bcr %>%
      ggplotly %>%
      layout(legend = list(orientation = "h",
                           x = 0.1,
                           y = -0.2,
                           title = list(text =""))) %>% 
      style(text = format(rw.bcr$Amount, big.mark = ","), textposition = "top")
    
  }) 
  
  
}