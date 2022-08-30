ui <- dashboardPage( title="TonFIST Dashboard", 
  dashboardHeader(
    title = "TonFIST Dashboard",
    titleWidth = 230
  ), #end of dashboard header
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",

      # style = "position: fixed;overflow: visible; color: #FFF; width: 220px; white-space: nowrap;",
 
      h3(HTML("<strong>Menu Items</strong>")),
      
      menuItem(
        "About TonFIST",
        tabName = "about",
        icon = icon("info")
      ),

      menuItem(
        "The Dashboard",
        tabName = "dash",
        icon = icon("dashboard")
      ),
      
      conditionalPanel("input.sidebar == 'dash' && input.countries == 'uganda'",
        h4("Scenarios"),
        selectInput("ug_scenarios",
                    "Select a scenario",
                    unique(Uganda$Scenarios),
                    selected = "Scenario 2")
      ), # end of conditional panel
      
      
      conditionalPanel("input.sidebar == 'dash' && input.countries == 'uganda'",
                       h4("Targets"),
                       selectInput("ug_targets",
                                   "Select a target",
                                   unique(Uganda$Targets),
                                   selected = "Target 1")
      ), # end of conditional panel
      
      conditionalPanel("input.sidebar == 'dash' && input.countries == 'rwanda'",
                       h4("Scenarios"),
                       selectInput("rw_scenarios",
                                   "Select a scenario",
                                   unique(Rwanda$Scenarios),
                                   selected = "Scenario 2")
      ), # end of conditional panel
      
      
      conditionalPanel("input.sidebar == 'dash' && input.countries == 'rwanda'",
                       h4("Targets"),
                       selectInput("rw_targets",
                                   "Select a target",
                                   unique(Rwanda$Targets),
                                   selected = "Target B1")
      ), # end of conditional panel
      
      div(class = "sticky_footer", 
          sidebarUserPanel("Developers:",
          subtitle = "Victor Mutugi, Dr. Brian Chiputwa"
      ))

    ) #end of sidebar menu
    
  ), #end of dashboard sidebar
  
  dashboardBody(
    tags$script(
      HTML("$('body').addClass('fixed');")
    ), #end of tags script
    
    
    
    fluidRow(
      tags$style(".nav-tabs {
                background-color: #17202A;
              }
              
              .nav-tabs-custom 
              .nav-tabs li.active:hover a, 
              .nav-tabs-custom 
              .nav-tabs li.active a {
              background-color: #327A00;
              border-top-color: #17202A;
              border: 5px solid;    
              border-right-color: #FF3E00;
              color: white;
              }
              
              .nav-tabs-custom .nav-tabs li.active {
                  border-top-color: #FF3E00;
              }
              
              .nav-tabs-custom .nav-tabs li a{
                  color: #FFFFFF;
              }")
    ), #end of fluidrow
    
    
    
    tags$head(
      # tags$style(
      #   HTML('
      #     .main-header.logo {
      #      font-family: "Georgia", Times, "Times New Roman", serif;
      #      font-weight: bold;
      #     font-size: 20px;
      #     }
      #     /* logo */
      #   .skin-blue .main-header .logo {
      #                         background-color: #2D2E30;
      #                         color:#FFFFFF;
      #                         }
      # 
      #   /* logo when hovered */
      #   .skin-blue .main-header .logo:hover {
      #                         background-color: #151313;
      #                         color:#FFFFFF;
      #                         }
      # 
      #   /* navbar (rest of the header) */
      #   .skin-blue .main-header .navbar {
      #                         background-color: #14CA02;
      #                         color:#FFFFFF;
      #                         }        
      # 
      #   /* main sidebar */
      #   .skin-blue .main-sidebar {
      #                         background-color: #151313;
      #                         color:#FFFFFF;
      #   }
      #   
      #   .skin-blue .main-header .logo:hover {
      #     background-color: #14CA02;
      #   }
      #   
      #   * { font-family: "Arial"; }
      #   
      #   ')
      # ),
      tags$link(
        rel = "stylesheet", 
        type = "text/css",
        href = "style.css"
      )
    ), # end of tags head
    
    
    fluidRow(
      tabItems(
        #The About page tabs
        tabItem(tabName = "about",
                tabBox(width = "100%",
                       fluidRow(
                         # style = "background-image: url(hero_image.png); background-size: cover; height: 780px;",
                         
                         # column(width = 12, 
                                # style = "height:550px;background-color: #FDFCFC;",
                                tags$img(src="hero_image.png", width = "100%")
                         # )
                         # column(width = 3, 
                         #        # style = "background-image: url(forest.jpg); background-size: cover; height: 280px;", 
                         #        align = "left", 
                         #        
                         #        
                         #        div(
                         #          img(
                         #            src = "tonf-logo.png",
                         #            height = 100,
                         #            width = 350,
                         #            style = "margin:10px 10px"
                         #          )
                         #          # h1(tags$p("Trees on Farms Investment Scenario Tool"))
                         #        )
                         # ),
                         # 
                         # tags$br(),
                         # tags$br(),
                         # 
                         # column(width = 9, align = "center", style = 'color: white; height: 250px;',
                         #        
                         #        tags$br(),
                         #        tags$br(),
                         #        tags$br(),
                         #        
                         #        
                         #        h1(HTML("<strong><u>Trees on Farms Investment Scenarios Tool (TonFIST)</u></strong>"))
                         # )
                       ),
                       
                  # tabPanel("About TonFIST",
                  #          icon=icon("address-card"),
                           
                           
                           fluidRow(
                             box(width = 12 ,
                                 column(width = 12, align = "center", style = "color: #FF7C01; line: 5px solid;",
                                        h2(HTML("<strong><u>The Objective</u></strong>"))
                                 ),
                                 
                                 
                                 column(width = 12, align = "center",  
                                        tags$p(h5("The major objective of the digital Trees on Farm (TonF) 
                                           Investment Scenarios Tool is to be able to account for the investment 
                                           expenses related to adopting TonF choices at different levels. 
                                           The tool takes into account two different types of investment requirements: 
                                           (1.) costs borne by the farmer associated with integrating TonF options, 
                                           such as the purchase of inputs, including tree seedlings, manure, 
                                           and fertilizers; and 
                                           (2.) institutional costs required for setting up an enabling environment, 
                                           such as the provision of infrastructure, including tree nurseries and 
                                           processing factories for tree commodities."
                                           ))
                                        )
                             )
                           ),
                           
                           fluidRow(
                             
                             box(width = 12,
                                 
                               column(width = 12, align = "center", style = "color: #FF7C01; line: 5px solid;",
                                        h2(HTML("<strong><u>Why TonFIST</u></strong>"))
                                 ),
                                 
                               column(width = 6, style = "height:400px; background-image: url(land.jpg); background-size: cover;",
                               
                             ), 
                             
                             column(width = 6,
                                    align = "center", style = "height:400px;background-color: #193B02; color: white;",
                                    
                                    tags$br(),
                                    
                                    tags$br(),
                                    
                                    tags$p(h5("Biodiversity exists naturally on Earth. Life as we know it is supported by this biodiversity, 
                                        which includes the trees, birds, bees, mammals, snakes, and more. However, biodiverse landscapes 
                                        are disappearing at an alarming rate, frequently due to the spread of agriculture. The pressing 
                                        need to feed the world's expanding population and the imperative to preserve biodiversity seem at 
                                        odds with one another. We will continue to lose biodiversity and the essential benefits that healthy 
                                        ecosystems offer if we do not improve our resource management. Growing food will become increasingly 
                                        challenging for us."
                                       )),
                                       
                                      tags$br(),
                                      tags$br(),
                                      tags$br(),
                                    tags$p(h4("Thankfully, farms may be run to maximize the importance of biodiversity protection. Our solution is 
                                       Trees on Farms for Biodiversity (TonF)."
                                          ))
                             )
                             )
                             
                           ),
                           
                           
                           fluidRow(
                             box(width = 12,
                                 
                                 column(width = 12, align = "center", style = "color: #FF7C01; line: 5px solid;",
                                        h2(HTML("<strong><u>Partners</u></strong>"))
                                 ),
                                 
                                 column(width = 6,
                                        align = "center", style = "height:250px;background-color: #FDFCFC; color: black;",
                                        
                                        tags$br(),
                                        
                                        tags$br(),
                                        
                                        tags$p(h5("With activity in five nations, the World Agroforestry (ICRAF) carries out the project's global 
                                       implementation; Uganda, Rwanda, and Peru also have direct management responsibilities. Tanjungpura 
                                       University, Pontianak, and the Centre for International Forestry Research (CIFOR) are the leaders in 
                                       Indonesia. Leading in Honduras is the Centro Agronómico Tropical de Investigación y Enseza (CATIE). 
                                       Knowledge management and outreach expertise are provided by the International Union for Conservation of Nature."
                                        ))
                                 ),
                                 
                                 column(width = 6, style = "height:250px;background-color: #FDFCFC;",
                                        tags$img(src="partners.png", width = "100%")
                                 )
                             )
                           ),
                           
                           
                           fluidRow(
                             box(width = 12,
                                 column(width = 12, align = "center", style = "color: #FF7C01;",
                                        h2(HTML("<strong><u>TonFIST Concept</u></strong>"))
                                 ),
                                 
                                 
                                 # column(width = 12, style = "background-color: #EEE1DA; color: #000000;",
                                 #        
                                 #        
                                 #        h4(HTML("Each country team decides on targets through a stakeholder engagement process at the start of the project. 
                                 #            There are three potential outcomes for each target. which are:")),
                                 #        tags$br(),
                                 #        
                                 #        h4(HTML("Scenario 1: The status quo, sometimes known as <strong>business as usual</strong> or <strong>status quo</strong>
                                 #            refers to the typical land-use systems at each country's project sites.")),
                                 #        tags$br(),
                                 #        
                                 #        h4(HTML("Scenario 2. Optimised land-use systems with TonF alternatives for better and enhanced livelihoods, i.e., the incorporation
                                 #            of tree species that offer farmers real benefits through food and nutrition, fodder, timber, fuelwood, and/or revenues.")),
                                 #        tags$br(),
                                 #        
                                 #        h4(HTML("Scenario 3: High biodiversity benefits in terms of ecosystem services, such as carbon sequestration, nitrogen fixation, 
                                 #            or decreased run-off/erosion, are provided by tree species from the IUCN Red List of Threatened Species."
                                 #          )),
                                 #        
                                 #        tags$br(),
                                 #        h4("By taking into account the political and financial frameworks (enabling environment) of the intervention alternatives that 
                                 #           will be included in national plans and programs, this enables the assessment of the investment costs necessary to 
                                 #           transition from the existing scenario 1 to scenarios 2 and/or scenario 3.")
                                 # ),
                                 
                                 column(width = 5, style = "height:450px;",
                                        tags$img(src="User_manual.png", width = "100%")
                                        
                                 ), 
                                 
                                 column(width = 7, style = "height:450px;",
                                        tags$img(src="User_flow_Diagram.png", width = "100%")
                                        
                                 ) 
                             )
                             
                             
                           ),
                  
                          tags$br(),
                          tags$br(),
                  
                           
                           fluidRow(
                             column(width = 1, style = "background-color: #E3E1DC; height: 300px;"),
                             
                              column(width = 10, style = "background-color: #E3E1DC; color: black; height: 300px;",   
                                tabsetPanel(id = "target-tabs",
                                  tabPanel("Uganda Targets",
                                           value = "uganda",
                                           
                                           tags$br(),
                                           
                                           column(width =12, alighn = 'center', style = "background-color: #E3E1DC; color: black",
                                                  tags$p((HTML("On the southern slopes of Uganda's Mount Elgon, in the Manafwa and Bududa districts, biodiversity assessments 
                                                               of the farming systems were conducted. Through careful integration of trees on farms, these assessments identified 
                                                               three target land use systems in which management may be enhanced: (1) Annual cropping systems, (2) coffee-banana 
                                                               agroforestry systems, and (3) Eucalyptus woodlot systems. For each land use system, a target was set by the country 
                                                               teams and each target has three scenarios as described above."))),
                                                  
                                                  tags$p((HTML("<strong>TARGET 1:</strong> Sustainably managed coffee-banana systems through the integration of 
                                                               30% of well managed indigenous species"))),
                                                  
                                                  tags$p((HTML("<strong>TARGET 2:</strong> Increase tree cover at landscape level without interfering with farm 
                                                               operations through guided and systematic integration of TonF options in annual cropping systems"))),
                                                  
                                                  tags$p((HTML("<strong>TARGET 3:</strong> Alternative species to eucalyptus promoted leading to a diversified woodlot system")))
                                           )
                                  ),
                                  
                                  tabPanel("Rwanda Targets",
                                           value = "rwanda",
                                           
                                           tags$br(),
                                           
                                           column(width =12, alighn = 'center', style = "background-color: #E3E1DC; color: black",
                                                  tags$p((HTML("The Trees on Farms (TonF) project in Rwanda has two broad targets i.e., Broad target 1 and Broad target 2 which are 
                                                               optimized for the maize-bean system in semi-arid Bugesera and irish-bean system in highland Gishwati respectively.
                                                               Each broad target has a sub-category(s) and for each target in the broad target subcategory i.e., Target B1, B2, 
                                                               and G3 has three scenarios: "))),
                                                  
                                                  tags$p((HTML("<strong>•	Broad target 1:</strong> Optimized trees on farms interventions in maize/bean systems of 
                                                                Bugesera."))),
                                                  
                                                  tags$p((HTML(" •	Target B1: Broad target 1 through fruit production"))),
                                                  
                                                   tags$p((HTML(" •	Target B2:Broad target 1 through tree products"))),
                                                  
                                                  tags$p((HTML("<strong>•	Broad target 2:</strong> Optimized trees on farms interventions in irish potato/bean systems of 
                                                               Gishwati."))),
                                                  
                                                  tags$p((HTML(" •	Target G3: Broad target 2 for soil erosion control and water management."))),
                                           )
                                  ),
                                  
                                  tabPanel("Peru Targets",
                                           value = "peru",
                                           
                                           tags$br(),
                                           tags$br(),
                                           tags$br(),
                                           
                                           
                                           column(width =12, alighn = 'center', style = "background-color: #FE0000; color: white; ; border-radius: 25px",
                                                  tags$p(h1(HTML("<strong>There is no data for Peru targets at the moment</strong>")))
                                           )
                                  ),
                                  
                                  
                                  tabPanel("Indonesia Targets",
                                           value = "indonesia",
                                           
                                           tags$br(),
                                           tags$br(),
                                           tags$br(),
                                           
                                           
                                           column(width =12, alighn = 'center', style = "background-color: #FE0000; color: white; ; border-radius: 25px",
                                                  tags$p(h1(HTML("<strong>There is no data for Indonesia targets at the moment</strong>")))
                                           )
                                  ),
                                  
                                  tabPanel("Honduras Targets",
                                           value = "honduras",
                                           
                                           tags$br(),
                                           tags$br(),
                                           tags$br(),
                                           
                                           column(width =12, alighn = 'center', style = "background-color: #FE0000; color: white; ; border-radius: 25px",
                                                  tags$p(h1(HTML("<strong>There is no data for Honduras targets at the moment</strong>")))
                                           )
                                  ),
                               )
                              ),
                             
                             column(width = 1, style = "background-color: #E3E1DC; height: 300px;")
                           ),
                           
                          
                           
                  # )
                )
          
        ),
        
        # The Dashboard page tabs
        tabItem(tabName = "dash",
                tabBox(id = "countries", width = 12,
                  tabPanel("Uganda Dashboard",
                           value = "uganda",
                     tags$p(HTML("<strong>This is the Uganda dashboard.</strong>")),
                     tags$p("To switch between the countries; Click on/ select the desired country to visualize that country's dashboard."),
                     tags$p("For each country there are two investment levels i.e., the institutional and individual farmer level charts respectively, from the tabs below."),
                     tags$p("To switch between the investment levels; Click on/ select the desired tab to visualize the data."),
                     tags$p(HTML("<strong>NOTE:</strong> The charts in the dashboard are interactive: Hover over a data point to view the data label (cost at each point).")),
                    tabsetPanel(
                      id = "uganda.tabs",
                      
                      tabPanel("Institutional level",
                               
                         fluidRow(
                           column(width = 2),
                           
                           column(width = 8,
                             withSpinner(plotlyOutput("ug_inst", height = "650px"))
                            ),
                           
                           column(width = 2)
                           
                          ),
                         
                         tags$br(),
                         tags$br()
                      ),
                      
                      tabPanel("Individual farmer level",
                               
                         fluidRow(
                           column(width = 2),
                           
                           column(width = 8,
                                  withSpinner(plotlyOutput("ug_indv", height = "650px"))
                           ),
                           
                           column(width = 2)
                           
                         ), #end of fluidrow
                         
                         
                         tags$br(),
                         tags$br(),
                         
                         fluidRow(
                           column(width = 6,
                                  withSpinner(plotlyOutput("ug_incremntal", height = "650px"))
                           ),
                           
                           column(width = 6,
                                  withSpinner(plotlyOutput("ug_percent_increase", height = "650px"))
                           )
                           
                         ), #end of fluidrow
                         tags$br(),
                         tags$br(),
                         
                         fluidRow(
                           column(width = 2),
                           
                           column(width = 8,
                                  withSpinner(plotlyOutput("ug_gross", height = "650px"))
                           ),
                           
                           column(width = 2)
                           
                         ), #end of fluidrow
                        
                        tags$br(),
                        tags$br(),
                        
                        fluidRow(
                          column(width = 6,
                                 withSpinner(plotlyOutput("ug_net", height = "650px"))
                          ),
                          
                          column(width = 6,
                                 withSpinner(plotlyOutput("ug_npv_person", height = "650px"))
                          )
                          
                        ),#end of fluidrow
                        
                        tags$br(),
                        tags$br(),
                        
                        fluidRow(
                          column(width = 2,
                          ),
                          
                          column(width = 8,
                                 withSpinner(plotlyOutput("ug_bcr", height = "650px"))
                          ),
                          
                          column(width = 2,
                          )
                          
                        ),
                        
                        tags$br(),
                        tags$br()
                        
                      ),
                      
                      tabPanel("Carbon Data",
                               fluidRow(
                                 column(width = 12,
                                        align = "center",
                                        style = "color:#FF7C01;",
                                        tags$p(h2(HTML("<strong><u>Institutional Level</u></strong>")))
                                        ),
                                 
                                 tags$br(),
                                 tags$br(),
                                
                               ),
                               
                               fluidRow(
                                 column(width = 6,
                                        withSpinner(plotlyOutput("Scen2_Inst_Carbon", height = "650px"))
                                        ),
                                 
                                 column(width = 6,
                                        withSpinner(plotlyOutput("Scen3_Inst_Carbon", height = "650px")))
                                 
                               ),
                               
                               tags$br(),
                               tags$br(),
                               
                               fluidRow(
                                 column(width = 6,
                                        withSpinner(plotlyOutput("Scen2_Inst_Seq_Carbon", height = "650px"))
                                        ),
                                 
                                 column(width = 6,
                                        withSpinner(plotlyOutput("Scen3_Inst_Seq_Carbon", height = "650px")))
                               ), 
                               
                               tags$br(),
                               tags$br(),
                               
                               fluidRow(
                                 column(width = 12,
                                        align = "center",
                                        style = "color:#FF7C01;",
                                        tags$p(h2(HTML("<strong><u>Individual Farmer Level</u></strong>")))
                                 ),
                                 
                                 tags$br(),
                                 tags$br(),
                                 
                               ),
                               
                               fluidRow(
                                 column(width = 6,
                                        withSpinner(plotlyOutput("Scen2_Indv_Carbon", height = "650px"))
                                 ),
                                 
                                 column(width = 6,
                                        withSpinner(plotlyOutput("Scen3_Indv_Carbon", height = "650px")))
                                 
                               ),
                               
                               tags$br(),
                               tags$br(),
                               
                               fluidRow(
                                 column(width = 6,
                                        withSpinner(plotlyOutput("Scen2_Indv_Seq_Carbon", height = "650px"))
                                 ),
                                 
                                 column(width = 6,
                                        withSpinner(plotlyOutput("Scen3_Indv_Seq_Carbon", height = "650px"))
                                        )
                               ),
                               
                               tags$br(),
                               tags$br(),
                               
                               fluidRow(
                                 column(width = 12,
                                        align = "center",
                                        style = "color:#FF7C01;",
                                        tags$p(h2(HTML("<strong><u>Total investment costs vs carbon stock</u></strong>")))
                                 )
                               ),
                               
                               tags$br(),
                               tags$br(),
                               
                               fluidRow(
                                 column(width = 6,
                                        withSpinner(plotlyOutput("Scen2_Total_Carbon", height = "650px"))
                                 ),
                                 
                                 column(width = 6,
                                        withSpinner(plotlyOutput("Scen3_Total_Carbon", height = "650px"))
                                 )
                                 
                               ),
                               
                               tags$br(),
                               tags$br(),
                               
                               fluidRow(
                                 column(width = 6,
                                        withSpinner(plotlyOutput("Scen2_Total_Seq_Carbon", height = "650px"))
                                 ),
                                 
                                 column(width = 6,
                                        withSpinner(plotlyOutput("Scen3_Total_Seq_Carbon", height = "650px")))
                               )
                               
                      ),
                      
                      tags$br(),
                      tags$br()
                              
                      # tabPanel("Investment Results",
                      #   tags$iframe(style = "height:700px; width: 100%; scrolling=yes",
                      #               src = "Investment_Scenarios_results_Uganda_VM.pdf")
                      # )
                      
                    )
                    
                  ),
                  
                  tabPanel("Rwanda Dashboard",
                           value = "rwanda",
                           tags$p(HTML("<strong>This is the Rwanda dashboard.</strong>")),
                           tags$p("To switch between the countries; Click on/ select the desired country to visualize that country's dashboard."),
                           tags$p("For each country there are two investment levels i.e., the institutional and individual farmer level charts respectively, from the tabs below."),
                           tags$p("To switch between the investment levels; Click on/ select the desired tab to visualize the data."),
                           tags$p(HTML("<strong>NOTE:</strong> The charts in the dashboard are interactive: Hover over a data point to view the data label (cost at each point).")),      
                           tabsetPanel(
                             id = "rwanda.tabs",
                             
                             tabPanel("Institutional level",
                                      fluidRow(
                                        column(width = 2),
                                        
                                        column(width = 8,
                                               withSpinner(plotlyOutput("rw_inst", height = "650px"))
                                        ),
                                        
                                        column(width = 2)
                                        
                                      ),
                                      
                                      tags$br(),
                                      tags$br()      
                             ),
                             
                             tabPanel("Individual farmer level",
                                      fluidRow(
                                        column(width = 2),
                                        
                                        column(width = 8,
                                               withSpinner(plotlyOutput("rw_indv", height = "650px"))
                                        ),
                                        
                                        column(width = 2)
                                        
                                      ), #end of fluidrow
                                      
                                      
                                      tags$br(),
                                      tags$br(),
                                      
                                      fluidRow(
                                        column(width = 6,
                                               withSpinner(plotlyOutput("rw_incremntal", height = "650px"))
                                        ),
                                        
                                        column(width = 6,
                                               withSpinner(plotlyOutput("rw_percent_increase", height = "650px"))
                                        )
                                        
                                      ), #end of fluidrow
                                      tags$br(),
                                      tags$br(),
                                      
                                      fluidRow(
                                        column(width = 2),
                                        
                                        column(width = 8,
                                               withSpinner(plotlyOutput("rw_gross", height = "650px"))
                                        ),
                                        
                                        column(width = 2)
                                        
                                      ), #end of fluidrow
                                      
                                      tags$br(),
                                      tags$br(),
                                      
                                      fluidRow(
                                        column(width = 6,
                                               withSpinner(plotlyOutput("rw_net", height = "650px"))
                                        ),
                                        
                                        column(width = 6,
                                               withSpinner(plotlyOutput("rw_npv_person", height = "650px"))
                                        )
                                        
                                      ),#end of fluidrow     \
                                      
                                      tags$br(),
                                      tags$br(),
                                      
                                      fluidRow(
                                        column(width = 2,
                                        ),
                                        
                                        column(width = 8,
                                               withSpinner(plotlyOutput("rw_bcr", height = "650px"))
                                        ),
                                        
                                        column(width = 2,
                                        )
                                        
                                      ),
                                      
                                      
                                      tags$br(),
                                      tags$br()
                                      
                             ),
                          
                             # tabPanel("Investment Results",
                             #          tags$iframe(style = "height:700px; width: 100%; scrolling=yes",
                             #                      src = "Investment_Scenarios_results_Rwanda_VM.pdf")
                             # )
                             
                           )
                           
                  ),
                  
                  tabPanel("Peru Dashboard",
                           value = "peru",
                           tags$p("The Peru dashboard is under construction and will be made available when data is loaded"),
                           
                           fluidRow(
                             column(width = 12,
                                    tags$img(src="coming-soon.png", width = "100%" , 
                                             height = 750)
                             )
                           )
                           # tabsetPanel(
                           #   id = "peru.tabs",
                           #   
                           #   tabPanel("Institutional level"
                           #            
                           #   ),
                           #   
                           #   tabPanel("Individual farmer level"
                           #            
                           #   ),
                           #   
                           # )
                           
                  ),
                  
                  tabPanel("Indonesia Dashboard",
                           value = "indonesia",
                           tags$p("The Indonesia dashboard is under construction and will be made available when data is loaded"),
                           
                           fluidRow(
                             column(width = 12,
                                    tags$img(src="coming-soon.png", width = "100%" , 
                                             height = 750)
                             )
                           )
                  #          tabsetPanel(
                  #            id = "peru.tabs",
                  #            
                  #            tabPanel("Institutional level"
                  #                     
                  #            ),
                  #            
                  #            tabPanel("Individual farmer level"
                  #                     
                  #            ),
                  #            
                  #          )
                  #          
                  ),

                  tabPanel("Honduras  Dashboard",
                           value = "honduras ",
                           tags$p("The Honduras  dashboard is under construction and will be made available when data is loaded"),
                           
                           fluidRow(
                             column(width = 12,
                                    tags$img(src="coming-soon.png", width = "100%" , 
                                             height = 750)
                             )
                           )
                  #          tabsetPanel(
                  #            id = "peru.tabs",
                  #            
                  #            tabPanel("Institutional level"
                  #                     
                  #            ),
                  #            
                  #            tabPanel("Individual farmer level"
                  #                     
                  #            ),
                  #            
                  #          )
                  #          
                  ),
                  
                )
                
        )
      )
    )
    
    
  ), #end of dashboard body
  
  footer = dashboardFooter(
    left = tags$a(href="https://www.worldagroforestry.org/blog/2021/08/04/feature-interview-trees-farms-come-hidden-costs-can-now-be-calculated", icon("fas fa-caret-up"), "World Agroforestry (ICRAF)", target="_blank"),
    right = tags$a(href="https://treesonfarmsforbiodiversity.com/about-trees-on-farms/", icon("fas fa-caret-up"), "IKI Trees on Farms", target="_blank")
  )
  
)