# ProViz
#
# MIT License
# 
# Copyright © 2021 SomaLogic, Inc.
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy 
# of the ProViz software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation the 
# rights to use, copy, modify, merge, publish, distribute, sublicense, and/or 
# sell copies of the Software, and to permit persons to whom the Software is 
# furnished to do so, subject to the following conditions outlined below. 
# Further,  ProViz and SomaLogic are trademarks owned by SomaLogic, Inc. No 
# license is hereby granted to these trademarks other than for purposes of 
# identifying the origin or source of the Software.
# 
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
# AUTHORS OR COPYRIGHT HOLDER(S) BE LIABLE FOR ANY CLAIM, DAMAGES, WHETHER 
# DIRECT OR  INDIRECT, OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, 
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR 
# THE USE OR OTHER DEALINGS IN THE SOFTWARE.

dashboardPage(skin='blue',
   dashboardHeader(title='ProViz'),

   dashboardSidebar(
      sidebarMenu(
         menuItem('Welcome to ProViz', tabName = 'welcome'),
         menuItem('Load & Filter ADAT', tabName = 'loadADAT', 
                  icon = icon('folder-open')),
         menuItem('Merge Data', tabName = 'mergeData',
                  icon = icon('code-branch')),
         menuItem('Create Group', tabName = 'createGroup',
                  icon = icon('pie-chart')),
         menuItem('Plots', icon=icon('bar-chart'), tabName = 'plots',
            menuSubItem('Boxplot', tabName = 'pltBoxplot'),
            menuSubItem('CDF', tabName = 'pltCDF'),
            menuSubItem('Scatter Plots', tabName = 'pltScatterPlot')
         ),
         menuItem('Statistical Tests', tabName = 'statTests',
                  icon = icon('table')
         ),
         radioButtons(inputId = 'rdoIDChoice', 
                      label = 'Label SOMAmers by:', 
                      choices = c('SOMAmer ID', 'Protein Name', 'Gene Symbol')
         ),
         htmlOutput(outputId = 'SLlogo')
      )
   ),

   dashboardBody(
      tags$style(".shiny-file-input-progress {display: none}"),
      tabItems(
         tabItem(tabName = 'welcome',
          fluidRow(
           column(12,
             htmlOutput(outputId = 'license')
           )
          )
         ),
         tabItem(tabName = 'loadADAT',
                 fluidRow(
                    column(3, 
                      fileInput('adat_file', 'Choose ADAT file',
                                accept=c('.adat')),
                      progressBar(id = 'loadProgbar', value = 0,
                                  display_pct = TRUE),
                      htmlOutput('loadPreviewText'),
                      box(width = 12, solidHeader = TRUE,
                        status = 'primary',
                        title = 'Categorical Filters',
                        checkboxInput(inputId = 'fltrCatExcludeNA',
                                      label = 'Filter samples with no entry'),
                        selectInput(inputId = 'fltrCatSelect',
                                    label = 'Filter By',
                                    choices = '<NONE>'),
                        selectInput(inputId = 'fltrCatCrit',
                                    label = 'Remove',
                                    choices = '<NONE>',
                                    multiple = TRUE,
                                    selected = ''),
                        actionButton(inputId = 'fltrCatApply',
                                     label = 'Apply',
                                     icon = icon('filter'))
                      ),
                      box(width = 12, solidHeader = TRUE,
                        status = 'primary',
                        title = 'Continuous Filters',
                        checkboxInput(inputId = 'fltrConExcludeNA',
                                      label = 'Filter samples with no entry'),
                        selectInput(inputId = 'fltrConSelect',
                                    label = 'Filter By',
                                    choices = '<NONE>'),
                        sliderInput(inputId = 'fltrConCrit',
                                    label = 'Range',
                                    min = 0, max = 0, step = 0,
                                    value = c(0, 0)),
                        actionButton(inputId = 'fltrConApply',
                                     label = 'Apply',
                                     icon = icon('filter'))
                      ),
                      fluidRow(
                         column(6,
                           actionButton(inputId = 'fltrReset',
                                        label = 'Reset All Filters',
                                        icon = icon('undo'))
                         ),
                         column(6,
                           downloadButton(outputId = 'fltrDownloadADAT',
                                          label = 'Download ADAT')
                         )
                      )
                    ),
                    column(width = 9,
                           h4('ADAT Preview'),
                           DT::dataTableOutput('loadPreviewTable')
                    )
                 )
         ),
         tabItem(tabName = 'mergeData',
                 fluidRow(
                   column(3,
                     verbatimTextOutput('fltrPreviewText'),
                     fileInput('merge_file', 'Data to merge',
                               accept=c('.csv', '.txt')),
                     htmlOutput('mergeMessage'),
                     br(),
                     selectInput(inputId = 'mergeADATCol',
                                 label = 'ADAT Merge Column',
                                 choices = '<NONE>'),
                     selectInput(inputId = 'mergeDataCol',
                                 label = 'Data Merge Column',
                                 choices = '<NONE>'),
                     radioButtons(inputId = 'mergeType',
                                  label = 'Type of Merge',
                                  choices = c('Keep All ADAT Rows',
                                              'Keep Only Intersection')),
                     fluidRow(
                        column(3,
                           actionButton(inputId = 'mergeApply',
                                        label = 'Merge',
                                        icon = icon('code-merge'))
                        ),
                        column(9,
                           downloadButton(outputId = 'mergeDownloadADAT',
                                          label = 'Download ADAT')
                        )
                     ),
                     br(),
                     htmlOutput('mergeADATstats')
                   ),
                   column(9,
                     h4('ADAT Preview'),
                     DT::dataTableOutput('mergeADATPreview'),
                     h4('Data File Preview'),
                     DT::dataTableOutput('mergeDataPreview')
                   )
                 )
         ),
         tabItem(tabName = 'createGroup',
                 fluidRow(
                    column(6,
                           box( width = 12, status = 'primary',
                                title = 'Create New Group',
                                solidHeader = TRUE,
                                fluidRow(
                                   column(12,
                                      textInput(inputId = 'grpNewColName',
                                                label = 'New Column Name',
                                                value = 'New Group'),
                                      radioButtons(inputId = 'grpCreateFrom',
                                                   label = 'Create From',
                                                   choices = c('Continuous',
                                                               'Categorical')),
                                      selectInput(inputId = 'grpSourceColName',
                                                  label = 'Source Column',
                                                  choices = '<NONE>'),
                                      textInput(inputId = 'grpGrpALabel',
                                                label = 'Group A Label',
                                                value = 'A'),
                                      textInput(inputId = 'grpGrpBLabel',
                                                label = 'Group B Label',
                                                value = 'B'),
                                      conditionalPanel(condition = 
                                          'input.grpCreateFrom == "Continuous"',
                                         sliderInput(inputId = 'grpConSplit',
                                                     label = 'Group Split Value',
                                                     max = 0, min = 0,
                                                     value = 0, step = 0),
                                         checkboxInput(inputId = 'grpLog10',
                                                       label = 'Log10 Transform')
                                      ),
                                      conditionalPanel(condition = 
                                          'input.grpCreateFrom == "Categorical"',
                                             selectInput(inputId = 'grpCatGrpA',
                                                         label = 'Group A',
                                                         choices = '<NONE>',
                                                         multiple = TRUE),
                                             selectInput(inputId = 'grpCatGrpB',
                                                         label = 'Group B',
                                                         choices = '<NONE>',
                                                         multiple = TRUE),
                                             verbatimTextOutput( outputId = 'grpWarning')
                                      ),
                                      br(),
                                      fluidRow(
                                         column(6,
                                           actionButton(inputId = 'grpApply',
                                                        label = 'Create New Group Column',
                                                        icon = icon('columns'))
                                         ),
                                         column(6,
                                           downloadButton(outputId = 'grpDownloadADAT',
                                                          label = 'Download ADAT')
                                         )
                                      ),
                                      br(),
                                      fluidRow(
                                         column(12,
                                           htmlOutput('grpADATstats')
                                         )
                                      )
                                   )
                                )
                           )
                    ),
                    column(6,
                           box(width = 12, status = 'primary',
                               title = 'Preview',
                               solidHeader = TRUE,
                               DT::dataTableOutput('grpPreviewTable')
                           )
                    )
                 )
         ),
         tabItem(tabName = 'pltBoxplot',
                 fluidRow(
                   column(12,
                          box(width = 12, status = 'primary',
                              uiOutput('pltBxUI')
                          )
                   )
                 ),
                 fluidRow(
                   tabBox(width = 12, title = 'Plot Settings',
                      tabPanel('Axes',
                        fluidRow(
                          column(6,
                                 box(width = 12, status = 'primary',
                                     selectInput(inputId = 'pltBxXaxis',
                                                 label = 'X-axis',
                                                 choices = '<NONE>'),
                                     textInput(inputId = 'pltBxXaxisTitle',
                                               label = 'X-axis Title'),
                                     checkboxInput(inputId = 'pltBxRemoveNA',
                                                   label = 'Remove NAs')
                                 )
                          ),
                          column(6,
                                 box(width = 12, status = 'primary',
                                     selectInput(inputId = 'pltBxYaxis',
                                                 label = 'Y-axis',
                                                 choices = '<NONE>'),
                                     textInput(inputId = 'pltBxYaxisTitle',
                                               label = 'Y-axis Title'),
                                     checkboxInput(inputId = 'pltBxYaxisLog10',
                                                   label = 'Y-axis Log10')
                                 )
                          )
                        )
                      ),
                      tabPanel('Colors and Points',
                        fluidRow(
                          column(6,
                                 box(width = 12, status = 'primary',
                                     # notches not supported in ggplotly?
                                     # checkboxInput(inputId = 'pltBxNotch',
                                     #               label = 'Notch boxes'),
                                     br(), br(),
                                     selectInput(inputId = 'pltBxColor',
                                                 label = 'Color',
                                                 choices = colorNames),
                                     fluidRow(
                                        column(6,
                                          sliderInput(inputId = 'pltBxAlpha',
                                                      label = 'Alpha',
                                                      value = 0.8, 
                                                      min = 0.0, max = 1.0, 
                                                      step = 0.1)
                                        )
                                     )
                                 )
                          ),
                          column(6,
                                 box(width = 12, status = 'primary',
                                     checkboxInput(inputId = 'pltBxBeeswarm',
                                                   label = 'Beeswarm'),
                                     selectInput(inputId = 'pltBxBeecolor',
                                                 label = 'Beeswarm point color',
                                                 choices = colorNames),
                                     fluidRow(
                                        column(6, 
                                          sliderInput(inputId = 'pltBxBeeAlpha',
                                                      label = 'Alpha',
                                                      value = 0.8, 
                                                      min = 0.0, max = 1.0, 
                                                      step = 0.1)
                                        ),
                                        column(6,
                                          sliderInput(inputId = 'pltBxBeeSize',
                                                      label = 'Point Size',
                                                      value = 1.0,
                                                      min = 0.5, max = 2.5,
                                                      step = 0.1)
                                        )
                                     )
                                 )
                          )
                        )
                      ),
                      tabPanel('Title and Size',
                        fluidRow(
                          column(8,
                                 box(width = 12, status = 'primary',
                                     textInput(inputId = 'pltBxTitle',
                                               label = 'Title'),
                                     textInput(inputId = 'pltBxSubtitle',
                                               label = 'Subtitle')
                                 )
                          ),
                          column(4,
                                 box(width = 12, status = 'primary',
                                     sliderInput(inputId = 'pltBxHeight',
                                                 label = 'Plot Height',
                                                 min = 200, max = 1000, 
                                                 step = 50, value=400),
                                     sliderInput(inputId = 'pltBxWidth',
                                                 label = 'Plot Width',
                                                 min = 200, max = 1000, 
                                                 ste = 50, value = 400)
                                 )
                          )
                        )
                      )
                   )
                 )
         ),
         tabItem(tabName = 'pltCDF',
                 fluidRow(
                    column(12,
                           box(width = 12, status = 'primary',
                               uiOutput('pltCDFUI')
                           )
                    )
                 ),
                 fluidRow(
                    tabBox(width = 12, title = 'Plot Settings',
                      tabPanel('Axes',
                         fluidRow(
                            column(6,
                                   box(width = 12, status = 'primary',
                                       selectInput(inputId = 'pltCDFXaxis',
                                                   label = 'X-axis',
                                                   choices = '<NONE>'),
                                       textInput(inputId = 'pltCDFXaxisTitle',
                                                 label = 'X-axis Title'),
                                       checkboxInput(inputId = 'pltCDFLog10',
                                                     label = 'Log10')
                                   )
                            )
                         )
                      ),
                      tabPanel('Lines and Points',
                         fluidRow(
                            column(6,
                                   box(width = 12, status = 'primary',
                                       selectInput(inputId = 'pltCDFColorBy',
                                                   label='Color By',
                                                   choices = '<NONE>'),
                                       checkboxInput(inputId = 'pltCDFRemoveNA',
                                                     label = 'Remove NAs')
                                   )
                            ),
                            column(6,
                                   box(width = 12, status = 'primary',
                                      sliderInput(inputId = 'pltCDFLineWidth',
                                                  label = 'Line Width',
                                                  min = 0.1, max = 1.5, 
                                                  step = 0.1, value = 0.5),
                                      sliderInput(inputId = 'pltCDFPointSize',
                                                  label = 'Point Size', 
                                                  min = 0, max = 2,
                                                  step = 0.1, value = 1)
                                   )
                            )
                         )
                      ),
                      tabPanel('Title and Size',
                        fluidRow(
                          column(8,
                                 box(width = 12, status = 'primary',
                                     textInput(inputId = 'pltCDFTitle',
                                               label = 'Title'),
                                     textInput(inputId = 'pltCDFSubtitle',
                                               label = 'Subtitle')
                                 )
                            ),
                            column(4,
                                   box(width = 12, status = 'primary',
                                       sliderInput(inputId = 'pltCDFHeight',
                                                   label = 'Plot Height',
                                                   min = 200, max = 1000, 
                                                   step = 50, value = 400),
                                       sliderInput(inputId = 'pltCDFWidth',
                                                   label='Plot Width',
                                                   min = 200, max = 1000, 
                                                   step = 50, value = 400)
                                   )
                            )
                         )
                      )
                    )
                 )
         ),
         tabItem(tabName = 'pltScatterPlot',
                 fluidRow(
                    column(12,
                           box( width = 12, status = 'primary',
                                uiOutput('pltSctrUI')
                           )
                    )
                 ),
                 fluidRow(
                    tabBox(width = 12, title = 'Plot Settings',
                       tabPanel('Axes',
                        fluidRow(
                          column(6,
                                 box(width = 12, status = 'primary',
                                     selectInput(inputId = 'pltSctrXaxis',
                                                 label = 'X-axis',
                                                 choices = '<NONE>'),
                                     textInput(inputId = 'pltSctrXaxisTitle',
                                               label = 'X-axis Title'),
                                     checkboxInput(inputId = 'pltSctrXaxisLog10',
                                                   label='X-axis Log10')
                                 )
                          ),
                          column(6,
                                 box(width = 12, status = 'primary',
                                     selectInput(inputId = 'pltSctrYaxis',
                                                 label = 'Y-axis',
                                                 choices = '<NONE>'),
                                     textInput(inputId = 'pltSctrYaxisTitle',
                                               label = 'Y-axis Title'),
                                     checkboxInput(inputId = 'pltSctrYaxisLog10',
                                                   label = 'Y-axis Log10')
                                 )
                          )
                        )
                       ),
                       tabPanel('Points',
                        fluidRow(
                          column(12,
                            box(width = 12, status = 'primary',
                             fluidRow(
                              column(6, 
                                radioButtons(inputId='pltSctrColorBy',
                                             label = 'Color By',
                                             choices = c('Static',
                                                         'Continuous',
                                                         'Category')),
                                fluidRow(
                                   column(6,
                                     sliderInput(inputId = 'pltSctrPtSize',
                                                 label = 'Point Size',
                                                 value = 1.0,
                                                 min = 0.5, max = 2.5,
                                                 step = 0.1),
                                   ),
                                   column(6,
                                     sliderInput(inputId = 'pltSctrPtAlpha',
                                                 label = 'Point Alpha',
                                                 value = 0.8,
                                                 min = 0.0, max = 1.0,
                                                 step = 0.1)
                                   )
                                )
                              ),
                              column(6,
                               fluidRow(   
                                  conditionalPanel(
                                    condition = 'input.pltSctrColorBy == "Static"',
                                    selectInput( inputId = 'pltSctrPtCol',
                                                 label = 'Point  Color',
                                                 choices = colorNames
                                    )
                                  ),
                                  conditionalPanel(
                                    condition = 'input.pltSctrColorBy == "Continuous" |
                                                 input.pltSctrColorBy == "Category"',
                                    selectInput(inputId = 'pltSctrColorByVar',
                                                label = 'Variable',
                                                choices = '<NONE>')
                                  ),
                                  conditionalPanel(
                                    condition = 'input.pltSctrColorBy == "Continuous"',
                                    fluidRow(
                                      checkboxInput(inputId = 'pltSctrColorByLog',
                                                    label = 'Log10 Transform')
                                    ),
                                    fluidRow(
                                      column(6,
                                        selectInput(inputId = 'pltSctrColorByStart',
                                               label = 'Start Color',
                                               choices = colorNames)
                                      ),
                                      column(6,
                                        selectInput(inputId = 'pltSctrColorByEnd',
                                                    label = 'End Color',
                                                    choices = colorNames)
                                      )
                                    )
                                  ),
                                  checkboxInput(inputId = 'pltSctrRemoveNA',
                                                label = 'Remove NAs')
                                 )
                               )
                              )
                            )
                          )
                        )
                     ),
                     tabPanel('Lines',
                      fluidRow(
                        column(6,
                               box(width = 12, status = 'primary',
                                   checkboxInput(inputId = 'pltSctrRegrLine',
                                                 label = 'Regression Line'),
                                   sliderInput(inputId = 'pltSctrRegrLineWidth',
                                               label = 'Regression Line Width',
                                               min = 0.1, max = 1.5, 
                                               step = 0.1, value = 1),
                                   selectInput(inputId = 'pltSctrRegrLineCol',
                                               label = 'Regression Line Color',
                                               choices = colorNames),
                                   selectInput(inputId = 'pltSctrRegrLineStyle',
                                               label = 'Regression Line Style',
                                               choices = lineStyleNames),
                                   selectInput(inputId = 'pltSctrRegrAddStats',
                                               label = 'Add R^2 to Plot',
                                               choices = c('<NONE>',
                                                           'Top-left',
                                                           'Top-right',
                                                           'Bottom-left',
                                                           'Bottom-right'))
                               )
                        ),
                        column(6,
                               box(width = 12, status = 'primary',
                                 fluidRow(
                                    column(4, 
                                       checkboxInput(inputId = 'pltSctrIdLine',
                                                     label = 'Identity Line')
                                    ),
                                    column(4,
                                       checkboxInput(inputId = 'pltSctrSquare',
                                                     label = 'Make axes equivalent')
                                    )
                                 ),
                                 sliderInput(inputId = 'pltSctrIdLineWidth',
                                             label = 'Identity Line Width',
                                             min = 0.1, max = 1.5,
                                             step = 0.1, value = 1),
                                 selectInput(inputId = 'pltSctrIdLineCol',
                                             label = 'Identity Line Color',
                                             choices = colorNames),
                                 selectInput(inputId = 'pltSctrIdLineStyle',
                                             label = 'Identity Line Style',
                                             choices = lineStyleNames),
                               )
                        )
                       )
                    ),
                    tabPanel('Title and Size',
                     fluidRow(
                       column(8,
                              box(width = 12, status = 'primary',
                                  textInput(inputId = 'pltSctrTitle',
                                            label = 'Title'),
                                  textInput(inputId = 'pltSctrSubtitle',
                                            label = 'Subtitle')
                              )
                       ),
                       column(4, 
                             box(width = 12, status = 'primary',
                                 sliderInput(inputId = 'pltSctrHeight',
                                             label = 'Plot Height',
                                             min = 200, max = 1000, 
                                             step = 50, value = 400),
                                 br(),
                                 sliderInput(inputId = 'pltSctrWidth',
                                             label = 'Plot Width',
                                             min = 200, max = 1000, 
                                             ste = 50, value = 400)
                             )
                       )
                     )
                    )
                    )
                 )
         ),
         tabItem(tabName = 'statTests',
                fluidRow(
                   column(3,
                          box(width = 12, status = 'primary',
                              title = 'Plot Settings', solidHeader = TRUE,
                              fluidRow(column(12,
                                 selectInput(inputId = 'stat2GrpResp',
                                             label = 'Two-group Response',
                                             choices = '<NONE>'),
                                 progressBar(id = 'statProgbar', value = 0,
                                             display_pct = TRUE),
                                 radioButtons(inputId = 'stat2GrpTest',
                                              label = 'Test Type',
                                              choices = c('t-test',
                                                          'U-test',
                                                          'KS-test'))
                              )),
                              fluidRow(column(12,
                                 downloadButton('downloadResults', 'Download')
                              ))
                          ),
                          fluidRow(
                           column(12,
                             DT::dataTableOutput(outputId = 'stat2GrpAnnoTable')
                           )
                          )
                   ),
                   column(9,
                          fluidRow(
                             column(6,
                                box(width = 12, status = 'primary',
                                    fluidRow(
                                       column(12,
                                          plotlyOutput(outputId = 'stat2GrpVolcano')
                                       )
                                    ),
                                    fluidRow(
                                       column(6,
                                              sliderInput(inputId = 'stat2GrpFold',
                                                          label = 'Fold Change (log2)',
                                                          min = 0, max = 10,
                                                          step = 0.1, value = 2)
                                       ),
                                       column(6,
                                              sliderInput(inputId = 'stat2GrpPvalue',
                                                          label = 'p-value',
                                                          min = 0, max = 0.1, 
                                                          step = .01, value = 0.05),
                                              radioButtons(inputId = 'stat2GrpCorrection',
                                                           choices = c('p-value', 'FDR', 'Bonferroni'),
                                                           label = '', inline = TRUE)
                                       )
                                    )
                                )
                             ),
                             column(6,
                                box(width = 12, status = 'primary',
                                    fluidRow(
                                       column(12,
                                          plotlyOutput(outputId = 'stat2GrpDistPlot')
                                       )
                                    ),
                                    fluidRow(
                                       column(4,
                                          radioButtons(inputId = 'stat2GrpBoxCDF',
                                                       choices = c('Boxplot','CDF'),
                                                       label = 'Plot Options',
                                                       inline = TRUE)
                                       ),
                                       column(4,
                                          br(),
                                          checkboxInput(inputId = 'stat2GrpPlotLog10',
                                                        label = 'Log10')
                                       ),
                                       column(4, 
                                        conditionalPanel(condition = 
                                          'input.stat2GrpBoxCDF == "Boxplot"',
                                          br(),
                                          checkboxInput(inputId  = 'stat2GrpPlotBeeswarm',
                                                        label = 'Beeswarm')
                                        )
                                       )
                                    ),
                                    fluidRow(
                                     column(12,
                                       radioButtons(inputId = 'stat2GrpDataLabel',
                                                    choices = c('SOMAmer',
                                                                'Protein Name', 
                                                                'Gene Symbol'),
                                                    label = 'Data Label',
                                                    inline = TRUE)
                                     )
                                    )
                                )
                             )
                          ),
                          fluidRow(
                             DT::dataTableOutput(outputId = 'stat2GrpResTable')
                          )
                   )
                )
         )
      )
   )
)