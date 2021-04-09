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

# enlarge the allowable import file size max
options(shiny.maxRequestSize = 100 * 1024 ^ 2)

function(input, output, session) {

   ########################################
   # License
   ########################################
   
   output$license <- renderUI(HTML(paste(
      a(img(src = 'SomaLogic_fullLogo.png', 
            width = '600px', height = '400px',
            style = 'display:block; margin-left: auto; margin-right: auto;'
           ),
           href = 'https://www.somalogic.com'
      ),
      h4('Use of ProViz constitutes acceptance of the following license terms:'),
      br(),
      helpText('MIT License'),
      br(),
      helpText('Copyright © 2021 SomaLogic, Inc.'),
      br(),
      helpText(paste('Permission is hereby granted, free of charge, to any person obtaining a copy',
         'of the ProViz software and associated documentation files (the "Software"),',
         'to deal in the Software without restriction, including without limitation the', 
         'rights to use, copy, modify, merge, publish, distribute, sublicense, and/or',
         'sell copies of the Software, and to permit persons to whom the Software is',
         'furnished to do so, subject to the following conditions outlined below.', 
         'Further,  ProViz and SomaLogic are trademarks owned by SomaLogic, Inc. No', 
         'license is hereby granted to these trademarks other than for purposes of', 
         'identifying the origin or source of the Software.')),
      br(),
      helpText(paste('The above copyright notice and this permission notice shall be included in all',
         'copies or substantial portions of the Software.')),
      br(),
      helpText(paste('THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR',
         'IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,', 
         'FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE', 
         'AUTHORS OR COPYRIGHT HOLDER(S) BE LIABLE FOR ANY CLAIM, DAMAGES, WHETHER', 
         'DIRECT OR  INDIRECT, OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,', 
         'TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR', 
         'THE USE OR OTHER DEALINGS IN THE SOFTWARE.'))
   )))
   
   ########################################
   # SL logo and link in left panel
   ########################################
   
   output$SLlogo <- renderUI(HTML(paste(
      br(), br(), br(), br(),
      a(img(src = 'SomaLogic_icon.png', 
            width = 75, height = 75,
            style = 'display:block; margin-left: auto; margin-right: auto;'),
         href = 'https://www.somalogic.com'
      ),
      HTML('<center>'),
      a(strong('www.somalogic.com'),
        href = 'https://www.somalogic.com'
      ),
      HTML('</center>')
   )))
      
   ########################################
   # SL logo and link in left panel
   ########################################
   
   output$docs <- renderUI(HTML(paste(
      br(), br(),
      HTML('<center>'),
      a(strong("ProViz User's Guide"), 
        href = 'https://somalogic.github.io/ProViz/'),
      HTML('</center>')
   )))
   
   ########################################
   # utility function to keep UI up to date
   ########################################
   
   UpdateUI <- function() {
      if(is.null(rv$adat)){
         return(NULL)
      }
      
      varName <- lookupID(input$fltrCatSelect, 'SOMAmer ID')
      updateSelectInput(session, 'fltrCatCrit',
                        selected = '',
                        choices = sort(unique(rv$adat[[varName]]))
      )

      varName <- lookupID(input$fltrConSelect, 'SOMAmer ID')
      rng <- range(rv$adat[[varName]], na.rm = TRUE)
      rng <- c(floor(rng[1]), ceiling(rng[2]))
      if(length(unique(rv$adat[[varName]]) == 1)){
         st <- 0
      } else {
         st <- 1
      }
      updateSliderInput(session, 'fltrConCrit',
                        min = rng[1], max = rng[2],
                        value = rng, step = st)

      updateCheckboxInput(session, 'fltrConExcludeNA',
                          value = FALSE)
      
      # get continuous and categorical variables
      con_i <- which(sapply(rv$adat, function(column) {
         return(is.numeric(column))# &
                # length(unique(na.omit(column))) > 10)
      }))
      cat_i <- which(sapply(rv$adat, function(column) {
                               column <- na.omit(column)
                               (!is.numeric(column) & length(column) > 0) |
                               (is.numeric(column) & length(unique(column)) <= 10)
                            })
      )

      # store these for other UI elements to use
      rv$conColumns <- sapply(colnames(rv$adat)[con_i], function(cn) {
         lookupID(cn, input$rdoIDChoice)
      })
      names(rv$conColumns) <- NULL
      
      rv$catColumns <- sapply(colnames(rv$adat)[cat_i], function(cn) {
         lookupID(cn, input$rdoIDChoice)
      })
      names(rv$catColumns) <- NULL
      
      # UI update functions
      z <- sapply(c('pltBxYaxis', 'pltCDFXaxis', 'pltSctrXaxis',
                    'pltSctrYaxis', 'fltrConSelect'),
                 function(ctrlId) {
                    updateSelectInput(session, ctrlId,
                                      choices = rv$conColumns) 
                 })

      z <- sapply(c('pltBxXaxis', 'fltrCatSelect'),
                  function(ctrlId) {
                    updateSelectInput(session, ctrlId,
                                      choices = rv$catColumns)
                 })

      # update categorical select boxes that need the <NONE> option
      updateSelectInput(session, 'pltCDFColorBy',
                        choices = c('<NONE>',
                                    rv$catColumns)
      )

      # update stat tests panel select boxes with only meta data columns
      metaColumns <- SomaDataIO::getMeta(rv$adat)
      
      # get variables with only two groups
      twoGrp <- which(sapply(metaColumns, function(c){
         length(unique(na.omit(rv$adat[[c]]))) == 2
      }))
      twoGrpCols <- metaColumns[twoGrp]
      updateSelectInput(session, 'stat2GrpResp',
                        choices = c('<NONE>', twoGrpCols))

      # update correlation variable select box
      metaConColumns <- metaColumns[which(metaColumns %in% rv$conColumns)]
      updateSelectInput(session, 'statCorrResp',
                        choices = c('<NONE>', metaConColumns)
      )
      
      # get variables with more than two but less then or equal to 10 groups
      multiGrp <- which(sapply(metaColumns, function(c){
         length(unique(na.omit(rv$adat[[c]]))) > 2 &
         length(unique(na.omit(rv$adat[[c]]))) <= 10 
      }))
      multiGrpCols <- metaColumns[multiGrp]
      updateSelectInput(session, 'statMultiResp',
                        choices = c('<NONE>', multiGrpCols))
      
      #update the scatter plot color by variable selection
      if(input$pltSctrColorBy == 'Continuous'){
         updateSelectInput(session, 'pltSctrColorByVar',
                           choices = c('<NONE>', rv$conColumns))
      } else {
         updateSelectInput(session, 'pltSctrColorByVar',
                           choices = c('<NONE>', rv$catColumns))
      }

      # update the group creation tab
      if(input$grpCreateFrom == 'Continuous' ){
         updateSelectInput(session, 'grpSourceColName',
                           choices = rv$conColumns, 
                           selected = rv$grpSelectedCol)
         if(!is.null(rv$grpSplitSetting)){
            updateSliderInput(session, 'grpConSplit',
                              value = rv$grpSplitSetting)
         }
      } else {
         updateSelectInput(session, 'grpSourceColName',
                           choices = rv$catColumns,
                           selected = rv$grpSelectedCol)
         updateSelectInput(session, 'grpCatGrpA',
                           selected = rv$grpCatGrpA)
         updateSelectInput(session, 'grpCatGrpB',
                           selected =rv$grpCatGrpB)
      }

      MonitorCatGrpSelections()
      
      # find possible merge columns for adat
      meta_cols <- SomaDataIO::getMeta(rv$adat)
      updateSelectInput(session, 'mergeADATCol',
                        choices = rv$metaColumns)
      
      # update the adat dimensions for use by multiple functions
      rv$loadMessage = helpText(pre(
         'Data Dimensions:',
         sprintf('    Rows .............. %i', 
                 nrow(rv$adat)),
         sprintf('    Columns ........... %i',
                 ncol(rv$adat)),
         sprintf('       Meta Data ...... %i',
                 length(rv$metaColumns)),
         sprintf('       SOMAmer Data ... %i',
                 nrow(rv$featureData))
         ))
   }
   
   PreservePlotStates <- function() {
      
      # capture state of plot selections that will get changes when 
      # changes are made to the ID selection
      if(is.null(rv$adat)){
         return(NULL)
      }
      
      # boxplot
      rv$pltBxXaxis = input$pltBxXaxis
      rv$pltBxYaxis = input$pltBxYaxis
      
      # cdf
      rv$pltCDFXaxis = input$pltCDFXaxis
      rv$pltCDFColorBy = input$pltCDFColorBy
      
      # scatter plot
      rv$pltSctrXaxis <- input$pltSctrXaxis
      rv$pltSctrYaxis <- input$pltSctrYaxis
      rv$pltSctrColorByVar <- input$pltSctrColorByVar
      
      # stats
      rv$statCorrResp <- input$statCorrResp
      rv$stat2GrpResp  <- input$stat2GrpResp
      rv$statMultiResp <- input$statMultiResp
   }
   
   RevertPlotStates <- function() {
      
      # complement to PreservePlotStates
      # reset plot selections that would have changed with 
      # changes to ID selection
      if(is.null(rv$adat)){
         return(NULL)
      }
      
      # boxplot
      updateSelectInput(session, 'pltBxXaxis',
                        selected = lookupID(rv$pltBxXaxis, input$rdoIDChoice))
      updateSelectInput(session, 'pltBxYaxis',
                        selected = lookupID(rv$pltBxYaxis, input$rdoIDChoice))
      
      # cdf
      updateSelectInput(session, 'pltCDFXaxis',
                        selected = lookupID(rv$pltCDFXaxis, input$rdoIDChoice))
      updateSelectInput(session, 'pltCDFColorBy', selected = rv$pltCDFColorBy)
      
      # scatter plot
      updateSelectInput(session, 'pltSctrXaxis',
                        selected = lookupID(rv$pltSctrXaxis, input$rdoIDChoice))
      updateSelectInput(session, 'pltSctrYaxis',
                        selected = lookupID(rv$pltSctrYaxis, input$rdoIDChoice))
      updateSelectInput(session, 'pltSctrColorByVar',
                        selected = lookupID(rv$pltSctrColorByVar,
                                             input$rdoIDChoice))
      
      # stats
      updateSelectInput(session, 'statCorrResp',
                        selected = rv$statCorrResp)
      updateSelectInput(session, 'stat2GrpResp',
                        selected = rv$stat2GrpResp)
      updateSelectInput(session, 'statMultiResp',
                        selected = rv$statMultiResp)
   }
   
   # change in choice of ID cascades to UpdateUI
   observeEvent(input$rdoIDChoice, {
      PreservePlotStates()
      UpdateUI()
      RevertPlotStates()
   })
   
   # always update if the data changes
   observeEvent(rv$adat, {
      UpdateUI()
   })
   
   ########################################
   # common handler for downloading 
   # a modified ADAT
   ########################################
   
   dlHandler <- downloadHandler(
      filename = 'ProViz_modified.adat',
      content = function(file) {
            SomaDataIO::write_adat(x = rv$adat, file = file)
      }
   )
   
   ########################################
   # Handlers for loading an ADAT
   ########################################
   
   # code to handle opening the ADAT, parsing the contents as needed
   observeEvent(input$adat_file, {
                if(is.null(input$adat_file)) {
                   rv$adat <- NULL
                   rv$adatOrig <- NULL
                   rv$featureData <- NULL
                   rv$metaColumns <- NULL
                   rv$idLookup <- NULL
                   rv$loadMessage <- 'No ADAT open'
                } else {
                   # update progress
                   updateProgressBar(session = session, id = 'loadProgbar',
                                     value = 15)
                   
                   adat <- try(SomaDataIO::read_adat(input$adat_file$datapath))
                   
                   if(inherits(adat, 'try-error')){
                      rv$adat <- NULL
                      rv$adatOrig <- NULL
                      rv$featureData <- NULL
                      rv$metaColumns <- NULL
                      rv$idLookup <- NULL
                      rv$loadMessage <- helpText('Error loading ADAT: ', 
                                                 br(), adat[1])
                   } else {
                      # update progress
                      updateProgressBar(session = session, id = 'loadProgbar',
                                        value = 50)
                      
                      rv$adat <- adat
                      rv$adatOrig <- rv$adat
                      rv$featureData <- SomaDataIO::getFeatureData(adat)
                      rv$metaColumns <- SomaDataIO::getMeta(adat)
                      
                      # update progress
                      updateProgressBar(session = session, id = 'loadProgbar',
                                        value = 75)
                      
                      # construct id lookup table 
                      createIdLookup()
                      
                      # update progress
                      updateProgressBar(session = session, id = 'loadProgbar',
                                        value = 100)
                     }
                }
   })

   output$fltrDownloadADAT <- dlHandler 
   
   output$loadPreviewText <- renderUI(
      rv$loadMessage
   )

   output$loadPreviewTable <- DT::renderDataTable(
      server = TRUE, rownames = FALSE,
      options = list(scrollX = TRUE,
                     deferRender = TRUE), {
         if(is.null(input$adat_file)){
            return(NULL)
         } else {
           # limit ADAT to only row metadata columns 
           # table is too slow to deal with all SOMAmer columns
           i <- which(colnames(rv$adat) %in% rv$metaColumns)
           rv$adat[, i]
         }
      }
   )

   ####################################
   # Handlers for filtering 
   ####################################
   
   observeEvent(input$fltrReset, {
      rv$adat <- rv$adatOrig
      UpdateUI()
   })

   observeEvent(input$fltrCatSelect, {
      varName <- lookupID(input$fltrCatSelect, 'SOMAmer ID')
      updateSelectInput(session, 'fltrCatCrit',
                        choices = sort(unique(rv$adat[[varName]]))
      )
   })

   observeEvent(input$fltrCatApply, {
      i <- integer()
      varName <- lookupID(input$fltrCatSelect, 'SOMAmer ID')
      
      if(!is.null(input$fltrCatCrit) ){
         i <- which(rv$adat[[varName]] %in% input$fltrCatCrit )
      }

      if(input$fltrCatExcludeNA ){
         i <- union(i, which(rv$adat[[varName]] == '') )
      }

      if(length(i) > 0 ){
         rv$adat <- rv$adat[-i,]
         UpdateUI()
      }
   })

   observeEvent(input$fltrConSelect, {
      varName <- lookupID(input$fltrConSelect, 'SOMAmer ID')
      
      rng <- range(rv$adat[[varName]], na.rm = TRUE)
      rng <- c(floor(rng[1]), ceiling(rng[2]))
      if(length(unique(rv$adat[[varName]]) == 1)){
         st <- 0
     } else {
         st <- 1
      }
      updateSliderInput(session, 'fltrConCrit',
                        min = rng[1], max = rng[2],
                        value = rng, step = st)
   })

   observeEvent(input$fltrConApply, {
      varName <- lookupID(input$fltrConSelect, 'SOMAmer ID')
      i <- which(rv$adat[[varName]] < input$fltrConCrit[1] |
                 rv$adat[[varName]] > input$fltrConCrit[2])

      if(input$fltrConExcludeNA ){
         i <- union(i, which(is.na(rv$adat[[varName]])) )
      }

      if(length(i) > 0){
         rv$adat <- rv$adat[-i,]
         UpdateUI()
      }
   })
   
   ########################################
   # Handlers for merge tab
   ########################################
   
   observeEvent(rv$mergedData, {
      if(is.null(rv$mergedData)){
         updateSelectInput(session, 'mergeDataCol', 
                           choices = '<BLAH>')
      }
      
      updateSelectInput(session, 'mergeDataCol',
                        choices = colnames(rv$mergedData))
   })
   
   # messages for the merge screen
   output$mergeMessage <- renderUI(
      if(is.null(rv$mergeMessage)){
         rv$mergeMessageDefault
      } else {
         rv$mergeMessage
      }
   )
   
   observeEvent(input$merge_file, {
      if(is.null(input$merge_file)) {
         rv$mergeData <- NULL
      } else {
         fn <- input$merge_file$name
         if(endsWith(fn, '.csv')){
            data <- try(readr::read_csv(input$merge_file$datapath))
         } else if(endsWith(fn, '.txt')) {
            data <- try(readr::read_delim(input$merge_file$datapath, 
                                          delim = '\t'))
         } else {
            rv$loadMessage <- rv$loadMessageDefault
            rv_mergedData <- NULL
         }
         
         if(inherits(data, 'try-error')){
            rv$mergedData <- NULL
            rv$loadMessage <- helpText('Error loading data: ', 
                                       br(), data[1])
         } else {
            rv$mergedData <- data 
         }
      }
   })
   
   output$mergeADATPreview <- DT::renderDataTable(
      server = TRUE,
      options = list(scrollX = TRUE, pageLength = 5,
                     deferRender = TRUE), {
         if(is.null(rv$adat)){
            return(NULL)
         } else {
           # limit ADAT to only row metadata columns 
           # table is too slow to deal with all SOMAmer columns
           rv$adat[, rv$metaColumns]
         }
      }
   )

   output$mergeDataPreview <- DT::renderDataTable(
      server = TRUE,
      options = list(scrollX = TRUE, pageLength = 5), {
         if(is.null(rv$mergedData)){
            return(NULL)
         } else {
            rv$mergedData
         }
      }
   )
   
   observeEvent(input$mergeApply, {
      if(is.null(rv$adat) | is.null(rv$mergedData) |
         input$mergeADATCol == '' | input$mergeDataCol == ''){
         return(NULL)
      }
      
      by_vec = c(input$mergeDataCol)
      names(by_vec) = input$mergeADATCol
      
      if(input$mergeType == 'Keep All ADAT Rows'){
         merged_adat <- try(dplyr::left_join(rv$adat, rv$mergedData, 
                                             by = by_vec,
                                             suffix = c('_orig', '_merged')))
         
         if(inherits(merged_adat, 'try-error')){
            rv$mergeMessage <- helpText('Error merging data: ', 
                                         br(), merged_adat[1])
         } else {
            rv$adat <- merged_adat
            rv$mergeMessage <- rv$mergeMessageDefault
            rv$metaColumns <- SomaDataIO::getMeta(rv$adat)
         }
      }
      
      if(input$mergeType == 'Keep Only Intersection'){
         merged_adat <- try(dplyr::inner_join(rv$adat, rv$mergedData, 
                                              by = by_vec,
                                              suffix = c('_orig', '_merged')))
         
         if(inherits(merged_adat, 'try-error')){
            rv$mergeMessage <- helpText('Error merging data: ', 
                                         br(), merged_adat[1])
         } else {
            rv$adat <- merged_adat
            rv$mergeMessage <- rv$mergeMessageDefault
            rv$metaColumns <- SomaDataIO::getMeta(rv$adat)
         }
      }
   })
   
   output$mergeDownloadADAT <- dlHandler
   
   output$mergeADATstats <- renderUI(
      rv$loadMessage
   )
   
   ####################################
   # Handlers for grouping 
   ####################################
   
   observeEvent(input$grpCreateFrom, {
      if(input$grpCreateFrom == 'Continuous'){
         updateSelectInput(session, 'grpSourceColName',
                           choices = rv$conColumns)
     } else {
         updateSelectInput(session, 'grpSourceColName',
                           choices = rv$catColumns)
      }
   })

   observeEvent(input$grpSourceColName, {
      if(input$grpSourceColName == '<NONE>'){
         return(NULL)
      }
      varName <- lookupID(input$grpSourceColName, 'SOMAmer ID')
      
      if(input$grpCreateFrom == 'Continuous'){
         rng <- range(rv$adat[[varName]], na.rm = TRUE)
         if( input$grpLog10 ){
            rng <- round(log10(rng),2)
        } else {
            rng <- c(floor(rng[1]), ceiling(rng[2]))
         }
         if(length(unique(rv$adat[[varName]])) == 1){
            st <- 0
        } else {
            if( input$grpLog10 ){
               st <- 0.1
           } else {
               st <- 1
            }
         }
         updateSliderInput(session, 'grpConSplit',
                           min = rng[1], max=rng[2],
                           value = (mean(rng)), step = st)
     } else {
         vals <- unique(rv$adat[[varName]])
         updateSelectInput(session, 'grpCatGrpA', choices = vals)
         updateSelectInput(session, 'grpCatGrpB', choices = vals)
      }
   })

   observeEvent(input$grpCatGrpA, {
      MonitorCatGrpSelections()
   })

   observeEvent(input$grpCatGrpB, {
      MonitorCatGrpSelections()
   })

   MonitorCatGrpSelections = function() {
      varName <- lookupID(input$grpSourceColName, 'SOMAmer ID')
      vals <- unique(rv$adat[[varName]])
      valsA <- which(vals %in% input$grpCatGrpA)
      valsB <- which( vals %in% input$grpCatGrpB)

      msgs <- character()

      if(length(intersect(valsA, valsB)) != 0) {
         msgs <- append(msgs,
            'Group selections overlap.\nGroup B selection will overwrite common Group A selection.')
      }

      if(length(vals) > length(c(valsA, valsB))){
         msgs <- append(msgs,
            'Not all groups are represented in selections. Missing selections will recieve NA.')
      }

      if(length(valsA) == 0){
         msgs <- append(msgs, 'Group A has no content.')
      }

      if(length(valsB) == 0){
         msgs <- append(msgs, 'Group B has no content.')
      }

      rv$grpWarning <- paste(msgs, collapse = '\n')
   }

   output$grpWarning <- renderPrint(cat(rv$grpWarning))

   observeEvent(input$grpLog10, {
      varName <- lookupID(input$grpSourceColName, 'SOMAmer ID')
      rng <- range(rv$adat[[varName]], na.rm = TRUE)
      if(input$grpLog10){
         rng <- round(log10(rng),2)
     } else {
         rng <- c(floor(rng[1]), ceiling(rng[2]))
      }
      if( length(unique(rv$adat[[varName]])) == 1){
         st <- 0
     } else {
         if(input$grpLog10){
            st <- 0.1
        } else {
            st <- 1
         }
      }
      updateSliderInput(session, 'grpConSplit',
                        min = rng[1], max = rng[2],
                        value = (mean(rng)), step = st)
   })

   observeEvent(input$grpNewColName, {
      if(input$grpNewColName == ''){
         updateTextInput(session, 'grpNewColName', value = 'New Group')
      }
   })

   observeEvent(input$grpGrpALabel, {
      if(input$grpGrpALabel == ''){
         updateTextInput( session, 'grpGrpALabel', value = 'A')
      }
   })

   observeEvent(input$grpGrpBLabel, {
      if(input$grpGrpBLabel == ''){
         updateTextInput(session, 'grpGrpBLabel', value = 'B')
      }
   })

   output$grpPreviewTable = DT::renderDataTable(
      server = TRUE, 
      options = list(deferRender = TRUE), {
      if(input$grpSourceColName == '<NONE>'){
         rv$grpNewGroupData <- NULL
         return(NULL)
      }
      varName <- lookupID(input$grpSourceColName, 'SOMAmer ID')
      
      if(input$grpCreateFrom == 'Continuous'){
         df <- data.frame(rv$adat[[varName]],
                          rep(input$grpGrpALabel, length(rv$adat[[varName]]))
         )
         colnames(df) <- c(input$grpSourceColName, input$grpNewColName)
         if(input$grpLog10){
            df[,1] <- log10(df[,1])
         }
         
         labels <- rep(input$grpGrpALabel, length(rv$adat[[varName]]))
         i <- which(df[, 1] >= input$grpConSplit)
         labels[i] <- input$grpGrpBLabel
         labels[which(is.na(df[,1]))] <- NA
         df[, 2] <- labels
     } else {
         df <- data.frame(rv$adat[[varName]],
                          rep(NA, length(rv$adat[[varName]]))
         ) 
         colnames(df) <- c(input$grpSourceColName, input$grpNewColName)

         grpA_i <- which(rv$adat[[varName]] %in% input$grpCatGrpA)
         if( length(grpA_i) > 0){
            df[grpA_i, 2] <- input$grpGrpALabel
         }

         grpB_i <- which(rv$adat[[varName]] %in% input$grpCatGrpB)
         if(length(grpB_i) > 0){
            df[grpB_i, 2] = input$grpGrpBLabel
         }
      }
      rv$grpNewGroupData <- df[,2]
      df
   })

   observeEvent(input$grpApply, {
      if(is.null(rv$grpNewGroupData) ){
         return(NULL)
      }
      
      # preserve selections
      rv$grpSelectedCol <- input$grpSourceColName
      if(input$grpCreateFrom == 'Continuous'){
         rv$grpSplitSetting <- input$grpConSplit
      } else {
         rv$grpCatGrpA <- input$grpCatGrpA
         rv$grpCatGrpB <- input$grpCatGrpB
      }
      
      rv$metaColumns <- c(rv$metaColumns, input$grpNewColName)
      rv$adat[[input$grpNewColName]] <- rv$grpNewGroupData
   })

   output$grpDownloadADAT <- dlHandler
   
   output$grpADATstats <- renderUI(
      rv$loadMessage
   )
   
   ####################################
   # Handlers for Boxplot 
   ####################################
   
   output$pltBxUI <- renderUI({
      plotlyOutput('boxPlot',
                   height = input$pltBxHeight,
                   width = input$pltBxWidth)
   })

   output$boxPlot <- renderPlotly({
         if(is.null(rv$adat)){
            return(NULL)
         }

         df <- data.frame(SampleId = rv$adat$SampleId,
                          X = rv$adat[[lookupID(input$pltBxXaxis, 
                                                'SOMAmer ID')]])
         if(input$pltBxYaxisLog10){
            df$Y <- round(log10(as.numeric(rv$adat[[lookupID(input$pltBxYaxis, 
                                                             'SOMAmer ID')]])), 2)
         } else {
            df$Y <- as.numeric(rv$adat[[lookupID(input$pltBxYaxis, 
                                                 'SOMAmer ID')]])
         }
         
         # drop NAs
         if(input$pltBxRemoveNA){
            df <- na.omit(df)
            if(nrow(df) == 0){
               return(NULL)
            }
         }
         
         # generate plot
         plt <- ggplot(df, aes(x = X, y = Y, 
                               text = paste0('SampleId: ', SampleId, '\n',
                                             input$pltBxXaxis, ': ', X, '\n',
                                             paste0(ifelse(input$pltBxYaxisLog10, 'log10(', ''),
                                                    input$pltBxYaxis, 
                                                    ifelse(input$pltBxYaxisLog10, '): ', ': ')),
                                                Y
                                            ))) +  
            geom_boxplot(color = 'black', 
                         fill = colPalette[which(colorNames == input$pltBxColor)],
                         alpha = input$pltBxAlpha) +
            xlab(input$pltBxXaxisTitle) +
            ylab(input$pltBxYaxisTitle) +
            theme_minimal() +
            theme(plot.margin = margin(2, 1, 1, 1, 'lines'))
         
         if(input$pltBxBeeswarm){
            plt <- plt + 
               geom_beeswarm(shape = 21, color = 'black', 
                             size = input$pltBxBeeSize,
                             fill = colPalette[which(colorNames == input$pltBxBeecolor)],
                             alpha = input$pltBxBeeAlpha)
         }
         
         pltly <- ggplotly(plt, tooltip = 'text', 
                           height = input$pltBxHeight, 
                           width = input$pltBxWidth) %>% 
            layout(title = list(text = 
                                paste0(input$pltBxTitle, '<br>',
                                       '<sup>', input$pltBxSubtitle, '</sup>'))
                    ) 
         
         # take out outliers if beeswarm is selected
         pltly$x$data[[1]]$marker$opacity <- ifelse(input$pltBxBeeswarm, 0, 1)
         
         # add NA label if NAs present (plotly drops the label)
         if(any(is.na(df$X))){
            pltly$x$layout$xaxis$tickvals <- 
               c(pltly$x$layout$xaxis$tickvals,
                 length(pltly$x$layout$xaxis$tickvals) + 1)
            pltly$x$layout$xaxis$ticktext <- 
               c(pltly$x$layout$xaxis$ticktext, 'NA')
         }
         
         return(pltly)
      })

   ####################################
   # Handlers for CDF plots 
   ####################################
   
   output$pltCDFUI <- renderUI({
      plotlyOutput('cdfPlot',
                 height = input$pltCDFHeight,
                 width = input$pltCDFWidth)
   })

   output$cdfPlot <- renderPlotly({
      if(is.null(rv$adat)){
         return(NULL)
      }

      df <- data.frame(SampleId = rv$adat$SampleId,
                       X = rv$adat[[lookupID(input$pltCDFXaxis, 'SOMAmer ID')]])
      if(input$pltCDFLog10){
         df$X <- round(log10(as.numeric(rv$adat[[lookupID(input$pltCDFXaxis, 
                                                          'SOMAmer ID')]])), 2)
      } else {
         df$X <- as.numeric(rv$adat[[lookupID(input$pltCDFXaxis, 
                                              'SOMAmer ID')]])
      }

      if(input$pltCDFColorBy == '<NONE>'){
         df$grp <- ''
      } else {
         df$grp <- rv$adat[[lookupID(input$pltCDFColorBy, 'SOMAmer ID')]]
      }

      # drop NAs
      # cannot have NAs in the x-axis
      i <- which(is.na(df$X))
      if(length(i) > 0){
         df <- df[-i, ]
      }
      
      # option to remove NAs in color by column
      if(input$pltCDFRemoveNA){
         df <- na.omit(df)
      }
      
      if(nrow(df) == 0){
         return(NULL)
      }
      
      # convert to CDF data 
      ggdf <- ecdf_prep(df, data_col = 'X', group_col = 'grp',
                     label_col = 'SampleId')
      
      # generate the plot
      plt <- ggplot(ggdf, aes(x = X, y = Y, group = grp,
                    text = paste0('SampleId: ', SampleId, '\n',
                                  paste0(ifelse(input$pltCDFLog10, 
                                                'log10(', ''),
                                         input$pltCDFXaxis,
                                         ifelse(input$pltCDFLog10,
                                                '): ', ': ')), X, '\n',
                                  'P(X < x) = ', round(Y, 2), '\n',
                                  ifelse(input$pltCDFColorBy != '<NONE>', 
                                         paste0(input$pltCDFColorBy, ': '),
                                         ''), grp))) + 
         geom_step(aes(color = grp), 
                   size = input$pltCDFLineWidth) + 
         xlab(input$pltCDFXaxisTitle) +
         ylab('Cumulative Probability') +
         theme_minimal() +
         theme(plot.margin = margin(2, 1, 1, 1, 'lines'))
         
      # points on/off
      if(input$pltCDFPointSize != 0.0){
         plt <- plt + geom_point(aes(fill = grp), size = input$pltCDFPointSize,
                                 color = 'black', shape = 21)
      }
      
      plt <- plt +
         guides(color = guide_legend(title = input$pltCDFColorBy),
                fill = guide_legend(title = input$pltCDFColorBy))
         
      # legend adjustments
      if(input$pltCDFColorBy == '<NONE>'){
         plt <- plt + 
            theme(legend.position = 'none') 
      }
      
      pltly <- ggplotly(plt, tooltip = 'text',
               height = input$pltCDFHeight, 
               width = input$pltCDFWidth)  %>% 
      layout(title = list(text = paste0(input$pltCDFTitle, '<br>',
                                        '<sup>', input$pltCDFSubtitle, 
                                        '</sup>'))
      )
      
      # plotly specific legend adjustments
      unq_grps <- levels(factor(ggdf$grp))
      if(any(is.na(ggdf$grp))){
         unq_grps <- c(unq_grps, 'NA')
      }
      for(i in 1:length(unq_grps)){
         pltly$x$data[[i]]$legendgroup <- unq_grps[i]
         pltly$x$data[[i]]$name <- unq_grps[i]
      }
      
      return(pltly)
   })

   ####################################
   # Handlers for scatter plot 
   ####################################
   
   observeEvent(input$pltSctrColorBy, {
      if(is.null(rv$adat)){
         return(NULL)
      }

      # update the scatter plot color by variable selection
      if(input$pltSctrColorBy == 'Continuous'){
         updateSelectInput(session, 'pltSctrColorByVar',
                           choices = c('<NONE>', rv$conColumns))
     } else {
         updateSelectInput(session, 'pltSctrColorByVar',
                           choices = c('<NONE>', rv$catColumns))
      }
   })

   output$pltSctrUI <- renderUI({
      plotlyOutput('scatterPlot',
                 height = input$pltSctrHeight,
                 width = input$pltSctrWidth)
   })

   output$scatterPlot <- renderPlotly({
      if(is.null(rv$adat)){
         return(NULL)
      }

      df <- data.frame(SampleId = rv$adat$SampleId, 
                       X = as.numeric(rv$adat[[lookupID(input$pltSctrXaxis, 
                                                        'SOMAmer ID')]]),
                       Y = as.numeric(rv$adat[[lookupID(input$pltSctrYaxis,
                                                        'SOMAmer ID')]]))

      if(input$pltSctrXaxisLog10){
         df$X <- round(log10(df$X), 2)
      }
      if(input$pltSctrYaxisLog10){
         df$Y <- round(log10(df$Y), 2)
      }
      
      if(input$pltSctrColorBy == 'Static' |
         input$pltSctrColorByVar == '<NONE>'){
         df$grp <- ''
      } else if(input$pltSctrColorBy == 'Continuous') {
         df$grp <- as.numeric(rv$adat[[lookupID(input$pltSctrColorByVar, 
                                                'SOMAmer ID')]])
         if(input$pltSctrColorByLog){
            df$grp <- round(log10(df$grp), 2)
         }
      } else if(input$pltSctrColorBy == 'Category') {
         df$grp <- factor(rv$adat[[lookupID(input$pltSctrColorByVar, 
                                            'SOMAmer ID')]])
      }
      
      # drop NAs
      if(input$pltSctrRemoveNA){
         df <- na.omit(df)
         if(nrow(df) == 0){
            return(NULL)
         }
      }
      
      # make the actual plot
      plt <- ggplot(df, aes(x = X, y = Y,
              text = paste0('SampleId: ', SampleId, '\n',
                       paste0(ifelse(input$pltSctrXaxisLog10, 
                                     'log10(', ''),
                              input$pltSctrXaxis,
                              ifelse(input$pltSctrXaxisLog10,
                                     '): ', ': ')), X, '\n',
                       paste0(ifelse(input$pltSctrYaxisLog10,
                                     'log10(', ''),
                              input$pltSctrYaxis,
                              ifelse(input$pltSctrYaxisLog10,
                                     '): ', ': ')), Y, '\n',
                       ifelse(input$pltSctrColorByVar != '<NONE>', 
                              paste0(ifelse(input$pltSctrColorBy == 'Continuous' &
                                            input$pltSctrColorByLog,
                                            'log10(', ''),
                                     input$pltSctrColorByVar,
                                     ifelse(input$pltSctrColorBy ==  'Continuous' &
                                            input$pltSctrColorByLog,
                                            '): ', ': ')),
                              ''), grp))) 
      
      # color-by options
      if(input$pltSctrColorBy == 'Static') {
         plt <- plt +
            geom_point(shape = 19, alpha = input$pltSctrPtAlpha,
                       color = colPalette[which(colorNames == 
                                                input$pltSctrPtCol)], 
                       size = input$pltSctrPtSize)
      } else if(input$pltSctrColorBy == 'Continuous') {
         plt <- plt +
            geom_point(aes(color = grp), shape = 19, 
                       alpha = input$pltSctrPtAlpha,
                       size = input$pltSctrPtSize)
            if(!input$pltSctrColorByVar == '<NONE>') {
               plt <- plt +
                  scale_color_gradient(low = colPalette[which(colorNames == 
                                                       input$pltSctrColorByStart)],
                                high = colPalette[which(colorNames ==
                                                        input$pltSctrColorByEnd)],
                                na.value = 'grey')
            }
      } else if(input$pltSctrColorBy == 'Category') {
         plt <- plt +
            geom_point(aes(color = grp), shape = 19,
                       alpha = input$pltSctrPtAlpha,
                       size = input$pltSctrPtSize)
      }
      
      plt <- plt +
         xlab(input$pltSctrXaxisTitle) +
         ylab(input$pltSctrYaxisTitle) +
         theme_minimal() +
         theme(plot.margin = margin(2, 1, 1, 1, 'lines'))
      
      # regression options
      if(input$pltSctrRegrLine){
         reg <- lm(Y ~ X, df)
         plt <- plt +
            geom_abline(intercept = coef(reg)[1], slope = coef(reg)[2],
                        size = input$pltSctrRegrLineWidth,
                        color = colPalette[which(colorNames ==
                                                 input$pltSctrRegrLineCol)],
                        linetype = which(lineStyleNames == 
                                         input$pltSctrRegrLineStyle)) 
         if(input$pltSctrRegrAddStats != '<NONE>'){
            if(input$pltSctrRegrAddStats == 'Top-left' |
               input$pltSctrRegrAddStats == 'Bottom-left'){
               reg_x <- min(df$X)
               hj <- 'right' 
               #note ggplot's interpretation of hj is opposite to plotly's
            } else {
               reg_x <- max(df$X)
               hj <- 'left'
            }
            if(input$pltSctrRegrAddStats == 'Top-left' |
               input$pltSctrRegrAddStats == 'Top-right'){
               reg_y <- max(df$Y)
               vj <- 'bottom'
            } else {
               reg_y <- min(df$Y)
               vj <- 'top'
            }
            plt <- plt +
               geom_text(aes(x = reg_x, y = reg_y, 
                             label = paste0('r = ', 
                                            round(cor(X, Y, 
                                                      use = 'complete.obs'), 2)
                                            )),
                         hjust = hj, vjust = vj)
         }
      }
      
      # identity line options
      if(input$pltSctrIdLine){
         plt <- plt +
            geom_abline(intercept = 0, slope = 1,
                        size = input$pltSctrIdLineWidth,
                        color = colPalette[which(colorNames ==
                                                 input$pltSctrIdLineCol)],
                        linetype = which(lineStyleNames == 
                                         input$pltSctrIdLineStyle)) 
      }
      
      # fixed axes
      if(input$pltSctrSquare){
         plt <- plt +
            xlim(range(c(df$X, df$Y))) +
            ylim(range(c(df$X, df$Y)))
      }
      
      # legend adjustments
      if(input$pltSctrColorByVar == '<NONE>' |
         input$pltSctrColorBy == 'Static'){
         plt <- plt + 
            theme(legend.position = 'none') 
      } else if(input$pltSctrColorBy == 'Category') {
         plt <- plt +
            labs(color = input$pltSctrColorByVar)
      } else if(input$pltSctrColorBy == 'Continuous') {
         plt <- plt +
            labs(color = ifelse(input$pltSctrColorByLog,
                                paste0('log10(', input$pltSctrColorByVar, ')'),
                                input$pltSctrColorByVar)
            )
      }
      
      pltly <- ggplotly(plt, tooltip = 'text',
                        height = input$pltSctrHeight,
                        width = input$pltSctrWidth) %>%
         layout(title = list(text =
                             paste0(input$pltSctrTitle, '<br>',
                                    '<sup>', input$pltSctrSubtitle, '</sup>')))  
      
      if(input$pltSctrRegrLine &
         input$pltSctrRegrAddStats != '<NONE>'){
         pltly <- pltly %>% 
            style(textposition = hj)
            #note ggplot's interpretation of hj is opposite to plotly's
      }
      
      return(pltly)
   })
   
   ####################################
   # Handlers for Stats Tests tab
   ####################################
   
   # Utility functions
   
   getStatResultsTable <- function() {
      # return the appropriate statistical test results table
      if(input$statTests == 'Correlation'){
         resTable <- rv$statCorrTable
      } else if(input$statTests == 't-test' |
                input$statTests == 'U-test' |
                input$statTests == 'KS-test') {
         resTable <- rv$stat2GrpTable
      } else if(input$statTests == 'ANOVA' |
                input$statTests == 'Kruskal-Wallis') {
         resTable <- rv$statMultiTable
      }
      
      return(resTable)
   }
  
   
   # statistical test functions
   getMaxFoldChange <- function(df) {
      # find the maximum fold change between groups
      # for all pairs of groups
      # df is expected to have 2 columns - data, grps
      
      df <- df[complete.cases(df), ]
      grps <- unique(df$grps)
      max_fold_change <- 0.0
      for(g1_idx in 1:(length(grps)-1)) {
         for(g2_idx in (g1_idx + 1):length(grps)) {
            g1 <- which(df$grps == grps[g1_idx])
            g2 <- which(df$grps == grps[g2_idx])
            fold_change <- log2(abs(median(df$data[g1])) /
                                abs(median(df$data[g2])))
            if(abs(fold_change) > abs(max_fold_change)) {
               max_fold_change <- fold_change
            }
         }
      }
      
      return(max_fold_change)
   }
   
   statCorrTests <- function() {
      # perform correlation tests
      if(is.null(rv$adat) | input$statCorrResp == '<NONE>'){
         return(NULL)
      }
      
      # if the selection didn't change
      if(input$statCorrResp == rv$statCorrResp &
         input$statCorrMethod == rv$statCorrMethod){
         df <- rv$statCorrTable
         # user-friendly column names
         colnames(df)[1:3] = c('SOMAmer', 'Protein Name', 'Gene Symbol')
         return(df)
      }
     
      # prepare the results table 
      respID <- input$statCorrResp -> rv$statCorrResp
      rv$statCorrMethod <- input$statCorrMethod
      vars <- SomaDataIO::getFeatures(rv$adat)
      df <- data.frame(rv$featureData[, c('AptName', 
                                          'TargetFullName', 'EntrezGeneSymbol')])
      
      # get log10 SOMAmers
      adat <- log10(rv$adat)
   
      # perform the tests 
      tbl <- data.frame(t(sapply(vars, function(v){
         z <- suppressWarnings(cor.test(adat[[respID]], adat[[v]],
                        method = ifelse(input$statCorrMethod == 'Pearson',
                                        'pearson', 'spearman')))
         
         # increment the progress bar
         p <- which(vars == v)
         if((p / length(vars) * 100) %% 5 == 0){
            updateProgressBar(session = session, id = 'statProgbar',
                              value = p / length(vars) * 100)
         }
         
         if(input$statCorrMethod == 'Pearson'){
            c(t.statistic = round(as.numeric(z$statistic), 2),
              r = round(as.numeric(z$estimate), 2),
              p.value = z$p.value)
         } else {
            c(S.statistic = round(as.numeric(z$statistic), 2),
              rho = round(as.numeric(z$estimate), 2),
              p.value = z$p.value)
         }
      })))
      
      # provide multiple testing corrections
      tbl$FDR <- signif(p.adjust(tbl$p.value, method='BH'), 2)
      tbl$Bonferroni <- signif(p.adjust(tbl$p.value, method='bonferroni'), 2)
      df <- cbind(df, tbl)
      df$p.value <- signif(df$p.value, 2)
      
      # store the results table
      rv$statCorrTable <- df
      
      # user-friendly column names
      colnames(df)[1:3] = c('SOMAmer', 'Protein Name', 'Gene Symbol')
      rv$statCorrTable 
   }
   
   statMultiGrpTests <- function() {
      # perform multi-group tests
      if(is.null(rv$adat) | input$statMultiResp== '<NONE>'){
         return(NULL)
      }
      # if the selection didn't change
      if(input$statMultiResp == rv$statMultiResp &
         input$statTests == rv$statMultiTest) {
         df <- rv$statMultiTable
         # user-friendly column names
         colnames(df)[1:3] = c('SOMAmer', 'Protein Name', 'Gene Symbol')
         return(df)
      }
     
      # prepare the results table 
      respID <- lookupID(input$statMultiResp, 'SOMAmer ID') 
      rv$statMultiResp <- respID
      rv$statMultiTest <- input$statTests
      vars <- SomaDataIO::getFeatures(rv$adat)
      df <- data.frame(rv$featureData[, c('AptName', 
                                          'TargetFullName', 'EntrezGeneSymbol')])
      adat <- rv$adat
      
      # remove rows with NA for response 
      na_i <- which(is.na(adat[[respID]]))
      if(length(na_i) > 0) {
         adat <- adat[-na_i, ]
      }
      
      # calculate max-fold change between group medians
      df$Max.Fold.Change <- sapply(vars, function(v){
         round(getMaxFoldChange(data.frame(data = adat[[v]],
                                     grps = adat[[respID]])), 2)
      })
      updateProgressBar(session = session, id = 'statProgbar', value = 5)
      
      # log10 SOMAmers
      adat <- log10(adat)
  
      # perform the tests 
      if(input$statTests == 'ANOVA'){
         tbl <- data.frame(t(sapply(vars, function(v){
            z <- suppressWarnings(aov(as.formula(sprintf('%s ~ %s', v, respID)),
                                      adat))
            
            # increment the progress bar
            p <- which(vars == v)
            if((p / length(vars) * 100) %% 5 == 0){
               updateProgressBar(session = session, id = 'statProgbar',
                                 value = p / length(vars) * 100)
            }
            
            z_summary <- summary(z) 
            c(F = round(as.numeric(z_summary[[1]][1,4]),2),
              p.value = z_summary[[1]][1,5])
         })))
      } else if(input$statTests == 'Kruskal-Wallis'){
         tbl <- data.frame(t(sapply(vars, function(v){
            z <- suppressWarnings(kruskal.test(as.formula(sprintf('%s ~ %s', v, respID)),
                                               adat))
            
            # increment the progress bar
            p <- which(vars == v)
            if((p / length(vars) * 100) %% 5 == 0){
               updateProgressBar(session = session, id = 'statProgbar',
                                 value = p / length(vars) * 100)
            }
            
            c(chi.squared = round(as.numeric(z$statistic, 2)),
              p.value = z$p.value)
         })))
      }
      
      # provide multiple testing corrections
      tbl$FDR <- signif(p.adjust(tbl$p.value, method='BH'), 2)
      tbl$Bonferroni <- signif(p.adjust(tbl$p.value, method='bonferroni'), 2)
      df <- cbind(df, tbl)
      df$p.value <- signif(df$p.value, 2)
      
      # store the results table
      rv$statMultiTable <- df
      
      # user-friendly column names
      colnames(df)[1:3] = c('SOMAmer', 'Protein Name', 'Gene Symbol')
      rv$statMultiTable 
   }
   
   stat2GrpTests <- function() {
      # perform 2-group tests
      if(is.null(rv$adat) | input$stat2GrpResp == '<NONE>'){
         return(NULL)
      }
      
      # if the selection didn't change
      if(input$stat2GrpResp == rv$stat2GrpResp &
         input$statTests == rv$stat2GrpTest) {
         df <- rv$stat2GrpTable
         # user-friendly column names
         colnames(df)[1:3] = c('SOMAmer', 'Protein Name', 'Gene Symbol')
         return(df)
      }
     
      # prepare the results table 
      respID <- lookupID(input$stat2GrpResp, 'SOMAmer ID')
      rv$stat2GrpResp <- respID
      rv$stat2GrpTest <- input$statTests
      vars <- SomaDataIO::getFeatures(rv$adat)
      df <- data.frame(rv$featureData[, c('AptName', 
                                          'TargetFullName', 'EntrezGeneSymbol')])
      adat <- rv$adat
      
      # remove rows with NA for response 
      na_i <- which(is.na(adat[[respID]]))
      if(length(na_i) > 0) {
         adat <- adat[-na_i, ]
      }
      
      # find the groups
      grps <- unique(adat[[respID]])
      grp1_idx <- which(adat[[respID]] == grps[1])
      grp2_idx <- which(adat[[respID]] == grps[2])
      
      # calculate fold change between group medians
      df$Fold.Change <- sapply(vars, function(v){
         signif(log2(median(adat[grp1_idx, v]) / 
                     median(adat[grp2_idx, v])), 2)
      })
    
      # log10 SOMAmers
      adat <- log10(adat)
   
      # perform the tests 
      if(input$statTests == 'KS-test'){
         tbl <- data.frame(t(sapply(vars, function(v){
            z <- suppressWarnings(ks.test(adat[grp1_idx, v], 
                                          adat[grp2_idx,v]))
            j <- which(df$AptName == v)
            if(df$Fold.Change[j] < 0){
               z$signedKS <- -z$statistic
            } else {
               z$signedKS <- z$statistic
            }
            
            # increment the progress bar
            p <- which(vars == v)
            if((p / length(vars) * 100) %% 5 == 0){
               updateProgressBar(session = session, id = 'statProgbar',
                                 value = p / length(vars) * 100)
            }
            
            c(KS.Dist = round(as.numeric(z$statistic),2),
              Signed.KS.Dist = round(as.numeric(z$signedKS),2),
              p.value = z$p.value)
         })))
      } else if(input$statTests == 't-test'){
         tbl <- data.frame(t(sapply(vars, function(v){
            z <- suppressWarnings(t.test(adat[grp1_idx, v], 
                                         adat[grp2_idx, v], var.equal = FALSE))
            
            # increment the progress bar
            p <- which(vars == v)
            if((p / length(vars) * 100) %% 5 == 0){
               updateProgressBar(session = session, id = 'statProgbar',
                                 value = p / length(vars) * 100)
            }
            
            c(t.statistic = round(as.numeric(z$statistic),2),
              p.value = z$p.value)
         })))
      } else if(input$statTests == 'U-test'){
         tbl <- data.frame(t(sapply(vars, function(v){
            z <- suppressWarnings(wilcox.test(adat[grp1_idx, v], 
                                              adat[grp2_idx, v]))
            
            # increment the progress bar
            p <- which(vars == v)
            if((p / length(vars) * 100) %% 5 == 0){
               updateProgressBar(session = session, id = 'statProgbar',
                                 value = p / length(vars) * 100)
            }
            
            c(W.statistic = round(as.numeric(z$statistic), 2),
              p.value = z$p.value)
         })))
      }
      
      # provide multiple testing corrections
      tbl$FDR <- signif(p.adjust(tbl$p.value, method='BH'), 2)
      tbl$Bonferroni <- signif(p.adjust(tbl$p.value, method='bonferroni'), 2)
      df <- cbind(df, tbl)
      df$p.value <- signif(df$p.value, 2)
      
      # store the results table
      rv$stat2GrpTable <- df
      
      # user-friendly column names
      colnames(df)[1:3] = c('SOMAmer', 'Protein Name', 'Gene Symbol')
      rv$stat2GrpTable 
   }

   # stat test figures
   output$stat2GrpVolcano <- renderPlotly({
      if(is.null(rv$adat)) {
         return(NULL)
      }
      
      if(input$statTests == 'Correlation') {
         if(input$statCorrResp == '<NONE>') {
            return(NULL)
         }
         df <- rv$statCorrTable
         ifelse(input$statCorrMethod == 'Pearson',
                xAxis <- 'r',
                xAxis <- 'rho')
      } else if(input$statTests == 't-test' |
                input$statTests == 'U-test' |
                input$statTests == 'KS-test') {
         if(input$stat2GrpResp == '<NONE>') {
            return(NULL)
         }
         df <- rv$stat2GrpTable
         xAxis <- 'Fold.Change'
      } else if(input$statTests == 'ANOVA' |
                input$statTests == 'Kruskal-Wallis') {
         if(input$statMultiResp == '<NONE>') {
            return(NULL)
         }
         df <- rv$statMultiTable
         xAxis <- 'Max.Fold.Change'
      }
      
      # set the p-value column name
      if(input$stat2GrpCorrection == 'p-value') {
         pcolumn <- 'p.value'
      } else {
         pcolumn <- input$stat2GrpCorrection
      }
      
      # mark the significant points from the volcano plot criteria
      df$pt_cols <- '0' # non-significant indicator
     
      i <- which(df[[xAxis]] > input$stat2GrpFold |
                 df[[xAxis]] < (-input$stat2GrpFold))
      j <- which(df[[pcolumn]] < input$stat2GrpPvalue )
      k <- intersect(i,j)
      
      df$pt_cols[k] <- '1' # significant indicator
      
      # mark the selected points
      df$pt_cols[rv$statTableRowSelect] <- '2' # selected marker
      
      # make the actual plot
      plt <- ggplot(df, aes(x = !!sym(xAxis), y = -log10(!!sym(pcolumn)),
                            color = pt_cols, key = AptName, size = pt_cols,
                            text = paste0('SOMAmer: ', AptName, '\n',
                                          'Protein Name: ', TargetFullName, '\n',
                                          'Gene Symbol: ', EntrezGeneSymbol, '\n',
                                          if(input$statTests == 'Correlation') {
                                             if(input$statCorrMethod == 'Pearson') {
                                                paste0('Pearson r: ', r, '\n')
                                             } else {
                                                paste0('Spearman rho: ', rho, '\n')
                                             }
                                          } else if(input$statTests == 'ANOVA' |
                                                    input$statTests == 'Kruskal-Wallis') {
                                             paste0('Maximum Fold Change: ', Max.Fold.Change, '\n')
                                          } else {
                                             paste0('Fold Change: ', Fold.Change, '\n')
                                          },
                                          input$stat2GrpCorrection, ': ', 
                                               signif(!!sym(pcolumn), 3))
                                   )) +
         geom_point(shape = 19) +
         scale_color_manual(values = 
            c('#132B43', '#56B1F7', 'darkorange')[sort(as.numeric(unique(df$pt_cols))+1)]) +
         scale_size_manual(values =
            c(1.0, 1.0, 2.5)[sort(as.numeric(unique(df$pt_cols))+1)]) +
         xlab(if(input$statTests == 'Correlation') {
                 ifelse(input$statCorrMethod == 'Pearson',
                        'Pearson r', 'Spearman rho')
              } else if(input$statTests == 'ANOVA' |
                        input$statTests == 'Kruskal-Wallis') {
                 'Maximum Fold Change'
              } else {
                 'Fold Change (log2)'
              }) +
         ylab(paste0('-log10(', input$stat2GrpCorrection, ')')) +
         geom_vline(xintercept = c(input$stat2GrpFold, -input$stat2GrpFold),
                    size = 0.5, color = 'red') +
         geom_hline(yintercept = -log10(input$stat2GrpPvalue),
                    size = 0.5, color = 'red') +
         theme_minimal() +
         theme(legend.position = 'none',
               plot.margin = margin(1, 1, 1, 1, 'lines')) 
     
      pltly <- ggplotly(p = plt, source = 'stat2GrpVolcano', tooltip = 'text') %>% 
         event_register(event = 'plotly_click') %>% 
         event_register(event = 'plotly_doubleclick')
      
      return(pltly)
   })
   
   output$stat2GrpDistPlot <- renderPlotly({
      # generate boxplots or CDFs of grouped data
      # from the selected SOMAmer
      if(input$statTests == 'Correlation') {
         return(statScatterPlot())
      } else {
         return(statDistributionPlot())
      }
   })

   statScatterPlot <- function() {
      # generate scatter plot of the selected SOMAmer
      # from the Volcano plot and the endpoint
      resTable <- getStatResultsTable()
      
      if(!is.null(rv$statTableRowSelect)) {
         row_sel <- rv$statTableRowSelect
         var_name <- resTable$AptName[row_sel]
         if(input$stat2GrpDataLabel == 'SOMAmer') {
            lab_name <- var_name
         } else if(input$stat2GrpDataLabel == 'Protein Name') {
            lab_name <- 
               rv$featureData$TargetFullName[which(rv$featureData$AptName == 
                                                   var_name)]
         } else if(input$stat2GrpDataLabel == 'Gene Symbol') {
            lab_name <- 
               rv$featureData$EntrezGeneSymbol[which(rv$featureData$AptName == 
                                                        var_name)]
         }
      } else {
         return(NULL)
      }
 
      i <- which(colnames(rv$adat) == var_name)
      if(input$stat2GrpPlotLog10){
         df <- data.frame(SampleId = rv$adat$SampleId,
                          Y = log10(rv$adat[,i]))
      } else {
         df <- data.frame(SampleId = rv$adat$SampleId,
                          Y = rv$adat[,i])
      }
      df$X<- rv$adat[[input$statCorrResp]]
      df <- df[complete.cases(df), ]
      
      plt <- ggplot(df, aes(x = X, y = Y, 
                            text = paste0('SampleId: ', SampleId, '\n',
                                          input$statCorrResp, ': ', X, '\n',
                                          lab_name, ': ', round(Y, 2)
                            ))) +  
         geom_point(color = '#56B1F7') +
         xlab(input$statCorrResp) +
         ylab(paste0(ifelse(input$stat2GrpPlotLog10, 'log10(', ''), lab_name,
                     ifelse(input$stat2GrpPlotLog10, ')', ''))) 
         
         # regression options
         if(input$statSelPlotRegLine){
            reg <- lm(Y ~ X, df)
            plt <- plt +
               geom_abline(intercept = coef(reg)[1], slope = coef(reg)[2],
                           size = .75, color = 'black', linetype = 2)
            if(input$statSelPlotAddCorr != '<NONE>'){
               if(input$statSelPlotAddCorr == 'Top-left' |
                  input$statSelPlotAddCorr == 'Bottom-left'){
                  reg_x <- min(df$X)
                  hj <- 'right' 
                  #note ggplot's interpretation of hj is opposite to plotly's
               } else {
                  reg_x <- max(df$X)
                  hj <- 'left'
               }
               if(input$statSelPlotAddCorr == 'Top-left' |
                  input$statSelPlotAddCorr == 'Top-right'){
                  reg_y <- max(df$Y)
                  vj <- 'bottom'
               } else {
                  reg_y <- min(df$Y)
                  vj <- 'top'
               }
               plt <- plt +
                  geom_text(aes(x = reg_x, y = reg_y, 
                       label = paste0(ifelse(input$statCorrMethod == "Pearson", 
                                             'r = ', 'rho = '), 
                                      round(cor(X, Y, 
                                                method = ifelse(input$statCorrMethod == 'Pearson',
                                                                'pearson', 'spearman'),
                                                use = 'complete.obs'), 2)
                       )),
                       hjust = hj, vjust = vj)
            }
         }
      
         plt <- plt + theme_minimal() +
            theme(plot.margin = margin(1, 1, 1, 1, 'lines'))
      
      pltly <- ggplotly(plt, tooltip = 'text')
      
      if(input$statSelPlotRegLine &
         input$statSelPlotAddCorr != '<NONE>'){
         pltly <- pltly %>% 
            style(textposition = hj)
            #note ggplot's interpretation of hj is opposite to plotly's
      }
      
      return(pltly)
   }
   
   statDistributionPlot <- function() {
      # generate boxplot or cdf of the selected SOMAmer
      # from the Volcano plot
      resTable <- getStatResultsTable()
      
      if(!is.null(rv$statTableRowSelect)) {
         row_sel <- rv$statTableRowSelect
         var_name <- resTable$AptName[row_sel]
         if(input$stat2GrpDataLabel == 'SOMAmer') {
            lab_name <- var_name
         } else if(input$stat2GrpDataLabel == 'Protein Name') {
            lab_name <- 
               rv$featureData$TargetFullName[which(rv$featureData$AptName == 
                                                   var_name)]
         } else if(input$stat2GrpDataLabel == 'Gene Symbol') {
            lab_name <- 
               rv$featureData$EntrezGeneSymbol[which(rv$featureData$AptName == 
                                                        var_name)]
         }
      } else {
         return(NULL)
      }
 
      i <- which(colnames(rv$adat) == var_name)
      if(input$stat2GrpPlotLog10){
         df <- data.frame(SampleId = rv$adat$SampleId,
                          Y = log10(rv$adat[,i]))
      } else {
         df <- data.frame(SampleId = rv$adat$SampleId,
                          Y = rv$adat[,i])
      }
      
      if(input$statTests == 't-test' |
         input$statTests == 'U-test' |
         input$statTests == 'KS-test') {
            df$X <- rv$adat[[input$stat2GrpResp]]
      } else if (input$statTests == 'ANOVA' |
                 input$statTests == 'Kruskal-Wallis') {
            df$X <- rv$adat[[input$statMultiResp]]
      }
      df <- df[complete.cases(df), ]
      
      if(input$stat2GrpBoxCDF == 'Boxplot'){
         plt <- ggplot(df, aes(x = X, y = Y, 
                               text = paste0('SampleId: ', SampleId, '\n',
                                             if(input$statTests == 't-test' |
                                                input$statTests == 'U-test' |
                                                input$statTests == 'KS-test') {
                                                input$stat2GrpResp
                                             } else if(input$statTests == 'ANOVA' | 
                                                       input$statTests == 'Kruskal-Wallis') {
                                                input$statMultiResp
                                             }, ': ', X, '\n',
                                             ifelse(input$stat2GrpPlotLog10, 
                                                    'log10(', ''), lab_name,
                                             ifelse(input$stat2GrpPlotLog10, 
                                                    ')', ''), ': ', round(Y, 2)
                               ))) +  
            geom_boxplot(aes(fill = X), color = 'black') +
            xlab(if(input$statTests == 't-test' |
                    input$statTests == 'U-test' |
                    input$statTests == 'KS-test') {
                        input$stat2GrpResp
                 } else if(input$statTests == 'ANOVA' | 
                           input$statTests == 'Kruskal-Wallis') {
                    input$statMultiResp
                 }) +
            ylab(paste0(ifelse(input$stat2GrpPlotLog10, 'log10(', ''), lab_name,
                        ifelse(input$stat2GrpPlotLog10, ')', ''))) +
            theme_minimal() +
            theme(plot.margin = margin(1, 1, 1, 1, 'lines'))
            
            if(input$stat2GrpPlotBeeswarm){
               plt <- plt + 
                  geom_beeswarm(shape = 21, color = 'black', 
                                size = 1.0, fill = 'grey', alpha = 0.8)
            }
        
         # turn off legend
         # this has to be done independently to the ggplot build (above) ??
         plt <- plt +
            theme(legend.position = 'none') 
         
         pltly <- ggplotly(plt, tooltip = 'text')
         
         # take out outliers if beeswarm is selected
         pltly$x$data[[1]]$marker$opacity <- 
            ifelse(input$stat2GrpPlotBeeswarm, 0, 1)
      } else {
         # rename columns to fit with CDF
         colnames(df) <- c('SampleId', 'X', 'grp')
         
         # generate CDF data
         ggdf <- ecdf_prep(df, data_col =  'X', group_col = 'grp',
                           label_col = 'SampleId')
         
         plt <- ggplot(ggdf, aes(x = X, y = Y, group = grp,
                                 text = paste0('SampleId: ', SampleId, '\n',
                                               lab_name, ': ', round(X, 2), '\n', 
                                               input$stat2GrpResp, ': ', grp, '\n',
                                               'Cumulative prob: ', round(Y, 2)
                                 ))) +
            geom_step(aes(color = grp)) + 
            xlab(paste0(ifelse(input$stat2GrpPlotLog10, 'log10(', ''), lab_name,
                        ifelse(input$stat2GrpPlotLog10, ')', ''))) +
            ylab('Cumulative Probability') +
            theme_minimal() +
            theme(plot.margin = margin(2, 1, 1, 1, 'lines')) +
            guides(color = guide_legend(title = input$stat2GrpResp))
         
         pltly <- ggplotly(plt, tooltip = 'text')
      }
     
      return(pltly)
   }
   
   # UI reactives
   
   output$downloadStatTable<- downloadHandler(
      filename = function() {
         sprintf('ProViz_%s_table.csv', input$statTests)
      },
      content = function(file) {
            write.csv(getStatResultsTable(), file, row.names=FALSE)
      }
   )
   
   output$stat2GrpAnnoTable <- DT::renderDataTable(
      server = TRUE, rownames = FALSE, colnames = '',
      selection = 'none', options = list(dom = 't'), 
      {
         if(is.null(rv$statTableRowSelect)){
            return(NULL)
         } else {
            res_table <- getStatResultsTable()
            var_name <- res_table$AptName[rv$statTableRowSelect]
            row_num <- which(rv$featureData$AptName == var_name)
            df <- data.frame(
               Label = c('SOMAmer', 'Protein Full Name', 'Protein Short Name',
                         'UniProt ID', 'Entrez Gene Id', 'Entrez Gene Symbol'),
               Details = t(rv$featureData[row_num, 
                            c('AptName', 'TargetFullName', 'Target', 
                              'UniProt', 'EntrezGeneID',
                              'EntrezGeneSymbol')]))
         }
         df
      }
   )
   
   output$statResTable <- DT::renderDataTable(
      server = TRUE, rownames = FALSE,
      selection = 'single',
      options = list(scrollX = TRUE, deferRender = TRUE),
      {
         if(input$statTests == 'Correlation'){
            return(statCorrTests())
         } else if(input$statTests == 't-test' |
                   input$statTests == 'U-test' |
                   input$statTests == 'KS-test'){
            return(stat2GrpTests())
         } else if(input$statTests == 'ANOVA' |
                   input$statTests == 'Kruskal-Wallis'){
            return(statMultiGrpTests())
         }
      })
   observeEvent(input$stat2GrpCorrection, {
      updateSliderInput(session, inputId = 'stat2GrpPvalue',
                        label = input$stat2GrpCorrection)
   })
   
   observeEvent(event_data(event = 'plotly_doubleclick', source = 'stat2GrpVolcano'), {
      # unset the selected variables
      rv$statTableRowSelect <- NULL
   })
   
   observeEvent(event_data(event = 'plotly_click', source = 'stat2GrpVolcano'), {
      # set the selected variables globally
      clickData <- event_data(event = 'plotly_click', source = 'stat2GrpVolcano')
      resTable <- getStatResultsTable()
      rv$statTableRowSelect <- which(resTable$AptName == clickData$key)
   })
   
   observeEvent(input$statResTable_cell_clicked, {
      row_sel <- input$statResTable_rows_selected
      
      if(is.null(row_sel)){
         rv$statTableRowSelect <- NULL
      } else {
         rv$statTableRowSelect <- row_sel
      }
   })
   
   observeEvent(input$statTests, {
      # unset the plot selection
      rv$statTableRowSelect <- NULL
      
      if(input$statTests == 'Correlation'){
         updateSliderInput(session, inputId = 'stat2GrpFold',
                           label = 'Correlation',
                           min = 0.0, max = 1.0, step = 0.01, value = 0.75)
      } else {
         updateSliderInput(session, inputId = 'stat2GrpFold',
                           label = 'Fold Change (log2)',
                           min = 0, max = 10,
                           step = 0.1, value = 2)
      }
   })
   
   observeEvent(input$statCorrMethod, {
      # unset the plot selection
      rv$statTableRowSelect <- NULL
   })
}

