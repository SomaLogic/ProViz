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
options(shiny.maxRequestSize = 30 * 1024 ^ 2)

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
   output$SLlogo<- renderUI(HTML(paste(
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
         return(is.numeric(column) &
                length(unique(na.omit(column))) > 10)
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

      # get continuous variables with only two groups
      twoGrp <- which(sapply(rv$adat, function(c){
         length(unique(na.omit(c))) == 2
      }))
      twoGrpCols <- sapply(colnames(rv$adat)[twoGrp], function(cn) {
         lookupID(cn, input$rdoIDChoice)
      })
      updateSelectInput(session, 'stat2GrpResp',
                        choices = c('<NONE>', twoGrpCols))

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
      rv$stat2GrpResp  <- input$stat2GrpResp
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
      updateSelectInput(session, 'stat2GrpResp',
                        selected = rv$stat2GrpResp)
   }
   
   # change in choice of ID cascades to UpdateUI
   observeEvent(input$rdoIDChoice, {
      PreservePlotStates()
      UpdateUI()
      RevertPlotStates()
   })
   
   # always update in the data changes
   observeEvent(rv$adat, {
      UpdateUI()
   })

   ########################################
   # Handlers for loading
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

   dlHandler <- downloadHandler(
      filename = 'ProViz_modified.adat',
      content = function(file) {
            SomaDataIO::write_adat(x = rv$adat, file = file)
      }
   )
   
   output$fltrDownloadADAT <- dlHandler 
   
   output$loadPreviewText <- renderUI(
      rv$loadMessage
   )

   output$loadPreviewTable <- DT::renderDataTable(
      server = FALSE, rownames = FALSE,
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
   # UI observers for filtering 
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
      server = FALSE,
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
      server = FALSE,
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
            i <- which(colnames(rv$mergedData) == input$mergeDataCol)
            rv$metaColumns <- c(rv$metaColumns, colnames(rv$mergedData)[-i])
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
            i <- which(colnames(rv$mergedData) == input$mergeDataCol)
            rv$metaColumns <- c(rv$metaColumns, colnames(rv$mergedData)[-i])
         }
      }
   })
   
   output$mergeDownloadADAT <- dlHandler
   
   output$mergeADATstats <- renderUI(
      rv$loadMessage
   )
   
   ####################################
   # UI observers for grouping 
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
      server = FALSE, 
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
   # Boxplot handlers
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
         
         pltly
      })

   ####################################
   # CDF plots handlers
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
      
      pltly     
   })

   ####################################
   # scatter plot handlers
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
                             label = paste0('R^2 = ', 
                                            round(cor(X, Y, 
                                                      use = 'complete.obs'), 3)
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
      
      pltly
   })
   
   ####################################
   # define pieces of the Stats Tests tab
   ####################################
   output$downloadResults <- downloadHandler(
      filename = sprintf('ProViz_%s_table.csv', input$stat2GrpTest),
      content = function(file) {
            write.csv(rv$stat2GrpTable, file, row.names=FALSE)
      }
   )
   
   output$stat2GrpAnnoTable <- DT::renderDataTable(
      server = FALSE, rownames = FALSE, colnames = '',
      selection = 'none', options = list(dom = 't'), 
      {
         if(is.null(rv$stat2GrpRowSelect)){
            return(NULL)
         } else {
            var_name <- rv$stat2GrpTable$AptName[rv$stat2GrpRowSelect]
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
   
   output$stat2GrpResTable <- DT::renderDataTable(
      server=FALSE,
      rownames = FALSE,
      selection = 'single',
      options = list(scrollX = TRUE, deferRender = TRUE),
      {
         if(is.null(rv$adat) | input$stat2GrpResp == '<NONE>'){
            return(NULL)
         }
         
         print(input$stat2GrpResp)
         print(rv$stat2GrpResp)
         
         if(input$stat2GrpResp == rv$stat2GrpResp){
            df <- rv$stat2GrpTable
            # user-friendly column names
            colnames(df)[1:3] = c('SOMAmer', 'Protein Name', 'Gene Symbol')
            return(df)
         }
         
         respID <- lookupID(input$stat2GrpResp, 'SOMAmer ID')
         vars <- SomaDataIO::getFeatures(rv$adat)
         df <- data.frame(rv$featureData[, c('AptName', 
                                             'TargetFullName', 'EntrezGeneSymbol')])
         
         grps <- unique(rv$adat[[respID]])
         i <- which(rv$adat[[respID]] == grps[1])
         
         df$Fold.Change <- sapply(vars, function(v){
            signif(log2(median(rv$adat[i,v])/median(rv$adat[-i,v])), 2)
         })
   
         adat <- log10(rv$adat)
   
         if(input$stat2GrpTest == 'KS-test'){
            tbl <- data.frame(t(sapply(vars, function(v){
               z <- suppressWarnings(ks.test(adat[i,v], adat[-i,v]))
               j <- which(df$AptName== v)
               if(df$Fold.Change[j] < 0){
                  z$signedKS <- -z$statistic
               } else {
                  z$signedKS <- z$statistic
               }
               
               # increment the progress bar
               i <- which(vars == v)
               if((i / length(vars) * 100) %% 5 == 0){
                  updateProgressBar(session = session, id = 'statProgbar',
                                    value = i / length(vars) * 100)
               }
               
               c(KS.Dist = round(as.numeric(z$statistic),2),
                 Signed.KS.Dist = round(as.numeric(z$signedKS),2),
                 p.value = z$p.value)
            })))
         } else if(input$stat2GrpTest == 't-test'){
            tbl <- data.frame(t(sapply(vars, function(v){
               z <- suppressWarnings(t.test(adat[i,v], adat[-i,v],
                                     var.equal = FALSE))
               
               # increment the progress bar
               i <- which(vars == v)
               if((i / length(vars) * 100) %% 5 == 0){
                  updateProgressBar(session = session, id = 'statProgbar',
                                    value = i / length(vars) * 100)
               }
               
               c(t.statistic = round(as.numeric(z$statistic),2),
                 p.value = z$p.value)
            })))
         } else if(input$stat2GrpTest == 'U-test'){
            tbl <- data.frame(t(sapply(vars, function(v){
               z <- suppressWarnings(wilcox.test(adat[i,v], adat[-i,v]))
               
               # increment the progress bar
               i <- which(vars == v)
               updateProgressBar(session = session, id = 'statProgbar',
                                 value = i / length(vars) * 100)
               
               c(W.statistic = round(as.numeric(z$statistic)),
                 p.value = z$p.value)
            })))
         }
         
         # provide multiple testing corrections
         tbl$FDR <- signif(p.adjust(tbl$p.value, method='BH'), 2)
         tbl$Bonferroni <- signif(p.adjust(tbl$p.value, method='bonferroni'), 2)
         df <- cbind(df, tbl)
         df$p.value <- signif(df$p.value, 2)
         rv$stat2GrpTable <- df
         
         # user-friendly column names
         colnames(df)[1:3] = c('SOMAmer', 'Protein Name', 'Gene Symbol')
         df
   })

   output$stat2GrpVolcano <- renderPlotly({
      if(is.null(rv$adat) || input$stat2GrpResp == '<NONE>'){
         return(NULL)
      }
      
      df <- rv$stat2GrpTable
      
      # set the p-value column name
      if(input$stat2GrpCorrection == 'p-value') {
         pcolumn <- 'p.value'
      } else {
         pcolumn <- input$stat2GrpCorrection
      }
      
      # mark the significant points from the volcano plot criteria
      df$pt_cols <- '0' # non-significant indicator
      
      i <- which(rv$stat2GrpTable$Fold.Change > input$stat2GrpFold |
                 rv$stat2GrpTable$Fold.Change < (-input$stat2GrpFold))
      j <- which(rv$stat2GrpTable[[pcolumn]] < input$stat2GrpPvalue )
      k <- intersect(i,j)
      
      df$pt_cols[k] <- '1' # significant indicator
      
      # mark the selected points
      df$pt_cols[rv$stat2GrpRowSelect] <- '2' # selected marker
      
      # make the actual plot
      plt <- ggplot(df, aes(x = Fold.Change, y = -log10(!!sym(pcolumn)),
                            color = pt_cols, key = AptName,
                            text = paste0(paste0('SOMAmer: ', AptName, '\n',
                                                 'Protein Name: ', TargetFullName, '\n',
                                                 'Gene Symbol: ', EntrezGeneSymbol, '\n',
                                                 'Fold Change: ', Fold.Change, '\n',
                                                 input$stat2GrpCorrection, ': ', 
                                                      signif(!!sym(pcolumn), 3))
                                   ))) +
         geom_point(shape = 19) +
         scale_color_manual(values = c('#132B43', '#56B1F7', 'darkorange')) +
         xlab('Fold Change (log2)') +
         ylab(paste0('-log10(', input$stat2GrpCorrection, ')')) +
         geom_vline(xintercept = c(input$stat2GrpFold, -input$stat2GrpFold),
                    size = 0.5, color = 'red') +
         geom_hline(yintercept = -log10(input$stat2GrpPvalue),
                    size = 0.5, color = 'red') +
         theme_minimal() +
         theme(legend.position = 'none',
               plot.margin = margin(1, 1, 1, 1, 'lines')) 
     
         sel_df <- subset(df, pt_cols == '2')
         if(nrow(sel_df) > 0){
            plt <- plt + geom_point(data = sel_df, 
                                    aes(x = Fold.Change, y = -log10(!!sym(pcolumn))),
                                    color = 'darkorange', size = 2.5)
         }
            
      pltly <- ggplotly(p = plt, source = 'stat2GrpVolcano', tooltip = 'text') %>% 
         event_register(event = 'plotly_click') %>% 
         event_register(event = 'plotly_doubleclick')
      
      pltly
   })
   
   output$stat2GrpDistPlot <- renderPlotly({

      if(!is.null(rv$stat2GrpRowSelect)){
         row_sel <- rv$stat2GrpRowSelect
         var_name <- rv$stat2GrpTable$AptName[row_sel]
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
      df$X<- rv$adat[[input$stat2GrpResp]]
      
      if(input$stat2GrpBoxCDF == 'Boxplot'){
         plt <- ggplot(df, aes(x = X, y = Y, 
                               text = paste0('SampleId: ', SampleId, '\n',
                                             input$stat2GrpResp, ': ', X, '\n',
                                             lab_name, ': ', round(Y, 2)
                               ))) +  
            geom_boxplot(aes(fill = X), color = 'black') +
            xlab(input$stat2GrpResp) +
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
     
      pltly 
   })
   
   # Stat tab UI observers
   observeEvent(input$stat2GrpCorrection, {
      updateSliderInput(session, inputId = 'stat2GrpPvalue',
                        label = input$stat2GrpCorrection)
   })
   
   observeEvent(event_data(event = 'plotly_doubleclick', source = 'stat2GrpVolcano'), {
      # unset the selected variables
      rv$stat2GrpRowSelect <- NULL
   })
   
   observeEvent(event_data(event = 'plotly_click', source = 'stat2GrpVolcano'), {
      # set the selected variables globally
      clickData <- event_data(event = 'plotly_click', source = 'stat2GrpVolcano')
      rv$stat2GrpRowSelect <- which(rv$stat2GrpTable$AptName == clickData$key)
   })
   
   observeEvent(input$stat2GrpResTable_cell_clicked, {
      row_sel <- input$stat2GrpResTable_rows_selected
      
      if(is.null(row_sel)){
         rv$stat2GrpRowSelect <- NULL
      } else {
         rv$stat2GrpRowSelect <- row_sel
      }
   })
}
