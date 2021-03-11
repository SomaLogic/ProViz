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

library(shiny)
library(shinydashboard)
library(shinyWidgets)

library(DT)
library(dplyr)
library(ggbeeswarm)
library(ggplot2)
library(magrittr)
library(plotly)
library(readr)

library(SomaDataIO)

# common pieces for chart selections
colPalette <- c("dodgerblue", "red", "#E69F00", "#009E73", "cyan",
                "#999999","#CC79A7", "green", "black", "#D55E00", "#F0E442")
colorNames <- c('Blue', 'Red', 'Bright Orange', 'Dark Green', 'Cyan', 'Grey',
              'Magenta', 'Bright Green', 'Black', 'Dark Orange')
lineStyleNames <- c('Solid', 'Dashed', 'Dotted', 'Dash-Dot', 'Long Dash')

####################################
# reactive storage variable
####################################
rv <- reactiveValues()

# ADAT variables
rv$adat <- NULL
rv$adatOrig <- NULL
rv$conColumns <- NULL
rv$catColumns <- NULL
rv$metaColumns <- NULL
rv$featureData <- NULL
rv$idLookup <- NULL

# messages for the load/filter screen
rv$loadMessage <- 'No ADAT open'

# merged data
rv$mergedData <- NULL

# messages for the merge screen
rv$mergeMessageDefault <-  paste('Only files with comma-separated values ',
                                 '(.csv) or tab-separated values (.txt) are ',
                                 'supported for merging.  The first row ',
                                 'should contain column names.',
                                 sep = '')
rv$mergeMessage <- NULL

# grouping variables
rv$grpWarning <- ""
rv$grpNewGroupData <- NULL

# stats variables
rv$stat2GrpTable <- NULL
rv$stat2GrpRowSelect <- NULL

# variables for preserving state of selections and settings
# that get adjusted during UI updates

# Group panel
rv$grpSelectedCol <- NULL
rv$grpSplitSetting <- NULL
rv$grpCatGrpA <- NULL
rv$grpCatGrpB <- NULL

# boxplots
rv$pltBxXaxis <- NULL
rv$pltBxYaxis <- NULL

# cdf
rv$pltCDFXaxis <- NULL
rv$pltCDFColorBy <- NULL

#scatter plot
rv$pltSctrXaxis <- NULL
rv$pltSctrYaxis <- NULL
rv$pltSctrColorByVar <- NULL

# stats
rv$stat2GrpResp <- '<NONE>' 

####################################
# utility functions          
####################################

ecdf_prep <- function(df, data_col = 'X', group_col = 'grp',
                      label_col = 'SampleId') {
  # returns a dataframe with ecdf data compatible
  # with ggplot() and ggplotly()
  # expects a dataframe with 3 columns in a particular order 
  
  x_low <- min(df[[data_col]])
  x_high <- max(df[[data_col]])
  
  cdf_df <- as.data.frame(do.call(rbind, lapply(unique(df[[group_col]]), 
                    function(grp) {
                      if(is.na(grp)){
                        sub_df <- df[which(is.na(df[[group_col]])),
                                     c(data_col, group_col, label_col)] 
                      } else {
                        sub_df <- df[which(df[[group_col]] == grp), 
                                     c(data_col, group_col, label_col)]
                      }
                      sub_df$Y <- ecdf(sub_df[[data_col]])(sub_df[[data_col]])
                      sub_df[[group_col]] <- factor(sub_df[[group_col]])
                      sub_df
                    })))
  cdf_df[order(cdf_df[[data_col]]), c(colnames(df), 'Y')]
}

# function to create a lookup table for IDs
# allow conversion from any id (SOMAmer, Protein Name, Gene Symbol)
# to any other id
createIdLookup <- function() {
  #gather the individual IDs into one long vector
  lookup <- lapply(1:nrow(rv$featureData), function (i) {
    list('SOMAmer ID' = rv$featureData$AptName[i],
         'Protein Name' = 
           ifelse(rv$featureData$TargetFullName[i] == '',
                  paste0('[', rv$featureData$AptName[i], ']'),
                  rv$featureData$TargetFullName[i]),
         'Gene Symbol' = 
           paste0(rv$featureData$EntrezGeneSymbol[i],
                  ' [', rv$featureData$AptName[i], ']')
    )
  })
  
  # need to deal with duplicate gene names
  
  # repeat the ids and apply names of each alternative id
  rv$idLookup <- rep(lookup, 3)
  names(rv$idLookup) <- c(rv$featureData$AptName,
                          ifelse(rv$featureData$TargetFullName == '',
                                 paste0('[', rv$featureData$AptName, ']'),
                                 rv$featureData$TargetFullName),
                          paste0(rv$featureData$EntrezGeneSymbol,
                                 ' [', rv$featureData$AptName, ']')
  )
}
# interface function to the idLookup table
# this allows cleaner access to the table
# if an ID is requested and not found, it is returned
# as is, and can be assumed to be a metaData item
# rather than a SOMAmer ID, Protein Name, or Gene Symbol
lookupID <- function(id, targetType) {
  targetID <- rv$idLookup[[id]][[targetType]]
  if(is.null(targetID)){
    targetID <- id
  }
  targetID
}

