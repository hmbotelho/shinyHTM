# shinyHTM

### Exploring High Throughput Microscopy data with Shiny, Plotly and Fiji

One of the biggest challenges in High Throughput Microscopy is the visualization, inspection and quality control of the large datasets that are usually produced. Nevertheless, these tasks are crucial for assessing the quality of the data and ensure that biologically meaningful quantitative data is extracted.


## What is shinyHTM?

An interactive web-based tool which uses the R ``shiny`` package to inspect, plot and visualize high throughput microscopy data and images.

**The goal:**  to be an experimentation platform for a possible improvement of [Christian Tischer](https://www.embl.de/services/core_facilities/almf/members/index.php?s_personId=CP-60005645)'s [HTM Explorer](https://github.com/tischi/HTM_Explorer). Namely by implementing:

* Expand plotting capabilities 
* Enhanced interactivity 
* More user friendliness 
* Better interaction with Fiji
* ...

This is a personal project of mine and pretty much a work in progress.

The app runs in   a browser window like so:

![snapshot](https://github.com/hmbotelho/shinyHTM/blob/master/snapshot.jpg)


## Main features
* Configuration of customized multiwell plates
* Interactive scatter plots, jitter plots and heatmaps
* Click any data point to open the raw image in Fiji
* Hover mouse over plot to read the data
* Show all data points or only individual plates
* Contains a sample dataset
* Data normalization
* __Soon to come:__ treatment summary


## Requirements
* R or RStudio
* A "tidy" *csv file (_e.g._ a [CellProfiler](http://cellprofiler.org/) output file)
* Optional: microscopy images and [Fiji](https://fiji.sc/)


## How to run
* Download the repository  
* Unzip  
* Open R or RStudio 
* Write the following commands. Adapt to your case.
``
setwd("folder_with_unzipped_files");  
source("run.r")
``