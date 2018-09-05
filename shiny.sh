#!/bin/sh

exec Rscript -e 'shiny::runApp(port=8787, launch.browser=FALSE, host="0.0.0.0")'

