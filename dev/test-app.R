setwd("~")
runApp(appDir = dirname(rstudioapi::getSourceEditorContext()$path),
       host="0.0.0.0",port=1234)
