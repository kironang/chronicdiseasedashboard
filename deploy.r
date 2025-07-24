setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(rsconnect)
rsconnect::setAccountInfo(name='name', token='token', secret='secret')

rsconnect::deployApp('dashboard')
