setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(rsconnect)
rsconnect::setAccountInfo(name='paste-your-name-here',
			  token='paste-your-token-here',
			  secret='paste-your-secret-here')
rsconnect::deployApp('dashboard')