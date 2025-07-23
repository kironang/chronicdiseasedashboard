install.packages('rsconnect')
library(rsconnect)
rsconnect::setAccountInfo(name='insert name here',
			  token='insert token here',
			  secret='insert secret here')
rsconnect::deployApp('../dashboard/app.r')