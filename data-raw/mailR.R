install.packages("rJava")
install.packages("mailR")
library(rJava)
library(mailR)

sender <- 'plevy@ceh.ac.uk'
recipients <- c('<olihit@ceh.ac.uk>','holcoo@ceh.ac.uk')
MailToStatus <- c('olihit@ceh.ac.uk', 'holcoo@ceh.ac.uk')
smtpServer <- 'cehmail.nbu.ac.uk'

  sendmail(from=Mailfrom, 
           to=recipients,
           msg="Body of the email", 
           subject=paste(ScriptName, ' Failed'),
           control=list(smtpServer=smtpServer))

sender <- "plevy@ceh.ac.uk" # Replace with a valid address
recipients <- c("petere.levy@gmail.com") # Replace with one or more valid addresses
recipients <- "plevy@ceh.ac.uk" # Replace with one or more valid addresses
email <- send.mail(from = sender,
to = recipients,
subject="Subject of the email",
body = "Body of the email",
smtp = list(host.name =smtpServer, port = 25),
authenticate = FALSE,
send = FALSE)
## Not run: email$send() # execute to send email