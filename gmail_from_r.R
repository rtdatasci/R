# gmail r
# 
# REQUIREMENTS
# console.developers.google.com for api enabling; enable gmail api
# download json file with secret in gmail_secret.json
# Client ID: xxxxxxxxxxxxxxx.apps.googleusercontent.com
# Client Secret: xxxxxxxxxxxxxxxxxxxxxxxxxxx
  
  
library(gmailr)

# Link to secret saved in json
use_secret_file("~/rtdatasci_github/gmail_secret.json")

# Authorize
gm_auth_configure(path = "~/rtdatasci_github/gmail_secret.json")
gm_auth(email = TRUE, cache = ".secret")

# will be prompted to gmail authorizatio in browser 
# when successful: Authentication complete. Please close this page and return to R.

gm_auth(key=clientid , secret = secret)



# text format message:
date <- format(Sys.Date(), "%Y-%B-%d")
msg <- glue::glue("test :", date)
msg

# setup
my_email_message <- gm_mime() %>% 
  gm_to("your_gmail_address@gmail.com") %>% 
  gm_from("your_gmail_address@gmail.com") %>% 
  gm_subject("testing gmailr") %>% 
  gm_text_body(msg)  
  # alternatively try html method
  #gm_html_body(html_msg)  # where you can input italics size etc in a separateley created html_msg version of msg

# can also create a graph
# save the graph to file eg. mygraph <- ggsave("abc.png")
# then add to mime object at the end of text_body() %>% gm_attach_file("abc.png")


str(my_email_message)

# send message
gm_send_message(my_email_message)

# create/save draft without sending
gm_create_draft(my_email_message)
  
  

