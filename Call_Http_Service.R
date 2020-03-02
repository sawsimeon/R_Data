library(RCurl)
library(jsonlite)

post_query <- function(method, args) {
    hdr <- c("Content-Type" = "application/x-www-form-urlencoded")
    resp <- postForm(
        paste0("http://localhost:8000/", method),
        .opts= list(httpheader = hdr,
        postfields = toJSON(ars))))
    )
}

