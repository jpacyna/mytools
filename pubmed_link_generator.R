pubmed_topic <- function(...){
  # ------------------------------------------------------------------------------- #
  # Function to prep the individual topic term lists (OR)                           #
  #                                                                                 #
  # ethics <- pubmed_topic("ethic", "ethics", "ethic*")                             #
  # gx <- pubmed_topic("genetic", "genomic", "genetics", "genomics", "gene", "geno*")
  # ------------------------------------------------------------------------------- #  
  theVector <- c(...)
  theVector <- purrr::map(theVector, function(x) if(stringr::str_detect(x, "\\*")){x} else{paste0("'",x,"'")})
  theVector <- paste0(theVector,"[title%2Fabstract]")
  theVector <- paste(theVector, collapse = "+OR+")
  theVector <- paste0("(",theVector,")")
  theVector
  }


pubmed_link <- function(..., from = "2000-01-01", to = "3000"){
  # ------------------------------------------------------------------------------- #
  # Function for assembling the URL after topic lists have been established         #
  #                                                                                 #  
  # pubmed_link(gx, ethics, from = "2021-01-01", to = "2022-12-31")                 #
  # ------------------------------------------------------------------------------- #    
  date_from <- stringr::str_replace_all(from,"-","%2F")
  date_to <- stringr::str_replace_all(to,"-","%2F")
  date_range <- paste0("((",date_from,"[Date+-+Publication]+%3A+",date_to,"[Date+-+Publication]))")
  x <- paste0("https://pubmed.ncbi.nlm.nih.gov/?term=",date_range,"+AND+",paste(..., sep = "+AND+"),"&sort=pubdate")
  writeClipboard(noquote(x))
  request <- noquote(paste(unlist(lapply(substitute(list(...))[-1], deparse)), collapse = ", "))
  writeLines(          
  paste0(
    "\n\n",
    noquote(paste0("Pubmed link for topic(s): '",request,"' has been copied to the clipboard. You may paste it in your browser.")),
    "\n\n",
    noquote(x),
    "\n\n"
    ))
  }