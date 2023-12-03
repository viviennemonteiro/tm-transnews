library(dplyr)
library(purrr)

# Function to creates sections from document
read_sections <- function(path) {
  file_content <- readLines(path, warn = FALSE)
  if (!identical(grep("^Bibliography", file_content), integer(0))) {
    file_end <- grep("^Bibliography", file_content) - 3 #removes Bibliography and contact sections
    file_content <- paste(file_content[1:file_end], collapse=NULL)
  }
  
  doc_sections <- list()
  current_section <- character()
  
  # Iterate through each line in the file
  for (line in file_content) {
    # Check if the line is the separator
    if (grepl("^_{50,}$", line)) {
      # If there is content in the current section, add it to the list
      if (length(current_section) > 0) {
        doc_sections <- c(doc_sections, list(current_section))
        current_section <- character()
      }
    } else {
      # Add the line to the current section
      #current_section <- paste(current_section, line, sep="\n")
      current_section <- c(current_section, line)
    }
  }
  
  # Add the last section if there is content
  if (length(current_section) > 0) {
    doc_sections <- c(doc_sections, list(current_section))
  }
  
  # Return the list of sections
  return(doc_sections)
}

# Function to extract information from the text
extract_document <- function(doc) {
  # Extract relevant information using regular expressions  
  title <- sub("Title: (.+)", "\\1", doc[grep("Title:", doc)])
  author <- sub("Author: (.+)", "\\1", doc[grep("Author:", doc)])
  publication_year <- sub("Publication year: (.+)", "\\1", doc[grep("Publication year:", doc)])
  publication_date <- sub("Publication date: (.+)", "\\1", doc[grep("Publication date:", doc)])
  publisher <- sub("Publisher: (.+)", "\\1", doc[grep("Publisher:", doc)])
  location <- sub("Location: (.+)", "\\1", doc[grep("Location:", doc)])
  page <- sub("Pages: (.+)|First page: (.+)", "\\1", doc[grep("Pages:|First page:", doc)])
  column <- sub("column: (.+)", "\\1", doc[grep("column:", doc)])
  section <- sub("Section: (.+)", "\\1", doc[grep("Section:", doc)])
  subject <- sub("Subject: (.+)", "\\1", doc[grep("Subject:", doc)])
  people <- sub("People: (.+)", "\\1", doc[grep("People:", doc)])
  source_type <- sub("Source type: (.+)", "\\1", doc[grep("Source type:", doc)])
  identifier_keyword <- sub("Identifier / keyword: (.+)", "\\1", doc[grep("Identifier / keyword:", doc)])
  document_type <- sub("Document type: (.+)", "\\1", doc[grep("Document type:", doc)])
  proquest_document_id <- sub("ProQuest document ID: (.+)", "\\1", doc[grep("ProQuest document ID:", doc)])
  document_url <- sub("Document URL: (.+)", "\\1", doc[grep("Document URL:", doc)])
  copyright <- sub("Copyright: (.+)", "\\1", doc[grep("Copyright:", doc)])

  # Extract the full text
  if (!identical(grep("Full text:", doc), integer(0))) {
    full_text_start <- grep("Full text:", doc)
    full_text_end <- (grep("^$", doc[full_text_start:length(doc)]) - 1 + full_text_start)[1]
    full_text <- sub("Full text: ", "", paste(doc[full_text_start:full_text_end], collapse = " "))
  } else {
    full_text <- NA
  }
  #full_text_start <- grep("Full text:", doc)
  #full_text_end <- grep("Subject:", doc) - 1
  #full_text <- sub("Full text: ", "", paste(doc[full_text_start:full_text_end], collapse = " "))

  # List of variables
  variables <- c("title", "author", "publication_year", "publication_date", "publisher",
                 "location", "page", "column", "section", "subject", "people", 
                 "source_type", "identifier_keyword", "document_type", 
                 "proquest_document_id", "document_url", "copyright", "full_text")
  
  # Simplified code using lapply to correct for missing variables
  for (var in variables) {
    assign(var, ifelse(identical(get(var), character(0)), NA, get(var)))
  }

  # Return a data frame with the extracted information
  tibble(!!!set_names(syms(variables), variables))
}


#set up df and loop through all sections of the individual document
#full_doc <- "data/text_files/pg-final37.txt"
#doc <- read_sections(full_doc)
#doc_df <- tibble()
#for (s in 1:length(doc)) {
#  doc_df <- doc_df |>
#    bind_rows(extract_document(doc[[s]]))
#}


files <- list.files(path="data/text_files", pattern="*.txt", full.names=TRUE, recursive=FALSE)
doc_df <- tibble()
for (f in 1:length(files)) {
  doc <-read_sections(files[f])
  for (s in 1:length(doc)) {
    doc_df <- doc_df |>
      bind_rows(extract_document(doc[[s]]))
  }
  cat(basename(files[f]), "processed (",f," of ", length(files), ")\n")
}  

# Write the result to a CSV file
#write.csv(doc_df, "~/Projects/thesis/data/output.csv", row.names = FALSE)

#write to rda file
save(doc_df, file="data/trans_news_data.Rda")
