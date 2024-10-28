library(rvest)
library(httr)
library(xml2)
library(stringr)
library(dplyr)
library(ggplot2)

# Main domain
domain <- "https://translationalneurodegeneration.biomedcentral.com"
year <- c(2019, 2020, 2021, 2022, 2023, 2024)

# Initialize a list to store all article links and their publication dates
all_article_data <- list(
  Links = character(),
  Publish_Date = character()
)

# Function to extract article links and their publication dates from a given page
get_article_data <- function(url) {
  page <- read_html(url)
  article_links <- page %>%
    html_nodes("a[itemprop='url']") %>%
    html_attr("href")
  complete_article_links <- paste0(domain, article_links)
  complete_article_links <- complete_article_links[-1]  # Remove the first link as it's not a complete article link
  
  # Extract publication dates
  publish_dates <- page %>%
    html_nodes(xpath = "//span[@data-test='published-on']/span[@itemprop='datePublished']") %>%
    html_text() %>%
    str_extract("\\d{4}")
  
  return(list(Links = complete_article_links, Publish_Date = publish_dates))
}
# Initialize page number
page_num <- 1

# Loop through each page to extract article links and their publication dates
while (TRUE) {
  page_url <- paste0(domain, "/articles?searchType=journalSearch&sort=PubDate&page=", page_num)
  article_data <- get_article_data(page_url)
  
  # Check if article links are empty, if so, break the loop
  if (length(article_data$Links) == 0) {
    break
  }
  
  # Append article data to all_article_data
  all_article_data$Links <- c(all_article_data$Links, article_data$Links)
  all_article_data$Publish_Date <- c(all_article_data$Publish_Date, article_data$Publish_Date)
  
  page_num <- page_num +1 
}

# Filter articles based on the publication year
selected_links <- all_article_data$Links[str_extract(all_article_data$Publish_Date, "\\d{4}") %in% as.character(year)]

# Initialize lists to store scraped data
article_data <- list(
  Title = vector("list", length(selected_links)),
  Publish_Date = vector("list", length(selected_links)),
  Extract1 = vector("list", length(selected_links)),
  Keywords = vector("list", length(selected_links)),
  Co_Authors = vector("list", length(selected_links)),
  Co_Author_Emails = vector("list", length(selected_links)),
  Author_name = vector("list", length(selected_links))
)
# Loop through each selected article link to scrape data
for (i in seq_along(selected_links)) {
  article_url <- selected_links[i]
  article_page <- read_html(article_url)
  
  # If HTML content is successfully downloaded, proceed with scraping
  if (!is.null(article_page)) {
    # Extract article title
    article_title <- article_page %>%
      html_node("h1.c-article-title") %>%
      html_text(trim = TRUE)
    
    # Extract published date
    publish_date <- article_page %>%
      html_node("time") %>%
      html_attr("datetime")
    
    # Check if the publication year is in the specified years
    if (!is.na(publish_date) && str_extract(publish_date, "\\d{4}") %in% as.character(year)) {
      article_data$Title[[i]] <- article_title
      article_data$Publish_Date[[i]] <- publish_date
      
      # Extract additional information
      extract1 <- article_page %>%
        html_node(".c-article-section__content p") %>%
        html_text()
      article_data$Extract1[[i]] <- extract1
      
      # Extract keywords
      li_classes <- article_page %>%
        html_nodes("ul.c-article-subject-list li") %>%
        html_attr("class")
      unique_classes <- unique(li_classes)
      li_class_list <- as.list(unique_classes)
      keywords <- lapply(li_class_list, function(class_attr) {
        article_page %>%
          html_nodes(sprintf("li.%s", class_attr)) %>%
          html_text()
      })
      article_data$Keywords[[i]] <- keywords
      
      # Extract author information
      cor_author <- article_page %>% html_nodes("#corresponding-author-list")
      cor_author <- html_text(cor_author)
      names <- stringr::str_extract_all(cor_author, "\\b[A-Z][a-z]+\\s[A-Z][a-z]+\\b")[[1]]
      cor_email <- article_page %>% html_nodes("#corresponding-author-list a") %>% html_attr("href")
      emails <- stringr::str_extract_all(cor_email, "(?<=mailto:)[^\\s]+")
      article_data$Co_Authors[[i]] <- names
      article_data$Co_Author_Emails[[i]] <- emails
      
      # Extract Author names
      authors <- article_page %>% html_nodes("a[data-test='author-name']") %>% html_text()
      article_data$Author_name[[i]] <- paste(authors, collapse = ", ")
    }
  }
}
# Remove articles with missing data
article_data <- article_data[sapply(article_data, function(x) any(lengths(x) > 0))]

# Write data to a CSV file with custom separator
writeLines(
  do.call(
    paste,
    c(
      as.data.frame(do.call(rbind, article_data)),
      sep = "#@#",
      collapse = "\n"
    )
  ),
  "sample_data_5_years.csv"
)

# Confirm that the file has been written successfully
cat("Scraped data has been saved to 'sample_data.csv'")
view(sample_data_5_years.csv)

#DATA SORTING INTO COLUMNS AND REPLACING WITH NA

library(rvest)
library(stringr)  # Load the stringr package

# Assuming 'df' is your dataframe
# Filter articles based on the publication year
selected_links <- all_article_data$Links[str_extract(all_article_data$Publish_Date, "\\d{4}") %in% as.character(year)]

# Initialize lists to store scraped data
article_data <- list(
  Title = character(),
  Publish_Date = character(),
  Extract1 = character(),
  Keywords = character(),
  Co_Authors = character(),
  Co_Author_Emails = character(),
  Author_name = character()
)

# Loop through each selected article link to scrape data
for (i in seq_along(selected_links)) {
  article_url <- selected_links[i]
  
  # Read HTML content from the article URL
  article_page <- read_html(article_url)
  
  # If HTML content is successfully downloaded, proceed with scraping
  if (!is.null(article_page)) {
    # Extract article title
    title_node <- article_page %>% html_node("h1.c-article-title")
    title <- ifelse(is.null(title_node), NA, title_node %>% html_text(trim = TRUE))
    article_data$Title <- c(article_data$Title, title)
    
    # Extract published date
    publish_date_node <- article_page %>% html_node("time")
    publish_date <- ifelse(is.null(publish_date_node), NA, publish_date_node %>% html_attr("datetime"))
    article_data$Publish_Date <- c(article_data$Publish_Date, publish_date)
    
    # Extract additional information
    extract_node <- article_page %>% html_node(".c-article-section__content p")
    extract <- ifelse(is.null(extract_node), NA, extract_node %>% html_text())
    article_data$Extract1 <- c(article_data$Extract1, extract)
    
    # Extract keywords
    li_classes <- article_page %>%
      html_nodes("ul.c-article-subject-list li") %>%
      html_attr("class")
    unique_classes <- unique(li_classes)
    li_class_list <- as.list(unique_classes)
    keywords <- lapply(li_class_list, function(class_attr) {
      article_page %>%
        html_nodes(sprintf("li.%s", class_attr)) %>%
        html_text()
    })
    keyword_text <- ifelse(length(keywords) == 0, NA, paste(keywords, collapse = ", "))
    article_data$Keywords <- c(article_data$Keywords, keyword_text)
    
    # Extract author information
    cor_author <- article_page %>% html_nodes("#corresponding-author-list")
    cor_author <- html_text(cor_author)
    names <- stringr::str_extract_all(cor_author, "\\b[A-Z][a-z]+\\s[A-Z][a-z]+\\b")[[1]]
    cor_email <- article_page %>% html_nodes("#corresponding-author-list a") %>% html_attr("href")
    emails <- stringr::str_extract_all(cor_email, "(?<=mailto:)[^\\s]+")
    co_authors <- ifelse(length(names) == 0, NA, paste(names, collapse = ", "))
    co_author_emails <- ifelse(length(emails) == 0, NA, paste(emails, collapse = ", "))
    article_data$Co_Authors <- c(article_data$Co_Authors, co_authors)
    article_data$Co_Author_Emails <- c(article_data$Co_Author_Emails, co_author_emails)
    
    # Extract Author names
    authors <- article_page %>% html_nodes("a[data-test='author-name']") %>% html_text()
    author_text <- ifelse(length(authors) == 0, NA, paste(authors, collapse = ", "))
    article_data$Author_name <- c(article_data$Author_name, author_text)
  }
}

# Create a data frame with the scraped data
df <- data.frame(
  Title = article_data$Title,
  Publish_Date = article_data$Publish_Date,
  Extract1 = article_data$Extract1,
  Keywords = article_data$Keywords,
  Co_Authors = article_data$Co_Authors,
  Co_Author_Emails = article_data$Co_Author_Emails,
  Author_name = article_data$Author_name
)

# Write the data to a CSV file
write.csv(df, "sample_data_5_years_sorted.csv", row.names = FALSE)

# Confirm that the file has been written successfully
cat("Sorted data has been saved to 'sample_data_5_years_sorted.csv'")

# Open the sorted data frame in the data viewer
View(df)


# Assuming 'df' is your dataframe

# Delete rows with empty cells
df <- subset(df, complete.cases(df))

View(df)

#FINDING OUT THE DATA STRUCTURE TO VISUALISE
library(data.table)

# Define a custom function to read the file with multi-character separator
fread_multisep <- function(file, sep, ncols, ...) {
  # Read the file as a single character string
  lines <- readLines(file)
  # Split each line using the multi-character separator
  lines <- strsplit(lines, sep, fixed = TRUE)
  # Pad or trim each line to ensure consistency in number of elements
  lines <- lapply(lines, function(x) {
    if (length(x) < ncols) {
      # If the number of elements is less than expected, pad the line with empty strings
      c(x, rep("", ncols - length(x)))
    } else if (length(x) > ncols) {
      # If the number of elements is more than expected, trim the line to the expected number of columns
      x[1:ncols]
    } else {
      x
    }
  })
  # Convert the list to a data.table
  data <- as.data.table(do.call(rbind, lines))
  # Return the data.table
  return(data)
}

# Read the CSV file with the correct separator using the custom function
data <- fread_multisep("sample_data_5_years.csv", sep = "#@#", ncols = 7, header = FALSE, stringsAsFactors = FALSE)

# Set column names
colnames(data) <- c("Title", "Publish_Date", "Extract1", "Keywords", "Co_Authors", "Co_Author_Emails", "Author_name")

# Display the structure of the data
str(data)

# 1st Data Representation and Visualization using Histograms
library(stringr)
library(dplyr)
library(ggplot2)

# Extract keywords from the data and combine them into a single character string
all_keywords <- data %>%
  pull(Keywords) %>%
  str_split(",\\s*") %>%
  unlist()

# Count the frequency of each keyword
keyword_freq <- table(all_keywords)

# Convert the frequency table to a data frame
keyword_freq_df <- as.data.frame(keyword_freq, stringsAsFactors = FALSE)
colnames(keyword_freq_df) <- c("Keyword", "Frequency")

# Order the data frame by frequency and select the top 10 keywords
top_10_keywords <- keyword_freq_df %>%
  arrange(desc(Frequency)) %>%
  head(10)

# Create a bar chart
ggplot(top_10_keywords, aes(x = reorder(Keyword, Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 10 Keywords in Scraped Articles",
       x = "Keyword",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()


#2nd Data Visualization through Scatter plot
library(ggplot2)

# Assuming 'df' is your dataframe with the 'Publish_Date' column
# Convert 'Publish_Date' to Date format if it's not already in that format
df$Publish_Date <- as.Date(df$Publish_Date)

# Count the frequency of paper publishing by date
article_counts <- table(df$Publish_Date)

# Convert the frequency table to a data frame
article_counts_df <- data.frame(Date = as.Date(names(article_counts)),
                                Count = as.numeric(article_counts))

# Create scatter plot
ggplot(article_counts_df, aes(x = Date, y = Count)) +
  geom_point() +
  labs(title = "Number of Articles Published Over Time",
       x = "Date",
       y = "Number of Articles")



#3rd Data Visualization through DoughNut graph
library(ggplot2)

# Assuming 'df' is your dataframe with the 'Publish_Date' column
# Convert 'Publish_Date' to Date format if it's not already in that format
df$Publish_Date <- as.Date(df$Publish_Date)

# Count the frequency of paper publishing by year
paper_freq <- table(format(df$Publish_Date, "%Y"))

# Calculate percentages
paper_percent <- round(100 * paper_freq / sum(paper_freq), 1)

# Convert the frequency and percentage table to a data frame
paper_freq_df <- data.frame(
  Year = names(paper_freq),
  Frequency = as.numeric(paper_freq),
  Percent = paper_percent
)

# Define custom color palette ranging from brown to red via shades of orange and yellow
custom_palette <- c("#8C510A", "#D8B365", "#F6E8C3", "#FEE08B", "#FDAE61", "#F46D43", "#D73027")

# Create a doughnut chart with custom colors
ggplot(paper_freq_df, aes(x = "", y = Frequency, fill = Year)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.position = "right") +
  scale_fill_manual(values = custom_palette) +  # Set custom colors
  labs(title = "Frequency of Paper Publishing by Year",
       caption = paste0("Percentages: ", paste(paper_percent, collapse = ", ")))

