# lib
library(tidyverse)
library(tabulizer)

# retrieve data
url <- "https://basebien.com/PNSPublic/DocPublic/Historiquedesprixdesappartementspardep.pdf"

raw_df_list <- extract_tables(url)

parse_raw <- function(mat) {
    nr <- nrow(mat)
    col_sel <- c(2, 4:14)
    colname <- mat[1, col_sel] %>%
        str_squish()

    df <- mat[2:nr, col_sel] %>%
        as_tibble()
    colnames(df) <- colname

    df <- df %>%
        mutate_at(
            .vars = vars(`Ile de France`:"Val-d'Oise"),
            .funs = ~ str_remove(., " ")
        ) %>%
        mutate_at(
            .vars = vars(`Ile de France`:"Val-d'Oise"),
            .funs = ~ parse_number(.)
        )


    return(df)
}

# parse_raw(raw_df_list[[1]])

df <- lapply(raw_df_list, parse_raw) %>%
    bind_rows() %>%
    mutate(
        Trimestre = str_replace(Trimestre, "T", "Q"),
        Trimestre = paste0(str_sub(Trimestre, 4, 7), str_sub(Trimestre, 1, 2)),
        date = lubridate::yq(Trimestre)
    ) %>%
    pivot_longer(cols = `Ile de France`:"Val-d'Oise")



ggplot(df, aes(y = value, x = date, color = name)) +
    geom_line() +
    scale_x_date(date_breaks = "3 years") +
    geom_vline(xintercept = as.Date("2020-01-01")) +
    theme_minimal() 
