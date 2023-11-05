library(npi)
library(dplyr)
library(purrr)

# Define the list of ZIP codes
zip_codes <- c(
  "95301", "95303", "95304", "95305", "95307", "95309", "95310", "95312",
  "95313", "95315", "95316", "95317", "95318", "95319", "95320", "95321",
  "95322", "95323", "95324", "95325", "95326", "95327", "95328", "95329",
  "95330", "95333", "95334", "95335", "95336", "95337", "95338", "95340",
  "95341", "95343", "95344", "95345", "95348", "95350", "95351", "95352",
  "95353", "95354", "95355", "95356", "95357", "95358", "95360", "95361",
  "95363", "95364", "95365", "95366", "95367", "95368", "95369", "95370",
  "95372", "95373", "95374", "95375", "95376", "95377", "95378", "95379",
  "95380", "95381", "95382", "95383", "95385", "95386", "95387", "95388",
  "95389", "95391", "95397", "93610", "93612", "93620", "93622", "93635",
  "93637", "93901", "93902", "93905", "93906", "93907", "93908", "93912",
  "93915", "93920", "93921", "93922", "93923", "93924", "93925", "93926",
  "93927", "93928", "93930", "93932", "93933", "93940", "93942", "93943",
  "93944", "93950", "93953", "93954", "93955", "93960", "95001", "95003",
  "95005", "95006", "95007", "95010", "95017", "95018", "95019", "95033",
  "95041", "95060", "95061", "95062", "95063", "95064", "95065", "95066",
  "95067", "95073", "95076"
)

# Define a function to extract physician details for a ZIP code
fetch_data <- function(zip) {
  results <- npi_search(postal_code = zip, limit = 1200)
}

# Fetch data for all ZIP codes
all_data <- map_df(zip_codes, fetch_data) 

all_data_filtered <- all_data

physicians <- npi_flatten(all_data_filtered) %>%
  filter(
    addresses_address_purpose == "LOCATION",
    # taxonomies_state == "CA"
  ) %>%
  select(
    npi, credential = basic_credential, first_name = basic_first_name, 
    last_name = basic_last_name, gender = basic_gender,
    last_update = basic_last_updated, Specialty = taxonomies_desc, 
    True_Specialty = taxonomies_primary, taxonomies_license, 
    address = addresses_address_1, city = addresses_city,
    state = addresses_state, zip_code = addresses_postal_code, 
    phone_number = addresses_telephone_number
  )

# Assuming your data frame is named df
df_wide <- physicians %>%
    filter(!is.na(last_name)) %>%
    # Assign a rank with TRUE specialties first
    mutate(Specialty_rank = if_else(True_Specialty, 1, 2)) %>%
    # Arrange by npi, specialty rank, and last_update
    arrange(npi, Specialty_rank, last_update) %>%
    distinct(npi, last_name, Specialty, .keep_all = T) %>%
    # Group by npi and specialty to ensure uniqueness
    group_by(npi, Specialty) %>%
    # Ensure that each specialty for an npi is unique
    filter(row_number() == 1) %>%
    ungroup() %>%
    # Group by npi to create a sequence for each specialty
    group_by(npi) %>%
    # Create a new sequence number for each specialty within each npi
    mutate(Specialty_seq = row_number()) %>%
    ungroup() %>%
    # Spread the specialties into separate columns
    pivot_wider(
        id_cols = c(npi, credential, first_name, last_name, gender, last_update, taxonomies_license),
        names_from = Specialty_seq,
        names_prefix = "Specialty",
        values_from = Specialty
    ) %>%
    # Remove duplicate npi rows, keeping the first occurrence
    distinct(npi, .keep_all = TRUE) %>%
    # Reshape to long, remove NAs, and pivot wider again to close gaps
    pivot_longer(
        cols = starts_with("Specialty"),
        names_to = "Specialty_key",
        values_to = "Specialty_value",
        values_drop_na = TRUE
    ) %>%
    group_by(npi) %>%
    mutate(Specialty_seq = row_number()) %>%
    pivot_wider(
        id_cols = c(npi, credential, first_name, last_name, gender, last_update, taxonomies_license),
        names_from = Specialty_seq,
        names_prefix = "Specialty",
        values_from = Specialty_value
    ) %>%
    ungroup() %>%
    rename(
        True_Specialty = Specialty1
    ) 

final_data <- 
    physicians %>% 
    # removing organizations
    filter(!is.na(last_name)) %>%
    # Assign a rank with TRUE specialties first
    mutate(
        zip_code = substr(zip_code, 1, 5),
        Specialty_rank = if_else(True_Specialty, 1, 2)
    ) %>%
    # Arrange by npi, specialty rank, and last_update
    arrange(npi, Specialty_rank, last_update) %>%
    select(-c(True_Specialty, Specialty)) %>% 
    distinct(npi, .keep_all = T) %>%
    left_join(
        df_wide
    )

# add county grab
