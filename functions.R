library(tidyverse)
library(magrittr)
library(openxlsx)
library(readxl)
library(writexl)
library(skimr)
library(janitor)
library(lubridate)


# as400 cleaning function
as400_cleaning <- function(df) {
  df %>%
    clean_names() %>%
    slice(-1:-2) %>%
    select(ship_location, bill_of_lading, loc_bol_number, chep, ship_date) %>%
    rename(plt_qty = chep,
           ship_location = ship_location) %>%
    mutate(ship_date = ymd(ship_date),
           bill_of_lading = as.character(bill_of_lading)) %>%
    select(-loc_bol_number) %>%
    rename_with(~ paste0(., "_as400")) %>% 
    
    # re-group
    group_by(bill_of_lading_as400, ship_location_as400, ship_date_as400) %>% 
    summarise(plt_qty_as400 = sum(plt_qty_as400)) %>% 
    
    
    relocate(ship_location_as400, bill_of_lading_as400, ship_date_as400, plt_qty_as400)
}

# jde cleaning function
jde_cleaning <- function(df) {
 
  df_2 <- df %>%
    clean_names() %>%
    slice(-1:-3)
  colnames(df_2) <- df_2[1, ]
  
  df_2 %>%
    janitor::clean_names() %>% 
    dplyr::slice(-1) %>% 
    mutate(branch_plant = gsub("[^0-9]", "", branch_plant)) %>% 
    mutate(branch_plant = as.numeric(branch_plant)) %>%
    
    separate(number_number_of_pallets_actual_ship_date_receipt_date, c("a", "b", "c", "d", "e", "f"), sep = ",") %>% 
    
    dplyr::select(branch_plant, customer_po_number, b, c, d) %>% 
    dplyr::rename(plt_qty = b,
                  ship_location = branch_plant,
                  actual_ship_date = c,
                  receipt_date = d) %>% 
    dplyr::mutate(actual_ship_date = lubridate::mdy(actual_ship_date),
                  receipt_date = lubridate::mdy(receipt_date)) %>% 
    dplyr::mutate(ship_or_receipt = ifelse(is.na(actual_ship_date), "Receipt", "Ship")) %>% 
    dplyr::mutate(plt_qty = as.numeric(plt_qty)) %>% 
    dplyr::rename_with(~ paste0(., "_jde")) %>% 
    
    # re-group
    dplyr::group_by(customer_po_number_jde, actual_ship_date_jde, receipt_date_jde, ship_location_jde, ship_or_receipt_jde) %>%
    dplyr::summarise(plt_qty_jde = sum(plt_qty_jde)) %>%
    
    dplyr::relocate(ship_location_jde, customer_po_number_jde, actual_ship_date_jde, receipt_date_jde, ship_or_receipt_jde, plt_qty_jde)
    
    
}

# chep cleaning function
chep_cleaning <- function(df) {

  chep_2 <- df %>%
    slice(-1:-10) %>%
    clean_names() %>%
    select(-1) %>%
    rename_with(~ "a", everything()[1]) %>%
    slice(-1) %>%
    separate(a, c("product_code", "sender_global_id", "sender_external_code", "sender_name", "sender_town", "sender_country",
                  "receiver_global_id", "receiver_external_code", "receiver_name", "receiver_town", "receiver_country", "plt_qty",
                  "transaction_status", "reason_code", "reason_description", "reference_1", "reference_2", "reference_3",
                  "date_of_notification", "movement_date", "pricing_date", "transaction_type", "corr_rev"), sep = ",") %>%
    select(sender_name, receiver_name, plt_qty, reference_2, reference_3) %>%
    mutate(reference_2 = as.double(reference_2),
           reference_3 = as.double(reference_3),
           ship_location = str_sub(reference_2, 1, nchar(reference_2) - 5),
           bill_of_lading = str_sub(reference_2, -5))
  

  chep_3 <- chep_2 %>%
    select(sender_name, ship_location) %>%
    unique() %>%
    rename(receiver_name = sender_name,
           receipt_location = ship_location)
  

  chep_2 <- chep_2 %>%
    left_join(chep_3) %>%
    rename(customer_po_number = reference_3) %>%
    select(-reference_2) %>%
    relocate(ship_location, sender_name, receipt_location, receiver_name, customer_po_number, bill_of_lading, plt_qty) %>%
    mutate(plt_qty = as.double(plt_qty),
           customer_po_number = as.character(customer_po_number)) %>%
    rename_with(~ paste0(., "_chep")) %>% 
    
    # re-group
    group_by(customer_po_number_chep, bill_of_lading_chep, ship_location_chep, sender_name_chep, receipt_location_chep, receiver_name_chep) %>% 
    summarise(plt_qty_chep = sum(plt_qty_chep)) %>% 
    relocate(ship_location_chep, sender_name_chep, receipt_location_chep, receiver_name_chep, customer_po_number_chep, bill_of_lading_chep, plt_qty_chep)
    
}


# comparing chep x as400 [based on chep]
chep_as400_based_on_chep <- function(chep_df, as400_df) {
  chep_df %>%
    data.frame() %>% 
    dplyr::left_join(as400_df %>% dplyr::select(bill_of_lading_as400, ship_location_as400, plt_qty_as400) %>% 
                       mutate(bill_of_lading_as400_2 = bill_of_lading_as400), by = c("bill_of_lading_chep" = "bill_of_lading_as400_2")) %>% 
    dplyr::mutate(plt_qty_as400 = ifelse(is.na(plt_qty_as400), 0, plt_qty_as400),
                  plt_qty_chep = ifelse(is.na(plt_qty_chep), 0, plt_qty_chep)) %>% 
    dplyr::mutate(plt_qty_chep_plt_qty_as400 = plt_qty_chep - plt_qty_as400) %>% 
    dplyr::select(-customer_po_number_chep) %>% 
    dplyr::mutate(Match = ifelse(plt_qty_chep_plt_qty_as400 == 0, "Y", "N")) %>% 
    # dplyr::filter(!is.na(ship_location_as400)) %>%            ###### Watch Point ######
    rename("Ship Location (CHEP)" = ship_location_chep,
           "Sender Name (CHEP)" = sender_name_chep,
           "Receipt Location (CHEP)" = receipt_location_chep,
           "Receiver Name (CHEP)" = receiver_name_chep,
           "Bill of Lading (CHEP)" = bill_of_lading_chep,
           "Plt Qty (CHEP)" = plt_qty_chep,
           "Ship Location (Legacy)" = ship_location_as400,
           "Plt Qty (Legacy)" = plt_qty_as400,
           "Plt Qty (CHEP) - Plt Qty (Legacy)" = plt_qty_chep_plt_qty_as400,
           "Bill of Lading (Legacy)" = bill_of_lading_as400)
}


# comparing chep x as400 [based on as400]
chep_as400_based_on_as400 <- function(as400_df, chep_df) {
  as400_df %>%
    data.frame() %>% 
    dplyr::left_join(chep_df %>% 
                       mutate(bill_of_lading_chep_2 = bill_of_lading_chep), by = c("bill_of_lading_as400" = "bill_of_lading_chep_2")) %>% 
    dplyr::mutate(plt_qty_as400 = ifelse(is.na(plt_qty_as400), 0, plt_qty_as400),
                  plt_qty_chep = ifelse(is.na(plt_qty_chep), 0, plt_qty_chep)) %>% 
    dplyr::mutate(plt_qty_chep_plt_qty_as400 = plt_qty_chep - plt_qty_as400) %>% 
    dplyr::mutate(Match = ifelse(plt_qty_chep_plt_qty_as400 == 0, "Y", "N")) %>% 
    dplyr::select(-customer_po_number_chep) %>% 
    rename("Ship Location (Legacy)" = ship_location_as400,
           "Bill of Lading (Legacy)" = bill_of_lading_as400,
           "Ship Date (Legacy)" = ship_date_as400,
           "Plt Qty (Legacy)" = plt_qty_as400,
           "Ship Location (CHEP)" = ship_location_chep,
           "Sender Name (CHEP)" = sender_name_chep,
           "Receipt Location (CHEP)" = receipt_location_chep,
           "Receiver Name (CHEP)" = receiver_name_chep,
           "Plt Qty (CHEP)" = plt_qty_chep,
           "Plt Qty (CHEP) - Plt Qty (Legacy)" = plt_qty_chep_plt_qty_as400,
           "Bill of Lading (CHEP)" = bill_of_lading_chep)
}


# comparing chep x jde [based on chep]
chep_jde_based_on_chep <- function(chep_df, jde_df) {
  chep_df %>%
    data.frame() %>%
    dplyr::left_join(jde_df %>% 
                       mutate(customer_po_number_jde_2 = customer_po_number_jde), by = c("customer_po_number_chep" = "customer_po_number_jde_2"), relationship = "many-to-many")  %>% 
    dplyr::select(-bill_of_lading_chep) %>% 
    dplyr::mutate(plt_qty_jde = ifelse(is.na(plt_qty_jde), 0, plt_qty_jde),
                  plt_qty_chep = ifelse(is.na(plt_qty_chep), 0, plt_qty_chep)) %>% 
    dplyr::mutate(plt_qty_chep_plt_qty_jde = plt_qty_chep - plt_qty_jde) %>% 
    dplyr::mutate(Match = ifelse(plt_qty_chep_plt_qty_jde == 0, "Y", "N")) %>% 
    rename("Ship Location (CHEP)" = ship_location_chep,
           "Sender Name (CHEP)" = sender_name_chep,
           "Receipt Location (CHEP)" = receipt_location_chep,
           "Receiver Name (CHEP)" = receiver_name_chep,
           "Customer PO # (CHEP)" = customer_po_number_chep,
           "Plt Qty (CHEP)" = plt_qty_chep,
           "Ship Location (JDE)" = ship_location_jde,
           "Actual Ship Date (JDE)" = actual_ship_date_jde,
           "Receipt Date (JDE)" = receipt_date_jde,
           "Ship or Receipt (JDE)" = ship_or_receipt_jde,
           "Plt Qty (JDE)" = plt_qty_jde,
           "Plt Qty (CHEP) - Plt Qty (JDE)" = plt_qty_chep_plt_qty_jde,
           "Customer PO # (JDE)" = customer_po_number_jde)
}


# comparing chep x jde [based on jde]
chep_jde_based_on_jde <- function(jde_df, chep_df) {
  jde_df %>%
    data.frame() %>% 
    dplyr::left_join(chep_df %>% 
                       mutate(customer_po_number_chep_2 = customer_po_number_chep), by = c("customer_po_number_jde" = "customer_po_number_chep_2"), relationship = "many-to-many") %>% 
    dplyr::mutate(plt_qty_jde = ifelse(is.na(plt_qty_jde), 0, plt_qty_jde),
                  plt_qty_chep = ifelse(is.na(plt_qty_chep), 0, plt_qty_chep)) %>% 
    dplyr::relocate(-plt_qty_chep, -plt_qty_jde) %>% 
    dplyr::mutate(plt_qty_chep_plt_qty_jde = plt_qty_chep - plt_qty_jde) %>% 
    dplyr::mutate(Match = ifelse(plt_qty_chep_plt_qty_jde == 0, "Y", "N")) %>% 
    select(-bill_of_lading_chep) %>% 
    rename("Ship Location (JDE)" = ship_location_jde,
           "Customer PO # (JDE)" = customer_po_number_jde,
           "Actual Ship Date (JDE)" = actual_ship_date_jde,
           "Receipt Date (JDE)" = receipt_date_jde,
           "Ship or Receipt (JDE)" = ship_or_receipt_jde,
           "Plt Qty (JDE)" = plt_qty_jde,
           "Ship Location (CHEP)" = ship_location_chep,
           "Sender Name (CHEP)" = sender_name_chep,
           "Receipt Location (CHEP)" = receipt_location_chep,
           "Receiver Name (CHEP)" = receiver_name_chep,
           "Plt Qty (CHEP)" = plt_qty_chep,
           "Plt Qty (CHEP) - Plt Qty (JDE)" = plt_qty_chep_plt_qty_jde,
           "Customer PO # (CHEP)" = customer_po_number_chep)
}



