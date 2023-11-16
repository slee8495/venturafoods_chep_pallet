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
    rename_with(~ paste0(., "_as400"))
}

# jde cleaning function
jde_cleaning <- function(df) {
 
  df_2 <- df %>%
    clean_names() %>%
    slice(-1:-3)
  colnames(df_2) <- df_2[1, ]
  
  df_2 %>%
    clean_names() %>%
    slice(-1) %>%
    select(branch_plant, customer_po_number, na_4, actual_ship_date, receipt_date) %>%
    rename(plt_qty = na_4,
           ship_location = branch_plant) %>%
    mutate(actual_ship_date = mdy(actual_ship_date),
           receipt_date = mdy(receipt_date)) %>%
    mutate(ship_or_receipt = ifelse(is.na(actual_ship_date), "Receipt", "Ship")) %>%
    rename_with(~ paste0(., "_jde"))
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
    rename_with(~ paste0(., "_chep"))
  
  chep_2
}


# comparing chep x as400 [based on chep]
chep_as400_based_on_chep <- function(chep_df, as400_df) {
  chep_df %>%
    data.frame() %>%  
    left_join(as400_df %>% select(bill_of_lading_as400, ship_location_as400, plt_qty_as400), 
              by = c("bill_of_lading_chep" = "bill_of_lading_as400")) %>%
    mutate(plt_qty_chep_plt_qty_as400 = plt_qty_chep - plt_qty_as400) %>%
    select(-customer_po_number_chep) %>%
    mutate(match = ifelse(plt_qty_chep_plt_qty_as400 == 0, "Y", "N"))
}


# comparing chep x as400 [based on as400]
chep_as400_based_on_as400 <- function(as400_df, chep_df) {
  as400_df %>%
    data.frame() %>% 
    left_join(chep_df, by = c("bill_of_lading_as400" = "bill_of_lading_chep")) %>%
    mutate(plt_qty_chep_plt_qty_as400 = plt_qty_chep - plt_qty_as400) %>%
    mutate(match = ifelse(plt_qty_chep_plt_qty_as400 == 0, "Y", "N"))
}


# comparing chep x jde [based on chep]
chep_jde_based_on_chep <- function(chep_df, jde_df) {
  chep_df %>%
    data.frame() %>%  
    left_join(jde_df, by = c("customer_po_number_chep" = "customer_po_number_jde")) %>%
    select(-bill_of_lading_chep) %>%
    mutate(plt_qty_chep_plt_qty_jde = plt_qty_chep - plt_qty_jde) %>%
    mutate(match = ifelse(plt_qty_chep_plt_qty_jde == 0, "Y", "N"))
}


# comparing chep x jde [based on jde]
merge_jde_chep <- function(jde_df, chep_df) {
  jde_df %>%
    data.frame() %>%  # Ensure it's a data frame
    left_join(chep_df, by = c("customer_po_number_jde" = "customer_po_number_chep")) %>%
    mutate(plt_qty_chep_plt_qty_jde = plt_qty_chep - plt_qty_jde) %>%
    mutate(match = ifelse(plt_qty_chep_plt_qty_jde == 0, "Y", "N"))
}



