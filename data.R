library(tidyverse)
library(magrittr)
library(openxlsx)
library(readxl)
library(writexl)
library(skimr)
library(janitor)
library(lubridate)

as400 <- read_csv("as400.csv")
jde <- read_csv("jde.csv")
chep <- read_csv("chep.csv")


## as400 data clean (create function)
as400 %>% 
  janitor::clean_names() %>% 
  dplyr::slice(-1:-2) %>% 
  dplyr::select(ship_location, bill_of_lading, loc_bol_number, chep, ship_date) %>% 
  dplyr::rename(plt_qty = chep,
                ship_location = ship_location) %>% 
  dplyr::mutate(ship_date = lubridate::ymd(ship_date)) %>% 
  dplyr::mutate(bill_of_lading = as.character(bill_of_lading)) %>% 
  dplyr::select(-loc_bol_number) %>% 
  dplyr::rename_with(~ paste0(., "_as400")) %>% 
  dplyr::relocate(-plt_qty_as400) -> as400_2

## jde data clean (create function)

jde %>% 
  janitor::clean_names() %>% 
  dplyr::slice(-1:-3) -> jde_2

colnames(jde_2) <- jde_2[1, ]

jde_2 %>% 
  janitor::clean_names() %>% 
  dplyr::slice(-1) %>% 
  dplyr::select(branch_plant, customer_po_number, na_4, actual_ship_date, receipt_date) %>% 
  dplyr::rename(plt_qty = na_4,
                ship_location = branch_plant) %>% 
  dplyr::mutate(actual_ship_date = lubridate::mdy(actual_ship_date),
                receipt_date = lubridate::mdy(receipt_date)) %>% 
  dplyr::mutate(ship_or_receipt = ifelse(is.na(actual_ship_date), "Receipt", "Ship")) %>% 
  dplyr::rename_with(~ paste0(., "_jde")) %>% 
  dplyr::relocate(-plt_qty_jde) -> jde_2


## chep data clean (create function)
chep %>% 
  dplyr::slice(-1:-10) %>% 
  janitor::clean_names() %>% 
  dplyr::select(-1)  %>% 
  dplyr::rename_with(~ "a", everything()[1]) %>%
  dplyr::slice(-1) %>% 
  tidyr::separate(a, c("product_code", "sender_global_id", "sender_external_code", "sender_name", "sender_town", "sender_country",
                       "receiver_global_id", "receiver_external_code", "receiver_name", "receiver_town", "receiver_country", "plt_qty",
                       "transaction_status", "reason_code", "reason_description", "reference_1", "reference_2", "reference_3",
                       "date_of_notification", "movement_date", "pricing_date", "transaction_type", "corr_rev"), sep = ",") %>% 
  dplyr::select(sender_name, receiver_name, plt_qty, reference_2, reference_3) %>% 
  dplyr::mutate(reference_2 = as.double(reference_2),
                reference_3 = as.double(reference_3)) %>% 
  dplyr::mutate(ship_location = stringr::str_sub(reference_2, 1, nchar(reference_2) -5),
                bill_of_lading = stringr::str_sub(reference_2, -5)) -> chep_2

chep_2 %>% 
  dplyr::select(sender_name, ship_location) %>% 
  unique() %>% 
  dplyr::rename(receiver_name = sender_name,
                receipt_location = ship_location) -> chep_3

chep_2 %>% 
  dplyr::left_join(chep_3) %>% 
  dplyr::rename(customer_po_number = reference_3) %>% 
  dplyr::select(-reference_2) %>% 
  dplyr::relocate(ship_location, sender_name, receipt_location, receiver_name, customer_po_number, bill_of_lading, plt_qty) %>% 
  dplyr::mutate(plt_qty = as.double(plt_qty)) %>% 
  dplyr::mutate(customer_po_number = as.character(customer_po_number)) %>% 
  dplyr::rename_with(~ paste0(., "_chep")) -> chep_2



#### comparing chep x as400

# Based on Chep Data
chep_2 %>% 
  data.frame() %>% 
  dplyr::left_join(as400_2 %>% dplyr::select(bill_of_lading_as400, ship_location_as400, plt_qty_as400), by = c("bill_of_lading_chep" = "bill_of_lading_as400")) %>% 
  dplyr::mutate(plt_qty_as400 = ifelse(is.na(plt_qty_as400), 0, plt_qty_as400),
                plt_qty_chep = ifelse(is.na(plt_qty_chep), 0, plt_qty_chep)) %>% 
  dplyr::mutate(plt_qty_chep_plt_qty_as400 = plt_qty_chep - plt_qty_as400) %>% 
  dplyr::select(-customer_po_number_chep) %>% 
  dplyr::mutate(Match = ifelse(plt_qty_chep_plt_qty_as400 == 0, "Y", "N")) %>% 
  rename("Ship Location (CHEP)" = ship_location_chep,
         "Sender Name (CHEP)" = sender_name_chep,
         "Receipt Location (CHEP)" = receipt_location_chep,
         "Receiver Name (CHEP)" = receiver_name_chep,
         "Bill of Lading (CHEP)" = bill_of_lading_chep,
         "Plt Qty (CHEP)" = plt_qty_chep,
         "Ship Location (Legacy)" = ship_location_as400,
         "Plt Qty (Legacy)" = plt_qty_as400,
         "Plt Qty (CHEP) - Plt Qty (Legacy)" = plt_qty_chep_plt_qty_as400) -> chep_as400

# Based on as400 data
as400_2 %>% 
  data.frame() %>% 
  dplyr::left_join(chep_2, by = c("bill_of_lading_as400" = "bill_of_lading_chep")) %>% 
  dplyr::mutate(plt_qty_as400 = ifelse(is.na(plt_qty_as400), 0, plt_qty_as400),
                plt_qty_chep = ifelse(is.na(plt_qty_chep), 0, plt_qty_chep)) %>% 
  dplyr::mutate(plt_qty_chep_plt_qty_as400 = plt_qty_chep - plt_qty_as400) %>% 
  dplyr::mutate(Match = ifelse(plt_qty_chep_plt_qty_as400 == 0, "Y", "N")) %>% 
  select(-customer_po_number_chep) %>% 
  rename("Ship Location (Legacy)" = ship_location_as400,
         "Bill of Lading (Legacy)" = bill_of_lading_as400,
         "Ship Date (Legacy)" = ship_date_as400,
         "Plt Qty (Legacy)" = plt_qty_as400,
         "Ship Location (CHEP)" = ship_location_chep,
         "Sender Name (CHEP)" = sender_name_chep,
         "Receipt Location (CHEP)" = receipt_location_chep,
         "Receiver Name (CHEP)" = receiver_name_chep,
         "Plt Qty (CHEP)" = plt_qty_chep,
         "Plt Qty (CHEP) - Plt Qty (Legacy)" = plt_qty_chep_plt_qty_as400) -> as400_chep


  

#### comparing chep x jde
chep_2 %>% 
  data.frame() %>% 
  dplyr::left_join(jde_2, by = c("customer_po_number_chep" = "customer_po_number_jde"))  %>% 
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
         "Plt Qty (CHEP) - Plt Qty (JDE)" = plt_qty_chep_plt_qty_jde)-> chep_jde


# Based on jde data
jde_2 %>% 
  data.frame() %>% 
  dplyr::left_join(chep_2, by = c("customer_po_number_jde" = "customer_po_number_chep")) %>% 
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
         "Plt Qty (CHEP) - Plt Qty (JDE)" = plt_qty_chep_plt_qty_jde) -> jde_chep


