library(tidyverse)
library(magrittr)
library(openxlsx)
library(readxl)
library(writexl)
library(skimr)
library(janitor)
library(lubridate)

as400 <- read_xlsx("C:/Users/slee/OneDrive - Ventura Foods/Ventura Work/SCE/Project/FY 24/CHEP Pallet/7th sample/as400.xlsx")
jde <- read_xlsx("C:/Users/slee/OneDrive - Ventura Foods/Ventura Work/SCE/Project/FY 24/CHEP Pallet/7th sample/jde.xlsx")
chep <- read_xlsx("C:/Users/slee/OneDrive - Ventura Foods/Ventura Work/SCE/Project/FY 24/CHEP Pallet/7th sample/chep.xlsx")



## as400 data clean (create function)
parse_as400_dates <- function(date_column) {
  
  parsed_dates <- mdy(date_column, quiet = TRUE)
  
  failed_indices <- which(is.na(parsed_dates))
  parsed_dates[failed_indices] <- ymd(date_column[failed_indices], quiet = TRUE)
  
  return(parsed_dates)
}


## as400 data clean (create function)
as400 %>% 
  clean_names() %>%
  slice(-1:-2) %>%
  select(ship_location, bill_of_lading, order_number, loc_bol_number, chep, ship_date) %>%
  mutate(order_number = str_remove(order_number, ".*-"), 
         plt_qty = chep,
         ship_location = ship_location,
         ship_date = parse_as400_dates(ship_date),
         bill_of_lading = as.character(bill_of_lading)) %>%
  select(-loc_bol_number) %>%
  rename_with(~ paste0(., "_as400")) %>%
  
  # Re-group and summarize
  group_by(bill_of_lading_as400, ship_location_as400, ship_date_as400, order_number_as400) %>% 
  summarise(plt_qty_as400 = sum(plt_qty_as400, na.rm = TRUE)) %>%
  
  relocate(ship_location_as400, order_number_as400, bill_of_lading_as400, ship_date_as400, plt_qty_as400)-> as400_2

## jde data clean (create function)

jde %>% 
  janitor::clean_names() %>% 
  dplyr::slice(-1:-3) -> jde_2

colnames(jde_2) <- jde_2[1, ]

jde_2 %>% 
  janitor::clean_names() %>% 
  dplyr::slice(-1) %>% 
  mutate(branch_plant = gsub("[^0-9]", "", branch_plant)) %>% 
  mutate(branch_plant = as.numeric(branch_plant)) %>%
  dplyr::select(branch_plant, customer_po_number, number_of_pallets, actual_ship_date, receipt_date) %>% 
  dplyr::rename(plt_qty = number_of_pallets,
                ship_location = branch_plant) %>%
  dplyr::mutate(actual_ship_date = lubridate::mdy(actual_ship_date),
                receipt_date = lubridate::mdy(receipt_date)) %>% 
  dplyr::mutate(ship_or_receipt = ifelse(is.na(actual_ship_date), "Receipt", "Ship")) %>% 
  dplyr::mutate(plt_qty = as.numeric(plt_qty)) %>% 
  dplyr::rename_with(~ paste0(., "_jde")) %>% 
  
  # re-group
  dplyr::group_by(customer_po_number_jde, actual_ship_date_jde, receipt_date_jde, ship_location_jde, ship_or_receipt_jde) %>%
  dplyr::summarise(plt_qty_jde = sum(plt_qty_jde)) %>%
  
  dplyr::relocate(ship_location_jde, customer_po_number_jde, actual_ship_date_jde, receipt_date_jde, ship_or_receipt_jde, plt_qty_jde) -> jde_2


## chep data clean (create function)
chep[-1:-13,] -> chep_1
colnames(chep_1) <- chep_1[1, ]
chep_1[-1, ] -> chep_1

chep_1 %>% 
  janitor::clean_names() %>% 
  dplyr::rename(plt_qty = quantity, reference_2 = reference2, reference_3 = reference3) %>%
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
  relocate(ship_location_chep, sender_name_chep, receipt_location_chep, receiver_name_chep, customer_po_number_chep, bill_of_lading_chep, plt_qty_chep) %>% 
  dplyr::mutate(receipt_location_chep = ifelse(is.na(receipt_location_chep), "0", receipt_location_chep)) -> chep_2



#### comparing chep x as400

# Based on Chep Data
chep_2 %>% 
  data.frame() %>% 
  dplyr::left_join(as400_2 %>% dplyr::select(bill_of_lading_as400, ship_location_as400, plt_qty_as400, order_number_as400) %>% 
                     mutate(bill_of_lading_as400_2 = bill_of_lading_as400), by = c("bill_of_lading_chep" = "bill_of_lading_as400_2")) %>% 
  dplyr::mutate(plt_qty_as400 = ifelse(is.na(plt_qty_as400), 0, plt_qty_as400),
                plt_qty_chep = ifelse(is.na(plt_qty_chep), 0, plt_qty_chep)) %>% 
  dplyr::mutate(plt_qty_chep_plt_qty_as400 = plt_qty_chep - plt_qty_as400) %>% 
  dplyr::mutate(Match = ifelse(plt_qty_chep_plt_qty_as400 == 0, "Y", "N")) %>% 
  dplyr::filter(!is.na(ship_location_as400)) %>% 
  rename("Ship Location (CHEP)" = ship_location_chep,
         "Sender Name (CHEP)" = sender_name_chep,
         "Receipt Location (CHEP)" = receipt_location_chep,
         "Receiver Name (CHEP)" = receiver_name_chep,
         "Bill of Lading (CHEP)" = bill_of_lading_chep,
         "Plt Qty (CHEP)" = plt_qty_chep,
         "Customer PO # (CHEP)" = customer_po_number_chep,
         "Ship Location (Legacy)" = ship_location_as400,
         "Plt Qty (Legacy)" = plt_qty_as400,
         "Plt Qty (CHEP) - Plt Qty (Legacy)" = plt_qty_chep_plt_qty_as400,
         "Bill of Lading (Legacy)" = bill_of_lading_as400,
         "Legacy Order # (Legacy)" = order_number_as400) -> chep_as400

# Based on as400 data
as400_2 %>% 
  data.frame() %>% 
  dplyr::left_join(chep_2 %>% 
                     mutate(bill_of_lading_chep_2 = bill_of_lading_chep), by = c("bill_of_lading_as400" = "bill_of_lading_chep_2")) %>% 
  dplyr::mutate(plt_qty_as400 = ifelse(is.na(plt_qty_as400), 0, plt_qty_as400),
                plt_qty_chep = ifelse(is.na(plt_qty_chep), 0, plt_qty_chep)) %>% 
  dplyr::mutate(plt_qty_chep_plt_qty_as400 = plt_qty_chep - plt_qty_as400) %>% 
  dplyr::mutate(Match = ifelse(plt_qty_chep_plt_qty_as400 == 0, "Y", "N")) %>% 
  rename("Ship Location (Legacy)" = ship_location_as400,
         "Bill of Lading (Legacy)" = bill_of_lading_as400,
         "Ship Date (Legacy)" = ship_date_as400,
         "Plt Qty (Legacy)" = plt_qty_as400,
         "Legacy Order # (Legacy)" = order_number_as400,
         "Ship Location (CHEP)" = ship_location_chep,
         "Sender Name (CHEP)" = sender_name_chep,
         "Receipt Location (CHEP)" = receipt_location_chep,
         "Receiver Name (CHEP)" = receiver_name_chep,
         "Plt Qty (CHEP)" = plt_qty_chep,
         "Plt Qty (CHEP) - Plt Qty (Legacy)" = plt_qty_chep_plt_qty_as400,
         "Bill of Lading (CHEP)" = bill_of_lading_chep,
         "Customer PO # (CHEP)" = customer_po_number_chep) -> as400_chep




#### comparing chep x jde
chep_2 %>% 
  data.frame() %>%
  dplyr::left_join(jde_2 %>% 
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
         "Customer PO # (JDE)" = customer_po_number_jde)-> chep_jde


# Based on jde data
jde_2 %>% 
  data.frame() %>% 
  dplyr::left_join(chep_2 %>% 
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
         "Customer PO # (CHEP)" = customer_po_number_chep) -> jde_chep