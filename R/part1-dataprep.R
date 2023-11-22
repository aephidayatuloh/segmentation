library(tidyverse)
library(readxl)

# Hari ini ----------------------------------------------------------------

hari_ini <- as_date("2020-01-01")

# Import dan Data Praproses -----------------------------------------------

xl_file <- "data/retail_program.xlsx"
excel_sheets(xl_file)

member <- read_xlsx(path = xl_file, sheet = "member")
transaction <- read_xlsx(path = xl_file, sheet = "transaction")
subproduct <- read_xlsx(path = xl_file, sheet = "subproduct")

member |> 
  glimpse()

transaction |> 
  glimpse()

subproduct |> 
  glimpse()

member |> 
  print(width = Inf)

member <- member |> 
  mutate(
    JoinDate = as_date(JoinDate), 
    YoungestKidDOB = as_date(YoungestKidDOB)
  )
member

member <- member |> 
  mutate(
    tenure_months = interval(start = JoinDate, end = hari_ini)/dmonths(1), 
    youngest_kid_age_join = interval(YoungestKidDOB, JoinDate)/dyears(1)
  )

member |> 
  summary()

member <- member |> 
  select(-c(JoinDate, YoungestKidDOB))

transaction |> 
  print(width = Inf)

transaction <- transaction |> 
  mutate(
    TransactionDate = as_date(TransactionDate), 
    TransactionDatetime = ymd_hms(paste(TransactionDate, TransactionTime)), 
    total_amount = Qty*PricePerUnit
  ) |>  
  # print(width = Inf)
  mutate(
    transaction_id = TransactionDatetime |> 
      as.character() |> 
      str_remove_all(pattern = "[[:punct:][:blank:]]") |> 
      paste(MemberID, sep = "-"), 
    .before = MemberID
  )

transaction |> 
  head() |> 
  print(width = Inf)



# Recency -----------------------------------------------------------------

r <- transaction |> 
  group_by(MemberID) |> 
  summarise(
    latest_transaction = max(TransactionDatetime)
  ) |> 
  transmute(
    MemberID, 
    recency = interval(latest_transaction, hari_ini)/ddays(1)
  )

r |> 
  summary()


# Frequency ---------------------------------------------------------------

f <- transaction |> 
  count(MemberID, transaction_id) |> 
  count(MemberID, name = "frequency")
f |> 
  summary()

avg_f <- transaction |>
  mutate(month_transaction = month(TransactionDatetime)) |> 
  count(MemberID, month_transaction, transaction_id) |> 
  count(MemberID, month_transaction) |> 
  group_by(MemberID) |> 
  summarise(
    avg_monthly_frequency = mean(n)
  )
avg_f

transaction |> 
  filter(MemberID == 5667) |> 
  select(MemberID, TransactionDatetime)


# Monetary ----------------------------------------------------------------

transaction |> 
  group_by(MemberID) |> 
  summarise(
    monetary = sum(total_amount)
  )

m <- transaction |> 
  group_by(MemberID) |> 
  summarise(
    monetary = sum(total_amount)
  )


# Average Amount Transaction ----------------------------------------------

transaction |> 
  count(transaction_id, sort = TRUE) |> 
  head() |> 
  print(width = Inf)

transaction |> 
  filter(MemberID == 35600) |> 
  select(TransactionDatetime, FKProductID, FKSubProductID, Qty, total_amount) |> 
  print(n = Inf, width = Inf)

transaction |> 
  filter(MemberID == 35600) |> 
  group_by(MemberID, transaction_id) |> 
  summarise(
    total_amount = sum(total_amount), 
    .groups = "drop_last"
  ) |> 
  print(n = Inf, width = Inf)

transaction |> 
  filter(MemberID == 35600) |> 
  group_by(MemberID, transaction_id) |> 
  summarise(
    total_amount = sum(total_amount), 
    .groups = "drop_last"
  ) |> 
  summarise(
    avg_monetary = mean(total_amount)
  ) |> 
  print(n = Inf, width = Inf)

avg_monetary <- transaction |> 
  group_by(MemberID, transaction_id) |> 
  summarise(
    total_amount = sum(total_amount), 
    .groups = "drop_last"
  ) |> 
  summarise(
    avg_monetary = mean(total_amount)
  )
avg_monetary


# Average Inter-Purchase Time ---------------------------------------------

transaction |> 
  mutate(TransactionDate = as_date(TransactionDatetime)) |> 
  print(width = Inf)

transaction |> 
  mutate(TransactionDate = as_date(TransactionDatetime)) |> 
  arrange(MemberID, TransactionDate, transaction_id) |> 
  print(width = Inf)

transaction |> 
  mutate(TransactionDate = as_date(TransactionDatetime)) |> 
  arrange(MemberID, TransactionDate, transaction_id) |> 
  distinct(transaction_id, .keep_all = TRUE) |> 
  print(width = Inf)

transaction |> 
  mutate(TransactionDate = as_date(TransactionDatetime)) |> 
  arrange(MemberID, TransactionDate, transaction_id) |> 
  distinct(transaction_id, .keep_all = TRUE) |> 
  group_by(MemberID) |> 
  mutate(
    next_transaction = lead(TransactionDate)
  ) |> 
  ungroup() |> 
  print(width = Inf)

interpurchase <- transaction |> 
  mutate(TransactionDate = as_date(TransactionDatetime)) |> 
  arrange(MemberID, TransactionDate, transaction_id) |> 
  distinct(transaction_id, .keep_all = TRUE) |> 
  group_by(MemberID) |> 
  mutate(
    next_transaction = lead(TransactionDate)
  ) |> 
  ungroup()

interpurchase |> 
  slice(1:50) |>
  mutate(
    interpurchase = interval(TransactionDate, next_transaction)/ddays(1)
  ) |> 
  select(MemberID, TransactionDate, next_transaction, interpurchase) |> 
  print(width = Inf)

interpurchase |> 
  slice(1:50) |>
  mutate(
    interpurchase = interval(TransactionDate, next_transaction)/ddays(1)
  ) |> 
  select(MemberID, TransactionDate, next_transaction, interpurchase) |> 
  group_by(MemberID) |> 
  summarise(
    avg_interpurchase = mean(interpurchase, na.rm = TRUE)
  ) |> 
  print(width = Inf)

interpurchase |> 
  slice(1:50) |>
  mutate(
    interpurchase = interval(TransactionDate, next_transaction)/ddays(1)
  ) |> 
  group_by(MemberID) |> 
  summarise(
    avg_interpurchase = mean(interpurchase, na.rm = TRUE)
  ) |> 
  mutate(
    avg_interpurchase = case_when(
      is.na(avg_interpurchase) ~ 0, 
      TRUE ~ avg_interpurchase
    )
  ) |> 
  print(width = Inf)

interpurchase <- interpurchase |> 
  mutate(
    interpurchase = interval(TransactionDate, next_transaction)/ddays(1)
  ) |> 
  group_by(MemberID) |> 
  summarise(
    avg_interpurchase = mean(interpurchase, na.rm = TRUE)
  ) |> 
  mutate(
    avg_interpurchase = case_when(
      is.na(avg_interpurchase) ~ 0, 
      TRUE ~ avg_interpurchase
    )
  )


# Transaction Frequency within last 3 months  -----------------------------

transaction |> 
  filter(month(TransactionDatetime) %in% 10:12) |> 
  count(MemberID, transaction_id) |> 
  head() |> 
  print(n = Inf, width = Inf)

transaction |> 
  filter(month(TransactionDatetime) %in% 10:12) |> 
  count(MemberID, transaction_id) |> 
  count(MemberID, name = "freq_last3mo") |> 
  head() |> 
  print(n = Inf, width = Inf)

last_3mo <- transaction |> 
  filter(month(TransactionDatetime) %in% 10:12) |> 
  count(MemberID, transaction_id) |> 
  count(MemberID, name = "freq_last3mo") 


# Consumption -------------------------------------------------------------

consumption <- transaction |> 
  left_join(
    subproduct, 
    by = join_by(FKSubProductID == SubProductID)
  ) |> 
  mutate(
    consumption = Qty*Weight
  )


# Average Consumption -----------------------------------------------------

avg_consumption <- consumption |> 
  group_by(MemberID, transaction_id) |> 
  summarise(
    TotalConsumption = sum(consumption)
  ) |> 
  group_by(MemberID) |> 
  summarise(
    avg_consumption = mean(TotalConsumption)
  )


# Merge All Tables --------------------------------------------------------

abt <- r |> 
  full_join(f, by = "MemberID") |> 
  full_join(m, by = "MemberID") |> 
  full_join(avg_f, by = "MemberID") |> 
  full_join(avg_monetary, by = "MemberID") |> 
  full_join(last_3mo, by = "MemberID") |> 
  full_join(interpurchase, by = "MemberID") |> 
  full_join(avg_consumption, by = "MemberID") |> 
  mutate(
    freq_last3mo = case_when(is.na(freq_last3mo) ~ 0, 
                             TRUE ~ freq_last3mo), 
    avg_consumption = case_when(is.na(avg_consumption) ~ 0, 
                                TRUE ~ avg_consumption)
  )

abt <- member |> 
  right_join(abt, by = "MemberID")

abt |> 
  summary()


# Export ------------------------------------------------------------------

abt |> 
  write_csv(file = "data/customer_segmentation.csv", na = "")
# file.show("data/customer_segmentation.csv")
