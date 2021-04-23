library(dplyr)
library(stringr)
library(openxlsx)
library(lubridate)
library(readxl)
# Baca File Export Dari Data Inbox
Inbox_EMAIL <- read_excel("~/Knowledge/Analisa_Request/Inbox.xlsx")
#Inbox_EMAIL <- Inbox
# From Tim MIS
Inbox_EMAIL_MIS <- filter(Inbox_EMAIL, `From` %in% c("Mohamad SEPTIAN FAUZI (400074)",
                                                    "Ria Astriana SURDIN",
                                                    "Saiful Anwar",
                                                    "Henri Bin Husni",
                                                    "Andika Mcenroe"))
# Yang dikirim TO atau Group DBSI T&O MIS CCTR
Handle_REQUEST <- filter(Inbox_EMAIL_MIS, grepl("DBSI T&O MIS CCTR", CC) | grepl("DBSI T&O MIS CCTR", To) )


#Fix Text Format (Remove line Feed, tab etc..)
Handle_REQUEST <- mutate(Handle_REQUEST, Contents = gsub("[\r\n\t]", "",Contents))

#Tipe email adalah Email Balasan, cirinya ada Prefix RE: dan mengandung Attachment
Handle_REQUEST_ADHOCK <-  filter(Handle_REQUEST, `Subject Prefix` == "RE:" & `Has Attachments` == "TRUE")

#Ambil Body Email Terakhir
Handle_REQUEST_ADHOCK <- mutate(Handle_REQUEST_ADHOCK, Last_Contents = str_extract(Contents, ".+?(?=From: )"))

# Cek Kata2 Please Find dari Body Email terakhir
Handle_REQUEST_ADHOCK <- filter(Handle_REQUEST_ADHOCK, grepl("please find", tolower(`Last_Contents`)) |
                                  grepl("pleas find", tolower(`Last_Contents`)) )

# Ambil Bagian From pada keselururan Body Email (Mencari kepada siapa Tim MIS Reply Email)
Handle_REQUEST_ADHOCK <- mutate(Handle_REQUEST_ADHOCK, Reply_From = str_extract(Contents, 
                                                      "(?<=From: )(.*?)(?= Sent:)"))
# Hilangkan <1bankid@dbs.com>
Handle_REQUEST_ADHOCK <- mutate(Handle_REQUEST_ADHOCK, Reply_From = gsub(" <(.*?)>","", Reply_From))

# Mencari kapan Email ini dikirim sama requested
Handle_REQUEST_ADHOCK <- mutate(Handle_REQUEST_ADHOCK, DTTime_Req = str_extract(Contents,"(?<=Sent: )(.*?)(?=To:)"))
Handle_REQUEST_ADHOCK <- mutate(Handle_REQUEST_ADHOCK, DTTime_Req = str_extract(DTTime_Req,"(?<=, ).*"))
Handle_REQUEST_ADHOCK <- mutate(Handle_REQUEST_ADHOCK, Tanggal = parse_date_time2(DTTime_Req,orders = "bdYIMp"))


# Filter yang Bukan Request yang Reply dari Email kita sendiri
Handle_REQUEST_ADHOCK <- filter(Handle_REQUEST_ADHOCK, `From` != `Reply_From`) %>%
  filter(as.Date(Received) > as.Date("2021-04-09"))


# Tulis Ke Excel
write.xlsx(Handle_REQUEST_ADHOCK ,"~/Handle_Adhock_Baru_1704210800.xlsx")


