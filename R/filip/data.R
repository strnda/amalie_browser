library(data.table); library(fst)

## tomst vlhkost ####

ls <- list.files(path = "./data_raw/rawdatadatabaze_TMS_vrty11_21/",
                 pattern = ".csv", 
                 full.names = TRUE)

dta_1 <- lapply(X = ls,
                FUN = fread,
                dec = ",")

names(x = dta_1) <- sapply(
  X = strsplit(x = gsub(pattern = "./data_raw/rawdatadatabaze_TMS_vrty11_21/|/|.csv",
                        replacement = "",
                        x = ls),
               split = "_"),
  FUN = "[", 
  2
)

dta_1 <- rbindlist(l = dta_1,
                   idcol = "ID", 
                   fill = TRUE)

ls <- list.files(path = "./data_raw/rawdatadatabaze_TMS4IGA_29112021/",
                 pattern = ".csv", 
                 full.names = TRUE)

dta_2 <- lapply(X = ls,
                FUN = fread,
                dec = ",")

names(x = dta_2) <- sapply(
  X = strsplit(x = gsub(pattern = "./data_raw/rawdatadatabaze_TMS4IGA_29112021/|/|.csv",
                        replacement = "",
                        x = ls),
               split = "_"),
  FUN = "[", 
  2
)

dta_2 <- rbindlist(l = dta_2,
                   idcol = "ID", 
                   fill = TRUE)
dta <- rbind(dta_1, dta_2)
dta[, `:=`(ID = as.factor(x = ID),
           date = as.POSIXct(x = V2, 
                             format = "%Y.%m.%d %H:%M"),
           V1 = NULL,
           V2 = NULL,
           V3 = NULL,
           V8 = NULL,
           V9 = NULL,
           V10 = NULL)]

setnames(x = dta, 
         old = paste0("V", 4:7),
         new = c(paste0("temp_", 1:3),
                 "signal"))

dta_m <- melt(data = dta, 
              id.vars = c("ID", "date"))

dta <- dta_m[, .(value = mean(x = value, 
                              na.rm  = TRUE)), 
             by = .(ID, date = format(x = date,
                                      format = "%Y-%m-%d"), variable)]

str(object = dta)

dta[, date := as.IDate(x = date)]

dta

?fst

write_fst(x = dta,
          path = "./data/vlhkost.fst")

## dendro ####

dta <- fread(input = "../../../Shared/Lesaci/Data_2021/vycistena data.csv")
dta <- dta[, .(.id, date_time, T1, Radius)]
dta <- na.omit(object = dta)

setnames(x = dta, 
         old = c(".id", "date_time", "T1", "Radius"),
         new = c("ID", "date", "temp", "radius"))

dta_m <- melt(data = dta, 
              id.vars = c("ID", "date"))

dta <- dta_m[, .(value = mean(x = value, 
                              na.rm  = TRUE)), 
             by = .(ID, date = format(x = date,
                                      format = "%Y-%m-%d"), variable)]

dta[, `:=`(ID = as.factor(ID),
           date = as.IDate(x = date))]

write_fst(x = dta,
          path = "./data/dendro.fst")

## mikroklima ####

dta <- fread(input = "./data_raw/MIKROKLIMA.csv")
dta[, `:=`(V1 = NULL,
           povodi = NULL,
           X = NULL,
           Y = NULL, 
           Wind.direction = NULL)]
dta
dta_m <- melt(data = dta,
              id.vars = c("ID", "DTM"))

dta_m[, variable := gsub(pattern = "\\.",
                         replacement = "_",
                         x = tolower(x = variable))]
setnames(x = dta_m, 
         old = "DTM", 
         new = "date")

dta <- list(dta_m[(variable != "rainfall") & (variable != "wind_gust"), 
                  .(value = mean(x = value, 
                                 na.rm  = TRUE)), 
                  by = .(ID, date = format(x = date,
                                           format = "%Y-%m-%d"), variable)],
            dta_m[(variable == "rainfall"), 
                  .(value = sum(x = value, 
                                na.rm  = TRUE)), 
                  by = .(ID, date = format(x = date,
                                           format = "%Y-%m-%d"), variable)],
            dta_m[(variable == "wind_gust"), 
                  .(value = max(x = value, 
                                na.rm  = TRUE)), 
                  by = .(ID, date = format(x = date,
                                           format = "%Y-%m-%d"), variable)])
dta <- rbindlist(l = dta)
dta[, date := as.IDate(x = date)]

# ggplot(data = dta) +
#   geom_line(mapping = aes(x = date, 
#                           y = value, 
#                           group = variable)) +
#   facet_wrap(facets = ID ~ variable, 
#              scales = "free") + 
#   theme_bw()

write_fst(x = dta,
          path = "./data/mikroklima.fst")

## eddy ####

dta <- fread(input = "./data_raw/EDDY_LIHOVAR.csv")
dta[, `:=`(V1 = NULL,
           X = NULL,
           Y = NULL, 
           date = as.POSIXct(x = DTM, 
                             format = "%Y-%m-%d %H"),
           DTM = NULL)]
dta

dta_m <- melt(data = dta,
              id.vars = c("ID", "date"))

dta <- list(dta_m[(variable != "P"), 
                  .(value = mean(x = value, 
                                 na.rm  = TRUE)), 
                  by = .(ID, date = format(x = date,
                                           format = "%Y-%m-%d"), variable)],
            dta_m[(variable == "P"), 
                  .(value = sum(x = value, 
                                na.rm  = TRUE)), 
                  by = .(ID, date = format(x = date,
                                           format = "%Y-%m-%d"), variable)])
dta <- rbindlist(l = dta)
dta[, date := as.IDate(x = date)]

write_fst(x = dta,
          path = "./data/eddy.fst")

## vrty ####

## rds ####

ls <- list.files(path = "~/Desktop/",
                 pattern = ".rds",
                 full.names = TRUE)

dta_all <- lapply(X = ls,
                  FUN = readRDS)

tms <- sapply(X = dta_all[grep(pattern = "tms4", 
                               x = ls, 
                               ignore.case = TRUE)], 
              FUN = "[", 1)

tms <- rbindlist(l = tms, 
                 fill = TRUE)

tms <- unique(x = tms[, .(DTM, ID, T1, T2, T3, vlhNK)])

setnames(x = tms, 
         old = c("DTM", "vlhNK"),
         new = c("date", "vlhkost"))

dta_m <- melt(data = tms, 
              id.vars = c("ID", "date"))

dta <- dta_m[, .(value = mean(x = value, 
                              na.rm  = TRUE)), 
             by = .(ID, date = format(x = date,
                                      format = "%Y-%m-%d"), variable)]
str(object = dta)

dta[, `:=`(date = as.IDate(x = date),
           ID = as.factor(x = ID),
           variable = tolower(x = variable))]

str(object = dta)
dta

write_fst(x = dta,
          path = "./data/vlhkost_od_lukase.fst")

## vrty vosko ####

vrty <- dta_all[grep(pattern = "tms4", 
                     x = ls, 
                     ignore.case = TRUE, 
                     invert = TRUE)][[1]]

vrty <- vrty[, .(ID, DTM, HLADINA, TEPLOTA)]

setnames(x = vrty, 
         old = c("DTM"),
         new = c("date"))

dta_m <- melt(data = vrty, 
              id.vars = c("ID", "date"))

dta_m[, variable := tolower(x = variable)]

write_fst(x = dta,
          path = "./data/vrty.fst")
