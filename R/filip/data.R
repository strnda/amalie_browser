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

# write_fst(x = dta,
#           path = "./data/vlhkost.fst")

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

# write_fst(x = dta,
#           path = "./data/dendro.fst")

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

# write_fst(x = dta,
#           path = "./data/mikroklima.fst")

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

# write_fst(x = dta,
#           path = "./data/eddy.fst")

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

# write_fst(x = dta,
#           path = "./data/vlhkost_od_lukase.fst")

## vrty vosko ####

vrty <- dta_all[grep(pattern = "tms4", 
                     x = ls, 
                     ignore.case = TRUE, 
                     invert = TRUE)][[1]]

vrty_nfo <- unique(x = vrty[, .(ID, x, y)])

names(vrty_nfo) <- toupper(x = names(x = vrty_nfo))

vrty_nfo[, `:=`(ID = as.factor(x = ID),
                X = as.numeric(x = gsub(pattern = "N", 
                                        replacement = "",
                                        x = X)),
                Y = as.numeric(x = gsub(pattern = "N", 
                                        replacement = "",
                                        x = Y)))]

str(vrty_nfo)

vrty_meta <- as.data.table(readxl::read_xlsx(path = "~/Desktop/vrty evidence základní.xlsx"))
setnames(x = vrty_meta, 
         old = "...2", 
         new = "name")

vrty_nfo <- merge(x = vrty_nfo, 
                  y = vrty_meta[, .(name, IMSI)],
                  by.x = "ID",
                  by.y = "IMSI")

# write_fst(x = vrty_nfo,
#           path = "./data/vrty_info.fst")

nfo_all <- fread(input = "./data/nfo_sensors.csv")
nfo_all <- rbind(nfo_all[senzor != "vrt"], 
                 data.table(ID = vrty_nfo$ID, senzor = "vrt"))

# write_fst(x = nfo_all, 
#           path = "./data/nfo_sensors.fst")

vrty <- vrty[, .(ID, DTM, HLADINA, TEPLOTA)]

setnames(x = vrty, 
         old = c("DTM"),
         new = c("date"))

dta_m <- melt(data = vrty, 
              id.vars = c("ID", "date"))

dta_m[, variable := tolower(x = variable)]

# write_fst(x = dta_m,
#           path = "./data/vrty.fst")

## sucho #### 

dta_bp <- read_fst(path = "../../../Shared/appka_Fila/data/drought/BP_CTRL_Amalie.fst", 
                   as.data.table = TRUE)
dta_bp[, ID := paste("BP", HruIds,
                     sep = "_")]

dta_kl <- read_fst(path = "../../../Shared/appka_Fila/data/drought/KL_CTRL_Amalie.fst", 
                   as.data.table = TRUE)
dta_kl[, ID := paste("KL", HruIds,
                     sep = "_")]

dta_sucho <- rbind(dta_bp, dta_kl)
dta_sucho <- dta_sucho[, .(ID, DTM, PREC, TOTR, PERC)]

setnames(x = dta_sucho,
         old = "DTM",
         new = "date")

names(x = dta_sucho)[3:5] <- tolower(x = names(x = dta_sucho)[3:5])

dta_sucho_m <- melt(data = dta_sucho, 
                    id.vars = c("ID", "date"))


dta_p <- dta_sucho_m[variable == "prec", .(value = sum(x = value, 
                                                       na.rm = TRUE)),
                     by = .(ID, variable, date = format(x = date,
                                                        format = "%Y-%m"))]


dta_r <- dta_sucho_m[variable != "prec", .(value = mean(x = value, 
                                                        na.rm = TRUE)),
                     by = .(ID, variable, date = format(x = date,
                                                        format = "%Y-%m"))]

dta_sucho_month <- rbind(dta_p, dta_r)
dta_sucho_month <- dta_sucho_month[, .(ID, date, variable, value)]
dta_sucho_month[, `:=`(ID = as.factor(x = ID),
                       date = as.IDate(x = paste0(date, "-01")))]

dta_sucho_month[variable == "totr" & value == 0, value := .01]
dta_sucho_month[variable == "perc" & value == 0, value := .01]

ggplot(data = dta_sucho_month[ID == "BP_1"]) +
  geom_line(mapping = aes(x = date,
                          y = value)) +
  geom_hline(mapping = aes(yintercept = q),
             colour = "red4") +
  facet_wrap(facets = ~variable, 
             scales = "free", 
             ncol =1)

# library(CoSMoS)
# 
# para <- dta_sucho_month[, .(unlist(fitDist(data = value, 
#                                            dist = "ggamma", 
#                                            n.points = 30, 
#                                            norm = "N2", 
#                                            constrain = FALSE))),
#                         by = .(ID, variable)]
# 
# para[, para := rep(x = c("scale", "shape1", "shape2"),
#                    times = .N / 3)]
# 
# para <- dcast(data = para,
#               formula = ID + variable ~ para,
#               value.var = "V1")
# 
# write_fst(x = para,
#           path = "./data/sucho_para.fst")

para <- read_fst(path = "./data/sucho_para.fst")

dta_sucho <- merge(x = dta_sucho_month, 
                   y = para, 
                   by = c("ID", "variable"))

dta_sucho[, index := qnorm(p = pggamma(q = value,
                                       scale = scale,
                                       shape1 = shape1,
                                       shape2 = shape2))]

dta_sucho[index < -3.5, index := rnorm(n = .N,
                                       mean = -2.5, 
                                       sd = .3)]

levels(x = dta_sucho$variable) <- c("SPI", "SRI", "SSI")


write_fst(x = dta_sucho,
          path = "./data/indexy_sucha.fst")
