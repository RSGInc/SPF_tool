
tours$HHEXPFACT = hhday$finalweight[match(paste(tours$HH_ID, tours$DAYNO, sep = "-"),
                                          paste(hhday$SAMPN, hhday$DAYNO, sep = "-"))]

tours$OUT_CHAUFFUER_ID <- as.numeric(tours$OUT_CHAUFFUER_ID)
tours$INB_CHAUFFUER_ID <- as.numeric(tours$INB_CHAUFFUER_ID)

tours_sample <- tours[TOURPURP==3 & PERSONTYPE %in% c(9,10,11), .(HH_ID, PER_ID, TOUR_ID, TOURPURP, ESCORTED_TOUR, CHAUFFUER_ID, ESCORTING_TOUR, 
                                                                  PERSONTYPE, OUT_ESCORT_TYPE, OUT_CHAUFFUER_ID, OUT_CHAUFFUER_PURP, OUT_CHAUFFUER_PTYPE, 
                                                                  INB_ESCORT_TYPE, INB_CHAUFFUER_ID, INB_CHAUFFUER_PURP, INB_CHAUFFUER_PTYPE, HHEXPFACT)]

out_sample1 <- tours_sample[!is.na(OUT_ESCORT_TYPE) & !is.na(PERSONTYPE) & OUT_ESCORT_TYPE!="NaN",]
inb_sample1 <- tours_sample[!is.na(INB_ESCORT_TYPE) & !is.na(PERSONTYPE) & INB_ESCORT_TYPE!="NaN",]

out_table1  <- xtabs(HHEXPFACT~OUT_ESCORT_TYPE+PERSONTYPE, data = out_sample1)
inb_table1  <- xtabs(HHEXPFACT~INB_ESCORT_TYPE+PERSONTYPE, data = inb_sample1)


## add marginal totals to all final tables
out_table1   <- addmargins(as.table(out_table1))
inb_table1   <- addmargins(as.table(inb_table1))


## reshape data in required form for visualizer
out_table1 <- as.data.frame.matrix(out_table1)
out_table1$id <- row.names(out_table1)
out_table1 <- melt(out_table1, id = c("id"))
colnames(out_table1) <- c("esc_type", "child_type", "freq_out")
out_table1$esc_type <- as.character(out_table1$esc_type)
out_table1$child_type <- as.character(out_table1$child_type)
out_table1 <- out_table1[out_table1$esc_type!="Sum",]
out_table1$child_type[out_table1$child_type=="Sum"] <- "Total"

inb_table1 <- as.data.frame.matrix(inb_table1)
inb_table1$id <- row.names(inb_table1)
inb_table1 <- melt(inb_table1, id = c("id"))
colnames(inb_table1) <- c("esc_type", "child_type", "freq_inb")
inb_table1$esc_type <- as.character(inb_table1$esc_type)
inb_table1$child_type <- as.character(inb_table1$child_type)
inb_table1 <- inb_table1[inb_table1$esc_type!="Sum",]
inb_table1$child_type[inb_table1$child_type=="Sum"] <- "Total"

table1 <- out_table1
table1$freq_inb <- inb_table1$freq_inb
table1$esc_type[table1$esc_type=='1'] <- "Ride Share"
table1$esc_type[table1$esc_type=='2'] <- "Pure Escort"
table1$esc_type[table1$esc_type=='3'] <- "No Escort"
table1$child_type[table1$child_type=='6'] <- 'Driv Student'
table1$child_type[table1$child_type=='7'] <- 'Non-DrivStudent'
table1$child_type[table1$child_type=='8'] <- 'Pre-Schooler'


#worker_table <- worker_table[, c("DropOff", "Ride Share","Pure Escort","No Escort","Total")]

## write outputs
fwrite(table1, file.path(outdir, "esctype_by_childtype.csv"), row.names = F)
# fwrite(table2, file.path(outdir, "esctype_by_chauffeurtype.csv"), row.names = F)