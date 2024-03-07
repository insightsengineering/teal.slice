# run this script before every release

# disabling linter because of multiple object name violations
# nolint start

Rd_db <- getFromNamespace("Rd_db", "tools")
Rd_contents <- getFromNamespace("Rd_contents", "tools")
Rd_list <- Rd_db(dir = ".")
Rd <- Rd_contents(Rd_list)

Rd$Aliases <- ifelse(Rd$Name %in% c("teal_slice", "teal_slices"), Rd$Name, Rd$Aliases)

Meta_dir <- file.path("inst", "Meta")
if (!dir.exists(Meta_dir)) dir.create(Meta_dir)
saveRDS(Rd, file = file.path(Meta_dir, "Rd.rds"))

# nolint end
