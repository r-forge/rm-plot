
for (r.file in list.files("pkg/rm.plot2/R", pattern = "R$", full.names = TRUE)) {
	tryCatch(source(r.file), error = function(e) print(e))
}




options(error = recover)

options(error = NULL)