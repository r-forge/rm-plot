
for (file in list.files("pkg/rm.plot/R", full = TRUE)) {
	tryCatch(source(file), error = function(e) print(e))
}

options(error = recover)
options(error = NULL)
