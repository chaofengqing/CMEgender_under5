

## supplementary table for MDG regions and countries ##
colname.table <- c("MDG Region", "Country")
table.df <- matrix(NA, nr = R, nc = length(colname.table))
dimnames(table.df)[[1]] <- regions.r
dimnames(table.df)[[2]] <- colname.table

for (reg in regions.r) {
  table.df[reg, "MDG Region"] <- reg
  country.r <- ExternalMakeCountryNamesFull(name.c[reg.c == reg])
  table.df[reg, "Country"   ] <- paste(country.r, collapse = "; ")
}#end of r loop

table <- xtable(
  table.df, 
  caption = paste("\\bf{Classification of countries by MDG regions.}"
                  ),
  label = "tb-sup-RegionCountry",
  align = "|l|p{3cm}|p{11cm}|" # the 1st entry is for rownames
)
print(table, floating = FALSE, tabular.environment = 'longtable',
      include.colnames = FALSE, include.rownames = FALSE, size = "small",
      hline.after = c(0:(nrow(table) - 1)))
# paste column names and info after \begin{tabular}
# \hline
# MDG Region &  Country \\
# \hline
