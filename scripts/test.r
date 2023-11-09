
df <- data.frame(first = c(1, 2, 3, 4), second = c(8, 9, 10, 11), third = c(55, 66, 77, 78))
print(df)
col_names <- c("second", "third")
new_data <- data.frame(df[col_names])
print(new_data)
col_names <- c("first", "third")
new_data <- data.frame(df[col_names])
print(new_data)