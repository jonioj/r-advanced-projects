napis <- "Happy Easter 2023!"

substr (napis, 7, 12) <- "Holiday"

sprintf ("%20s%.4f", "data science", pi)

dir <- "~/Documents/Dydaktyka/"
filename <- "plik_z_danymi_v"
paste (dir, filename, 5, ".txt", sep="")
sprintf ("~/Documents/Dydaktyka/plik_z_danymi_v%d.txt", 5)
sprintf ("%s%s%d.txt", dir, filename, 5)

sprintf ("Test #%d of %d", 1:3, 3)
paste ("Test #", 1:3, " of ", 3, sep = "")


str_c (c("Happy","Easter!"), collapse=" ")

str_c (c ("Happy", "2023"), c("Easter", "!"), sep = "", collapse = " ")

x <- "abc123ąęś"
str_sub(x, 4)

str_pad(c ("abc", "defghij"), 10, side = "left")

str_count ("a1a2a3", fixed("a"))

str_extract_all (fruit, fixed ("berry"))

napis <- "Pierwsze zdanie. Drugie zdanie. Trzecie zdanie. Czwarte zdanie."
str_split (napis, boundary())


str_count ("a1b2c3d4", "[a-c]")

str_count ("a1b2ß3ą4", "[a-z]")

str_extract_all ("abdwzAWZ12! @","\\p{Lu}[0-9]+")


strings <- c(" 219 733 8965", "329-293-8753 ", "banana", "595 794 7569",
             "387 287 6718", "apple", "233.398.9187  ", "482,952,3315",
             "239 923 8115 and 842 566 4692", "Work: 579-499-7527", "$1000",
             "Home: 543.355.3679")
phone <- "([2-9][0-9]{2})[-\\s\\.]([0-9]{3})[-\\s\\.]([0-9]{4})"
phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"
str_match (strings, phone)
str_extract (strings, phone)

library(magrittr)
file <- readLines ("notowania.html")
n <- which (str_detect (file, "EUR/PLN"))
str_extract (file[n+1], "\\d+,\\d+") %>% str_replace(",",".") %>% as.numeric() -> kupno
str_extract (file[n+3], "\\d+,\\d+") %>% str_replace(",",".") %>% as.numeric() -> sprzedaz
str_extract (file[n+5], "\\d{4}[:graph:]+\\s[:graph:]+\\d{2}") -> dzien