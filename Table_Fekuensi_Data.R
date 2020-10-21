# Membuat Table Frekuensi data

# Langkah 1 : Input Data
  data <- c(1120,1160,963,1210,1160,1160,813,1230,1370,1140,995,935,1110,994,1020,
          960,1180,799,958,1140,1100,1210,1150,1250,1260,1220,1030,1100,774,840,
          874,694,940,833,701,916,692,1020,1050,969,831,726,456,824,702,1120,
          1100,832,764,821,768,845,864,862,698,845,744,796,1040,759,781,865,845,
          944,984,897,822,1010,771,676,649,846,812,742,801,1040,860,874,848,890,
          744,749,838,1050,918,986,797,923,975,815,1020,906,901,1170,912,746,919,
          718,714,740)

# Langkah 2 : Urutkan Data
  sort(data)

# Langkah 3 : Defisikan lenght sebagai panjang dari data
  n <- length(data)

# Langkah 4 : Menentukan max dan min dari data
  dataMax <- max(data)
  dataMin <- min(data)

# Langkah 5 : Menentukan Jumlah Kelas(k)
  k <- 1+3.3*log10(n)

# Langkah 6 : Membulatkan k menjadi kFix
  kFix <- round(k)
  
# Langkah 7 : Menetukan Interfal (p)
  p <- (dataMax-dataMin)/kFix
  
# Langkah 8 : Membulatka p menjadi pFix
  pFix <- ceiling(p)

# Langkah 9 : Function Untuk Memcari Frekuensi
  frekuensi <- function(x,y,z)
  {
    a <- 0
    for (i in 1:n) {
      if (x[i] >= y && x[i] <= z) {
        a = a + 1
      }
    }
    print(a)
  }
  
# Langkah 10 : Mencari frekuensi dengan fungsi frekuensi, lalu disimpan pada f
  f <- c(
    frekuensi(data, 456, 456+115),   #1
    frekuensi(data, 456+116, 456+116+115),   #2
    frekuensi(data, 456+116+116, 456+116+116+115), #24
    frekuensi(data, 456+116+116+116, 456+116+116+116+115),   #30
    frekuensi(data, 456+116+116+116+116, 456+116+116+116+116+115),   #18
    frekuensi(data, 456+116+116+116+116+116, 456+116+116+116+116+116+115),   #13
    frekuensi(data, 456+116+116+116+116+116+116, 456+116+116+116+116+116+116+115),   #11
    frekuensi(data, 456+116+116+116+116+116+116+116, 456+116+116+116+116+116+116+116+115))#1

# Langkah 11 : Membuat dataframe dari data 
  nilai1 <- paste(456, 456+115, sep = "-")
  nilai2 <- paste(456+116, 456+116+115, sep = "-")
  nilai3 <- paste(456+116+116, 456+116+116+115, sep = "-")
  nilai4 <- paste(456+116+116+116, 456+116+116+116+115, sep = "-")
  nilai5 <- paste(456+116+116+116+116, 456+116+116+116+116+115, sep = "-")
  nilai6 <- paste(456+116+116+116+116+116, 456+116+116+116+116+116+115, sep = "-")
  nilai7 <- paste(456+116+116+116+116+116+116, 456+116+116+116+116+116+116+115, sep = "-")
  nilai8 <- paste(456+116+116+116+116+116+116+116, 456+116+116+116+116+116+116+116+115, sep = "-")
  
  # Menambahkan column Nilai
  table <- edit(
    data.frame(
      "Nilai" = c(nilai1, nilai2, nilai3, nilai4, nilai5, nilai6, nilai7, nilai8)
      )
    )
  # Menambahkan colum frekuensi
  table$frekuensi=f
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  