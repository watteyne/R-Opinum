load("c:/dev/R/projects/GEG/valorisation/sampleData.rdata")

cons <- inputVariables$CONS$TimeSeries
fee <- inputVariables$FEE$TimeSeries
price <- inputVariables$PRICE$TimeSeries

fee <- data.frame(Dates=cons$Dates, Values=approx(fee$Dates, fee$Values, cons$Dates, method='constant', rule=1:2, f=0)$y)
price <- data.frame(Dates=cons$Dates, Values=approx(price$Dates, price$Values, cons$Dates, method='constant', rule=1:2, f=0)$y)

print(fee)
print(price)
cons$Values <- fee$Values + price$Values * cons$Values
print(cons)

list(TimeSeries=cons)
