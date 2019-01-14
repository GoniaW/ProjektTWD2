library(xlsx)
library(data.table)
library(ggplot2)
df <- as.data.table(read.csv2(file='JA.csv', header=TRUE, sep=';', encoding = 'UTF-8',dec=","))

dzień_kroki <- df[,.(Kroki = mean(Kroki, na.rm=TRUE)), by=Dzień.tygodnia]

dzień_kroki_plt <- ggplot(data=dzień_kroki, aes(x=Dzień.tygodnia, y=Kroki)) + geom_col()

sen_kroki_dzien <- ggplot(data=df, aes(x=Sen, y=Kroki, color=Dzień.tygodnia)) + geom_point(size=10)

gestosc_slaby <- ggplot() + 
  geom_density_2d(data = df[Dzień.tygodnia=="poniedziałek",], aes(x=Sen, y=Kroki, color='red')) +
  geom_density_2d(data = df[Dzień.tygodnia=="wtorek",], aes(x=Sen, y=Kroki, color='blue')) +
  geom_density_2d(data = df[Dzień.tygodnia=="środa",], aes(x=Sen, y=Kroki, color='green')) +
  geom_density_2d(data = df[Dzień.tygodnia=="czwartek",], aes(x=Sen, y=Kroki, color='yellow')) +
  geom_density_2d(data = df[Dzień.tygodnia=="piątek",], aes(x=Sen, y=Kroki, color='black')) +
  geom_density_2d(data = df[Dzień.tygodnia=="sobota",], aes(x=Sen, y=Kroki, color='brown')) +
  geom_density_2d(data = df[Dzień.tygodnia=="niedziela",], aes(x=Sen, y=Kroki, color='grey'))

szereg_kroki <- ggplot() + 
  geom_line(data=df, aes(x=X.U.FEFF.Data,y=Kroki,group=1))

szereg_sen <- ggplot() + 
  geom_line(data=df, aes(x=X.U.FEFF.Data,y=Sen,group=1)) + geom_hline(yintercept = mean(df$Sen, na.rm = TRUE))

dzien_kroki_violin <- ggplot(data=df, aes(x=Dzień.tygodnia, y=Kroki)) + geom_violin()

dzien_sen_violin <- ggplot(data=df, aes(x=Dzień.tygodnia, y=Sen)) + geom_violin()

szereg_dlugosc_kroku <- ggplot(data = df[,.(X.U.FEFF.Data, ratio=Odległość/Kroki * 1000)], aes(x=X.U.FEFF.Data,y=ratio, group=1) ) + geom_line()

dzien_dlugosc_kroku <- ggplot(data = df[,.(ratio=mean(Odległość/Kroki * 1000, na.rm=TRUE)), by=Dzień.tygodnia], aes(x=Dzień.tygodnia, y=ratio)) + geom_col()
