# DiskBase pro Linux


## Co to je

Linux verze free windows DiskBase (https://sourceforge.net/projects/diskbase/) , program pro vedení katalogu disků.

## Stav

Stabilní funkce: 
- načtení a zobrazení databáze disků
- vyhledávání souborů
- exporty dat
- načtení nových dat do databáze

 
Netestované (nestabilní) funkce: 
- tisk dat
 

## Požadavky
- Lazarus (FPC) kompilátor
- Linux prostředí

- utilitka lsblk, balíček util-linux
- pustit příkaz visudo a zapsat řádek "user  ALL= NOPASSWD: /usr/bin/lsblk", kde "user" je aktuálně přihlášený uživatel v linuxu (pepik,franta,admin,..)


## Jak zkompilovat
- nainstalovat Lazarus (1.6 a vyšší, 1.8RC5 dop.) dle návodu - http://wiki.freepascal.org/Installing_Lazarus
- získat zdrojový kod z https://github.com/ibv/DiskBase
- instalace atgauge_package.lpk z adreáře SOURCE/atgauge_package
- zkompilovat podpůrné dynamické knihovny ze SOURCE/DLLs, pustit skript "build" v SOURCE/DLLs
- v Lazarus IDE, menu - File -> Open -> "/cesta/k/DiskBase.lpi"
- v Lazarus IDE, menu - Run -> F9 (run) or Ctrl+F9 (compile) or Shift+F9 (link)


