gt4lsa verze 2.01 (g77 Linux)                       2000.09.20

Upravy:

  - jmena souboru prevedena na mala pismena, odstraneny znaky ^Z (032)
    na konci souboru
  - gt4lsa.for: doplnena inicializace iout=0
  - gttime.for: uprava fce GTTIME pro mereni casu
  - doplnen Makefile pro Linux

-----------------------------------------------------------------------

GT4LSA verze 2.0                                    1994.09.10

V tomto adresari jsou zdrojove texty upravene pro vypocet
v DOUBLE PRECISION ve verzi pro Lahey Fortran.

V adresari jsou dve vypoctene ulohy PRIKLAD a PRCH

GT4LSA < PRIKLAD.1

GT4LSA < PRCH.1

Na obrazovku se vypisi vysledky, ktere jsou ulozeny v souborech
PRIKLAD.3, resp. PRCH.3. Obe ulohy jsou regularni, pro singularni
soustavy lze uvest seznam indexu neznamych, pro ktere ma byt
minimalizovana jejich Euklidovska norma (pocet je ctvrty parametr,
je-li roven poctu neznamych, generuje se tento seznam automaticky).

Do parametrickeho souboru, ktery program GT4LSA cte ze
standardniho vstupniho zarizeni, se zapisuje jmeno souboru rovnic
oprav a parametry ulohy:

    * pozadavek na vypocet vahovych koeficientu
         0  vypocet vahovych koeficientu se nepozaduje
         1  pouze vypocet vahovych koeficientu neznamych
         2  pouze vahove koeficienty vyrovnanych mereni
         3  vahove koeficienty vyrovnanych mereni a neznamych
    * pocet neznamych
    * pocet pozorovani
    * pocet neznamych, ktere vstupuji do podminky minimalizace
      vektoru neznamych v pripade singularni soustavy. Je-li
      tato hodnota rovna poctu neznamych, vytvori program
      seznam koeficientu automaticky; v opacnem pripade musi
      byt seznam danych koeficientu nasledovat v parametrickem
      souboru.


Priklad:
*******
                      (1 0 2)     (7)
soustava              (0 1 1)*x = (5),   P=diag(4,3,4,2)
                      (0 0 1)     (3)
                      (2 1 0)     (4)

Do parametrickeho souboru zapiseme

                      priklad.2
                      3 3 4 3

Obsah souboru priklad.2 bude

                      3 4

                      2 1 3
                      4 7 1 2
                      2 2 3
                      3 5 1 1
                      1 3
                      4 3 1
                      2 1 2
                      2 4 2 1

a odpovidajici vysledky (priklad.3)

###    3    4    0    0        34       .0000
          1   9.999999999999999E-001
          2        2.000000000000000
          3        3.000000000000000
###       .00       .00       .06    .00    .05    .06    .00
          1           1   2.190721649484536E-001
          2           2   3.195876288659796E-001
          3           3   5.670103092783504E-002
          4           4   4.690721649484534E-001
          5           5   1.365979381443299E-001
          6           6   2.731958762886599E-001
          7           7   5.670103092783504E-002


Prechod Microsoft Fortran --> Lahey Fortran
*******************************************

Protoze Lahey Fortran neumoznuje zadavat pomoci direktivy include
datovy typ promennych (REAL, DOUBLE PRECISION), prejmenoval
jsem puvodni zdrojove soubory *.FOR --> *.F a doplnil pomocny
program include, ktery provede potrebne (viz soubor prevod_f.bat).
Jako primarni je tedy nutno chapat soubory s priponou *.F (zde
jsem si jiz pripravil "cesticku" pro prevod programu GT4LSA
na Unixovskou pracovni stanici --- na IBM RISC 6000 pod operacnim
systemem AIX includy pracuji podle meho puvodniho ocekavani).

Krome nove definice parametrickeho souboru je hlavni zmenou, ze
predpokladam soubor rovnic oprav jako textovy. Pokud by to
v budoucnu melo predstavovat potiz, rozsirim program tak, aby
se umel vyporadat jak s textovym tak s binarnim formatem
vstupnich dat. Novy format zapisu rovnic oprav (ktery je totozny
s puvodnim navrhem) by mel predstavovat "standard", pro chystanou
radu diplomovych praci se zamerenim na MNC software.

                                                            A.C.