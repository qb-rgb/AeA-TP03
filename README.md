Quentin Baert  

# AeA : Le jeu de la lettre qui saute

Exercice 1 : Modélisation et initialisation du jeu
--------------------------------------------------

La classe `Liste` se trouve dans le dossier `src/main/scala/liste`.  
La classe `Graphe` se trouve dans le dossier `src/main/scala/graphe`.

Voici le graphe correspondant au `dico3court` après initialisation de la liste de successeurs.

```
===== Graphe =====

gag -> gai
gag -> gaz
----
gai -> gag
gai -> gaz
gai -> gui
----
gaz -> gag
gaz -> gai
----
gnu -> glu
----
glu -> gnu
glu -> alu
----
gui -> gai
gui -> guy
gui -> gue
----
guy -> gui
guy -> gue
----
gre -> gue
gre -> are
----
gue -> gui
gue -> guy
gue -> gre
----
ace -> acm
ace -> aie
ace -> are
----
acm -> ace
----
agi -> ami
----
ait -> aie
ait -> ail
ait -> air
ait -> art
----
aie -> ace
aie -> ait
aie -> ail
aie -> air
aie -> are
----
ail -> ait
ail -> aie
ail -> air
----
air -> ait
air -> aie
air -> ail
air -> apr
air -> avr
----
alu -> glu
----
ami -> agi
----
arc -> are
arc -> art
----
are -> gre
are -> ace
are -> aie
are -> arc
are -> art
----
art -> ait
art -> arc
art -> are
----
apr -> air
apr -> avr
----
avr -> air
avr -> apr
----
sur -> mur
----
mur -> sur
```

Exercice 2 : Calcul des composantes connexes
--------------------------------------------

Pour obtenir la composante connexe d'un mot du graphe, il suffit de lancer un parcours à partir de ce mot.  
Pour les composantes suivantes, la méthode `Graphe.dfsPrint(m)` est utilisée : celle ci effectue un parcours en profondeur depuis le mot `m`.

#### Composantes connexes du graphe de `dico3court`

```
0:  gag  gai  gaz  gui  guy  gue  gre  are  ace  acm  aie  ait  ail  air  apr  avr  art  arc
1:  gel
2:  gks
3:  gin
4:  gnu  glu  alu
5:  agi  ami
6:  and
7:  sur  mur
8:  mat
```

#### Composante connexe des mots _lion_ et _peur_

```
lion  lien  mien  miel  fiel  fier  fief  hier  huer  puer  ruer
----
tuer  muer  muet  guet  suer  suez  tuez  reer  beer  reel  lier
lied  sied  sien  tien  bien  rien  riez  pied  pieu  lieu  dieu
pneu  nier  fuel  duel  quel  full  pull  ciel  pion  paon  pain
vain  vais  mais  hais  haie  hait  haut  vaut  vaux  veux  veuf
oeuf  neuf  nerf  cerf  cern  serf  self  sera  aera  aere  here
hele  tele  fele  fela  hela  dela  deja  deca  decu  vecu  veau
venu  tenu  tetu  fetu  feru  fera  fora  fort  font  foot  flot
flou  clou  clot  clos  ilot  plot  plut  peut  veut  vent  vend
pend  perd  pere  gere  gene  cene  cane  cade  cage  cake  cafe
came  camp  vamp  lame  lama  pama  puma  papa  sapa  saga  sage
sake  sape  pape  page  pane  pale  hale  hall  halo  haro  hase
vase  jase  jade  jude  rude  rode  roda  soda  sofa  soya  soja
role  mole  mile  mike  mise  miss  mess  mens  cens  cent  sent
sens  sans  sais  jais  jars  gars  pars  paru  parc  pari  sari
sali  soli  joli  poli  polo  solo  silo  kilo  kilt  tilt  tint
vint  vont  mont  mort  mord  mors  tors  tort  toit  tout  aout
cout  coit  voit  vois  voir  voix  voie  vote  hote  hate  hata
gata  gala  gale  gate  gite  mite  mita  mica  vite  vice  vise
aise  aire  aine  aile  aide  bile  bale  base  buse  bure  buee
nuee  dure  dune  hune  aune  aube  auge  ange  anse  ansi  luge
luxe  lune  luxa  cube  jube  jupe  jute  fute  cure  mure  mule
muse  musc  male  rale  race  rame  pame  pate  rate  rare  rave
have  mare  mari  marc  mark  mars  mans  mage  tare  taie  trie
tria  trio  brio  bric  eric  cric  chic  choc  chou  croc  froc
frac  frai  vrai  vrac  trac  tram  troc  trot  trop  drop  drap
trou  brou  broc  bloc  truc  irai  crin  coin  foin  loin  loir
soir  soif  soin  sain  sait  lait  laid  caid  raid  rail  bail
bain  gain  nain  nais  nait  naif  nuit  fuit  frit  fret  pret
pres  pref  bref  ores  tres  gres  gras  glas  gros  grog  greg
grec  gris  fris  cris  cuis  buis  puis  suis  suif  juif  juin
suit  huit  cuit  cuir  fuir  ouir  ouie  suie  skie  skia  spie
epie  epia  epee  suce  puce  suca  suiv  luit  soit  sont  sono
sort  sert  vert  vers  sers  sors  hors  lors  dors  dois  dais
daim  faim  fais  fait  fart  part  kart  port  pont  pond  fond
tond  todd  tord  nord  bord  bond  gond  gong  gang  sang  sana
fana  fane  fame  dame  dome  tome  toge  tige  pige  pire  lire
lice  lise  lese  lest  lent  kent  dent  deni  demi  defi  ment
meat  beat  beau  seau  peau  meut  meus  mous  moue  mode  iode
moud  coud  coup  corp  cour  four  tour  tous  toux  houx  houe
home  boue  boxe  boxa  bouc  boul  soul  seul  souk  bous  vous
nous  bois  pois  poix  paix  pair  hair  prix  pris  iris  ibis
ibid  bris  bras  brie  brin  brun  brut  crut  chut  chat  chas
char  chai  chah  shah  cher  chef  chez  clef  crue  drue  grue
doue  dose  dise  bise  pise  pese  pesa  lesa  pene  rene  rend
rond  fend  tend  pipe  pope  pore  fore  porc  pole  tole  sole
dine  dina  dira  dura  mura  aura  lira  rira  rire  sire  site
rite  rime  cime  dime  dire  rima  rama  rami  rata  rala  rixe
dosa  dota  iota  dote  cote  coke  cone  conf  zone  zona  rose
roue  roux  poux  pouf  pour  jour  joug  peur  heur  leur  doux
                                          ----
deux  ceux  yeux  bout  gout  mout  boit  doit  dont  donc  jonc
dort  prit  noix  noir  poil  phil  pool  taux  baux  baud  maux
faux  flux  faut  saut  saur  sauf  loup  menu  test  lyse  lyre
lisp  daze  face  fade  bang  banc  rang  ring  king  gant  tant
tact  tait  tain  taon  thon  faon  main  mail  tais  tank  long
boni  tard  lard  land  yard  dard  fard  pers  perl  sept  surf
turf  turc  gril  oree  orge  orme  urge  urne  uree  laic  sein
rein  clin  clip  slip  clan  flan  plan  plat  elan  clam  cran
aria  malt  cuba  abbe  basa  jasa  jusa  java  nasa  naja  baba
apre  apte  opte  opta  acte  acre  atre  etre  erre  erra  ocre
ogre  visa  vive  joie  volt  colt  cola  coma  kola  pays  gens
gena  gera  roll  judo  cadi  cepe  elut  elit  edit  emit  omit
omet  emet  emut  emue  elue  elle  omis  emis  elis  emir  hetu
vetu  velu  valu  velo  veto  tete  bete  bebe  beta  recu  zele
feue  feve  fevr  seve  seme  meme  sema  sexe  sexy  neve
```

Exercice 3 : Calculs de chemins
-------------------------------

Pour obtenir un chemin entre deux mot, un parcours est effectué à partir du premier mot, si le second mot est rencontré lors de ce parcours, alors il existe un chemin entre ces deux mots. Pour affichier le chemin, il faut mémoriser tous les mots rencontrés lors du parcours.

Le chemin suivant a été obtenu grâce à la méthode `Graphe.cheminDfs(m1, m2)` qui effectue un parcours en profondeur d'abord et imprime le chemin entre les mots `m1` et `m2` s'il existe.

#### Chemin entre les mots _lion_ et _peur_

```
lion -> pion -> paon -> pain -> vain -> vais -> mais -> hais -> haie -> hait ->
haut -> vaut -> vaux -> veux -> veuf -> oeuf -> neuf -> nerf -> cerf -> serf ->
sera -> aera -> aere -> here -> hele -> tele -> fele -> fela -> hela -> dela ->
deja -> deca -> decu -> vecu -> veau -> venu -> tenu -> tetu -> fetu -> feru ->
fera -> fora -> fort -> font -> foot -> flot -> flou -> clou -> clot -> ilot ->
plot -> plut -> peut -> veut -> vent -> vend -> pend -> perd -> pere -> gere ->
gene -> cene -> cane -> cade -> cage -> cake -> cafe -> came -> lame -> lama ->
pama -> papa -> sapa -> saga -> sage -> sake -> sape -> pape -> page -> pane ->
pale -> hale -> hase -> vase -> jase -> jade -> jude -> rude -> rode -> role ->
mole -> mile -> mike -> mise -> miss -> mess -> mens -> cens -> cent -> sent ->
sens -> sans -> sais -> jais -> jars -> gars -> pars -> paru -> parc -> pari ->
sari -> sali -> soli -> joli -> poli -> polo -> solo -> silo -> kilo -> kilt ->
tilt -> tint -> vint -> vont -> mont -> mort -> mord -> mors -> tors -> tort ->
toit -> tout -> aout -> cout -> coit -> voit -> vois -> voir -> voix -> voie ->
vote -> hote -> hate -> hata -> gata -> gala -> gale -> gate -> gite -> mite ->
vite -> vice -> vise -> aise -> aire -> aine -> aile -> bile -> bale -> base ->
buse -> bure -> dure -> dune -> hune -> aune -> aube -> cube -> cure -> mure ->
mule -> male -> rale -> race -> rame -> pame -> pate -> rate -> rare -> mare ->
tare -> taie -> trie -> tria -> trio -> brio -> bric -> eric -> cric -> crin ->
coin -> foin -> loin -> loir -> soir -> soif -> soin -> sain -> sait -> lait ->
laid -> caid -> raid -> rail -> bail -> bain -> gain -> nain -> nais -> nait ->
nuit -> fuit -> frit -> fret -> pret -> pres -> ores -> tres -> gres -> gras ->
gros -> gris -> fris -> cris -> cuis -> buis -> puis -> suis -> suif -> suit ->
soit -> sont -> sort -> sert -> vert -> vers -> sers -> sors -> hors -> lors ->
dors -> dois -> dais -> daim -> faim -> fais -> fait -> fart -> part -> port ->
pont -> pond -> fond -> tond -> todd -> tord -> nord -> bord -> bond -> gond ->
gong -> gang -> sang -> sana -> fana -> fane -> fame -> dame -> dome -> tome ->
toge -> tige -> pige -> pire -> lire -> lice -> lise -> lese -> lest -> lent ->
kent -> dent -> ment -> meat -> meut -> meus -> mous -> moue -> moud -> coud ->
coup -> cour -> four -> tour -> tous -> toux -> houx -> houe -> boue -> bouc ->
boul -> bous -> bois -> pois -> poix -> paix -> prix -> pris -> iris -> bris ->
brie -> brin -> brun -> brut -> crut -> crue -> drue -> doue -> dose -> rose ->
roue -> roux -> poux -> pouf -> pour -> peur
```

## Exercice 4

#### Chemin entre les mots _lion_ et _peur_

Le chemin suivant a été obtenu grâce à la méthode `Graphe.cheminBfs(m1, m2)` qui effectue un parcours en largeur d'abord et imprime le chemin entre les mots `m1` et `m2` s'il existe.

Le chemin obtenue entre deux mots à partir d'un parcours en largeur d'abord est aussi appelé __chemin le plus court__.

```
lion -> pion -> paon -> pain -> pair -> paix -> poix -> poux -> pouf -> pour ->
peur
```
