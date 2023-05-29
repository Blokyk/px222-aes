# Séance 1 -- 21/03/2023

## Notes de séance

Nous avons passer la plupart de la séance à lire la specification AES et à
réfléchir à la structure et l'implémentation du projet en haskell (e.g. comment
représenter les polynômes ou les données d'entrée)

# Séance 2 -- 5/04/2023

## Objectifs

Commun:
  - commencer l'implémentation des polynomes en Haskell

Stretch:
  - réfléchir à l'impl de $GF(2^8)$ en utilisant $GF(2)$

Léa:
  - finir la lecture de la spec AES
  - revoir les structures algébriques
  - écrire des tests pour l'impl des polynomes

Zoë:
  - finir impl de $\mathbb{Z}/2\mathbb{Z}$ (i.e. $GF(2)$)
  - tenter d'implémenter modulo de polynôme
  - lire la spec + bien comprendre les polynomes dans $GF(2)$

## Retrospective

Depuis la dernière séance, Nous avons setup l'environment de travail, par exemple en
établissant des fichiers pour le linting (`.editorconfig` et `hlint.yaml`), mais
surtout en utilisant Stack pour gérer et structurer le projet. Nous espérons que
l'investissement temporel nécessaire à cette organisation se révélera bénéfique,
notamment pour la mise en place de tests, mais aussi pour l'utilisation d'outils
externes (e.g. IDE).

De plus, on a défini `Group`, `Ring` et `Field`, et avons également commencé à travailler
sur `Bit`.

## Notes de séance

Pendant la séance, multiples tests ont été écrits, d'abord sous forme de scripts GHCI,
puis, pour mieux s'incorporer avec le système Stack, en code source "classique," (dans `test/`)
qui vérifie l'exactitude des résultats de certaines expressions à l'aide de `Control.Exception.assert`.

On a également complété l'implémentation de `Bit`, en y ajoutant des instances pour `Ring`
et `Field`.

L'implémentation de `Polynomial` fut aussi initiée. Au début, nous pensions
éventuellement faire une typeclass avec quelques méthodes à son coeur, soutenu de
fonctions auxiliaires agissant sur des instances de cette classe, mais au final
la "surface" de la classe se réduisait à celle d'une liste, donc nous avons préféré
faire un datatype paramétrisé par le type de ses coefficients, ce qui a rendu
l'implémentation beaucoup rapide et facile.
Bien que cette dernière soit encore incomplète, nous avons déjà codé la division
euclidienne, qui est une importante partie des mécanismes suivants.

# Séance 3 -- 18/04/2023

## Objectifs

Commun:
  - [ ] implémentation de 4.3 $(GF(256))$
  - [ ] écriture et planification du pseudocode de la partie Chiffrement (5)

Zoë:
  - [x] fixer l'implémentation de la multiplication
  - [x] écrire des tests
  - [x] cleanup général
  - [ ] implémenter la division entre bytes

Léa:
  - [x] implémentation de la fin du point 4 de la spec
  - [ ] écriture des tests
  - [ ] commencer l'implémentation du point 5 de la spec

## Retrospective

Tout d'abord, l'implémentation des polynômes a été presque complétée depuis la
dernière séance, ce qui nous a permis de rapidement commencé l'implémentation
de `Byte` $(GF(256))$. Une partie de notre énergie fut aussi dépensée à écrire
de meilleur outils pour nous mêmes, comme par exemple l'affichage de polynôme,
ou des utilitaires pour plus facilement manipuler nos nouveaux types. Cela nous
évite aussi de faire des erreurs "bêtes," comme créer des bytes avec 9 chiffres,
car nos petits utils peuvent vérifier la cohérence des données que l'on rentre. C'est
la raison pour laquelle nous avons par ex. choisi de ne pas exposer le constructeur
de `Polynomial` directement, mais à la place d'avoir différentes fonctions
permettant de créer un nouveau polynôme (cela nous garantis notamment un invariant
que l'on utilise souvent dans l'implémentation de `Polynomial`: le coefficient de
plus haut degré n'est jamais égal à zéro.)

### Une petite note sur les tests

Un autre aspect qui a été travaillé entre les séances est le harnais de tests:
avec l'expansion de la surface à tester, il devient nécessaire de séparer chaque
ensemble de tests, ainsi que de rendre leur écriture *et* lecture plus rapide et
facile. Ici aussi, nous avons choisi d'investir un peu plus de temps pour écrire
quelques utilitaires permettant de nous rapprocher de cette clarté idéale.
Un autre désavantage des tests effectué en utilisant des simples `assert`s est
le manque de feedback: si le test réussi, on voit simplement s'afficher un
autre "OK," et sinon l'application crashe immédiatement sans donner la "vraie"
valeur ni celle attendue. Le premier avantage des fonctions de `test/TestUtils.hs`
est qu'elles fournissent plus d'information dans chacun des cas (e.g. dans le cas
d'un échec, la valeur "réelle" et la valeur attendue sont toutes les deux affichées),
et permettent aussi de gérer des ensembles de tests plus facilement (e.g. on peut
donner un nom à un groupe de tests, et un ensemble ne s'arrête pas juste au premier
test qui échoue).

Dans les faits, nous obtenons la "hiérarchie" suivante pour les tests:
```
driver (e.g. do-block dans test/Main.hs)
    composant "Stuff"     <- ensemble de tests pour le module Stuff
        unité "foo"       <- tests pour la fonction Stuff.foo
            foo 1 == 2    <- cas de test #1
            foo 0 == -1   <- cas de test #2
            foo -10 == 10 <- cas de test #3
        unité "bar"
            ...
    composant "Things"
        ...
```

## Notes de séance

Au final, la division entre bytes s'annonce plus corsé que prévu,
notamment à cause de l'algorithme d'euclide nécessaire, dont nous n'avons
pour l'instant pas un prototype qui marche. Malgré nos efforts, nous ne sommes
pas certains de la cause exact du problème.

En plus de cela, nous avons également commencé à réfléchir à l'implémentation
du cipher plus en détail, comme par exemple comment transporter les clés entre
rounds, ou comment encoder le fameux "State" et le passer entre fonctions.

# Séance 4 -- 3/05/2023

## Objectifs

Commun:
  - [x] écriture du pseudo-code pour l'inversion de polynôme/Euclide étendu

Zoë:
  - [x] réflechir à la structure du cipher + début d'impl
  - [x] implémenter l'expansion de clé

Léa:
  - [x] écrire des tests pour Word
  - [x] étendre l'implémentation de Word (par ex. utilitaires)

## Rétrospective

### Implémentation du cipher

Pour implémenter le cipher, nous avions initialement pensé à utilisé la monade `State`,
qui permet de manipuler un objet avec un côté "pur" et un côté "mutable." Étant donné
la nature de l'algorithme AES, cela semblait être une plutôt bonne idée. Cependant,
une fois implémentée, elle n'était pas si utile que ça, surtout couplé avec la gestion
des clés que nous avons choisi (cf. paragraphe suivant). L'utilisation de `State`
partout finissait par nuire à la lisibilité et alourdir le code, surtout étant donné
que toutes les modifications d'état se faisait l'une à la suite des autres, et n'était
pas visible dans leur globalité, la plupart des opérations étant plutôt bien encapsulées,
puisque seules `cipher`/`decipher` devaient vraiment gérer un "état"

De plus, lors de cette séance, l'implémentation et la gestion des clés a été réfléchie.
Nous n'avons pas besoin de générer un énorme tableau de dix clés avant de commencer le
chiffrage. Il est plus simple d'utiliser qu'une clé à chaque tour et de la modifier en
conséquence. On ne garde ainsi qu'une seule clé en mémoire à la fois.
### Implémentation du cipher
Lors de l'implémentation du Cipher, la monade State qui était utilisée à finalement été abandonée , qui ne serait pas utiles puisque les changements appliqués à chaque tour au bloc et à la clé ne sont pas visibles par l'utilisateur, qui n'a besoin que de la sortie, donc la dernière étape. 
De plus, lors de cette séance, l'implémentation et la gestion des clés a été réfléchie. Nous n'avons pas besoin de 
générer un énorme tableau de dix clés avant de commencer le chiffrage. Il est plus simple d'utiliser qu'une clé à chaque tour et de la modifier en conséquence. On ne garde qu'une seule clé en mémoire, ainsi.


## Notes de séance

Une bonne partie de la séance fut dédié à mettre au clair comment le bug sur l'inverse
pourrait être réglé. Cette séance a aussi permis de réfléchir à l'implémentation du Cipher.

### Le problème avec l'inverse

La difficulté rencontrée avec l'inverse n'a pas été simple à surmonter. Un bug en
entraînant un autre, force a été de constater que l'inversion ne fonctionnait pas
à cause d'un bug dans la division euclidienne, qui ne fonctionnait pas quand il
y avait des zéros dans le dividende. Il s'agit donc du prochain objectif: fixer
la division euclidienne.

# Séance 5 -- 17/05/2023

## Objectifs

Commun:
  - [x] finir encryption/decryption

Zoë:
  - [x] vaincre le dragon de la division euclidienne
  - [x] faire les fonctions du cipher inverse (invSubBytes, invShiftRows, invMixColumns)

Léa:
  - [x] écrire le "driver" du cipher inverse (i.e. invCipherFunc)

## Rétrospective

Bien qu'il reste environ deux semaines avant le "vrai" jalon, il semblait impératif
que le cipher soit finit, ne serait-ce que dans un état "basique." Peu de temps après
la 4e séance, l'encryption était fonctionnelle, et il ne restait que la decryption
à faire. Cette dernière s'est révélée relativement facile à implémenter, en partie
grâce à l'expérience gagnée lors de l'écriture du cipher.

Une petite note cependant sur `subByte` et `invSubByte`: bien que notre implémentation
initiale était basée sur la multiplication de byte (comme spécifié dans la spec),
nous avons vite été informées que, dans l'esprit "mathématique" de cette version,
il serait à la place préférable d'utiliser une multiplication de polynôme. N'ayant
à l'époque pas plus d'indication, nous avons cherché d'autres ressources, jusqu'à
tomber sur *The Design of Rjindael* (2002), un livre écrit par les auteurs originaux
de l'algorithme éponyme, contenant plus de détails quant à certains aspects d'AES.
Dans la section sur `SubByte()` et son inverse, on peut trouver une remarque affirmant
qu'il est possible d'effectuer ces opérations avec une simple multiplication de
polynômes, et que ces-dits polynômes peuvent être déterminés en utilisant une
interpolation de Lagrange. Bien que cette technique soit habituellement utilisée
pour des approximations, il est ici possible de déterminer un polynôme dont le
graphe est strictement équivalent puisque nous sommes dans un corps fini. Ainsi,
après s'être muni des valeurs attendues pour chaque opération (par exemple en
utilisant notre implémentation originale), nous avons calculé les coefficients du
polynôme de Lagrange. Dans le cas de `subByte`, ceci s'est plutôt bien passé, et nous
a donné le polynôme suivant:

$P(X) = 99 + 143X^{127} + 181X^{191} + X^{223} + 244X^{239} + 37X^{247} + 249X^{251} + 9X^{253} + 5X^{254}$

Cependant, le polynôme correspondant à `invSubByte` s'est révélé être trop complexe:
il contenait bien trop de coefficients, et était beaucoup trop lent à calculer. Nous
avons donc opté pour un compromis entre pureté et fonctionnalité: au lieu de déterminer
un seul polynôme pour toute l'opération, on a choisi de déduire un polynôme pour chaque
"étape" de la fonction (un pour la substitution inverse, et un pour l'inversion), puis
de composer leurs *résultats*. Cette démarche fut bien plus fructueuse:

- la substitution correspond à $f^{-1}(X) = 5 + 5X + 254X^2 + 127X^4 + 90X^8 + 120X^{16} +
  89X^{32} + 219X^{64} + 110X^{128}$
- l'inversion correspond à $g(X) = X^{254}$

### Division euclidienne 2: le retour

Correctement implémenter la division euclidienne n'a pas été facile, et a notamment
requis de générer un grand nombre de tests aléatoires pour s'assurer que "fixer" un
bug n'allait pas juste en créer un autre, comme c'était le cas avec les précédentes
tentatives.

Pour rappel, comme nous l'avons découvert [à la séance précédente](#le-problème-avec-linverse),
l'algorithme ne gérait pas correctement les divisions lorsqu'une dividende intermédiaire
contenait des zéros, parce que la façon usuelle dont nous construisons les polynômes
éliminent les coefficients nuls de plus haut degré. Nous avons donc deux solutions
possibles:

    1. garder les coefficients nuls et les traiter différemment
    2. déterminer le nombre de zéros à insérer dans le quotient

Bien que l'option 1. paraît être la plus simple, c'est aussi la moins élégante et
surtout la plus dangereuse: en cassant temporairement un tel invariant, nous devons
être prudent quant aux opérations et fonctions que nous utilisons. Ainsi, nous avons
choisi de commencer par explorer la seconde option. Cependant, trouver une formule
pour le nombre de zéros nécessaires qui couvrait toutes les possibilités se révéla
plus dur que prévu (là encore, les tests automatiques mentionnés plus haut furent
vitaux), et, pressés pas le temps, nous avons finalement opté pour option 1., ce
qui, au final, n'était pas aussi "horrible" que nous l'avions anticipé: nous n'avions
qu'une seule fonction à réimplémenter, et la violation de l'invariant a pu être
restreinte à `divEuclide` seulement. Bien que le code final est toujours relativement
inesthétique, pour l'instant cette implémentation semble marcher.

## Notes de séance

Pendant la séance, nous avons d'abord mis au clair certaines parties de l'algorithme
qui n'était pas forcément comprises de la même façon par nous deux. Ensuite, on a
commencé à "décortiquer" quelles parties seraient les plus rapide à implémenter en C
(enfin plutôt celles qui requiert le moins de théorie/mathématique pure plutôt que
des opérations sur des bytes et bits).

Nous avons aussi discuté de ce qu'il nous restait à faire pour l'implémentation en
haskell, et en avons conclu qu'un petit ménage au niveau de l'API ne serait pas de
trop, mais aussi qu'implémenter différent mode de cipher serait une bonne idée qui
ne devrait pas prendre *trop* de temps.

# Séance 6 -- 5/06/2023

## Objectifs

Commun:
  - [ ] réfléchir à l'implémentation en C (+ makefile etc ?)
  - [ ] compléter le carnet de bord et ajouter des infos

Léa:
  - [ ] faire différent "Block cipher mode" (ECB + CBC + ...)
  - [ ] doc: API

Zoë:
  - [ ] ajouter infos technique au CdB
  - [ ] doc: choix techniques