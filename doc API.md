# Description

La librairie lib-chloride permet d'encrypter/ décrypter une liste de Byte en utilisant
l'algorithme AES, ou avec différentes méthodes s'appuyant sur l'AES (mode ECB, CBC, etc...)

## Démarrage rapide

> Ce projet requiert [Stack](https://docs.haskellstack.org/en/stable/) pour être utilisé.
> Une installation à travers GHCup est recommandée.

Pour tester `lib-chloride` directement, il suffit de lancer la commande `stack run` dans
le dossier `lib-chloride/`, et une petite démonstration vous sera présenté!

```bash
cd lib-chloride/ && stack run
```

```haskell
import Byte
import Cipher
import Cipher.Internal.Utils

main = do
    -- On créer un bloc de données (16 bytes)
    let block = intsAsCipherData [0x32, 0x43, 0xf6, 0xa8, 0x88, 0x5a, 0x30, 0x8d, 0x31, 0x31, 0x98, 0xa2, 0xe0, 0x37,0x07, 0x34]
    -- La clé à utiliser (ici elle fait 16 bytes, mais elle pourrait aussi faire 24 ou 32 bytes)
    let key   = intsAsCipherData [0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf,0x4f, 0x3c]
    -- Encryptage :
    let res = encrypt key block
    putStrLn $ showByteBlock res
    -- Décryptage :
    let inv = decrypt key res
    putStrLn $ showByteBlock inv
```

Ici, nos données sont initialement représentées sous forme d'entier (`Int`), qui sont
ensuite converti en liste de bytes par `intsAsCipherData`. On encrypte d'abord ce bloc
avec la clé donnée, pour ensuite décrypter le résultat de l'opération précédente, ce
qui devrait nous donner le bloc initial.

## Autres types de chiffrement

Il est possible de chiffrer un bloc avec une méthode dite *Electronic Codebook* (ECB),
qui permet de chiffrer plus de 16 bytes à la fois. Pour cela il suffit de remplacer
l'appel à `encrypt` par `encryptECB`, et de même pour le décryptage avec `decryptECB`.

Une autre technique de chiffrement est le *Cipher Block Chaining* (CBC), qui, pour
chaque bloc de 16 bytes, utilise le résultat du chiffrement précédent et le XOR avec
le bloc à encrypter. Cette fonctionnalité est disponible en utilisant `encryptCBC`/`decryptCBC`.

# Détails et choix techniques

## Implémentation

En surface, le cipher prend en entrée deux tableaux de `Byte`s, la clé et le bloc, et
retourne comme résultat un autre tableau de `Byte`s.

Dans un premier temps, le Cipher modifie le bloc à l'aide de fonctions comme `SubBytes`,
`ShiftRows`, `MixColumns` et `AddRoundKey` . Les trois premières fonctions modifient
simplement le bloc sans paramètre supplémentaire, et c'est la quatrième (`AddRoundKey`)
qui utilise la clé. Nous reprenons alors le résultat de l'itération précédente comme
état initial pour la suivant, et ceci 10, 12 ou 14 fois, selon la taille de la clé.

Les bytes sont représentés en format *big-endian*, ça a dire que le "premier" bit
est celui de poids le plus fort, et le dernier est celui le plus faible. Ceci est
un détail important, notamment lorsqu'on souhaite utiliser les primitives de cette
librairie plus en détail. C'est aussi le cas pour les words, représentant 4 bytes
chacun, toujours en *big-endian*.

En réalité, il se trouve qu'à la fois `Word` et `Byte` sont construits avec des
polynômes (de `Byte`s et de `Bit`s, respectivement); cette structure mathématique
est en fait la fondation sur laquelle repose une grande partie de la librairie.

## Outillage

Ce projet utilise la boîte d'outils [Stack](https://docs.haskellstack.org/en/stable/),
ce qui nous a permis de vite commencer à coder sans s'embêter trop avec la création
`Makefile` ou la structure du projet. En plus de cela, Stack s'intègre à une pléthore
d'autres outils, tels que des IDEs, ce qui est extrêmement pratique sur le long-terme.
Finalement, deux fonctionnalités qui se sont révélées être décisives nous assurent que
c'est un bon choix pour de futurs projets:

  - Stack permet de facilement interagir et inspecter sa librairie grâce à `stack ghci`,
    qui est une commande qui "augmente" GHCi pour permettre de plus facilement
    explorer son propre code. Ceci fût extrêmement pratique lors des inévitables
    séances de debugging;
  - Stack permet d'écrire des batteries de tests directement en Haskell, comme n'importe
    quel autre fichier source; ceci à deux avantages majeurs:
      - Support transparent par l'IDE avec complétion, erreurs, linting, etc...
      - plus grandes expressivité que des scripts GHCi

Pour aiser encore plus l'écriture de tests, qui peut souvent être un travail répétitif
et remplit de redondance, nous avons mis en plus un "harnais de test," se situant
dans `lib-chloride/test/Runner.hs`, qui a deux objectifs principaux: le premier est
d'alléger le code de test en éliminant le plus possible ce qui n'est pas directement
des données de tests, le deuxième est de rendre l'action *d'effectuer* un test plus
utile et productive. Les tests sont d'abord catégorisés par module cible, puis par
méthode (ou parfois groupe de méthode), chacun aillant un nom attribué. Ensuite,
on lance tous les tests appartenant à un module, et on comptabilise les succès et
échecs pour chaque méthode. Au lieu de s'arrêter dès qu'un des tests n'est pas validé
(comme c'est le cas avec la méthode `Control.Exception.assert`), ici on fait
module-par-module. On pourrait bien sûr tester tous les modules d'un seul coup,
mais on a souvent des tests qui présupposent que le comportement d'autres
composants est correct; ainsi, *tout* tester d'un seul coup risquerait de créer des
erreurs en cascade.

Pour rendre cela plus concret, voici un pseudo-code de test dans un langage "idéal,"
fait spécifiquement pour les tests:

```
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

et voici comment celui-ci se traduirait en utilisant notre harnais:

```haskell
main =
    do
        testStuff
        testThings

testStuff
    = runTests "Stuff" [testFoo, testBar]

testFoo
    = newTest "foo" [
           (
            foo 1,
            2
        ), (
            foo 0,
            -1
        ), (
            foo -10,
            10
        )
    ]

testBar
    = newTest "bar" [
        ...
    ]

testThings
    = runTest "Things" [...]
```

Comme vous pouvez le voir, bien qu'on ne soit pas au niveau d'un DSL, ce style est
extrêmement déclaratif. Chaque test est soit nommé, soit implicitement numéroté, ce
qui le rends facile à retrouver et discerner au cas où il échoue. Par exemple, si
`foo -10` valait soudainement 5 au lieu de 10, lancer `stack test` nous dirait
exactement ce qui ne va pas et où:

```
BEGIN: Stuff

  foo: 1 OKs, 1 FAILs
    [FAIL] testFoo #2
        Expected: 10
        but got:  5
```

Pour quelque chose qui tient en une petite dizaine de lignes (en ignorant les
commentaires), c'est plus élégant, utile et informatif que des `assert` qui crashe
constamment!