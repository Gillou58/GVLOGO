|========================================================|
|
|                  G V S O F T                                           |
|                  Projet : GVLogo                                       |
|                  Description : Traitement des listes                     |
|                  Unité : GVLists.pas                                   |
|                  Ecrit par  : VASSEUR Gilles                           |
|                  e-mail : g.vasseur58@laposte.net                      |
|                  Copyright : © G. VASSEUR                              |
|                  Date:    19-06-2014 16:08:15                          |
|                  Version : 2.0.0                                       |
|                                                                        |
|========================================================|

//
// Unité pour le traitement des listes
//
// ##############################################################
//
// LISTES SIMPLES
//
// 1. Un mot est un ensemble de caractères autres que le caractère blanc.
// 2. Une liste est un ensemble de mots placés entre crochets : [ et ].
// 3. Une liste peut comprendre d'autres listes.
// 4. Un élément d'une liste est soit un mot soit une liste.
// 5. Il existe une liste qui ne comprend aucun élément et qu'on appelle
// la liste vide.
//
// Exemples :
//
// [coucou] est une liste valide.
// [coucou [encore coucou]] est une liste valide.
// [coucou coucou2] est une liste valide.
// [[coucou]] est une liste valide.
// [[coucou] n'est pas une liste valide (il manque un crochet fermant).
//
// Les listes sont à la base du langage GVLogo.
// Elles permettent un traitement des chaînes de caractères à partir de
// primitives simples.
// Les lignes d'un programme en GVLogo sont considérées comme des listes.
//
// On notera que les éléments sont numérotés de 0 à Count-1.
// Cette façon de compter est conforme aux listes de Pascal.
//

Le logiciel de test fonctionne avec des versions récentes de :
* Delphi (Win32 et Win64)
* Lazarus (Win32 et Linux)

Le dossier Length contient un programme à tester avec Lazarus. Il montre les problèmes liés à la reconnaissance des caractères accentués.