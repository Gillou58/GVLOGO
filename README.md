﻿GVLOGO
======

French LOGO language with LAZARUS (Free Pascal)

Implémentation de LOGO avec LAZARUS (Free Pascal)


************************************************

Logiciel GVLOGO - V1.0.0 - Gilles VASSEUR 2014

Sommaire :

1. Objectifs du travail présenté
2. LICENCE
3. Contenu des répertoires
4. Contact

************************************************

1- Objectifs du travail présenté

GVLOGO est un logiciel qui implémente pour Windows (32 et 64 bits) et Linux une version du langage LOGO. Il est par ailleurs fourni avec tous les programmes sources et les utilitaires nécessaires à la compréhension de son fonctionnement.

Le langage utilisé pour son développement est le PASCAL. La réalisation utilise l’EDI LAZARUS qui repose lui-même sur le compilateur FREEPASCAL. Ce sont deux produits suffisamment stables pour intéresser tout programmeur. Qui plus est, ils sont entièrement gratuits.

Les objectifs sont donc :
* d’offrir une version gratuite et opérationnelle d'un dialecte de LOGO.
* de permettre à un programmeur inexpérimenté de prendre conscience des difficultés rencontrées lors de la mise en oeuvre d'un tel projet.


2- LICENCE

Les documents et logiciels du dossier GVLOGO sont protégés selon les termes de la licence GNU GPL qui doit être présente dans le dossier
"GNU GPL".

a. Version anglaise valable juridiquement :

GVLOGO - logiciel  de programmation
Copyright (C) 2014-2015 Gilles VASSEUR — Tous droits réservés.

    This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.

b. Version française pour information :

GVLOGO - logiciel  de programmation
Copyright (C) 2014 Gilles VASSEUR — Tous droits réservés.
  
  Ce programme est un logiciel libre ; vous pouvez le redistribuer ou le modifier suivant les termes de la “GNU General Public License” telle que publiée par la Free Software Foundation : soit la version 3 de cette licence, soit (à votre gré) toute version ultérieure.
  
  Ce programme est distribué dans l’espoir qu’il vous sera utile, mais SANS AUCUNE GARANTIE : sans même la garantie implicite de COMMERCIALISABILITÉ ni d’ADÉQUATION À UN OBJECTIF PARTICULIER. Consultez la Licence Générale Publique GNU pour plus de détails.
  
  Vous devriez avoir reçu une copie de la Licence Générale Publique GNU avec ce programme ; si ce n’est pas le cas, consultez : <http://www.gnu.org/licenses/>.

3- Contenu des répertoires

a. Le répertoire "3rdparty" contient les bibliothèques nécessaires au fonctionnement de GVLOGO.

b. Répertoire "docs" : contient la documentation de GVLOGO (programmation et mode d'emploi - *** travail en cours ***).

c. Répertoire "GNU GPL" : contient la licence GNU GPL sous plusieurs formats, en anglais et en français.

d. Répertoire "medias" : contient les images nécessaires pour les programmes, mais aussi pour la documentation.

e. Répertoire "tests" : contient tous les logiciels de tests des unités de GVLOGO.

   => testgverrors : test du traitement des erreurs

   => testgvwords : test des mots

   => testgvlists : test des listes

   => testgvproplists : test des listes de propriétés

   => testgvturtles : test de la tortue graphique (version avec la bibliothèque BGRABITMAP)

   => testeasyturtle : programme de dessin pour enfants (test approfondi de la tortue graphique en version simple)

   => testgveval : tests de l’évaluation d’expressions 

   => testgvkernel : test du noyau de GVLOGO
   
   => testgvlocvars : test des variables locales

   => testgvautomat : test de l’interpréteur

   => testgvhighlighter : test de la syntaxe colorée d’un éditeur

f. Répertoire "units" : contient toutes les unités nécessaires à GVLOGO.

g. Répertoire "GVLogo" : contient le programme GVLOGO sous sa forme la plus évoluée.

4- Contact

Vous pouvez me contacter par mail : gillesvasseur58@gmail.com

Vous pouvez aussi vous rendre sur mon site pour les éventuelles mises à jour : www.lettresenstock.org

Les suggestions, modifications, propositions et questions sont les bienvenues.

Gilles VASSEUR, le 07 mars 2015

N.B. : Un fichier "history.txt" décrit les changements apportés au projet (dans le dossier "docs").

 
