{ |==========================================================|
|
|                  G V S O F T                                           |
|                  Projet : GVLogo                                       |
|                  Description : Listes de propriétés                    |
|                  Unité : GVPropLists.pas                               |
|                  Ecrit par  : VASSEUR Gilles                           |
|                  e-mail : g.vasseur58@laposte.net                      |
|                  Copyright : © G. VASSEUR                              |
|                  Date:    08-08-2014 21:39:46                          |
|                  Version : 1.0.0                                       |
|                                                                        |
  |============================================================| }

// GVPropLists - part of GVLOGO
// Copyright (C) 2014 Gilles VASSEUR
//
// This program is free software: you can redistribute it and/or modify it under the terms of
// the GNU General Public License as published by the Free Software Foundation,
// either version 3 of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
// without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
// See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with this program.
//  If not, see <http://www.gnu.org/licenses/>.

// Unité pour le traitement des listes de propriétés
//
// ##############################################################
//
// Une liste de propriétés associe des valeurs à des caractéristiques
// choisies pour un objet.
// Ainsi, un chien pourra être défini par les propriétés : race, âge, sexe.
// Chacune de ces propriétés aura une valeur particulière.
//
// Les listes de propriétés sont elles-mêmes à la base du noyau de
// l'interpréteur GVLOGO.
//