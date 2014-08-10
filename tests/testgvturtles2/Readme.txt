{ |==========================================================|
|                                                                        |
|                  G V S O F T                                           |
|                  Projet : GVLogo                                       |
|         Description : Tortue graphique (version BGRABitmap)   |
|                  Unité : GVTurtles2.pas                                |
|                  Ecrit par  : VASSEUR Gilles                           |
|                  e-mail : g.vasseur58@laposte.net                      |
|                  Copyright : © G. VASSEUR                              |
|                  Date:    08-08-2014 15:14:48                          |
|                  Version : 1.1.0                                       |
|                                                                        |
|============================================================| }

// GVTurtles2 - part of GVLOGO
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

// Unité de la tortue graphique de GVLOGO
//
// ##############################################################
//
// La tortue graphique permet de dessiner sur une surface
// en fonction d'ordres simples.
//
// Cette unité utilise la bibliothèque BGRABITMAP pour une 
// meilleure gestion du graphisme.
// N.B. : ne fonctionne qu'avec LAZARUS