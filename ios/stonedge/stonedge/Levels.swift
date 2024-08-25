//
//  Levels.swift
//  stonedge
//
//  Created by Pascal Bourguignon on 15/03/2024.
//

import Foundation

let levels = [

  // start green target
  """
    Tutorial 1
    | Green cells are solid ground.
    | The stone can be rolled over it in any orientation.
    | The target is the white cell.
    | The stone should stand vertically on it.

    ..........
    ..........
    ..OOOOOO..
    ..SOOOOO..
    ..OOOOOO..
    ..OOOOOT..
    ..OOOOOO..
    ..........
    ..........
    """,

  // start green ice target
  """
    Tutorial 2
    | Blue cells are ice: not solid enough to support a vertical stone, but
    | solid enough for an horizontal stone (weight is spread over two cells).

    ..........
    ..........
    ..OOIOOO..
    ..SOIOOO..
    ..OOIIIO..
    ..OOIIIT..
    ..OOIOOO..
    ..........
    ..........
    """,

    // start green crumble target
  """
    Tutorial 3
    | Orange cells are weak:
    | they crumble and disappear once the stone has moved over them.

    ..........
    ..........
    ..OOCOOO..
    ..SOCOOO..
    ..OOCCCO..
    ..OOCCCT..
    ..OOCOOO..
    ..........
    ..........
    """,


  """
    Level 36

    ...............
    ...............
    ......AO.......
    ....OOOIIO.....
    ..SOOOICOO1OT..
    ...IIOIOOO.....
    ......IO.......
    ...............
    ...............

    A RED 1
    1 PATHWAY CLOSED
    """,
  """
    Level 37

    ...........
    ...........
    .....OC....
    ....CII....
    ..OO1BS23..
    ..OR4TCOO..
    ....COL....
    .....O5....
    ...........
    ...........

    B BLUE 1
    R RED 4
    L RED 2 3 5
    1 PATHWAY CLOSED
    2 PATHWAY CLOSED
    3 PATHWAY CLOSED
    4 PATHWAY OPEN
    5 PATHWAY OPEN
    """,
  """
    Level 38

    .................
    .................
    ..II.IIIICIIOIT..
    ..II..III.IIIII..
    ..SOII.II.IIIII..
    ..IIICOOC........
    .................
    .................
    """,
  """
    Level 39

    ..............
    ..............
    ..IIB1R2CLO...
    ..IIO.....O...
    ....O.....O...
    ....O.....3O..
    ....O.....OO..
    ....IO...OTO..
    ....SO...CO...
    .....OCOCOO...
    ..............
    ..............

    B BLUE 1
    R RED 2
    L RED 3
    1 PATHWAY CLOSED
    2 PATHWAY CLOSED
    3 PATHWAY CLOSED
    """,

  """
    Level 52

    .........
    ...RO....
    ...OC....
    ..SBOC1..
    ..OTIO2..
    ..3OCO...
    ..OC.....

    R RED 3
    B BLUE 1 2
    1 PATHWAY CLOSED
    2 PATHWAY CLOSED
    3 PATHWAY CLOSED
    """]
