//
//  Game.swift
//  stonedge
//
//  Created by Pascal Bourguignon on 05/03/2024.
//

import Foundation

open class Game {

    var title : String = ""
    var description : [String] = []
    var stone = Stone(x:0, y:0, orientation: vertical)
    var cells: [[Cell]] = []

    public init(stone: Stone = Stone(x:0, y:0, orientation: vertical), cells: [[Cell]]) {
        self.stone = stone
        self.cells = cells
    }

    public func move(direction: Direction) {
        // Moves the stone of the game in the given direction.
        var orientation : Orientation
        var x0: Int
        var y0: Int
        var z0: Int
        var x1: Int
        var y1: Int
        var z1: Int
        (orientation, x0, y0, z0, x1, y1, z1) = self.stone.coverage()
        if (orientation == Orientation.vertical) {
            self.cells[x0][y0].stoneLeavesCell(stone: self.stone)
        }else{
            self.cells[x0][y0].stoneLeavesCell(stone: self.stone)
            self.cells[x1][y1].stoneLeavesCell(stone: self.stone)
        }
        self.stone.move(in: direction)
        (orientation, x0, y0, z0, x1, y1, z1) = self.stone.coverage()
        if (orientation == Orientation.vertical) {
            self.cells[x0][y0].stoneMovedOverCell(stone: self.stone)
        }else{
            self.cells[x0][y0].stoneMovedOverCell(stone: self.stone)
            self.cells[x1][y1].stoneMovedOverCell(stone: self.stone)
        }
    }

}
