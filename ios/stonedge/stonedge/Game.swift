//
//  Game.swift
//  stonedge
//
//  Created by Pascal Bourguignon on 05/03/2024.
//

import Foundation


open class Game : ObservableObject {

    var title : String = ""
    var description : [String] = []
    @Published var stone = Stone(x:0, y:0, orientation: vertical)
    @Published var cells: [[Cell]] = []
    @Published var step: Int = 0

    public init(stone: Stone = Stone(x:0, y:0, orientation: vertical), cells: [[Cell]]) {
        self.stone = stone
        self.cells = cells
        self.step = 0
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
        print("before move \(direction) (\(self.stone.x),\(self.stone.y)) \(orientation) (\(x0),\(y0),\(z0)) (\(x1),\(y1),\(z1))")
        if (orientation == Orientation.vertical) {
            self.cells[y0][x0].stoneLeavesCell(stone: self.stone)
        }else{
            self.cells[y0][x0].stoneLeavesCell(stone: self.stone)
            self.cells[y1][x1].stoneLeavesCell(stone: self.stone)
        }
        self.stone.move(in: direction)
        (orientation, x0, y0, z0, x1, y1, z1) = self.stone.coverage()
        print("after move \(direction) (\(self.stone.x),\(self.stone.y)) \(orientation) (\(x0),\(y0),\(z0)) (\(x1),\(y1),\(z1))")
        if (orientation == Orientation.vertical) {
            self.cells[y0][x0].stoneMovedOverCell(stone: self.stone)
        }else{
            self.cells[y0][x0].stoneMovedOverCell(stone: self.stone)
            self.cells[y1][x1].stoneMovedOverCell(stone: self.stone)
        }
        step += 1
        print("step = \(step)")
    }

}
