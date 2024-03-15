//
//  Game.swift
//  stonedge
//
//  Created by Pascal Bourguignon on 05/03/2024.
//

import Foundation

class Game {
    
    var stone = Stone()
    var cells: [[Cell]] = []
    
    init(stone: Stone = Stone(), cells: [[Cell]]) {
        self.stone = stone
        self.cells = cells
    }
    
    func move(direction: Direction) {
        // Moves the stone of the game in the given direction.
        var orientation : Orientation
        var left: Int
        var back: Int
        var right: Int
        var front: Int
        (orientation, left, back, right, front) = self.stone.coverage()
        if (orientation == Orientation.vertical) {
            self.cells[left][back].stoneLeavesCell(stone: self.stone)
        }else{
            self.cells[left][back].stoneLeavesCell(stone: self.stone)
            self.cells[right][front].stoneLeavesCell(stone: self.stone)
        }
        self.stone.move(in: direction)
        (orientation, left, back, right, front) = self.stone.coverage()
        if (orientation == Orientation.vertical) {
            self.cells[left][back].stoneMovedOverCell(stone: self.stone)
        }else{
            self.cells[left][back].stoneMovedOverCell(stone: self.stone)
            self.cells[right][front].stoneMovedOverCell(stone: self.stone)
        }
    }
    
}
