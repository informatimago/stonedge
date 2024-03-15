//
//  Cell.swift
//  stonedge
//
//  Created by Pascal Bourguignon on 04/03/2024.
//

import Foundation

func signalGameWon(){
}
func signalGameLost(){
}


enum GameStatus {
    case playing, win, lose
}

protocol CellInterface {
    
    func stoneMovedOverCell(stone: Stone)
    // Do cell specific behavior when the stone moves over the cell.
    // May signal a game-won or game-lost condition.
    
    func stoneLeavesCell(stone: Stone)
    // Do cell specific behavior when the stone moves from the cell.
    // May signal a game-won or game-lost condition.
    
    func gameStatus(stone: Stone) -> GameStatus
    // Returns nil :win or :lose depending on what would happen if the stone was on the cell.
    
}


class Cell : CellInterface {
    
    // This is an abstract cell. Cells are square, and all of the same size.
    
    var x: Int = 0
    var y: Int = 0
    
    init(x: Int, y: Int) {
        self.x = x
        self.y = y
    }
    
    func stoneMovedOverCell(stone: Stone){
        // Nothing
    }
    
    func stoneLeavesCell(stone: Stone){
        // Nothing
    }
    
    func gameStatus(stone: Stone) -> GameStatus {
        return GameStatus.playing
    }
    
}

class SolidCell : Cell {
    // The stone may remain securely on a solid cell.
}

class TargetCell : Cell {
    // Once the stone is in vertical position on a target cell, the game is won.

    override func stoneMovedOverCell(stone: Stone){
        if (verticalp(direction: stone.direction)) {
            signalGameWon()
        }
    }

    override func gameStatus(stone: Stone) -> GameStatus {
        if (verticalp(direction: stone.direction)) {
            return GameStatus.win
        }
        return GameStatus.playing
    }

}

class EmptyCell : Cell {
  // When the stone is over an empty cell, the game is lost.
    
    override func stoneMovedOverCell(stone: Stone){
        signalGameLost()
    }

    override func gameStatus(stone: Stone) -> GameStatus {
        return GameStatus.lose
    }
    
}

class PathwayCell : Cell {
    // When a pathway cell is :open, it supports a stone;
    // when it's :closed the stone falls down and the game is lost.
    
    enum State {
        case closed, open
    }
    
    var state = State.closed // A pathway cell may be :open or :closed.
    
    init(x: Int, y: Int, state: State = State.closed) {
        super.init(x:x y:y)
        self.state = state
    }
    
    override func stoneMovedOverCell(stone: Stone) {
        if (state == State.closed){
            signalGameLost()
        }
    }

    override func gameStatus(stone: Stone) -> GameStatus {
        return GameStatus.lose
    }

    func switchCell(){
        switch self.state {
        case State.open: self.state = State.closed
        case State.closed: self.state = State.open
        }
    }
}


class ButtonCell : Cell {
    // This is an abstract button cell.
    // Button cells may switch the state of pathway-cells.
    
    var switches: [PathwayCell] = [] // A list of cells that may be switched when the stone is over the button cell.
    
    func switchPathwayCells(){
        for cell in switches {
            cell.switchCell()
        }
    }
}


class RedButtonCell : ButtonCell {
    // A red button cell switches its pathway cells
    // as soon as the stone is over it."
    
    override func stoneMovedOverCell(stone: Stone){
        switchPathwayCells()
    }
    
}


class BlueButtonCell : ButtonCell {
    // A blue button cell switches its pathway cells
    // only when the stone is over it in vertical position.
    
    override func stoneMovedOverCell(stone: Stone){
        if (verticalp(direction: stone.direction)){
            switchPathwayCells()
        }
    }
    
}


class CrumbleCell : Cell {
    // When a crumble cell is :open, it supports a stone;
    // when it's :closed the stone falls down and the game is lost.
    
    enum State {
        case open, closed
    }
    var state = State.open // A crumble cell goes from :open to :closed the first time it's walked over, and stays :closed thereafter.

    override func stoneMovedOverCell(stone: Stone) {
        if (state == State.closed){
            signalGameLost()
        }
    }

    override func gameStatus(stone: Stone) -> GameStatus {
        if (state == State.closed){
            return GameStatus.lose
        }
        return GameStatus.playing
    }

    override func stoneLeavesCell(stone: Stone) {
        self.state = State.closed
    }

}


class IceCell : Cell {
    // An ice cell supports an horizontal stone, but
    // when the stone is over it in vertical position, it breaks, the stone falls down, and the game is lost.
    
    override func stoneMovedOverCell(stone: Stone) {
        if (verticalp(direction: stone.direction)){
            signalGameLost()
        }
    }

    override func gameStatus(stone: Stone) -> GameStatus {
        if (verticalp(direction: stone.direction)){
            return GameStatus.lose
        }
        return GameStatus.playing
    }
}

