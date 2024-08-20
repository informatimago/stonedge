//
//  Cell.swift
//  stonedge
//
//  Created by Pascal Bourguignon on 04/03/2024.
//

import Foundation
import SwiftUI


public enum GameStatus {
    case playing, win, lose
}

public protocol CellInterface {

    func stoneMovedOverCell(stone: Stone)
    // Do cell specific behavior when the stone moves over the cell.
    // May signal a game-won or game-lost condition.

    func stoneLeavesCell(stone: Stone)
    // Do cell specific behavior when the stone moves from the cell.
    // May signal a game-won or game-lost condition.

    func gameStatus(stone: Stone) -> GameStatus
    // Returns nil :win or :lose depending on what would happen if the stone was on the cell.

    func cellColor() -> Color?;

}


open class Cell : CellInterface, ObservableObject, Identifiable {

    // This is an abstract cell. Cells are square, and all of the same size.

    var x: Int = 0
    var y: Int = 0

    public init(x: Int, y: Int) {
        self.x = x
        self.y = y
    }

    open func cellColor() -> Color? {
        return Color.white
    }

    open func stoneMovedOverCell(stone: Stone){
        // Nothing
        print("### stone moved over cell \(String(describing: type(of: self))) \(x),\(y)")
    }

    open func stoneLeavesCell(stone: Stone){
        // Nothing
        print("### stone   leaves   cell \(String(describing: type(of: self))) \(x),\(y)")
    }

    open func gameStatus(stone: Stone) -> GameStatus {
        return GameStatus.playing
    }

}

open class SolidCell : Cell {
    // The stone may remain securely on a solid cell.

    public override func cellColor() -> Color? {
        return Color.green
    }

}

open class TargetCell : Cell {
    // Once the stone is in vertical position on a target cell, the game is won.

    public override func cellColor() -> Color? {
        return Color.white
    }

    public override func stoneMovedOverCell(stone: Stone){
        super.stoneMovedOverCell(stone: stone)
        if (verticalp(direction: stone.orientation)) {
            signalGameWon()
        }
    }

    public override func gameStatus(stone: Stone) -> GameStatus {
        if (verticalp(direction: stone.orientation)) {
            return GameStatus.win
        }
        return GameStatus.playing
    }

}

open class EmptyCell : Cell {
  // When the stone is over an empty cell, the game is lost.

    public override func cellColor() -> Color? {
        return nil
    }

    public override func stoneMovedOverCell(stone: Stone){
        super.stoneMovedOverCell(stone: stone)
        signalGameLost(why: "moved over an empty cell")
    }

    public override func gameStatus(stone: Stone) -> GameStatus {
        return GameStatus.lose
    }

}

open class PathwayCell : Cell {
    // When a pathway cell is :open, it supports a stone;
    // when it's :closed the stone falls down and the game is lost.

    public enum State {
        case closed, open
    }

    @Published var state = State.closed // A pathway cell may be :open or :closed.

    public init(x: Int, y: Int, state: State = State.closed) {
        super.init(x:x, y:y)
        self.state = state
    }

    public override func cellColor() -> Color? {
        if state == State.open {
            return Color.green
        } else {
            return Color.yellow
        }
    }

    public override func stoneMovedOverCell(stone: Stone) {
        super.stoneMovedOverCell(stone: stone)
        if (state == State.closed){
            signalGameLost(why: "moved over a closed pathway cell")
        }
    }

    public override func gameStatus(stone: Stone) -> GameStatus {
        return GameStatus.lose
    }

    func switchCell(){
        switch self.state {
        case State.open: self.state = State.closed
        case State.closed: self.state = State.open
        }
    }
}


open class ButtonCell : Cell {
    // This is an abstract button cell.
    // Button cells may switch the state of pathway-cells.

    public var switches: [PathwayCell] = [] // A list of cells that may be switched when the stone is over the button cell.

    public init(x: Int, y: Int, commandedCell: PathwayCell? = nil) {
        super.init(x:x, y:y)
        if let commandedCell {
            switches.append(commandedCell)
        }
    }

    func switchPathwayCells(){
        for cell in switches {
            cell.switchCell()
        }
    }
}


open class RedButtonCell : ButtonCell {
    // A red button cell switches its pathway cells
    // as soon as the stone is over it."

    public override func cellColor() -> Color? {
        return Color.red
    }

    public override func stoneMovedOverCell(stone: Stone){
        super.stoneMovedOverCell(stone: stone)
        switchPathwayCells()
    }

}


open class BlueButtonCell : ButtonCell {
    // A blue button cell switches its pathway cells
    // only when the stone is over it in vertical position.

    public override func cellColor() -> Color? {
        return Color.blue
    }

    public override func stoneMovedOverCell(stone: Stone){
        if (verticalp(direction: stone.orientation)){
            switchPathwayCells()
        }
    }

}


open class CrumbleCell : Cell {
    // When a crumble cell is :open, it supports a stone;
    // when it's :closed the stone falls down and the game is lost.

    enum State {
        case open, closed
    }

    @Published var state = State.open // A crumble cell goes from :open to :closed the first time it's walked over, and stays :closed thereafter.

    public override func cellColor() -> Color? {
        if state == State.open {
            return Color.orange
        }else{
            return nil
        }
    }

    public override func stoneMovedOverCell(stone: Stone) {
        super.stoneMovedOverCell(stone: stone)
        if (state == State.closed){
            signalGameLost(why: "moved over a crumbled cell")
        }
    }

    public override func gameStatus(stone: Stone) -> GameStatus {
        if (state == State.closed){
            return GameStatus.lose
        }
        return GameStatus.playing
    }

    public override func stoneLeavesCell(stone: Stone) {
        super.stoneLeavesCell(stone: stone)
        self.state = State.closed
    }

}


open class IceCell : Cell {
    // An ice cell supports an horizontal stone, but
    // when the stone is over it in vertical position, it breaks, the stone falls down, and the game is lost.

    public override func cellColor() -> Color? {
        return Color.cyan
    }

    public override func stoneMovedOverCell(stone: Stone) {
        super.stoneMovedOverCell(stone: stone)
        if (verticalp(direction: stone.orientation)){
            signalGameLost(why: "moved vertically over an ice cell")
        }
    }

    public override func gameStatus(stone: Stone) -> GameStatus {
        if (verticalp(direction: stone.orientation)){
            return GameStatus.lose
        }
        return GameStatus.playing
    }
}
