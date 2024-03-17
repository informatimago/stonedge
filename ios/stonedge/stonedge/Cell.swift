//
//  Cell.swift
//  stonedge
//
//  Created by Pascal Bourguignon on 04/03/2024.
//

import Foundation
import SwiftUI

public func signalGameWon(){
}
public func signalGameLost(){
}


public enum GameStatus {
    case playing, win, lose
}

protocol DrawableCell: Identifiable {
    func view(with geometry: GeometryProxy, d: CGFloat, h: CGFloat, alpha: CGFloat, cornerRadius: CGFloat) -> AnyView
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
    
}


open class Cell : CellInterface,DrawableCell {
    
    // This is an abstract cell. Cells are square, and all of the same size.
    
    var x: Int = 0
    var y: Int = 0
    
    public init(x: Int, y: Int) {
        self.x = x
        self.y = y
    }

    func drawSquare(at point: CGPoint) -> some View {
         Rectangle()
             .frame(width: 3, height: 3)
             .offset(x: point.x - 1.5, y: point.y - 1.5) // Offset to center the square on the point
             .foregroundColor(Color.black)
     }
    
    open func cellPaths(with geometry: GeometryProxy, d: CGFloat, h: CGFloat, alpha: CGFloat, cornerRadius: CGFloat) -> (Path, Path)
    {
        let cosAlpha = cos(alpha)
        let sinAlpha = sin(alpha)
        
        // Compute relative points
        let O = CGPoint(x: 0, y: 0)
        let F = CGPoint(x: d * cosAlpha, y: d * sinAlpha)
        let B = CGPoint(x: d * cosAlpha, y: h + d * sinAlpha)
        let C = CGPoint(x: 0, y: h + 2 * d * sinAlpha)
        let D = CGPoint(x: -d * cosAlpha, y: h + d * sinAlpha)
        let E = CGPoint(x: -d * cosAlpha, y: d * sinAlpha)
        
        // Compute cell origin
        let Oxy = CGPoint(x: (CGFloat(x - y) * d * cosAlpha) + geometry.size.width / 2,
                          y: (CGFloat(x + y) * d * sinAlpha) + geometry.size.height / 2)
        
        // view the cell slab
        
        var path = Path()
        path.move(to: CGPoint(x: Oxy.x + O.x, y: Oxy.y + O.y))
        path.addLine(to: CGPoint(x: Oxy.x + F.x, y: Oxy.y + F.y))
        path.addArc(tangent1End: CGPoint(x: Oxy.x + B.x, y: Oxy.y + B.y), tangent2End: CGPoint(x: Oxy.x + C.x, y: Oxy.y + C.y), radius: cornerRadius)
        path.addArc(tangent1End: CGPoint(x: Oxy.x + C.x, y: Oxy.y + C.y), tangent2End: CGPoint(x: Oxy.x + D.x, y: Oxy.y + D.y), radius: cornerRadius)
        path.addArc(tangent1End: CGPoint(x: Oxy.x + D.x, y: Oxy.y + D.y), tangent2End: CGPoint(x: Oxy.x + E.x, y: Oxy.y + E.y), radius: cornerRadius)
        path.addArc(tangent1End: CGPoint(x: Oxy.x + E.x, y: Oxy.y + E.y), tangent2End: CGPoint(x: Oxy.x + O.x, y: Oxy.y + O.y), radius: cornerRadius)
        path.closeSubpath()

        let n = 4
        let e = d/16
        var ticks = Path()
        for u in [-1.0,1.0] {
            let e0x = Oxy.x - 2*u*e*cosAlpha
            let e0y = Oxy.y + h+2*e*sinAlpha
            let e1x = Oxy.x + -u*e*cosAlpha
            let e1y = Oxy.y + h+e*sinAlpha
            let etx = Oxy.x + 0.0
            let ety = Oxy.y + h
            let e2x = Oxy.x + 0.0
            let e2y = Oxy.y + h-e
            let e3x = Oxy.x + 0.0
            let e3y = Oxy.y + h-2*e
            for k in 0..<n {
                let dx = u*(CGFloat(k+1))*d*cosAlpha/CGFloat(n+1)
                let dy = (CGFloat(k+1))*d*sinAlpha/CGFloat(n+1)
                ticks.move(to: CGPoint(x:e0x+dx, y:e0y+dy))
                ticks.addLine(to: CGPoint(x:e1x+dx, y:e1y+dy))
                ticks.addArc(tangent1End:CGPoint(x:etx+dx, y:ety+dy), tangent2End: CGPoint(x:e2x+dx, y:e2y+dy), radius: cornerRadius)
                ticks.addLine(to: CGPoint(x:e2x+dx, y:e2y+dy))
                ticks.addLine(to: CGPoint(x:e3x+dx, y:e3y+dy))

//                let squareSize = CGSize(width: 3, height: 3)
//                [CGPoint(x:e0x+dx, y:e0y+dy),
//                 CGPoint(x:e1x+dx, y:e1y+dy),
//                 CGPoint(x:e2x+dx, y:e2y+dy),
//                 CGPoint(x:e3x+dx, y:e3y+dy)].forEach { point in
//                    let squareOrigin = CGPoint(x: point.x - squareSize.width / 2, y: point.y - squareSize.height / 2)
//                    let squareRect = CGRect(origin: squareOrigin, size: squareSize)
//                    path.addRect(squareRect)
//                }
            }
            

        }


        return (path,ticks)
    }

    open func cellColor() -> Color {
        return Color.white
    }
    

//    public func view(with geometry: GeometryProxy, d: CGFloat, h: CGFloat, alpha: CGFloat, cornerRadius: CGFloat) -> AnyView {
//        return AnyView(EmptyView())
//    }
//    
    open func view(with geometry: GeometryProxy, d: CGFloat, h: CGFloat, alpha: CGFloat, cornerRadius: CGFloat) -> AnyView {
        let (path, ticks) = cellPaths(with: geometry, d: d, h: h, alpha: alpha, cornerRadius:cornerRadius)
        return AnyView(
            path
                .fill(cellColor())
                .overlay(path.stroke(Color.black, lineWidth: 2))
                .overlay(ticks.stroke(Color.black, lineWidth: 1))
                .scaleEffect(x: 1, y: -1)
            
        )
    }

    open func stoneMovedOverCell(stone: Stone){
        // Nothing
    }
    
    open func stoneLeavesCell(stone: Stone){
        // Nothing
    }
    
    open func gameStatus(stone: Stone) -> GameStatus {
        return GameStatus.playing
    }
    
}

open class SolidCell : Cell {
    // The stone may remain securely on a solid cell.

    public override func cellColor() -> Color {
        return Color.green
    }

}

open class TargetCell : Cell {
    // Once the stone is in vertical position on a target cell, the game is won.

    public override func cellColor() -> Color {
        return Color.white
    }

    public override func stoneMovedOverCell(stone: Stone){
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

    public override func view(with geometry: GeometryProxy, d: CGFloat, h: CGFloat, alpha: CGFloat, cornerRadius: CGFloat) -> AnyView {
        return AnyView(EmptyView())
    }

    public override func stoneMovedOverCell(stone: Stone){
        signalGameLost()
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
    
    public var state = State.closed // A pathway cell may be :open or :closed.
    
    public init(x: Int, y: Int, state: State = State.closed) {
        super.init(x:x, y:y)
        self.state = state
    }
    
    public override func cellColor() -> Color {
        return Color.yellow
    }

    public override func stoneMovedOverCell(stone: Stone) {
        if (state == State.closed){
            signalGameLost()
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

    public override func cellColor() -> Color {
        return Color.red
    }

    public override func stoneMovedOverCell(stone: Stone){
        switchPathwayCells()
    }
    
}


open class BlueButtonCell : ButtonCell {
    // A blue button cell switches its pathway cells
    // only when the stone is over it in vertical position.

    public override func cellColor() -> Color {
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
    var state = State.open // A crumble cell goes from :open to :closed the first time it's walked over, and stays :closed thereafter.

    public override func cellColor() -> Color {
        return Color.orange
    }

    public override func stoneMovedOverCell(stone: Stone) {
        if (state == State.closed){
            signalGameLost()
        }
    }

    public override func gameStatus(stone: Stone) -> GameStatus {
        if (state == State.closed){
            return GameStatus.lose
        }
        return GameStatus.playing
    }

    public override func stoneLeavesCell(stone: Stone) {
        self.state = State.closed
    }

}


open class IceCell : Cell {
    // An ice cell supports an horizontal stone, but
    // when the stone is over it in vertical position, it breaks, the stone falls down, and the game is lost.

    public override func cellColor() -> Color {
        return Color.cyan
    }

    public override func stoneMovedOverCell(stone: Stone) {
        if (verticalp(direction: stone.orientation)){
            signalGameLost()
        }
    }

    public override func gameStatus(stone: Stone) -> GameStatus {
        if (verticalp(direction: stone.orientation)){
            return GameStatus.lose
        }
        return GameStatus.playing
    }
}

