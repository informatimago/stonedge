//
//  StonedgeGameView.swift
//  stonedge
//
//  Created by Pascal Bourguignon on 05/03/2024.
//

import SwiftUI
import Foundation



func testBoard() -> [[Cell]] {
    let rows = 4
    let cols = 4
    var cells = Array(repeating: Array<Cell>(repeating: EmptyCell(x:0, y:0), count: cols), count: rows)
    for row in 0..<rows {
        for col in 0..<cols {
            cells[row][col] = EmptyCell(x: col, y: row)
        }
    }
    cells[0][0] = SolidCell(x:0, y:0)
    cells[0][1] = TargetCell(x:0, y:1)
    cells[0][2] = PathwayCell(x:0, y:2, state:.open)
    cells[0][3] = PathwayCell(x:0, y:3, state:.closed)
    cells[1][2] = RedButtonCell(x:1, y:2, commandedCell:cells[0][2] as? PathwayCell)
    cells[1][3] = BlueButtonCell(x:1, y:3, commandedCell:cells[0][3] as? PathwayCell)
    cells[2][2] = SolidCell(x:2, y:2)
    cells[2][3] = SolidCell(x:2, y:3)
    cells[3][2] = CrumbleCell(x:3, y:2)
    cells[3][3] = IceCell(x:3, y:3)
    return cells
}


struct BoardView: View {
    var cells: [[Cell]] = testBoard()
    let d: CGFloat = 50*2 // Side of the cell square
    let h: CGFloat = 10*2 // Height of the cell slab
    let alpha: CGFloat = CGFloat.pi / 6 // Projection angle
    let cornerRadius: CGFloat = 6*2
    
    // Function to sort cells in reverse order of x+y
    func sortedCells() -> [Cell] {
        let flattenedCells = cells.flatMap { $0 }
        return flattenedCells.sorted { ($0.x + $0.y) > ($1.x + $1.y) }
    }
    
    var body: some View {
        GeometryReader { geometry in
            ForEach(sortedCells()) { cell in
                cell.view(with: geometry, d: d, h: h, alpha: alpha, cornerRadius: cornerRadius)
            }.offset(y: geometry.size.height*0.4)
//
//            let cells : [Cell] = sortedCells()
//            ForEach(0..<cells.count, id: \.self) { index in
//                let cell = cells[index]
//                cell.view(with: geometry, d: d, h: h, alpha: alpha, cornerRadius:cornerRadius)
//            }
        }

    }

}



struct StonedgeGameView: View {
    
//    let game : Game;
    
//    init(game: Game){
//        self.game = game
//    }
//    
    var body: some View {
        GeometryReader { geometry in
            
            ZStack {
                // Background Gradient
                LinearGradient(gradient: Gradient(colors: [.blue, .white]), startPoint: .top, endPoint: .bottom)
                    .edgesIgnoringSafeArea(.all)
                
                BoardView()
                // Text("• stonedge •")
            }
            .scaleEffect(x: 1, y: -1) // Flip the y-axis
            .scaledToFit()
            .rotationEffect(.degrees(180), anchor: .center) // Rotate 45 degrees around the center
            .frame(width: geometry.size.width, height: geometry.size.height) // Set frame to GeometryReader's size

      }

    }
    
}


#Preview {
    StonedgeGameView()
}
