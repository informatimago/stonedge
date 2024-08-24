//
//  BoardView.swift
//  stonedge
//
//  Created by Pascal Bourguignon on 13/07/2024.
//  Copyright © 2024 Ogamita Ltd. All rights reserved.
//

import SwiftUI


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
    cells[1][0] = TargetCell(x:0, y:1)
    cells[2][0] = PathwayCell(x:0, y:2, state:.open)
    cells[3][0] = PathwayCell(x:0, y:3, state:.closed)
    cells[2][1] = RedButtonCell(x:1, y:2, commandedCell:cells[0][2] as? PathwayCell)
    cells[3][1] = BlueButtonCell(x:1, y:3, commandedCell:cells[0][3] as? PathwayCell)
    cells[2][2] = SolidCell(x:2, y:2)
    cells[3][2] = SolidCell(x:2, y:3)
    cells[2][3] = CrumbleCell(x:3, y:2)
    cells[3][3] = IceCell(x:3, y:3)
    return cells
}


struct BoardView: View {
    @Binding var cells: [[Cell]]


    static func computeBoundsFor(cell:Cell, in geometry: GeometryProxy) -> CGRect
    {
        // compute a bounding rectangle for the Cell, given the display parametrs (d, h cosAlpha, sinAlpha and its coordinates cell.x, cell.y)
        let x = cell.x
        let y = cell.y
        let cosAlpha = cos(alpha)
        let sinAlpha = sin(alpha)

        let O = CGPoint(x: 0, y: 0)
        let F = CGPoint(x: d * cosAlpha, y: d * sinAlpha)
        let C = CGPoint(x: 0, y: h + 2 * d * sinAlpha)
        let E = CGPoint(x: -d * cosAlpha, y: d * sinAlpha)

        // Compute cell origin
        let Oxy = CGPoint(x: (CGFloat(x - y) * d * cosAlpha) + geometry.size.width / 2,
                          y: (CGFloat(x + y) * d * sinAlpha) + geometry.size.height / 2)

        let bottom = Oxy.y
        let left = Oxy.x + E.x
        let top = Oxy.y + C.y
        let right = Oxy.x + F.x

        return CGRect(x: left, y: top, width: right - left, height: bottom - top)
    }

    static func computeBoundsFor(cells:[[Cell]], in geometry: GeometryProxy) -> CGRect
    {
        // for each cell in cells, if the cell is not EmptyCell,
        // then computeBoundsFor(cell:cell, in:geometry)
        // and take the union of the result rectangles.
        var bounds = CGRect(x:0, y:0, width:0, height:0)
        for row in cells {
            for cell in row {
                if !(cell is EmptyCell) {
                    bounds = bounds.union(computeBoundsFor(cell: cell, in:geometry))
                }
            }
        }
        return bounds
    }

    static func computeScaleFor(cells:[[Cell]], in geometry: GeometryProxy) -> (CGFloat, CGFloat, CGFloat)
    {

        // let rows = cells.count
        // let cols = cells[0].count
        // let cosAlpha = cos(alpha)
        // let sinAlpha = sin(alpha)
        // let width  = d * cosAlpha * (CGFloat(cols) + CGFloat(rows))
        // let height = d * sinAlpha * (CGFloat(cols) + CGFloat(rows))
        //
        // let hscale = (geometry.size.width  - 2*inset) / width
        // let vscale = (geometry.size.height - 2*inset) / height
        //
        // print("computeScaleFor(cols=\(cols), rows = \(rows)) -> size = \(width)x\(height) in \(geometry.size.width  - 2*inset)x\(geometry.size.height - 2*inset) -> scale = \(hscale)x\(vscale)")
        // return min(hscale,vscale)

        let bounds = computeBoundsFor(cells:cells, in:geometry)
        let hscale = (geometry.size.width  - 2*inset) / bounds.size.width
        let vscale = (geometry.size.height - 2*inset) / bounds.size.height
        let center_x = bounds.origin.x + bounds.size.width / 2
        let center_y = bounds.origin.y + bounds.size.height / 2
        return (min(hscale,vscale),
                center_x, center_y)
    }

    // Function to sort cells in reverse order of x+y
    func sortedCells() -> [Cell]
    {
        let flattenedCells = cells.flatMap { $0 }
        return flattenedCells.sorted { ($0.x + $0.y) > ($1.x + $1.y) }
    }

    func drawSquare(at point: CGPoint) -> some View
    {
        Rectangle()
            .frame(width: 3, height: 3)
            .offset(x: point.x - 1.5, y: point.y - 1.5) // Offset to center the square on the point
            .foregroundColor(Color.black)
    }


    // PathWayCells are open or closed. Closed pathway cells should show they're closed by drawing a sign
    // ButtonCells should draw a button (cylinder) above the cell.

    // Note: red can be activated vertically or horizontally
    //       blue can only be activated vertically
    //       both are always switching on->off or off->on
    //       We could have one-shot button cells (yellow?) that would switch only once.

    public func cellPaths(cell: Cell, with geometry: GeometryProxy, d: CGFloat, h: CGFloat, alpha: CGFloat, cornerRadius: CGFloat) -> (Path, Path)
    {
        let x = cell.x
        let y = cell.y
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
        // print( "cell  Oxy(\(x),\(y)) = ", Oxy)

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


    public func computeViewForCell(cell: Cell, with geometry: GeometryProxy, d: CGFloat, h: CGFloat, alpha: CGFloat, cornerRadius: CGFloat) -> AnyView
    {

        if cell is EmptyCell
        {
            return AnyView(EmptyView())
        }

        if let color = cell.cellColor() {
            let (path, ticks) = cellPaths(cell: cell, with: geometry, d: d, h: h, alpha: alpha, cornerRadius:cornerRadius)
            let x = cell.x
            let y = cell.y
            let cosAlpha = cos(alpha)
            let sinAlpha = sin(alpha)
            let Oxy = CGPoint(x: (CGFloat(x - y) * d * cosAlpha) + geometry.size.width / 2,
                              y: (CGFloat(x + y) * d * sinAlpha) + geometry.size.height / 2)
            return AnyView(ZStack {
                               AnyView(
                                 path
                                   .fill(color)
                                   .overlay(path.stroke(Color.black, lineWidth: 2))
                                   .overlay(ticks.stroke(Color.black, lineWidth: 1))
                               )
                               Text("\(cell.x),\(cell.y)")
                                 .rotationEffect(.degrees(180), anchor: .center)
                                 .position(x: Oxy.x, y: Oxy.y)
                                 .offset(y:0.75*d)
                           })
        }else{
            return AnyView(EmptyView())
        }
    }

    var body: some View {
        GeometryReader { geometry in
            ZStack{
                ForEach(sortedCells()) { cell in
                    computeViewForCell(cell: cell, with: geometry, d: d, h: h, alpha: alpha, cornerRadius: cellCornerRadius)
                }

                // ForEach(sortedCells()) { cell in
                //     let cosAlpha = cos(alpha)
                //     let sinAlpha = sin(alpha)
                //     let Oxy = CGPoint(x: (CGFloat(cell.x - cell.y) * d * cosAlpha) + geometry.size.width / 2,
                //                       y: (CGFloat(cell.x + cell.y) * d * sinAlpha) + geometry.size.height / 2)
                //
                // }
                // .scaleEffect(x: 1, y: -1)

            }
        }

    }

}


#Preview {
    @State var cells = testBoard()
    @State var step = 0
    return GeometryReader { geometry in
        let frameSize = geometry.size
        let (scaleFactor, centerX, centerY) = BoardView.computeScaleFor(cells:cells, in:geometry)
        BoardView(cells: $cells)
                .scaleEffect(scaleFactor)
                .offset(x: (frameSize.width / 2 - centerX) * scaleFactor,
                        y: (frameSize.height / 2 - centerY) * scaleFactor)
    }
}
