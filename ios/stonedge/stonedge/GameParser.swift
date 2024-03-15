//
//  GameParser.swift
//  stonedge
//
//  Created by Pascal Bourguignon on 05/03/2024.
//

import Foundation


// Assuming Cell and its subclasses are defined as before, with constructors accepting x and y parameters

enum LinkType {
    case red, blue, pathway
}
struct Definition {
    var name : String
    var link : LinkType
    var connected : [String] // for red or blue
    var state : PathwayCell.State // for pathway
}


func parseGame(specification: String) -> Game? {
    let lines = specification.uppercased().split(separator: "\n").map(String.init)
    
    // Find the index of the first definition line
    let firstDefIndex = lines.firstIndex { $0.contains("PATHWAY") || $0.contains("RED") || $0.contains("BLUE") } ?? lines.endIndex
    
    let gridLines = Array(lines[..<firstDefIndex])
    let cellDefLines = Array(lines[firstDefIndex...])
    
    let height = gridLines.count
    let width = gridLines.map { $0.count }.max() ?? 0
    
    var grid : [[Cell]] = []
    var stone : Stone? = nil
    
    var definitions: [String: Definition] = [:]
    var namedCells: [String: Cell] = [:]
    
    // Process cell definitions
    for line in cellDefLines {
        let components = line.split(separator: " ").map(String.init)
        guard let cellName = components.first?.first else {continue}
        let connectedCellNamesString = components.dropFirst(2).joined()
        let connectedCellNames = Array(connectedCellNamesString).map { String($0) }
        let name : String = String(cellName)
        var link : LinkType = .pathway
        var connected : [String] = [] // for red or blue
        var state : PathwayCell.State = .closed // for pathway
        switch components[1] {
        case "PATHWAY":
            link = .pathway
            state = components[2] == "OPEN" ? .open : .closed
        case "RED":
            link = .red
            connected = connectedCellNames
        case "BLUE":
            link = .blue
            connected = connectedCellNames
        default:
            print("Error: Unknown directive \(components[1])")
            break
        }
        
        let definition : Definition = Definition(name:name, link:link, connected:connected, state:state)
        print("Definition = \(definition)")
        
        definitions[definition.name]=definition
    }
    
    // Fill the grid and track coordinates
    for (y, line) in gridLines.enumerated() {
        grid.append([])
        for (x, char) in line.enumerated() {
            let cell = createCell(from: String(char), x: x, y: y, definitions:definitions)
            namedCells[String(char)]=cell
            grid[y].append(cell)
            if String(char) == "O" {
                stone = Stone(x:x, y:y, orientation: vertical)
            }
        }
        for x in grid[y].count..<width {
            grid[y][x] = EmptyCell(x:x, y:y)
        }
    }
    
    for (y, line) in gridLines.enumerated() {
        for (x, char) in line.enumerated() {
            
            if (x < 2) || (y < 2) || (width - 2 < x) || (height - 2 < y) {
                if grid[y][x] is EmptyCell {
                    // ok
                } else {
                    print("Only EmptyCell can be near the border \(x) \(y) \(String(describing: type(of: grid[y][x])))")
                    return nil
                }
            }
            
            let cellName = String(char)
            if let cell = namedCells[cellName] {
                if (cell.x == x) && (cell.y == y) {
                    if let definition = definitions[cellName] {
                        switch definition.link {
                        case .red, .blue:
                            let button = cell as! RedButtonCell
                            button.switches = definition.connected.map{ name in
                                namedCells[name] as! PathwayCell
                            }
                        case .pathway:
                            break
                        }
                    }
                }
            }
        }
    }
    
    if let stone = stone {
        return Game(stone: stone, cells: grid)
    }
    return nil
}

func createCell(from cellName: String, x: Int, y: Int, definitions: [String: Definition]) -> Cell {
    if let definition = definitions[cellName] {
        switch definition.link {
        case .red:
            return RedButtonCell(x:x, y:y
            )
        case .blue:
            return BlueButtonCell(x:x, y:y)
        case .pathway:
            return PathwayCell(x:x, y:y)
        }
    }
    switch cellName {
    case " ", ".":
        return EmptyCell(x: x, y: y)
    case "S", "O":
        return SolidCell(x: x, y: y)
    case "T":
        return TargetCell(x: x, y: y)
    case "I":
        return IceCell(x: x, y: y)
    case "C":
        return CrumbleCell(x: x, y: y)
    default:
        return SolidCell(x: x, y: y)
    }
}

