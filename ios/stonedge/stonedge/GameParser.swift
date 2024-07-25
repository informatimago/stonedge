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

let emptyName = "."
let emptyName2 = " "
let startName = "S"
let solidName = "O"
let targetName = "T"
let iceName = "I"
let crumbleName = "C"


func parseGame(specification: String) -> Game? {
    var lines = specification.split(separator: "\n").map(String.init)

    let titleIndex = lines.firstIndex(where: { line in !line.isEmpty && !line.allSatisfy({ $0 == "." })})
    let firstGridIndex = lines.firstIndex(where: { line in
        // Check if the line is not empty and all characters in the line are dots.
        return !line.isEmpty && line.allSatisfy({ $0 == "." })
    })

    var title = ""
    var description : [String] = []

    if titleIndex != nil && (firstGridIndex == nil || (titleIndex! < firstGridIndex!)) {
        title = lines[titleIndex!]
        description = Array(lines[(titleIndex! + 1)..<(firstGridIndex ?? lines.count)])
    }

    lines = lines.map{ $0.uppercased() }
    // Find the index of the first definition line
    let firstDefIndex = lines.firstIndex { $0.contains("PATHWAY") || $0.contains("RED") || $0.contains("BLUE") } ?? lines.endIndex
    let gridLines = Array(lines[(firstGridIndex ?? firstDefIndex)..<firstDefIndex])
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
        print("\(definition)")

        definitions[definition.name]=definition
    }

    // Fill the grid and track coordinates
    for (y, line) in gridLines.enumerated() {
        grid.append([])
        for (x, char) in line.enumerated() {
            let cell = createCell(from: String(char), x: x, y: y, definitions:definitions)
            print("[\(y)][\(x)] \(String(char)) = \(cell)")
            namedCells[String(char)]=cell
            grid[y].append(cell)
            if String(char) == startName {
                stone = Stone(x:x, y:y, orientation: vertical)
            }
        }
        for x in grid[y].count..<width {
            grid[y].append(EmptyCell(x:x, y:y))
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
                        case .red:
                            let button = cell as! RedButtonCell
                            button.switches = definition.connected.map{ name in
                                namedCells[name] as! PathwayCell
                            }
                        case .blue:
                            let button = cell as! BlueButtonCell
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
        let game = Game(stone: stone, cells: grid)
        game.title = title
        game.description = description
        return game
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
    case emptyName, emptyName2:
        return EmptyCell(x: x, y: y)
    case startName, solidName:
        return SolidCell(x: x, y: y)
    case targetName:
        return TargetCell(x: x, y: y)
    case iceName:
        return IceCell(x: x, y: y)
    case crumbleName:
        return CrumbleCell(x: x, y: y)
    default:
        return SolidCell(x: x, y: y)
    }
}
