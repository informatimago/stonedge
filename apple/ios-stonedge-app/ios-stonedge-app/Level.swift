//
//  Level.swift
//  stonedge
//
//  Created by Pascal Bourguignon on 24/10/2025.
//

import Foundation

enum LinkType {
    case red, blue, pathway
}
struct Definition {
    var name : String
    var link : LinkType
    var connected : [String] // for red or blue
    var state : PathwayCell.State // for pathway
}

open class Level : ObservableObject {
    
    // A Level is an abstract representation of a Stonedge Game Level, sans stone.
    // A Level must have one StartCell.
    
    @Published var title: String = "" // Level title - single line
    @Published var description: [String] = [] // Level description - multiple lines
    @Published var cells: [[Cell]] = [[]]
    @Published var definitions: [String: Definition] = [:]
    @Published var namedCells: [String: Cell] = [:]

    public init(title:String, description:[String], cells:[[Cell]]
                // , connections?
    )
    {
        self.title = title
        self.description = description
        self.cells = cells
        // connecitons?
    }
 
    
    public func startCell() -> StartCell?
    {
        for row in self.cells {
            for cell in row {
                if let startCell : StartCell = cell as? StartCell {
                    return startCell
                }
            }
        }
        return nil
    }
    
}
