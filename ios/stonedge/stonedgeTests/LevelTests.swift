//
//  LevelTests.swift
//  stonedgeTests
//
//  Created by Pascal Bourguignon on 15/03/2024.
//

import Foundation
import XCTest


@testable import stonedge

final class LevelTests: XCTestCase {
    
    let levels = [
    """
    .........
    .........
    ..SOTIC..
    .........
    .........
    """,

    """
    ..........
    ..........
    ..A.B..S..
    ..123456..
    ..........
    ..........
    
    A red 1
    B blue 2 3 456
    1 pathway closed
    2 pathway closed
    3 pathway closed
    4 pathway open
    5 pathway open
    6 pathway open
    """,

    """
    ...............
    ...............
    ......AO.......
    ....OOOOOO.....
    ..SOOOOOOO1OT..
    ...IIOOOOO.....
    ......IO.......
    ...............
    ...............

    A red 1
    1 pathway closed
    """,
    """
Level With Title
And with description
lines. Let write several
description lines, and
let's be happy.

.......
........
..SOOT..
..
..
"""]
    
    
    override func setUpWithError() throws {
        // Put setup code here. This method is called before the invocation of each test method in the class.
    }
    
    override func tearDownWithError() throws {
        // Put teardown code here. This method is called after the invocation of each test method in the class.
    }
    
    func testTitle() throws {
        let gameOrNil = parseGame(specification: levels[3])
        XCTAssertNotNil(gameOrNil,"parseGame(\(levels[3]))")
        let game = gameOrNil!
        print("title=\(game.title)")
        print("description=\(game.description)")
        XCTAssert(game.title == "Level With Title")
        XCTAssert(game.description == ["And with description",
                                       "lines. Let write several",
                                       "description lines, and",
                                       "let's be happy."])
    }
    
    func testSimpleCells() throws {
        let gameOrNil = parseGame(specification: levels[0])
        XCTAssertNotNil(gameOrNil,"parseGame(\(levels[0]))")
        let game = gameOrNil!
        XCTAssert(game.stone.x == 2)
        XCTAssert(game.stone.y == 2)
        XCTAssert(game.cells[2][2] is SolidCell)
        XCTAssert(game.cells[2][3] is SolidCell)
        XCTAssert(game.cells[2][4] is TargetCell)
        XCTAssert(game.cells[2][5] is IceCell)
        XCTAssert(game.cells[2][6] is CrumbleCell)
    }
    
    
    func testLinkedCells() throws {
        let gameOrNil = parseGame(specification: levels[1])
        XCTAssertNotNil(gameOrNil,"parseGame(\(levels[1]))")
        let game = gameOrNil!
        XCTAssert(game.cells[2][2] is RedButtonCell)
        XCTAssert(game.cells[2][4] is BlueButtonCell)
        XCTAssert(game.cells[3][2] is PathwayCell)
        XCTAssert(game.cells[3][3] is PathwayCell)
        XCTAssert(game.cells[3][4] is PathwayCell)
        XCTAssert(game.cells[3][5] is PathwayCell)
        XCTAssert(game.cells[3][6] is PathwayCell)
        XCTAssert(game.cells[3][7] is PathwayCell)
        let red = game.cells[2][2] as? RedButtonCell
        let blue = game.cells[2][4] as? BlueButtonCell
        if red != nil {
            if let pathway = red!.switches[0] as PathwayCell? {
                XCTAssert(pathway === game.cells[3][2])
            }
        }
        if blue != nil {
            for i in 0..<5 {
                if let pathway = blue!.switches[i] as PathwayCell? {
                    XCTAssert(pathway === game.cells[3][3+i])
                }
            }
        }
        
    }
    
    //    func testExample() throws {
    //        // This is an example of a functional test case.
    //        // Use XCTAssert and related functions to verify your tests produce the correct results.
    //        // Any test you write for XCTest can be annotated as throws and async.
    //        // Mark your test throws to produce an unexpected failure when your test encounters an uncaught error.
    //        // Mark your test async to allow awaiting for asynchronous code to complete. Check the results with assertions afterwards.
    //    }
    //
    //    func testPerformanceExample() throws {
    //        // This is an example of a performance test case.
    //        self.measure {
    //            // Put the code you want to measure the time of here.
    //        }
    //    }
    //
    //    func testRotate() throws {
    //        // test `rotate` and `rotations` defined in Stone.
    //
    //        let tests: [([Int], [Int])] = [
    //          // [direction, rotatedDirection]
    //          ([1,  0,  0],  [0,  0,  -1]),
    //          ([1,  0,  0],  [0,  0,  1]),
    //          ([1,  0,  0],  [1,  0,  0]),
    //          ([1,  0,  0],  [1,  0,  0]),
    //          ([-1, 0,  0],  [0,  0,  1]),
    //          ([-1, 0,  0],  [0,  0,  -1]),
    //          ([-1, 0,  0],  [-1, 0,  0]),
    //          ([-1, 0,  0],  [-1, 0,  0]),
    //          ([0,  1,  0],  [0,  1,  0]),
    //          ([0,  1,  0],  [0,  1,  0]),
    //          ([0,  1,  0],  [0,  0,  -1]),
    //          ([0,  1,  0],  [0,  0,  1]),
    //          ([0,  -1, 0],  [0,  -1, 0]),
    //          ([0,  -1, 0],  [0,  -1, 0]),
    //          ([0,  -1, 0],  [0,  0,  1]),
    //          ([0,  -1, 0],  [0,  0,  -1]),
    //          ([0,  0,  1],  [1,  0,  0]),
    //          ([0,  0,  1],  [-1, 0,  0]),
    //          ([0,  0,  1],  [0,  1,  0]),
    //          ([0,  0,  1],  [0,  -1, 0]),
    //          ([0,  0,  -1], [-1, 0,  0]),
    //          ([0,  0,  -1], [1,  0,  0]),
    //          ([0,  0,  -1], [0,  -1, 0]),
    //          ([0,  0,  -1], [0,  1,  0])
    //        ]
    //
    //        var rotation = -1
    //        for test in tests {
    //            let (direction, expectedRotation) = test
    //            rotation = (rotation+1) % 4
    //            let rotatedDirection = rotate(matrix: rotations[rotation], vector: direction)
    //            XCTAssertEqual(rotatedDirection, expectedRotation, "\(rotations[rotation]) \(direction)")
    //        }
    //    }
    
}



