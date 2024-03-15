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
    """]

    
    override func setUpWithError() throws {
        // Put setup code here. This method is called before the invocation of each test method in the class.
    }

    override func tearDownWithError() throws {
        // Put teardown code here. This method is called after the invocation of each test method in the class.
    }

    func testSimpleCells() throws {
        let gameOrNil = parseGame(specification: levels[0])
        XCTAssertNotNil(gameOrNil,"parseGame(\(levels[0]))")
        let game = gameOrNil!
        XCTAssert(game.cells[2][2] is SolidCell)
        XCTAssert(game.cells[2][3] is SolidCell)
        XCTAssert(game.cells[2][4] is TargetCell)
        XCTAssert(game.cells[2][5] is IceCell)
        XCTAssert(game.cells[2][6] is CrumbleCell)


        // XCTAssertEqual(rotatedDirection, expectedRotation, "\(rotations[rotation]) \(direction)")

    }

//    
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



