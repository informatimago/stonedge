//
//  StoneTests.swift
//  stonedgeTests
//
//  Created by Pascal Bourguignon on 30/07/2024.
//

import Foundation
import XCTest


@testable import stonedge

final class StoneTests: XCTestCase {


    override func setUpWithError() throws {
        // Put setup code here. This method is called before the invocation of each test method in the class.
    }

    override func tearDownWithError() throws {
        // Put teardown code here. This method is called after the invocation of each test method in the class.
    }

    
    func testNormalize() throws {
        do {
            let stone = Stone(x: 0,y: 0,orientation: vertical)
            XCTAssert(stone.x == 0)
            XCTAssert(stone.y == 0)
            XCTAssert(stone.orientation == vertical)
            XCTAssert(stone.width() == 1)
            XCTAssert(stone.depth() == 1)
            XCTAssert(stone.height() == 2)
            stone.normalize()
            print("x=\(stone.x) y=\(stone.y) orientation=\(stone.orientation) w=\(stone.width()) d=\(stone.depth()) h=\(stone.height())")
            XCTAssert(stone.x == 0)
            XCTAssert(stone.y == 0)
            XCTAssert(stone.orientation == vertical)
            XCTAssert(stone.width() == 1)
            XCTAssert(stone.depth() == 1)
            XCTAssert(stone.height() == 2)
            
        }

        do {
            let stone = Stone(x: 0,y: 0,orientation: [0,0,-1])
            XCTAssert(stone.x == 0)
            XCTAssert(stone.y == 0)
            XCTAssert(stone.orientation == [0,0,-1])
            XCTAssert(stone.width() == 1)
            XCTAssert(stone.depth() == 1)
            XCTAssert(stone.height() == 2)
            stone.normalize()
            print("x=\(stone.x) y=\(stone.y) orientation=\(stone.orientation) w=\(stone.width()) d=\(stone.depth()) h=\(stone.height())")
            XCTAssert(stone.x == 0)
            XCTAssert(stone.y == 0)
            XCTAssert(stone.orientation == vertical)
            XCTAssert(stone.width() == 1)
            XCTAssert(stone.depth() == 1)
            XCTAssert(stone.height() == 2)
        }

        do {
            let stone = Stone(x: 0,y: 0,orientation: lateral)
            XCTAssert(stone.x == 0)
            XCTAssert(stone.y == 0)
            XCTAssert(stone.orientation == lateral)
            XCTAssert(stone.width() == 2)
            XCTAssert(stone.depth() == 1)
            XCTAssert(stone.height() == 1)
            stone.normalize()
            print("x=\(stone.x) y=\(stone.y) orientation=\(stone.orientation) w=\(stone.width()) d=\(stone.depth()) h=\(stone.height())")
            XCTAssert(stone.x == 0)
            XCTAssert(stone.y == 0)
            XCTAssert(stone.orientation == lateral)
            XCTAssert(stone.width() == 2)
            XCTAssert(stone.depth() == 1)
            XCTAssert(stone.height() == 1)
        }

        do {
            let stone = Stone(x: 0,y: 0,orientation: [-1,0,0])
            XCTAssert(stone.x == 0)
            XCTAssert(stone.y == 0)
            XCTAssert(stone.orientation == [-1,0,0])
            XCTAssert(stone.width() == 2)
            XCTAssert(stone.depth() == 1)
            XCTAssert(stone.height() == 1)
            stone.normalize()
            print("x=\(stone.x) y=\(stone.y) orientation=\(stone.orientation) w=\(stone.width()) d=\(stone.depth()) h=\(stone.height())")
            XCTAssert(stone.x == -1)
            XCTAssert(stone.y == 0)
            XCTAssert(stone.orientation == lateral)
            XCTAssert(stone.width() == 2)
            XCTAssert(stone.depth() == 1)
            XCTAssert(stone.height() == 1)
        }

        do {
            let stone = Stone(x: 0,y: 0,orientation: front)
            XCTAssert(stone.x == 0)
            XCTAssert(stone.y == 0)
            XCTAssert(stone.orientation == front)
            XCTAssert(stone.width() == 1)
            XCTAssert(stone.depth() == 2)
            XCTAssert(stone.height() == 1)
            stone.normalize()
            print("x=\(stone.x) y=\(stone.y) orientation=\(stone.orientation) w=\(stone.width()) d=\(stone.depth()) h=\(stone.height())")
            XCTAssert(stone.x == 0)
            XCTAssert(stone.y == 0)
            XCTAssert(stone.orientation == front)
            XCTAssert(stone.width() == 1)
            XCTAssert(stone.depth() == 2)
            XCTAssert(stone.height() == 1)
        }

        do {
            let stone = Stone(x: 0,y: 0,orientation: [0,-1,0])
            XCTAssert(stone.x == 0)
            XCTAssert(stone.y == 0)
            XCTAssert(stone.orientation == [0,-1,0])
            XCTAssert(stone.width() == 1)
            XCTAssert(stone.depth() == 2)
            XCTAssert(stone.height() == 1)
            stone.normalize()
            print("x=\(stone.x) y=\(stone.y) orientation=\(stone.orientation) w=\(stone.width()) d=\(stone.depth()) h=\(stone.height())")
            XCTAssert(stone.x == 0)
            XCTAssert(stone.y == -1)
            XCTAssert(stone.orientation == front)
            XCTAssert(stone.width() == 1)
            XCTAssert(stone.depth() == 2)
            XCTAssert(stone.height() == 1)
        }

        // XCTAssertNotNil(gameOrNil,"parseGame(\(levels[3]))")
    }

    
}
