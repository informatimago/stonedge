//
//  Stone.swift
//  stonedge
//
//  Created by Pascal Bourguignon on 21/02/2024.
//

import Foundation

//
//              ^
//             y|front
//              |
//              |
// left         |                 right
// -------------+-------------------->
//             0|                   x
//              |
//              |
//              |back
//

enum Direction : Int {
    case right = 0, left, front, back
}

enum Orientation {
    case vertical, lateral, front
}

func verticalp(direction: [Int]) -> Bool {
    (direction[2] != 0)
}

func lateralp(direction: [Int]) -> Bool {
    (direction[0] != 0)
}

func frontp(direction: [Int]) -> Bool {
    (direction[1] != 0)
    
}


//                   x               x                  x                  x
//                   y               y                  y                  y
//          right    z      left     z         front    z         back     z
//        -------   ---   --------  ---      --------  ---      -------   ---
//          0 0 1    z      0 0 -1  -z        1  0  0   x        1  0  0   x
//          0 1 0    y      0 1 0    y        0  0  1   z        0  0 -1  -z
//         -1 0 0   -x      1 0 0    x        0 -1  0  -y        0  1  0   y

let rotations: [[[Int]]] = [
    // right
    [[0, 0, 1],
     [0, 1, 0],
     [-1, 0, 0]],
    
    // left
    [[0, 0, -1],
     [0, 1, 0],
     [1, 0, 0]],
    
    // front
    [[1, 0, 0],
     [0, 0, 1],
     [0, -1, 0]],
    
    // back
    [[1, 0, 0],
     [0, 0, -1],
     [0, 1, 0]]
]

func rotate(matrix: [[Int]], vector: [Int]) -> [Int] {
    // Ensure the matrix and vector have compatible dimensions
    guard matrix.count == 3 && matrix.allSatisfy({ $0.count == 3 }) && vector.count == 3 else {
        fatalError("Matrix or Vector dimensions are incorrect")
    }

    // Perform the matrix-vector multiplication
    return (0..<3).map { i in
        (0..<3).reduce(0) { sum, j in
            sum + matrix[i][j] * vector[j]
        }
    }
}

func negate(vector: [Int]) -> [Int] {
    // Returns   - vector
    [-vector[0],-vector[1],-vector[2]]
}


class Stone {
    // A stone is made of two cubes of size equal to the cells.
    // To move, it rotates 1/4 turn on one of its edges that is in contact with the cells.
    
    var x: Int = 0 // Ordinate of the first cube of the stone.
    var y: Int = 0 // Coordinate of the first cube of the stone.
    var direction: [Int] = [0, 0, 1]
    // A unit vector indicating the direction of the stone.
    // The coordinate of the other cube of the stone is given by adding this vector to
    // the coordinates of the first cube.
    // Note: The stone is normalized so that the vertical coordinate of the direction is either 0 or 1.
    
    func normalize(for direction: Direction) {
        switch direction {
        case .left:
            if (self.direction[0]>0) {
                self.x += 1
                self.direction = negate(vector: self.direction)
            }
        case .right:
            if (self.direction[0]<0) {
                self.x -= 1
                self.direction = negate(vector: self.direction)
            }
        case .front:
            if (self.direction[1]>0) {
                self.y += 1
                self.direction = negate(vector: self.direction)
            }
        case .back:
            if (self.direction[1]<0) {
                self.y -= 1
                self.direction = negate(vector: self.direction)
            }
        }
    }
    
    func move(in direction: Direction) {
        normalize(for: direction)
        switch direction {
        case .left:
            self.x += 1
        case .right:
            self.x -= 1
        case .front:
            self.y += 1
        case .back:
            self.y -= 1
        }
        self.direction = rotate(matrix: rotations[direction.rawValue], vector: self.direction)
    }
    
    func coverage() -> (Orientation, Int, Int, Int, Int) {
        // Returns:   direction; left; back; right; front
        var orientation = Orientation.front
        if (verticalp(direction: self.direction)) {
            orientation = Orientation.vertical
        }else if (lateralp(direction: self.direction)) {
            orientation = Orientation.lateral
        }
        if (orientation == Orientation.vertical){
            return (orientation, self.x, self.y, self.x, self.y)
        }else{
            var x0 = self.x
            var x1 = x0 + self.direction[0]
            var y0 = self.y
            var y1 = y0 + self.direction[1]
            return (orientation, min(x0,x1), min(y0,y1), max(x0,x1), max(y0,y1))
        }
    }
}

