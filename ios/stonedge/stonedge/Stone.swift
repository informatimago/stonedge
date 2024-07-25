import SwiftUI

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

public enum Direction : Int {
    case right = 0, left, front, back
}

public enum Orientation {
    case vertical, lateral, front
}

public let lateral = [1,0,0]
public let front = [0,1,0]
public let vertical = [0,0,1]

public func verticalp(direction: [Int]) -> Bool {
    (direction[2] != 0)
}

public func lateralp(direction: [Int]) -> Bool {
    (direction[0] != 0)
}

public func frontp(direction: [Int]) -> Bool {
    (direction[1] != 0)

}


//                   x               x                  x                  x
//                   y               y                  y                  y
//          right    z      left     z         front    z         back     z
//        -------   ---   --------  ---      --------  ---      -------   ---
//          0 0 1    z      0 0 -1  -z        1  0  0   x        1  0  0   x
//          0 1 0    y      0 1 0    y        0  0  1   z        0  0 -1  -z
//         -1 0 0   -x      1 0 0    x        0 -1  0  -y        0  1  0   y

public let rotations: [[[Int]]] = [
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

public func rotate(matrix: [[Int]], vector: [Int]) -> [Int] {
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

public func negate(vector: [Int]) -> [Int] {
    // Returns   - vector
    [-vector[0],-vector[1],-vector[2]]
}


open class Stone {
    // A stone is made of two cubes of size equal to the cells.
    // To move, it rotates 1/4 turn on one of its edges that is in contact with the cells.

    var x: Int = 0 // Ordinate of the first cube of the stone.
    var y: Int = 0 // Coordinate of the first cube of the stone.

    var orientation: [Int] = [0, 0, 1]
    // A unit vector indicating the direction of the stone.
    // The coordinate of the other cube of the stone is given by adding this vector to
    // the coordinates of the first cube.
    // Note: The stone is normalized so that the vertical coordinate of the direction is either 0 or 1.

    public init(x: Int, y:Int, orientation: [Int])
    {
        self.x=x
        self.y=y
        self.orientation = orientation
    }

    public func normalize(for direction: Direction) {
        switch direction {
        case .left:
            if (self.orientation[0]>0) { // lateralp
                self.x += 1
                self.orientation = negate(vector: self.orientation)
            }
        case .right:
            if (self.orientation[0]<0) { // lateralp
                self.x -= 1
                self.orientation = negate(vector: self.orientation)
            }
        case .front:
            if (self.orientation[1]>0) { // frontp
                self.y += 1
                self.orientation = negate(vector: self.orientation)
            }
        case .back:
            if (self.orientation[1]<0) { // frontp
                self.y -= 1
                self.orientation = negate(vector: self.orientation)
            }
        }
    }

    public func move(in direction: Direction) {
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
        self.orientation = rotate(matrix: rotations[direction.rawValue], vector: self.orientation)
    }

    public func coverage() -> (Orientation, Int, Int, Int, Int, Int, Int) {
        // Returns:   direction, x0, y0, z0, x1, y1, z1
        var orientation = Orientation.front
        if (verticalp(direction: self.orientation)) {
            orientation = Orientation.vertical
        }else if (lateralp(direction: self.orientation)) {
            orientation = Orientation.lateral
        }
        if (orientation == Orientation.vertical){
            return (orientation, self.x, self.y, 0, self.x, self.y, 1)
        }else{
            let x0 = self.x
            let x1 = x0 + self.orientation[0]
            let y0 = self.y
            let y1 = y0 + self.orientation[1]
            return (orientation, min(x0,x1), min(y0,y1), 0, max(x0,x1), max(y0,y1), 0)
        }
    }

}
