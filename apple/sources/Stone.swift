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

public enum Orientation : Int {
    case lateral = 0, front, vertical
}

public let lateral = [1,0,0]
public let front = [0,1,0]
public let vertical = [0,0,1]

public func lateralp(direction: [Int]) -> Bool {
    (direction[Orientation.lateral.rawValue] != 0)
}

public func frontp(direction: [Int]) -> Bool {
    (direction[Orientation.front.rawValue] != 0)
}

public func verticalp(direction: [Int]) -> Bool {
    (direction[Orientation.vertical.rawValue] != 0)
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

public func vector_negate(vector: [Int]) -> [Int] {
    // Returns   - vector
    [-vector[0],-vector[1],-vector[2]]
}

public func vector_minus(_ a:[Int], _ b:[Int]) -> [Int] {
    // Returns the vector a-b:
    return [a[0]-b[0], a[1]-b[1], a[2]-b[2]]
}

public func vector_plus(_ a:[Int], _ b:[Int]) -> [Int] {
    // Returns the vector a+b:
    return [a[0]+b[0], a[1]+b[1], a[2]+b[2]]
}

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



open class Stone : ObservableObject {

    // A stone is made of two cubes of size equal to the cells.
    // To move, it rotates 1/4 turn on one of its edges that is in contact with the cells.

    @Published var x: Int = 0 // Ordinate of the first cube of the stone.
    @Published var y: Int = 0 // Coordinate of the first cube of the stone.

    @Published var orientation: [Int] = vertical
    // A unit vector indicating the direction of the stone.
    // The coordinate of the other cube of the stone is given by adding this vector to
    // the coordinates of the first cube.

    public init(x: Int, y:Int, orientation: [Int])
    {
        self.x=x
        self.y=y
        self.orientation = orientation
    }

    public func normalize() {
        // The stone is normalized so that the coordinates of the direction are either 0 or 1,
        if(self.orientation[Orientation.lateral.rawValue] < 0){
            self.x -= 1
            self.orientation[Orientation.lateral.rawValue] = 1
        }
        if(self.orientation[Orientation.front.rawValue] < 0){
            self.y -= 1
            self.orientation[Orientation.front.rawValue] = 1
        }
        if(self.orientation[Orientation.vertical.rawValue] < 0){
            self.orientation[Orientation.vertical.rawValue] = 1
        }
    }


    public func width() -> Int
    {
        if lateralp(direction: self.orientation) {
            return 2
        } else {
            return 1
        }
    }

    public func depth() -> Int
    {
        if frontp(direction: self.orientation) {
            return 2
        } else {
            return 1
        }
    }

    public func height() -> Int
    {
        if verticalp(direction: self.orientation) {
            return 2
        } else {
            return 1
        }
    }

    public func rotationAxisForMovement(direction: Direction) -> ([Int], [[Int]])
    {
        // PRE: self is normalized.
        switch direction {
        case .left:
            // x -
            return ([x, y, 0], rotations[Direction.left.rawValue])
        case .right:
            // x +
            return ([x+self.width(), y, 0], rotations[Direction.right.rawValue])
        case .back:
            // y -
            return ([x, y, 0], rotations[Direction.back.rawValue])
        case .front:
            // y +
            return ([x, y+self.depth(), 0], rotations[Direction.front.rawValue])
        }
    }

    public func move(in direction: Direction) {
        print("before move in \(direction): (\(x),\(y),\(orientation))")
        normalize()
        let p0 = [x, y, 0]
        let (c, rotationMatrix) = rotationAxisForMovement(direction: direction)
        let p = vector_plus(rotate(matrix: rotationMatrix, vector: vector_minus(p0,c)),c)
        let d = rotate(matrix: rotationMatrix, vector: [width(), depth(), height()])
        let o = rotate(matrix: rotationMatrix, vector: orientation)

        if(d[0]<0){
            x = p[0]+d[0]
            orientation[0] = -o[0]
        }else{
            x = p[0]
            orientation[0] = o[0]
        }
        if(d[1]<0){
            y = p[1]+d[1]
            orientation[1] = -o[1]
        }else{
            y = p[1]
            orientation[1] = o[1]
        }
        if(d[2]<0){
            orientation[2] = -o[2]
        }else{
            orientation[2] = o[2]
        }
        print("after move in \(direction): (\(x),\(y),\(orientation))")
    }

    /*

verticalp +
|
|                +---+
|                |   |
|                +---+
|                |   |
| ---+---+---+---+---+---+---+---+---
|    0   1   2   3   4   5   6   7
--> right/front -->
|
|                    +---+---+
|                    |   |   |
| ---+---+---+---+---+---+---+---+---
|    0   1   2   3   4   5   6   7

verticalp -
|
|                +---+
|                |   |
|                +---+
|                |   |
| ---+---+---+---+---+---+---+---+---
|    0   1   2   3   4   5   6   7
--> left/back -->
|
|        +---+---+
|        |   |   |
| ---+---+---+---+---+---+---+---+---
|    0   1   2   3   4   5   6   7


lateralp +
|
|                +---+---+
|                |   |   |
| ---+---+---+---+---+---+---+---+---
|    0   1   2   3   4   5   6   7
--> right/front -->
|
|                        +---+
|                        |   |
|                        +---+
|                        |   |
| ---+---+---+---+---+---+---+---+---
|    0   1   2   3   4   5   6   7


lateralp -
|
|                +---+---+
|                |   |   |
| ---+---+---+---+---+---+---+---+---
|    0   1   2   3   4   5   6   7
--> left/back -->
|
|            +---+
|            |   |
|            +---+
|            |   |
| ---+---+---+---+---+---+---+---+---
|    0   1   2   3   4   5   6   7


frontp +
|
|                +---+
|                |   |
| ---+---+---+---+---+---+---+---+---
|    0   1   2   3   4   5   6   7
--> right/front -->
|
|
|
|                    +---+
|                    |   |
| ---+---+---+---+---+---+---+---+---
|    0   1   2   3   4   5   6   7


frontp -
|
|                +---+
|                |   |
| ---+---+---+---+---+---+---+---+---
|    0   1   2   3   4   5   6   7
--> left/back -->
|
|            +---+
|            |   |
| ---+---+---+---+---+---+---+---+---
|    0   1   2   3   4   5   6   7



     */

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
