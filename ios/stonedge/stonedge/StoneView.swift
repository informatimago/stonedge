import SwiftUI

struct StoneView: View {

    @ObservedObject var stone: Stone

    private var stoneWidth: CGFloat {
        // Adjusts the stone width based on its orientation
        if lateralp(direction: stone.orientation) || frontp(direction: stone.orientation) {
            return squareSize * 2
        } else {
            return squareSize
        }
    }

    private var stoneHeight: CGFloat {
        // Adjusts the stone height based on its orientation
        if verticalp(direction: stone.orientation) {
            return squareSize * 2
        } else {
            return squareSize
        }
    }


    public func stonePaths(with geometry: GeometryProxy, d: CGFloat, h: CGFloat, alpha: CGFloat, cornerRadius: CGFloat) -> (Path, Path, Path)
    {
        let (orientation, x0, y0, z0, x1, y1, z1) = stone.coverage()

        let cosAlpha = cos(alpha)
        let sinAlpha = sin(alpha)

        // Compute relative points
        // let O = CGPoint(x: 0, y: 0)
        let H = CGPoint(x: 0, y: h)
        // let F = CGPoint(x: d * cosAlpha, y: d * sinAlpha)
        let B = CGPoint(x: d * cosAlpha, y: h + d * sinAlpha)
        // let C = CGPoint(x: 0, y: h + 2 * d * sinAlpha)
        let D = CGPoint(x: -d * cosAlpha, y: h + d * sinAlpha)
        // let E = CGPoint(x: -d * cosAlpha, y: d * sinAlpha)

        let Hu = CGPoint(x: 0, y: d + h)
        // let F = CGPoint(x: d * cosAlpha, y: d * sinAlpha)
        let Bu = CGPoint(x: d * cosAlpha, y: d + h + d * sinAlpha)
        let Cu = CGPoint(x: 0, y: d + h + 2 * d * sinAlpha)
        let Du = CGPoint(x: -d * cosAlpha, y: d + h + d * sinAlpha)

        let Ba = CGPoint(x: d * cosAlpha, y: 2*d + h + d * sinAlpha)
        let Ca = CGPoint(x: 0, y: 2*d + h + 2 * d * sinAlpha)
        let Da = CGPoint(x: -d * cosAlpha, y: 2*d + h + d * sinAlpha)


        //       Cu
        //
        // Du           Bu
        // |            |
        // |     Hu     |
        // |            |
        // |     C      |
        // |            |
        // D            B
        //
        //       H
        //       |
        //       O



        // Compute cell origin
        let Oxy = CGPoint(x: (CGFloat(x0 - y0) * d * cosAlpha) + geometry.size.width / 2,
                          y: (CGFloat(x0 + y0) * d * sinAlpha) + geometry.size.height / 2)
        let Pxy = CGPoint(x: (CGFloat(x1 - y1) * d * cosAlpha) + geometry.size.width / 2,
                          y: (CGFloat(x1 + y1) * d * sinAlpha) + geometry.size.height / 2)

        var dx = Pxy.x - Oxy.x
        var dy = Pxy.y - Oxy.y
        switch(orientation){
        case .vertical:
            dx=0
            dy=d
        default:
            break;
        }

        print( "stone Oxy(\(stone.x),\(stone.y)) = \(Oxy)  orientation = \(orientation) x0=\(x0) y0=\(y0) z0=\(z0) x1=\(x1) y1=\(y1) z1=\(z1) dx=\(dx) dy=\(dy)")

        // view the cell slab

        var path1 = Path()
        path1.move(to: CGPoint(x: Oxy.x + H.x, y: Oxy.y + H.y))
        path1.addArc(tangent1End: CGPoint(x: Oxy.x + H.x, y: Oxy.y + H.y), tangent2End: CGPoint(x: Oxy.x + B.x, y: Oxy.y + B.y), radius: cornerRadius)
        path1.addArc(tangent1End: CGPoint(x: Oxy.x + B.x, y: Oxy.y + B.y), tangent2End: CGPoint(x: Oxy.x + Bu.x, y: Oxy.y + Bu.y), radius: cornerRadius)
        path1.addArc(tangent1End: CGPoint(x: Oxy.x + Bu.x, y: Oxy.y + Bu.y), tangent2End: CGPoint(x: Oxy.x + Cu.x, y: Oxy.y + Cu.y), radius: cornerRadius)
        path1.addArc(tangent1End: CGPoint(x: Oxy.x + Cu.x, y: Oxy.y + Cu.y), tangent2End: CGPoint(x: Oxy.x + Du.x, y: Oxy.y + Du.y), radius: cornerRadius)
        path1.addArc(tangent1End: CGPoint(x: Oxy.x + Du.x, y: Oxy.y + Du.y), tangent2End: CGPoint(x: Oxy.x + D.x, y: Oxy.y + D.y), radius: cornerRadius)
        path1.addArc(tangent1End: CGPoint(x: Oxy.x + D.x, y: Oxy.y + D.y), tangent2End: CGPoint(x: Oxy.x + H.x, y: Oxy.y + H.y), radius: cornerRadius)
        path1.closeSubpath()

        let transform = CGAffineTransform(translationX: dx, y: dy)
        let path2 = path1.applying(transform)

        var pathF = path1
        var pathB = path2

        switch(orientation){
        case .vertical:
            pathF = path1
            pathB = path2
        case .lateral:
            pathF = path2
            pathB = path1
        case .front:
            pathF = path2
            pathB = path1
        }

        let ticks = Path()
        //         let n = 4
        //         let e = d/16
        //         var ticks = Path()
        //         for u in [-1.0,1.0] {
        //             let e0x = Oxy.x - 2*u*e*cosAlpha
        //             let e0y = Oxy.y + h+2*e*sinAlpha
        //             let e1x = Oxy.x + -u*e*cosAlpha
        //             let e1y = Oxy.y + h+e*sinAlpha
        //             let etx = Oxy.x + 0.0
        //             let ety = Oxy.y + h
        //             let e2x = Oxy.x + 0.0
        //             let e2y = Oxy.y + h-e
        //             let e3x = Oxy.x + 0.0
        //             let e3y = Oxy.y + h-2*e
        //             for k in 0..<n {
        //                 let dx = u*(CGFloat(k+1))*d*cosAlpha/CGFloat(n+1)
        //                 let dy = (CGFloat(k+1))*d*sinAlpha/CGFloat(n+1)
        //                 ticks.move(to: CGPoint(x:e0x+dx, y:e0y+dy))
        //                 ticks.addLine(to: CGPoint(x:e1x+dx, y:e1y+dy))
        //                 ticks.addArc(tangent1End:CGPoint(x:etx+dx, y:ety+dy), tangent2End: CGPoint(x:e2x+dx, y:e2y+dy), radius: cornerRadius)
        //                 ticks.addLine(to: CGPoint(x:e2x+dx, y:e2y+dy))
        //                 ticks.addLine(to: CGPoint(x:e3x+dx, y:e3y+dy))
        //
        // //                let squareSize = CGSize(width: 3, height: 3)
        // //                [CGPoint(x:e0x+dx, y:e0y+dy),
        // //                 CGPoint(x:e1x+dx, y:e1y+dy),
        // //                 CGPoint(x:e2x+dx, y:e2y+dy),
        // //                 CGPoint(x:e3x+dx, y:e3y+dy)].forEach { point in
        // //                    let squareOrigin = CGPoint(x: point.x - squareSize.width / 2, y: point.y - squareSize.height / 2)
        // //                    let squareRect = CGRect(origin: squareOrigin, size: squareSize)
        // //                    path.addRect(squareRect)
        // //                }
        //             }


        //        }


        return (pathF, pathB, ticks)
    }

    public func stoneColor() -> Color {
        return Color.brown
    }

    public func computeView(with geometry: GeometryProxy, d: CGFloat, h: CGFloat, alpha: CGFloat, cornerRadius: CGFloat) -> AnyView
    {
        let (path1, path2, ticks) = stonePaths(with: geometry, d: d, h: h, alpha: alpha, cornerRadius:cornerRadius)
        let x = stone.x
        let y = stone.y
        let cosAlpha = cos(alpha)
        let sinAlpha = sin(alpha)
        let Oxy = CGPoint(x: (CGFloat(x - y) * d * cosAlpha) + geometry.size.width / 2,
                          y: (CGFloat(x + y) * d * sinAlpha) + geometry.size.height / 2)
        return AnyView(ZStack(){
                           AnyView(
                             ZStack() {
                                 path1
                                   .fill(stoneColor())
                                   .overlay(path1.stroke(Color.black, lineWidth: 2))
                                 path2
                                   .fill(stoneColor())
                                   .overlay(path2.stroke(Color.black, lineWidth: 2))
                             }
                               .overlay(ticks.stroke(Color.black, lineWidth: 1))
                           )

                           Text("\(x),\(y)")
                             .foregroundColor(.red)
                             .rotationEffect(.degrees(180), anchor: .center)
                             .position(x: Oxy.x, y: Oxy.y)
                             .offset(y:0.75*d)
                       })
    }

    var body: some View {
        GeometryReader { geometry in
            computeView(with: geometry,
                       d: d,
                       h: h,
                       alpha: alpha,
                       cornerRadius: cellCornerRadius)

        }
    }

}



struct StoneView_Previews: PreviewProvider {
   static var previews: some View {
       @StateObject var stone = Stone(x: 3,y: 3, orientation: [1,0,0])
       StoneView(stone: stone)
   }
}
