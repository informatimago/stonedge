//
//  StonedgeGameView.swift
//  stonedge
//
//  Created by Pascal Bourguignon on 05/03/2024.
//  Copyright © 2024 Ogamita Ltd. All rights reserved.
//

import SwiftUI
import Foundation



struct StonedgeGameView: View {
    @Binding var levelIndex: Int
    @Binding var game: Game
    @Binding var gameView: Bool

    @State private var startPoint: CGPoint? = nil
    @State private var endPoint: CGPoint? = nil

    //    let game : Game;

    //    init(game: Game){
    //        self.game = game
    //    }
    //


    func detectSwipeDirection(from start: CGPoint, to end: CGPoint) {
        let cosAlpha = cos(alpha)
        let sinAlpha = sin(alpha)
        let dx = (end.x - start.x) / cosAlpha
        let dy = (end.y - start.y) / sinAlpha

        // alpha

        if abs(dx) > abs(dy) {
            // Horizontal swipe
            if dx > 0 {
                // Right swipe
                print("Swipe detected: NW -> SE")
                game.move(direction: .right)
            } else {
                // Left swipe
                print("Swipe detected: SE -> NW")
                game.move(direction: .left)
            }
        } else {
            // Vertical swipe
            if dy > 0 {
                // Down swipe
                print("Swipe detected: NE -> SW")
                game.move(direction: .front)
            } else {
                // Up swipe
                print("Swipe detected: SW -> NE")
                game.move(direction: .back)
            }
        }
    }


    var body: some View {
        VStack(spacing: 10) {
            HStack(spacing: 20){
                Text("  ")
                Button("Back") {
                    gameView = false;
                }
                Spacer()
            }
              .padding(.horizontal)
              .background(Color(UIColor.systemBackground))

            Spacer()

            GeometryReader { geometry in
                ZStack {
                    BoardView(cells: $game.cells)
                    StoneView(stone: $game.stone)
                }
                  .rotationEffect(.degrees(180), anchor: .center)
                  .offset(y: geometry.size.height*0.4)
                  .scaleEffect(x: 0.4, y: 0.4)
                  .gesture(
                    DragGesture()
                      .onChanged { value in
                          // Capture the start point
                          if startPoint == nil {
                              startPoint = value.startLocation
                          }
                          // Update the end point as the drag progresses
                          endPoint = value.location
                      }
                      .onEnded { _ in
                          // Calculate the direction when the drag ends
                          if let start = startPoint, let end = endPoint {
                              detectSwipeDirection(from: start, to: end)
                          }
                          // Reset points
                          startPoint = nil
                          endPoint = nil
                      }
                  )
            }
        }
    }
}


// #Preview {
//     StonedgeGameView(gameView: .constant(true),
//                      levelIndex:0)
// }
