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
    @Binding var user: User
    @Binding var gameView: Bool


    @State private var startPoint: CGPoint? = nil
    @State private var endPoint: CGPoint? = nil

    @State private var gameIsWon = false
    @State private var whyLost : String?
    @State private var showAlert = false

    func won()
    {
        gameIsWon = true
        showAlert = true
    }

    func lost(why:String)
    {
        gameIsWon = false
        whyLost = why
        showAlert = true
    }

    func detectSwipeDirection(from start: CGPoint, to end: CGPoint) {
        currentGame = self
        let cosAlpha = cos(alpha)
        let sinAlpha = sin(alpha)
        let dx = (end.x - start.x) / cosAlpha
        let dy = (end.y - start.y) / sinAlpha
        let swipeAngle = atan2(dy, dx) * 180 / .pi
    print("dx=\(dx) dy=\(dy) swipeAngle=\(swipeAngle)")
        if (swipeAngle < 90) {
            print("Swipe detected: SE -> NW .right")
            user.game.move(direction: .right)
        }
        else if (swipeAngle < 180) {
            print("Swipe detected: NE -> SW .back")
            user.game.move(direction: .back)
        }
        else if (swipeAngle < 270) {
            print("Swipe detected: NW -> SE .left")
            user.game.move(direction: .left)
        }
        else {
            print("Swipe detected: SW -> NE .front")
            user.game.move(direction: .front)
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
                let frameSize = geometry.size

                // Example: Assuming you have a method like this
                let (scaleFactor, centerX, centerY) = BoardView.computeScaleFor(cells:user.game.cells, in:geometry)

                ZStack {
                    BoardView(cells: $user.game.cells)
                    StoneView(stone: user.game.stone)

                    KeyEventHandlingView { keyCommand in
                        currentGame = self
                        if let input = keyCommand.input {
                            switch input {
                            case UIKeyCommand.inputEscape, "Q", "q":
                                print("Escape key pressed")
                                gameView = false;

                            case UIKeyCommand.inputUpArrow, "A", "a":
                                print("Up arrow key pressed")
                                user.game.move(direction: .right)

                            case UIKeyCommand.inputDownArrow, "X", "x":
                                print("Down arrow key pressed")
                                user.game.move(direction: .left)

                            case UIKeyCommand.inputLeftArrow, "Z", "z":
                                print("Left arrow key pressed")
                                user.game.move(direction: .back)

                            case UIKeyCommand.inputRightArrow, "S", "s":
                                print("Right arrow key pressed")
                                user.game.move(direction: .front)

                            default:
                                print("Key pressed: \(input)")
                                break
                            }
                        }
                    }

                } // ZStack
                  .rotationEffect(.degrees(180), anchor: .center)
                  .scaleEffect(scaleFactor)
                  .offset(x: -(frameSize.width / 2 - centerX) * scaleFactor,
                          y: -(frameSize.height / 2 - centerY) * scaleFactor)
                  .gesture(
                    DragGesture()
                      .onChanged { value in
                          // Capture the start point
                          if startPoint == nil {
                              startPoint = value.startLocation
                              print(".onChanged startPoint=\(startPoint)")
                          }else{
                              print(".onChanged endPoint=\(endPoint)")

                          }
                          // Update the end point as the drag progresses
                          endPoint = value.location
                      }
                      .onEnded { _ in
                          print(".onEnded \(endPoint)")
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

              .alert(isPresented: $showAlert) {
                  if gameIsWon {
                      return Alert(
                        title: Text("Victory"),
                        message: Text("You passed this level."),
                        dismissButton: .default(Text("Next"),
                                                action: {
                                                    user.won()
                                                    gameView = true
                                                }))
                  }
                  else {
                      var message : String
                      if let reason = whyLost {
                          message = "You failed this level" + ": " + reason + "."
                      }else {
                          message = "You failed this level."
                      }
                      return Alert(
                        title: Text("Failure"),
                        message: Text(message),
                        dismissButton: .default(Text("Try again"),
                                                action: {
                                                    user.newGame()
                                                    gameView = true
                                                }))
                  }
              }

            Spacer()

            VStack(spacing: 10){
                Button("↑") {
                    currentGame = self
                    user.game.move(direction: .right)
                }
                HStack(spacing: 10){
                    Spacer()
                    Button("←") {
                        currentGame = self
                        user.game.move(direction: .back)
                    }
                    Spacer()
                    Text(" ")
                    Spacer()
                    Button("→") {
                        currentGame = self
                        user.game.move(direction: .front)
                    }
                    Spacer()
                }
                Button("↓") {
                    currentGame = self
                    user.game.move(direction: .left)
                }
            }
              .background(Color(UIColor.systemBackground))

        }
    }


    // public func box()
    // {
    //     // Returns the CGRect needed to draw the board and stone.
    //     var gameBox : CGRect = StoneView.boxFor(stone: game.stone)
    //     for(var y=0; y<game.cells.count; y+=1) {
    //         for(var x=0; x<game.cells[y].count; x+=1) {
    //             gameBox.union(BoardView.boxFor(cell: game.cells[y][x]))
    //         }
    //     }
    //     return gameBox
    // }


}

var currentGame : StonedgeGameView?

public func signalGameWon(){
    print("signalGameWon: currentGame = \(currentGame)")
    if let game = currentGame {
        game.won();
    }
}

public func signalGameLost(why: String){
    if let game = currentGame {
        game.lost(why: why);
    }
}


// #Preview {
//     StonedgeGameView(gameView: .constant(true),
//                      levelIndex:0)
// }
