//
//  ContentView.swift
//  stonedge
//
//  Created by Pascal Bourguignon on 21/02/2024.
//  Copyright © 2024 Ogamita Ltd. All rights reserved.
//

import SwiftUI

struct ContentView: View {

    @State var game: Game
    @State private var gameView = false
    @State private var levelIndex = 0
    var user = User().loadUser()

    init()
    {
        let ll = (user.maxCompletedLevel + 1) % levels.count
        levelIndex = ll
        if let parsedGame = parseGame(specification: levels[ll]) {
            game = parsedGame
        }else{
            fatalError("Cannot parse game specification \(ll)")
        }
    }

    var body: some View {
        GeometryReader { geometry in
            ZStack {
                LinearGradient(gradient: Gradient(colors: [.blue, .white]),
                               startPoint: .top,
                               endPoint: .bottom)
                  .edgesIgnoringSafeArea(.all)

                if gameView {
                    StonedgeGameView(levelIndex: $levelIndex,
                                     game: $game,
                                     gameView: $gameView)
                }
                else {
                    UserView(gameView: $gameView,
                             levelIndex: $levelIndex,
                             maxLevelIndex: user.maxCompletedLevel,
                             currentLevelTitle: game.title,
                             currentLevelDescription: game.description.joined(separator: "\n"))
                }
            }
              .padding()
        }
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
