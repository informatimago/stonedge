//
//  ContentView.swift
//  stonedge
//
//  Created by Pascal Bourguignon on 21/02/2024.
//  Copyright © 2024 Ogamita Ltd. All rights reserved.
//

import SwiftUI

struct ContentView: View {

    @State var user: User
    @State private var gameView = false
    @State private var levelIndex = 0

    func newGame()
    {
        user.newGame()
        levelIndex = (user.maxCompletedLevel + 1) % levels.count
    }

    init()
    {
        let newUser = User().loadUser()
        let ll = (newUser.maxCompletedLevel + 1) % levels.count
        user = newUser
        levelIndex = ll
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
                                     user: $user,
                                     gameView: $gameView)
                }
                else {
                    UserView(user: $user,
                             gameView: $gameView,
                             levelIndex: $levelIndex,
                             maxLevelIndex: user.maxCompletedLevel,
                             currentLevelTitle: user.game.title,
                             currentLevelDescription: user.game.description.joined(separator: "\n"))
                }
            }
              .padding()
              .frame(maxWidth: .infinity, maxHeight: .infinity)
              .ignoresSafeArea(edges: .horizontal)

        }
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
