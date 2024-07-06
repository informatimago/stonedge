//
//  ContentView.swift
//  stonedge
//
//  Created by Pascal Bourguignon on 21/02/2024.
//

import SwiftUI

struct ContentView: View {
    
    @State private var gameView = false
    @State private var levelIndex = 0
    var user = User().loadUser()
    
    var body: some View {
        GeometryReader { geometry in
            ZStack {
                LinearGradient(gradient: Gradient(colors: [.blue, .white]),
                               startPoint: .top,
                               endPoint: .bottom)
                .edgesIgnoringSafeArea(.all)
                
                if gameView {
                    StonedgeGameView(gameView: $gameView,
                                     levelIndex:levelIndex)
                }
                else {
                    UserView(gameView: $gameView,
                             levelIndex: $levelIndex,
                             maxLevelIndex: user.maxCompletedLevel,
                             currentLevelTitle:"Level One",
                             currentLevelDescription:"A simple level\nvery easy to solve.\n")
                }
                    // .foregroundColor(.accentColor)
                //
                //            Image(systemName: "globe")
                //                .imageScale(.large)
                //
                //            Text("Hello, world!")
                // StonedgeGameView()
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
