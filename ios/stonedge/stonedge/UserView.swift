//
//  UserView.swift
//  stonedge
//
//  Created by Pascal Bourguignon on 18/03/2024.
//  Copyright © 2024 Ogamita Ltd. All rights reserved.
//

import SwiftUI

struct UserView: View {

    @Binding var user: User
    @Binding var gameView: Bool
    @Binding var levelIndex: Int
    var maxLevelIndex: Int
    var currentLevelTitle: String
    var currentLevelDescription: String

    var body: some View {
        VStack(spacing: 20) {
            // Title
            Text("Stonedge")
                .font(.largeTitle)
                .padding()

            // Level Name
            Text(currentLevelTitle)
                .font(.title)

            Text(currentLevelDescription)

            // Button Stack
            VStack(spacing: 10) {
                Button("Play") {
                    // Action for Play button
                    print("Play tapped")
                    user.resetGame()
                    gameView = true
                }
                Button("Next Level") {
                    // Action for Next button
                    print("Next tapped")
                    user.nextLevel()
                }
                Button("Previous Level") {
                    // Action for Previous button
                    print("Previous tapped")
                    user.previousLevel()
                }
                Button("Restart Game") {
                    // Action for Reset button
                    print("Reset tapped")
                    user.restartGame()
                }
            }

            Spacer()
        }
    }
}

#Preview {
    @State var user: User = User()
    @State var gameView: Bool = false;
    @State var levelIndex: Int = 1;
    return UserView(user: $user,
                    gameView: $gameView,
                    levelIndex: $levelIndex,
                    maxLevelIndex: 1,
                    currentLevelTitle: "Simple",
                    currentLevelDescription: "A simple test level")
}
