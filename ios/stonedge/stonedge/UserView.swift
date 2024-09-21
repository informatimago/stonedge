//
//  UserView.swift
//  stonedge
//
//  Created by Pascal Bourguignon on 18/03/2024.
//  Copyright © 2024 Ogamita Ltd. All rights reserved.
//

import SwiftUI

struct UserView: View {

    @ObservedObject var user: User
    @Binding var gameView: Bool
    @Binding var levelIndex: Int

    func maxLeveIndex() -> Int
    {
        return user.maxCompletedLevel
    }

    func currentLevelTitle() -> String
    {
        return user.game.title
    }

    func currentLevelDescription() -> String
    {
        return user.game.description.joined(separator: "\n")
    }

    @State private var showAlert = false
    private let aboutText = "Author: Pascal J. Bourguignon\n"
      + "\n"
      + "Inspired by the old Playtomo Stonedge Game on Blackberry.\n"
      + "\n"
      + "mailto:info@ogamita.com\n"
      + "\n"
      + "Copyright © 2024 Ogamita Ltd. All rights reserved."

    var body: some View {
        VStack(spacing: 20) {
            // Title
            Text("stonedge")
                .font(.largeTitle)
                .padding()

            // Level Name
            Text(currentLevelTitle())
                .font(.title)

            Text(currentLevelDescription())

            // Button Stack
            VStack(spacing: 10) {
                Button("Play") {
                    user.resetGame()
                    gameView = true
                }
                Button("Next Level") {
                    user.nextLevel()
                }
                Button("Best Reached Level") {
                    user.lastAvailableLevel()
                }
                Button("Previous Level") {
                    user.previousLevel()
                }
                Button("Restart Game") {
                    user.restartGame()
                }
                Button("About Stonedge…") {
                    showAlert = true
                }
            }

            Spacer()
        }
          .alert(isPresented: $showAlert) {
              return Alert(
                title: Text("About Stonedge"),
                message: Text(aboutText),
                dismissButton: .default(Text("OK"),
                                        action: {
                                            showAlert = false
                                        }))
          }
    }
}

#if swift(>=5.9)
#Preview {
    @StateObject var user: User = User()
    @State var gameView: Bool = false;
    @State var levelIndex: Int = 1;
    return UserView(user: user,
                    gameView: $gameView,
                    levelIndex: $levelIndex)
}
#endif
