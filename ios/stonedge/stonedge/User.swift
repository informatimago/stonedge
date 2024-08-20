//
//  User.swift
//  stonedge
//
//  Created by Pascal Bourguignon on 17/03/2024.
//  Copyright © 2024 Ogamita Ltd. All rights reserved.
//

import Foundation

public class User : Identifiable, ObservableObject {

    @Published var game : Game
    @Published var currentLevelIndex : Int = 0
    @Published var maxCompletedLevel : Int = 0

    public init()
    {
        if let parsedGame = parseGame(specification: levels[0]) {
            game = parsedGame
        }else{
            fatalError("Cannot parse game specification \(0)")
        }
    }

    public func won()
    {
        if (currentLevelIndex > maxCompletedLevel) {
            maxCompletedLevel = currentLevelIndex
        }
        nextLevel()
    }

    public func restartGame()
    {
        currentLevelIndex = 0
        saveUser()
    }

    public func nextLevel()
    {
        currentLevelIndex = currentLevelIndex + 1
        if (currentLevelIndex > maxCompletedLevel+1) {
            currentLevelIndex = maxCompletedLevel+1
        }
        if (currentLevelIndex > levels.count - 1) {
            currentLevelIndex = levels.count - 1
        }
        saveUser()
    }

    public func previousLevel()
    {
        currentLevelIndex = currentLevelIndex - 1
        if (currentLevelIndex < 0) {
            currentLevelIndex = 0
        }
        saveUser()
    }

    public func resetGame()
    {
        if let parsedGame = parseGame(specification: levels[currentLevelIndex]) {
            game = parsedGame
        }else{
            fatalError("Cannot parse game specification \(currentLevelIndex)")
        }
    }

    let currentLevelKey = "currentLevel"
    let maxCompletedLevelKey = "maxCompletedLevel"

    func saveInteger(value: Int, for key: String) {
        UserDefaults.standard.set(value, forKey: key)
    }

    func loadInteger(for key: String, defaultValue: Int) -> Int {
        return UserDefaults.standard.object(forKey: key) as? Int ?? defaultValue
    }

    public func loadUser() -> User {
        currentLevelIndex = loadInteger(for: currentLevelKey, defaultValue: 0)
        maxCompletedLevel = loadInteger(for: maxCompletedLevelKey, defaultValue: -1)
        return self
    }

    public func saveUser() -> User {
        saveInteger(value:currentLevelIndex, for:currentLevelKey)
        saveInteger(value:maxCompletedLevel, for:maxCompletedLevelKey)
        return self
    }

    // [<<] [<] [play] [>] [>>]

}
