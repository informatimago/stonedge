//
//  User.swift
//  stonedge
//
//  Created by Pascal Bourguignon on 17/03/2024.
//  Copyright © 2024 Ogamita Ltd. All rights reserved.
//

import Foundation

public class User {
    
    public var currentLevelIndex : Int = 0
    public var maxCompletedLevel : Int = 0

    public init()
    {
        loadUser()
    }

    let currentLevelKey = "currentLevel"
    let maxCompletedLevelKey = "maxCompletedLevel"
    
    func saveInteger(value: Int, for key: String) {
        UserDefaults.standard.set(value, forKey: key)
    }
    
    func loadInteger(for key: String) -> Int {
        return UserDefaults.standard.integer(forKey: key)
   
    }
    
    public func loadUser() -> User {
        currentLevelIndex = loadInteger(for: currentLevelKey)
        maxCompletedLevel = loadInteger(for: maxCompletedLevelKey)
        return self
    }
    
    public func saveUser() -> User {
        saveInteger(value:currentLevelIndex, for:currentLevelKey)
        saveInteger(value:maxCompletedLevel, for:maxCompletedLevelKey)
        return self
    }
    
    // [<<] [<] [play] [>] [>>]

}
