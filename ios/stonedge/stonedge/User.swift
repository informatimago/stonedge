//
//  User.swift
//  stonedge
//
//  Created by Pascal Bourguignon on 17/03/2024.
//  Copyright © 2024 Ogamita Ltd. All rights reserved.
//

import Foundation

public class User {
    
    var currentLevelIndex : Int = 0
    var maxCompletedLevel : Int = 0

    public init()
    {
        
    }

    let currentLevelKey = "currentLevel"
    let maxCompletedLevelKey = "maxCompletedLevel"
    
    func saveInteger(value: Int, for key: String) {
        UserDefaults.standard.set(value, forKey: key)
    }
    
    func loadInteger(for key: String) -> Int {
        return UserDefaults.standard.integer(forKey: key)

    }
    
    public func loadUser(){
        currentLevelIndex = loadInteger(for: currentLevelKey)
        maxCompletedLevel = loadInteger(for: maxCompletedLevelKey)
    }
    
    public func saveUser(){
        saveInteger(value:currentLevelIndex, for:currentLevelKey)
        saveInteger(value:maxCompletedLevel, for:maxCompletedLevelKey)
    }
    
    // [<<] [<] [play] [>] [>>]

}
