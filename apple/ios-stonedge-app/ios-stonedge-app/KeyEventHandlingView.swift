//
//  KeyEventHandlingView.swift
//  stonedge
//
//  Created by Pascal Bourguignon on 29/07/2024.
//  Copyright © 2024 Ogamita Ltd. All rights reserved.
//

import Foundation
import SwiftUI
import UIKit

struct KeyEventHandlingView: UIViewControllerRepresentable {
    class Coordinator: NSObject {
        var parent: KeyEventHandlingView

        init(_ parent: KeyEventHandlingView) {
            self.parent = parent
        }
    }

    var onKeyPress: (UIKeyCommand) -> Void

    public static let inputEscape = UIKeyCommand.inputEscape
    public static let inputUpArrow = UIKeyCommand.inputUpArrow
    public static let inputDownArrow = UIKeyCommand.inputDownArrow
    public static let inputLeftArrow = UIKeyCommand.inputLeftArrow
    public static let inputRightArrow = UIKeyCommand.inputRightArrow

    public static func input(command: UIKeyCommand) -> String?
    {
        return command.input
    }

    func makeCoordinator() -> Coordinator {
        Coordinator(self)
    }

    func makeUIViewController(context: Context) -> UIViewController {
        let viewController = KeyEventHandlingViewController()
        viewController.onKeyPress = onKeyPress
        return viewController
    }

    func updateUIViewController(_ uiViewController: UIViewController, context: Context) {
    }
}

class KeyEventHandlingViewController: UIViewController {
    var onKeyPress: ((UIKeyCommand) -> Void)?

    override var canBecomeFirstResponder: Bool {
        return true
    }

    override func viewDidAppear(_ animated: Bool) {
        super.viewDidAppear(animated)
        becomeFirstResponder()
    }

    override var keyCommands: [UIKeyCommand]? {
        return [
            UIKeyCommand(input: UIKeyCommand.inputEscape, modifierFlags: [], action: #selector(handleKeyPress(_:))),
           UIKeyCommand(input: UIKeyCommand.inputUpArrow, modifierFlags: [], action: #selector(handleKeyPress(_:))),
            UIKeyCommand(input: UIKeyCommand.inputDownArrow, modifierFlags: [], action: #selector(handleKeyPress(_:))),
            UIKeyCommand(input: UIKeyCommand.inputLeftArrow, modifierFlags: [], action: #selector(handleKeyPress(_:))),
            UIKeyCommand(input: UIKeyCommand.inputRightArrow, modifierFlags: [], action: #selector(handleKeyPress(_:))),
            UIKeyCommand(input: "a", modifierFlags: [], action: #selector(handleKeyPress(_:))),
            UIKeyCommand(input: "s", modifierFlags: [], action: #selector(handleKeyPress(_:))),
            UIKeyCommand(input: "z", modifierFlags: [], action: #selector(handleKeyPress(_:))),
            UIKeyCommand(input: "x", modifierFlags: [], action: #selector(handleKeyPress(_:)))
        ]
    }

    @objc private func handleKeyPress(_ sender: UIKeyCommand) {
        onKeyPress?(sender)
    }
}

