import SwiftUI
import AppKit

struct KeyEventHandlingView: NSViewControllerRepresentable {
    class Coordinator: NSObject {
        var parent: KeyEventHandlingView

        init(_ parent: KeyEventHandlingView) {
            self.parent = parent
        }
    }

    var onKeyPress: (NSEvent) -> Void

    public static let inputEscape = String(UnicodeScalar(27)!)
    public static let inputUpArrow = String(UnicodeScalar(0x2191)!)
    public static let inputDownArrow = String(UnicodeScalar(0x2193)!)
    public static let inputLeftArrow = String(UnicodeScalar(0x2190)!)
    public static let inputRightArrow = String(UnicodeScalar(0x2192)!)

    public static func input(command: NSEvent) -> String?
    {
        switch command.keyCode {
        case UInt16(27) /*escape*/:
            return inputEscape
        case UInt16(NSUpArrowFunctionKey):
            return inputUpArrow
        case UInt16(NSDownArrowFunctionKey):
            return inputDownArrow
        case UInt16(NSLeftArrowFunctionKey):
            return inputLeftArrow
        case UInt16(NSRightArrowFunctionKey):
            return inputRightArrow
        default:
            return command.characters
        }
    }

    func makeCoordinator() -> Coordinator {
        Coordinator(self)
    }

    func makeNSViewController(context: Context) -> NSViewController {
        let viewController = KeyEventHandlingViewController()
        viewController.onKeyPress = onKeyPress
        return viewController
    }

    func updateNSViewController(_ nsViewController: NSViewController, context: Context) {
    }
}

class KeyEventHandlingViewController: NSViewController {
    var onKeyPress: ((NSEvent) -> Void)?

    override var acceptsFirstResponder: Bool {
        return true
    }

    override func viewDidAppear() {
        super.viewDidAppear()
        view.window?.makeFirstResponder(self)
    }

    override func keyDown(with event: NSEvent) {
        onKeyPress?(event)
    }
}
