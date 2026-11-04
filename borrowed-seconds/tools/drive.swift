// Development helper, macOS only. Finds the game's window and posts real key events
// to its process, so the running game can be driven and photographed.
//   drive winid <pid>
//   drive key <pid> <down|up|tap> <name>
//   drive press <pid> <name> <seconds>
import Cocoa

let codes: [String: CGKeyCode] = [
  "w": 13, "a": 0, "s": 1, "d": 2, "p": 35, "r": 15, "space": 49, "enter": 36,
  "up": 126, "down": 125, "left": 123, "right": 124,
]
let args = CommandLine.arguments
guard args.count >= 3, let pid = Int32(args[2]) else { exit(2) }

func post(_ code: CGKeyCode, _ down: Bool) {
  let e = CGEvent(keyboardEventSource: nil, virtualKey: code, keyDown: down)!
  e.postToPid(pid)
  usleep(30000)  // the post is asynchronous: exiting at once can lose it
}

switch args[1] {
case "winid":
  let list = CGWindowListCopyWindowInfo([.optionOnScreenOnly], kCGNullWindowID) as! [[String: Any]]
  for w in list where (w[kCGWindowOwnerPID as String] as? Int32) == pid {
    if let n = w[kCGWindowNumber as String] as? Int, (w[kCGWindowLayer as String] as? Int) == 0 {
      print(n)
      exit(0)
    }
  }
  exit(1)
case "key":
  let code = codes[args[4]]!
  if args[3] == "down" || args[3] == "tap" { post(code, true) }
  if args[3] == "tap" { usleep(40000) }
  if args[3] == "up" || args[3] == "tap" { post(code, false) }
case "press":
  let code = codes[args[3]]!
  post(code, true)
  usleep(UInt32(Double(args[4])! * 1_000_000))
  post(code, false)
default:
  exit(2)
}
