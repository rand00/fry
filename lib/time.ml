
let of_bpm bpm = 1. /. (bpm /. 60.)

let of_fps fps = 1. /. fps

let bpm_of_fps fps = fps *. 60. (*secs*)

let fps_of_bpm bpm = bpm /. 60. (*secs*)

