
let of_bpm bpm = 1. /. (bpm /. 60.)

let bpm_of_fps fps = fps *. 60. (*secs*)

let fps_of_bpm bpm = bpm /. 60. (*secs*)

