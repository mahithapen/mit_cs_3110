(** [string_of_time tm] extracts string representing time of [tm] *)
let string_of_time (tm : Unix.tm) : string =
  Format.sprintf "%s %s %2d %02d:%02d:%02d %d"
    (match tm.tm_wday with
    | 0 -> "Sun"
    | 1 -> "Mon"
    | 2 -> "Tue"
    | 3 -> "Wed"
    | 4 -> "Thu"
    | 5 -> "Fri"
    | 6 -> "Sat"
    | _ -> "Invalid")
    (match tm.tm_mon with
    | 0 -> "Jan"
    | 1 -> "Feb"
    | 2 -> "Mar"
    | 3 -> "Apr"
    | 4 -> "May"
    | 5 -> "Jun"
    | 6 -> "Jul"
    | 7 -> "Aug"
    | 8 -> "Sep"
    | 9 -> "Oct"
    | 10 -> "Nov"
    | 11 -> "Dec"
    | _ -> "Invalid")
    tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec (tm.tm_year + 1900)

let current_time () = Unix.gmtime (Unix.time ()) |> string_of_time
