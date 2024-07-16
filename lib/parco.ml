type 'a parser = string -> ('a * string) option

let first : char parser =
 fun s ->
  if String.length s > 0 then Some (s.[0], String.sub s 1 (String.length s - 1))
  else None

let char (c : char) : char parser =
 fun s -> match s with "" -> None | s when s.[0] = c -> first s | _ -> None

let digit : char parser =
 fun s ->
  match s with
  | "" -> None
  | s when s.[0] >= '0' && s.[0] <= '9' -> first s
  | _ -> None

let alt (p1 : 'a parser) (p2 : 'a parser) : char parser =
 fun s ->
  match p1 s with
  | Some (parsed, remainder) -> Some (parsed, remainder)
  | None -> p2 s

let seq (p1 : 'a parser) (p2 : 'b parser) : ('a * 'b) parser =
 fun s ->
  match p1 s with
  | Some (r1, s1) -> (
      match p2 s1 with Some (r2, s2) -> Some ((r1, r2), s2) | None -> None)
  | None -> None

let rec many (p : 'a parser) : 'a list parser =
 fun s ->
  match p s with
  | Some (x, rest) -> (
      match many p rest with
      | Some (xs, rest') -> Some (x :: xs, rest')
      | None -> Some ([ x ], rest))
  | None -> Some ([], s)

let many1 (p : 'a parser) : 'a list parser =
 fun s ->
  match many p s with
  | Some (x, rest) when List.length x > 0 -> Some (x, rest)
  | _ -> None
