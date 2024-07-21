type 'a parser = string -> ('a * string) option

let first : char parser =
 fun s ->
  if String.length s > 0 then Some (s.[0], String.sub s 1 (String.length s - 1))
  else None

let char (c : char) : char parser =
 fun s -> match s with "" -> None | s when s.[0] = c -> first s | _ -> None

let alpha : char parser =
 fun s ->
  match s.[0] with
  | c when int_of_char 'a' <= int_of_char c && int_of_char c <= int_of_char 'z'
    ->
      first s
  | c when int_of_char 'A' <= int_of_char c && int_of_char c <= int_of_char 'Z'
    ->
      first s
  | _ -> None

let digit : char parser =
 fun s ->
  match s with
  | "" -> None
  | s when s.[0] >= '0' && s.[0] <= '9' -> first s
  | _ -> None

let alt (p1 : 'a parser) (p2 : 'a parser) : char parser =
 fun s -> match p1 s with Some (res, rem) -> Some (res, rem) | None -> p2 s

let seq (p1 : 'a parser) (p2 : 'b parser) : ('a * 'b) parser =
 fun s ->
  match p1 s with
  | Some (res, rem) -> (
      match p2 rem with
      | Some (res', rem') -> Some ((res, res'), rem')
      | None -> None)
  | None -> None

let rec many (p : 'a parser) : 'a list parser =
 fun s ->
  match p s with
  | Some (res, rem) -> (
      match many p rem with
      | Some (res', rem') -> Some (res :: res', rem')
      | None -> Some ([ res ], rem))
  | None -> Some ([], s)

let many1 (p : 'a parser) : 'a list parser =
 fun s ->
  match many p s with
  | Some (res, rem) when List.length res > 0 -> Some (res, rem)
  | _ -> None
