open Parco

let test_first () =
  assert (first "hello" = Some ('h', "ello"));
  assert (first "" = None)

let test_char () =
  assert (char 'a' "abc" = Some ('a', "bc"));
  assert (char 'a' "bcd" = None);
  assert (char 'a' "" = None)

let test_digit () =
  assert (digit "1abc" = Some ('1', "abc"));
  assert (digit "abc" = None);
  assert (digit "" = None)

let test_alt () =
  let p1 = char 'a' in
  let p2 = char 'b' in
  assert (alt p1 p2 "abc" = Some ('a', "bc"));
  assert (alt p1 p2 "bcd" = Some ('b', "cd"));
  assert (alt p1 p2 "cde" = None)

let test_seq () =
  let p1 = char 'a' in
  let p2 = digit in
  assert (seq p1 p2 "a2bc" = Some (('a', '2'), "bc"));
  assert (seq p1 p2 "abc" = None);
  assert (seq p1 p2 "b2ac" = None)

let test_many () =
  let p = char 'a' in
  assert (many p "abc" = Some ([ 'a' ], "bc"));
  assert (many p "aabc" = Some ([ 'a'; 'a' ], "bc"));
  assert (many p "aaabc" = Some ([ 'a'; 'a'; 'a' ], "bc"));
  assert (many p "bcd" = Some ([], "bcd"))

let test_many1 () =
  let p = char 'a' in
  assert (many1 p "abc" = Some ([ 'a' ], "bc"));
  assert (many1 p "aabc" = Some ([ 'a'; 'a' ], "bc"));
  assert (many1 p "bcd" = None)

let () =
  test_first ();
  test_char ();
  test_digit ();
  test_alt ();
  test_seq ();
  test_many ();
  test_many1 ()
