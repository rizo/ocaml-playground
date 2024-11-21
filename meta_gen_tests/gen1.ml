let x = 1 in
let y = 2 in
print_endline
@@ String.concat ""
     [
       "let z = 200 in ";
       "[";
       string_of_int x;
       ";";
       "100 + ";
       string_of_int y;
       ";";
       "z";
       "]";
     ]
