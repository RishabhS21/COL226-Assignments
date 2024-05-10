let () =
  (* Open the input file using the name provided as the first command-line argument *)
  let filename = Sys.argv.(1) in
  let in_channel = open_in filename in

  (* Create a lexing buffer from the input channel and parse the input file *)
  let lexbuf = Lexing.from_channel in_channel in
  let program = Parser.program Lexer.tokenize lexbuf in

  (* Convert the parsed program into a string for printing *)
  let program_str = Ast.string_of_program program in

  (* Print the program string and close the input channel *)
  print_string program_str;
  close_in in_channel