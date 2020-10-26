module Examples where

import Language.MiniPascal

empty :: MonadLang m => Module m
empty = Module {variables = [], functions = [], body = block []}

greeting :: MonadLang m => Module m
greeting =
  Module
    { variables = [("name", String, stringLit "")],
      functions =
        [ Function
            { name = "greet",
              type_ = String,
              arguments = [("name", String)],
              variables = [],
              body =
                block
                  [ "greet" .= string "Hello, " .+ deref "name" .+ string "!"
                  ]
            }
        ],
      body =
        block
          [ readln [Variable "name" (Just String)],
            writeln [call "greet" [deref "name"]]
          ]
    }

emptyFunction :: String
emptyFunction =
  unlines
    [ "function f():integer;",
      "begin end;",
      "begin end."
    ]

functionWithoutReturn :: String
functionWithoutReturn =
  unlines
    [ "function f():integer;",
      "begin writeln(1); end;",
      "begin end."
    ]

duplicatedVariable :: String
duplicatedVariable =
  unlines
    [ "var a : integer = 0; a : real = 1.0;",
      "function f():integer;",
      "begin writeln(1); end;",
      "begin end."
    ]

duplicatedFunction :: String
duplicatedFunction =
  unlines
    [ "function f():integer; begin f := 1; end;",
      "function f():integer; begin f := 1; end;",
      "begin end."
    ]

invalidSourceFact :: String
invalidSourceFact =
  unlines
    [ "var input:real=0.0;",
      "function factorial(n:integer)  : integer;",
      "var result : integer=1;",
      "begin",
      "  while n > 1 do begin",
      "    result := result * n;",
      "    n := n - 1;",
      "  end;",
      "  factorial := result;",
      "end;",
      "",
      "begin",
      "  while input > 0 do begin",
      "    writeln('Enter number, 0 to exit:');",
      "    readln(input);",
      "    writeln(input, '! = ', factorial(input));",
      "  end;",
      "  writeln('Leaving.');",
      "end."
    ]

unformattedSourceFact :: String
unformattedSourceFact =
  unlines
    [ "var input:integer=1;",
      "function factorial(n:integer)  : integer;",
      "var result : integer=1;",
      "begin",
      "while n > 1 do begin     result := result * n;n := n - 1;    end;",
      "factorial := result;",
      "end;",
      "",
      "begin",
      "while input > 0 do begin",
      "writeln('Enter number, 0 to exit:');",
      "readln(input);",
      "writeln(input, '! = ', factorial(input));",
      "end;",
      "writeln('Leaving.');",
      "end."
    ]

formattedSourceFact :: String
formattedSourceFact =
  unlines
    [ "var",
      "  input : integer = 1;",
      "",
      "function factorial(n : integer) : integer;",
      "var",
      "  result : integer = 1;",
      "begin",
      "  while (n > 1) do",
      "  begin",
      "    result := (result * n);",
      "    n := (n - 1);",
      "  end;",
      "  factorial := result;",
      "end;",
      "",
      "begin",
      "  while (input > 0) do",
      "  begin",
      "    writeln('Enter number, 0 to exit:');",
      "    readln(input);",
      "    writeln(input, '! = ', factorial(input));",
      "  end;",
      "  writeln('Leaving.');",
      "end."
    ]

edslFact :: MonadLang m => Module m
edslFact =
  Module
    { variables =
        [ ("input", Integer, intLit 1)
        ],
      functions =
        [ Function
            { name = "factorial",
              type_ = Integer,
              arguments = [("n", Integer)],
              variables = [("result", Integer, intLit 1)],
              body =
                block
                  [ (while_ (deref "n" .> int 1))
                      ( block
                          [ "result" .= deref "result" .* deref "n",
                            "n" .= deref "n" .- int 1
                          ]
                      ),
                    "factorial" .= deref "result"
                  ]
            }
        ],
      body =
        block
          [ (while_ (deref "input" .> int 0))
              ( block
                  [ writeln [string "Enter number, 0 to exit:"],
                    readln [Variable "input" (Just Integer)],
                    writeln [deref "input", string "! = ", call "factorial" [deref "input"]]
                  ]
              ),
            writeln [string "Leaving."]
          ]
    }
