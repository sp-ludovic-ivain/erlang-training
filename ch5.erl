-module(ch5).
-export([area/0, char_to_shape/1, get_dimension/2, get_number/1, calculate/3, date_parts/1]).

area() ->
    Answer = io:get_line("R)ectangle, T)riangle, E)llipse > "),
    Value = hd(Answer),
    Shape = char_to_shape(Value),
    case Shape of
        rectangle -> Num = get_dimension("Width", "Height");
        triangle -> Num = get_dimension("Base", "Height");
        ellipse -> Num = get_dimension("Axis maj", "Axis min");
        _ -> Num = {error, "Unknow shape" ++ [Shape]}
    end,
    Area = calculate(Shape, element(1, Num), element(2, Num)),
    Area.


char_to_shape(Var) ->
    case Var of
    $R -> rectangle;
    $r -> rectangle;
    $T -> triangle;
    $t -> triangle;
    $E -> ellipse;
    $e -> ellipse;
    _ -> unknow
    end.

get_number(Prompt) ->
    String = io:get_line("Enter " ++ Prompt ++ " >"),
    {Test, _} = string:to_float(String),
    case Test of
        error -> {Value, _} = string:to_integer(String);
        _ -> Value = Test
    end,
    Value.

get_dimension(Prompt1, Prompt2) ->
    Value1 = get_number(Prompt1),
    Value2 = get_number(Prompt2),
    {Value1, Value2}.

calculate(unknow, _, Err) -> io:format("~s ~n",[Err]);
calculate(_, error, _) -> io:format("Error in first number");
calculate(_, _, error) -> io:format("Error in second number");
calculate(_, X, Y) when X < 0 ; Y < 0 -> io:format('number can not be negative');
calculate(Shape, X, Y) -> geom:area(Shape, X, Y).



date_parts(Date) -> 
    [Y, M, D] = re:split(Date, "[-]", [{return,list}]),
    [element(1, string:to_integer(Y)),
     element(1, string:to_integer(M)),
     element(1, string:to_integer(D))].
