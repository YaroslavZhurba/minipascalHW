var
  input : integer = 1;

function factorial(n : integer) : integer;
var
result : integer = 1;
begin
while (n > 1) do
begin
result := (result * n); n := (n - 1); end;factorial := result;
end;

begin
while (input > 0) do
begin
writeln('Enter number, 0 to exit:');
readln(input);
writeln(input, '! = ', factorial(input));   end;
writeln('Leaving.');          end.
