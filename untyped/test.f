/* Examples for testing */

true;
1;


a: Bool;
b: Nat;
c: Bool -> Nat;

c a;

lambda x:Bool. c a;

lambda x:Bool. x;

(lambda x:Bool->Bool. if x false then true else false) (lambda x:Bool. if x then false else true);

succ (pred 0);
iszero (pred (succ (succ 0)));

succ (1);
succ (0);
pred (0);
iszero (0);
iszero (1);

succ (false);
pred (true);
iszero (true);
