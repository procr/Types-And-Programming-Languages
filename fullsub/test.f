/* Examples for testing */

true;
1;


a: Bool;
b: Nat;
c: Bool -> Nat;

c a;

lambda x:Bool. c a;

lambda x:Bool. x;

(lambda x:Bool->Bool. if x false then true else false) (lambda x:Bool. if x then
false else true);

succ (pred 0);
iszero (pred (succ (succ 0)));

succ (1);
succ (0);
pred (0);
iszero (0);
iszero (1);


 lambda x:Top. x;
  (lambda x:Top. x) (lambda x:Top. x);
 (lambda x:Top->Top. x) (lambda x:Top. x);
 

(lambda r:{x:Top->Top}. r.x r.x) 
  {x=lambda z:Top.z, y=lambda z:Top.z}; 


{x=true, y=false}; 
{x=true, y=false}.x;
{true, false}; 
{true, false}.1; 


if true then {x=true,y=false,a=false} else {x=false,y=false,a=false};

lambda x:Bool. x;
(lambda x:Bool->Bool. if x false then true else false) 
  (lambda x:Bool. if x then false else true); 

lambda x:Nat. succ x;
(lambda x:Nat. succ (succ x)) (succ 0); 

