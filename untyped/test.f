/* Examples for testing */

/* for arith testing */

true;
if false then true else false; 

0; 
succ (pred 0);
iszero (pred (succ (succ 0))); 


/* for untyped lambda calculus */

lambda x. x;
(lambda x. x) (lambda x. x x);

x/;

x;
lambda x. x;
(lambda x. x) (lambda x. x x);

w/;
a/;

(lambda b.w (lambda a.b a)) (lambda b.b a);
