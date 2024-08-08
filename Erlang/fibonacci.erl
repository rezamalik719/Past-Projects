-module(fibonacci).
-export([fibonacci/0,join/1, start/0]).

fibonacci() -> 
  receive
    {Number, Customer} when Number > 1 ->
       NewCust = spawn(fibonacci,join,[Customer]),
       P = spawn(fibonacci,fibonacci,[]),
       Q = spawn(fibonacci,fibonacci,[]),
       P!{Number - 1,NewCust},
       Q!{Number - 2,NewCust};
    {Number, Customer} -> 
       Customer ! Number
  end,
  fibonacci().

join(Customer) -> 
  receive V1 -> receive V2 ->
   Customer ! V1+V2 
  end end.


start() ->
  F = spawn(fibonacci, fibonacci, []),
  F ! {10,self()}.
