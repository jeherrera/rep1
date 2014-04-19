open Async.Std

let fork d f1 f2 = 
  ignore (Deferred.both (d>>= f1) (d>>=f2))

let rec nyc listOfD list2bd =( 
  match listOfD with
  |hd::tl ->  (hd >>= (fun x -> nyc tl (x::list2bd)))
  |[] -> (return list2bd))

let deferred_map l f =
  let rec helper lst1 acc =
  	match lst1 with
  	|h::t -> h >>= (fun a-> helper t (a::acc))
  	|[] -> return acc in
  helper (List.map f l) [] 