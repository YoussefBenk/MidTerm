(* TD 1 *)

(* Reverse *)
let reverse nb = 
	let rec reverse1 nb result =
		if nb = 0 then 
			result  
		else 
			reverse1 (nb/10) (result*10 + nb mod 10) 
	in reverse1 nb 0;; 

(* Prime *)
let prime nb = 
	if nb < 2 then 
		invalid_arg "Erreur 1 : Le nombre doit etre superieur a 2"
	else
		let rec prime1 n =
			if n = 1 then 
				true
			else 
				if ((nb mod n) = 0) then 
					false 
				else prime1 (n-1) 
		in prime1 (nb-1) ;; 

(* Listes *)

(* Longueur *)
let rec length = function 
	| [] 	-> 0
	| _::r 	-> 1 + length r ;; 

(* Count *)
let rec count value liste = 
	match liste with 
		| [] 	-> 0 
		| t::r 	-> (if t = value then 1 else 0) + (count value r) ;;    
	
(* Renvoie le ieme element *)
let rec nth position liste = 
	match (position, liste) with
		| (1, t::_)	-> t 
		| (_, []) 	-> invalid_arg "Erreur 2 : Liste trop courte"
		| (n, _::r) -> (nth (n-1) r) ;; 

(* Renvoie la position d'un element *)
let rec search_pos value liste = 
	match liste with 
		| [] 					-> invalid_arg "Erreur 3 : L'element ne fait pas partie de la liste"
		| t::_  when t = value	-> 1 
		| _::r 					-> 1 + search_pos value r ;;  

(* Cree une liste *)
let rec init_list n value = 
	match n with 
		| 0 -> []
		| _ -> value::(init_list (n-1) value) ;; 