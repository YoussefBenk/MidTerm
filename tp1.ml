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
let prime nb = g
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
		in prime1 (nb-1)  
