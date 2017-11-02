(* TD 1 *)

(* Reverse *)
let reverse nb = 
	let rec reverse1 nb result =
		if nb = 0 then 
			result  
		else 
			reverse1 (nb/10) (result*10 + nb mod 10) 
	in reverse1 nb 0;; 