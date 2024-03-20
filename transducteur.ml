(* Boite à outils *)
let rec in_list elem = List.fold_left (fun acc x -> x=elem || acc) false 


let list_of_string chaine = List.rev (String.fold_left (fun acc x -> x::acc) [] chaine)
	

(**
	* Ce module permet la représentation de transducteur en OCaml
  * par soucis d'éxécution, le Transducteur doit être déterministe
*)
module Transducteur =
	struct
		type transi_uplet = int * char * string * int
		type t = Transducteur of int * int list * transi_uplet list

		(* fonction de type : string -> string 
			Réécrit le mot passé en paramêtre et renvoit la réécriture *)
		let exec transduct mot =
			let Transducteur(debut, fins, transi_uplets) = transduct in

			(* Fonction de transition  mu : Q X A -> Q X A 
				Prends un état d'entrée et une lettre 
				Renvoit l'état suivant (si existe) et la réécriture *)
			let mu q a = 
				let rec aux reste = 
					match reste with 
					| [] -> failwith "Mot non reconnu !"   (* La transition n'existe pas donc : erreur ! *)
					| t::queue -> let (q1, l1, r, q2) = t in 
						if (q, a) = (q1, l1) then (q2, r)
						else aux queue
				in 
					aux transi_uplets
			in 

			(* Fonction qui parcours l'entrée et tient à jours la sortie *)
			let rec aux q entree sortie = 
				match entree with 
				| [] -> if in_list q fins then sortie else failwith "Mot non reconnu !"
				| t::queue -> let (next_q, r) = mu q t in aux next_q queue (sortie^r)
			in

			aux debut (list_of_string mot) ""
	end

(*
module Automate =
	struct
		type 'a transition = ('a * string * 'a)
		type 'a t = Automate of ('a * ('a transition) list)
	end
*) 
