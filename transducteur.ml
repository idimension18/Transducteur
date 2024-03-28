(* Boite à outils *)
let rec in_list elem = List.fold_left (fun acc x -> x=elem || acc) false 


let list_of_string chaine = List.rev (String.fold_left (fun acc x -> x::acc) [] chaine)
	

(**
	* Ce module permet la représentation de transducteur en OCaml
  * par soucis d'éxécution, le Transducteur doit être déterministe
*)
module Transducteur =
	struct
		type transi_uplet = int * char * char * int
		type t = Transducteur of int * int list * transi_uplet list

		
		(* Boite à outils *)
		(* -------------------- *)

		(* Calcule l'indice maximale du transducteur 
			Cela peut être utile pour rajouter des états *)
		let q_max t = let Transducteur(_, _, transi_list) = t in
			List.fold_left 
				(fun acc x -> 
					let (q1, _, _, q2) = x in 
					if q1 > q2 then
						if q1 > acc then q1
						else acc
					else
						if q2 > acc then q2
						else acc) 
					0 transi_list 

		
		(* Fonctions d'éxécution *)
		(* -------------------------------- *)
		
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
				| t::queue -> let (next_q, r) = mu q t in aux next_q queue (sortie^(Char.escaped r))
			in

			aux debut (list_of_string mot) ""

		
		(* Construction de nouveaux transducteurs *)
		(* ---------------------------------------- *)
		
		(* Concatenation de deux transducteur t1 et t2 *)
		let concac t1 t2 =
			let Transducteur(s1, fin1, transi_list1) = t1 in
			
			(* fonction de type : int -> t -> t
				Afin de concaténer t2 il faut rennomer ces états à partir du max de t1 *)
			let re_indice base t = let Transducteur(s, f, transis) = t in
				Transducteur(s + base, 
					List.fold_right (fun x acc -> (x+base)::acc) f [],
					List.fold_right (fun x acc -> let (q1,l,e,q2) = x in (q1+base,l,e,q2+base)::acc) transis [])
			in

			(* Fonction de type : int list -> int -> transi_uplet list
				On branche la fin de t1 sur le debut de t2 *)
			let branchage fins1 s2 transis2 = 
				List.fold_right
					( fun x acc -> let (q1, l, e, q2) = x in
						if q1 <> s2 then x::acc
						else
							(List.fold_right (fun fin1 acc_fins -> (fin1, l, e, q2)::acc_fins) fins1 []) @ acc )
					transis2 []
			in
			
			let Transducteur(s1, fin1, transis1) = t1 in
			let Transducteur(s2, fin2, transis2) = re_indice ((q_max t1)+1) t2 in
			Transducteur(s1, fin2, transis1 @ (branchage fin1 s2 transis2) )
		
		
		let kleene t = ()

		
		(* Application divers *)
		(* -------------------- *)
		
		(* Fonction de type : t -> t -> string -> string 
			Calculs la composition : (t2 o t1) entree *)
		let compose t1 t2 entree = exec t2 (exec t1 entree)


		(* Calcule l'automate reconnaissant le langage d'entrée *)
		let projection_gauche = ()

		
		(* Calcule l'automate reconnaison le langage de sortie *)
		let projection_droite = ()
	end
