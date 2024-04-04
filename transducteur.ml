(* Boite à outils *)
(* ------------------ *)

let rec in_list elem = List.fold_left (fun acc x -> x=elem || acc) false 


let list_of_string chaine = List.rev (String.fold_left (fun acc x -> (Char.escaped x)::acc) [] chaine)

let string_of_char_list  = List.fold_left (fun acc x -> acc^x) ""
	
	
let est_prefixe pref chaine = 
	let rec aux p c = 
		match (p, c) with
		|  ([], _) -> true
		| (_, []) -> false
		| (t1::q1, t2::q2) ->
			if t1 = t2 then (aux q1 q2)
			else false 
	in
	aux (list_of_string pref) (list_of_string chaine)


let enleve_prefixe pref chaine =
		let rec aux p c = 
		match (p, c) with
		| ([], _) -> string_of_char_list c
		| (_, []) -> ""
		| (t1::q1, t2::q2) ->
			if t1 = t2 then (aux q1 q2)
			else string_of_char_list c
	in
		aux (list_of_string pref) (list_of_string chaine)


(**
	* Ce module permet la représentation de transducteur en OCaml
  * par soucis d'éxécution, le Transducteur doit être déterministe
*)
module Transducteur =
	struct
		type transi_uplet = int * string * string * int
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


		(* fonction de type : int -> t -> t
			On veut rajouter un etat, alors une opération de rennomage est nécéssaire *)
		let re_indice base t = let Transducteur(s, f, transis) = t in
			Transducteur(s + base, 
				List.fold_right (fun x acc -> (x+base)::acc) f [],
				List.fold_right (fun x acc -> let (q1,l,e,q2) = x in (q1+base,l,e,q2+base)::acc) transis [])

			
		(* Fonctions d'éxécution *)
		(* -------------------------------- *)
		
		(* fonction de type : string -> string 
			Réécrit le mot passé en paramêtre et renvoit la réécriture 
			ATTENTION : Elle ne marche que pour les transducteur deterministe *)
		let exec_deterministe transduct mot =
			let Transducteur(debut, fins, transi_uplets) = transduct in
			
			(* Fonction de type : int -> string -> int * string
				Fonction de transition  mu : Q X A -> Q X A 
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


		
		(* Fonction de type : string -> string list
			Renvoit toutes les réécriture possible de l'entrée sous forme d'une liste 
			ATTENTION : Si le transducteur présente des boucles par epsilon transition, 
			l'éxécution ne termine pas *)
		let exec transduct mot =
			let Transducteur(debut, fins, transi_uplets) = transduct in
			
			(* Fonction de type : int -> string -> string -> (int * string * string) list
				Fonction de transition  mu : Q X A -> P(Q X A) 
				Prends un état d'entrée, un entrée, et la réécriture courrante
				puit renvoit les prochains état possibles *)
			let mu_all etat = let (q, entree, sortie) = etat in
				List.fold_left
					(fun acc x -> let (q1, l, e, q2) = x in
						match (q1, l) with
						| (a, b) when a=q && (est_prefixe b entree) -> (q2, enleve_prefixe l entree, sortie^e)::acc
						| _ -> acc)
					[] transi_uplets
			in


			(* Fonction de type : int -> bool 
				Test s'il y a des epsilon transition depuis un état final 
				C'est important pour savoir si l'éxécution termine ou pas *)
			let est_final q =
				List.fold_left 
					(fun acc x -> 
						match x with
						| (q1, l, r, _) when q1 = q -> l <> "" && acc
						| _ -> acc) 
				true transi_uplets
			in

			
			(* Fonction de type : (int * string * string) list -> (int * string * string) list * string list
				Parmis les possiblité, on filtre les etats terminaux
				On retourne aussi les écriture sous forme d'une liste *)
			let filtre_terminaux etats  = 
				List.fold_left 
					(fun acc x -> let (etats_next, sorties) = acc in
						match x with
						| (q1, entree, sortie) when in_list q1 fins && entree = "" -> 
							if est_final q1 then (etats_next, sortie::sorties)
							else (x::etats_next, sortie::sorties)
						| _ -> (x::etats_next, sorties) )		
					([], []) etats
			in

			
			(* Fonction qui parcours l'entrée et tient à jours la sortie *)
			let rec aux etats sorties =
				let next = List.fold_left (fun acc x -> (mu_all x) @ acc) [] etats in
				let (next_filtre, new_sorties) = (filtre_terminaux next) in
				match next_filtre with
				| [] -> sorties @ new_sorties
				| _ -> aux next_filtre new_sorties
			in

			aux [(debut, mot, "")] []
		
		(* Construction de nouveaux transducteurs *)
		(* ---------------------------------------- *)

		(* Renvoit l'union de t1 et t2 *)
		let union t1 t2 = ()
		
		(* Concatenation de deux transducteur t1 et t2 *)
		let concac t1 t2 = 
			
			(* Fonction de type : int list -> int -> transi_uplet list -> transi_uplet
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
		

		(* Passage de t à l'étoile de Kleene *)
		let kleene t = 

			(* Fonction de type : int list -> int -> transi_uplet list -> transi_uplet
				On branche la fin de t sur son début *)
			let branchage fins s transis =
				List.fold_right
					( fun x acc -> let (q1, l, e, q2) = x in
						if q1 <> s then x::acc
						else
							(List.fold_right (fun fin acc_fins -> (fin, l, e, q2)::acc_fins) fins []) @ acc )
					transis []
			in
			
			let Transducteur(s, fins, transis) = t in
			let new_etat = (q_max t)+1 in 
			Transducteur(new_etat, new_etat::fins, (branchage (new_etat::fins) s transis))
			

		
		(* Application divers *)
		(* -------------------- *)
		
		(* Fonction de type : t -> t -> string -> string 
			Calculs la composition : (t2 o t1) entree 
			dans le cas ou t1 et t2 sont deterministe *)
		let compose_deterministe t1 t2 entree = exec_deterministe t2 (exec_deterministe t1 entree)

		
		(* Fonction de type : t -> t -> string -> string list
			Calcul la composition : (t2 o t1) entree *)
		let compose t1 t2 entree = List.fold_left (fun acc x -> (exec t2 x) @ acc ) [] (exec t1 entree)
	end



(* Example : *)
(* ------------------*)

(* Automate d'incrémentation binaire non deterministe 
	Les bit de poids faible sont à droite *)
let incr_bin = Transducteur.Transducteur(0, [1], 
	[(0, "1", "1", 0); (0, "0", "0", 0); (0, "0", "1", 1); (1, "1", "0", 1)])


