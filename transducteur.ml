(**
  * Afin de pouvoir implémenter Transducteur/Automate
  * Il doivent être déterministe
*)

module Transducteur =
	struct
		type etat = Etat of string * string * etat
		type t = Transducteur of etat   (* Etat Entrant *)
		let exec transduct mot =
			let Transducteur(debut) = transduct in

			let aux deb entree sortie =
				let Etat(_, _, liste) = deb in
				match liste with
				| [] -> failwith " pas possible "
				| t :: q -> truc

			in

			aux debut mot ""


	end


module Automate =
	struct
		type 'a transition = ('a * string * 'a)
		type 'a t = Automate of ('a * ('a transition) list)
	end

