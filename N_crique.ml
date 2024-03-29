(*grafo esempio: *)
let f = function
 1 -> [2;5]
|2 -> [1;3;5]
|3 -> [2;4;6]
|4 -> [3]
|5 -> [1;2;6]
|6 -> [3;5;7]
|7 -> [6]
|_ -> [];;

(*tipo grafo*)
type ’a graph = Graph of (’a -> ’a list)

let g = Graph f

(*TROVA ELEMENTO E RESTITUISCE INDICE, se non lo trova ritorna -1 *)
let rec func x lst c = match lst with
    | [] -> -1
    | hd::tl -> if (hd=x) then c else func x tl (c+1)
let trova_i x lst = func x lst 0


(*sostituisce elemento di indice pos con a*) 
let sostituisci l pos a  = List.mapi (fun i x -> if i = pos then a else x) l


let dfs_cricca risultati cont inizio n (Graph g)=
        let estendi cammino =
          List.map (function x -> x::cammino)
            (List.filter (function x -> (List.mem x (g (inizio) ) && not(List.mem x cammino)) && (x > inizio)) (g (List.hd cammino)))
                 in let rec search_aux risultati cont lista  = match lista with
                     [] -> []
                     | cammino::rest ->  if (List.length cammino) = n then
                                                  let cammino_sort = (List.sort compare cammino) in
                                                          if n = 2 then [cammino_sort]
                                                          else
                                                          let indice = trova_i cammino_sort risultati in
                                                                  if indice > -1 then
                                                                      let nuovo_cont = ((List.nth cont indice) + 1) in
                                                                          if nuovo_cont = n-1 then [cammino_sort]
                                                                          else search_aux risultati (sostituisci cont indice nuovo_cont) rest
                                                                  else search_aux (risultati @ [cammino_sort]) (cont @ [1]) rest
                                       else search_aux risultati cont ((estendi cammino) @ rest)
         in search_aux risultati cont [[inizio]];;

(*COMMENTO DFS_CRICCA:
START:
CHIAMA SEARCH AUX:
   controlla  "[[inizio]]":
	se vuoto -> []
	altrimenti controlla: SE cammino è una lista lunga N ALLORA controlla 					
						SE la combinazione dei nodi del cammino (cammino_sort) è gia presente nella lista risultati allora 
							incrementa il valore di cont corrispondente e se diventa uguale a n-1, ritorna cammino_sort
						ALTRIMENTI aggiungi cammino_sort risultati, e aggiungi "1" come nuovo elemento in cont ed esegue search_aux sulla coda
       		      ALTRIMENTI richiama search_aux passandogli results e il risultato della concatenazione tra ESTENDI cammino e la coda di "lista"

	FUNZIONE ESTENDI:
	riceve cammino
	Filtra la lista dei successori dell'ultimo nodo visitato (ovvero List.hd cammino) selezionando quelli che soddisfano tutte le seguenti condizioni:
	- non sono ancora stati visitati nel cammino, 
	- sono collegati anche a inizio
	- non sono minori di inizio
	e per ogni nodo x trovato richiama search_aux passando x::cammino come "lista"
*)

exception NotFound;;
(* funzione che richiama ogni volta dfs_cricca per un nuovo vertice di inizio fino a quando non trova una cricca*)
let rec check inizio n (Graph g) =
	if (g inizio) = [] then (print_endline("nessuna cricca della dimensione inserita"); raise NotFound )		(*se il vertice non ha vicini -> notfound*)
	else let cammini = (dfs_cricca [] [] inizio n (Graph g)) in if (List.length cammini) > 0 then (List.hd cammini) (*se dfs_cricca trova una cricca, ritornala *)
		else check (succ inizio) n (Graph g)									(*altrimenti richiama check sul prossimo nodo*)

exception InputNotCorrect
(*funzione main*)
let cricca n (Graph g) =
   if n < 2 then (print_endline("N non puo essere minore di 2"); raise InputNotCorrect)
   else check 1 n (Graph g);;


(*############################################## OLD VERSION ##############################################
(*grafo esempio: *)
let f = function
 1 -> [2;5]
|2 -> [1;3;5]
|3 -> [2;4;6]
|4 -> [3]
|5 -> [1;2;6]
|6 -> [3;5;7]
|7 -> [6]
|_ -> [];;


(*tipo grafo*)
type ’a graph = Graph of (’a -> ’a list)

let g = Graph f

(*TROVA ELEMENTO E RESTITUISCE INDICE, se non lo trova ritorna -1 *)
let rec func x lst c = match lst with
    | [] -> -1
    | hd::tl -> if (hd=x) then c else func x tl (c+1)
let trova_i x lst = func x lst 0


(*sostituisce elemento di indice pos con a*) 
let sostituisci l pos a  = List.mapi (fun i x -> if i = pos then a else x) l

(*funzione che tramite ricerca in profondità controlla se esiste una cricca di grandezza n partendo dal vertice "inizio" *)
(*risultati: lista combinazioni di vertici candidate a essere una cricca, cont: lista del numero di occorrenze di ciascun elemento di "risultati" *)
let dfs_cricca risultati cont inizio n (Graph g)=
    let estendi cammino = 
	List.map (function x -> x::cammino)
        (List.filter (function x -> not (List.mem x cammino)) (g (List.hd cammino)))
             in let rec search_aux risultati cont lista  = match lista with
                 [] -> []
                 | cammino::rest -> if (List.length cammino) = n then 
		 	 		if (List.mem inizio (g (List.hd cammino))) then 
						let cammino_sort = (List.sort compare cammino) in 
							if n = 2 then [cammino_sort]
	    			     			else let indice = trova_i cammino_sort risultati in 
								if indice > -1 then 
	 				    			    let nuovo_cont = ((List.nth cont indice) + 1) in 
									if nuovo_cont = n-1 then [cammino_sort]
					   	 			else search_aux risultati (sostituisci cont indice nuovo_cont) rest
								else search_aux (risultati @ [cammino_sort]) (cont @ [1]) rest
		            		 else search_aux risultati cont rest
                       		     else search_aux risultati cont ((estendi cammino) @ rest)
     in search_aux risultati cont [[inizio]]
	      
(*COMMENTO DFS_CRICCA:
START:
CHIAMA SEARCH AUX:
   controlla  "[[inizio]]":
	se vuoto -> []
	altrimenti controlla: SE cammino è una lista lunga N ALLORA controlla 
					SE l'ultimo nodo è collegato al nodo iniziale del cammino, ordina la lista dei nodi del cammino (come "cammino_sort"), poi:
						SE la combinazione dei nodi del cammino (cammino_sort) è gia presente nella lista risultati allora 
							incrementa il valore di cont corrispondente e se diventa uguale a n-1, ritorna cammino_sort
						ALTRIMENTI aggiungi cammino_sort risultati, e aggiungi "1" come nuovo elemento in cont ed esegue search_aux sulla coda
					ALTRIMENTI non lo aggiunge
        		      ALTRIMENTI richiama search_aux passandogli results e il risultato della concatenazione tra ESTENDI cammino e la coda di "lista"

	FUNZIONE ESTENDI:
	riceve cammino
	Filtra la lista dei successori dell'ultimo nodo visitato (ovvero List.hd cammino) selezionando solo quelli non ancora visitati nel cammino, 
	e per ogni nodo x trovato richiama search_aux passando x::cammino come "lista"
*)

exception NotFound;;
(* funzione che richiama ogni volta dfs_cricca per un nuovo vertice di inizio fino a quando non trova una cricca*)
let rec check inizio n (Graph g) =
	if (g inizio) = [] then (print_endline("nessuna cricca della dimensione inserita"); raise NotFound )		(*se il vertice non ha vicini -> notfound*)
	else let cammini = (dfs_cricca [] [] inizio n (Graph g)) in if (List.length cammini) > 0 then (List.hd cammini) (*se dfs_cricca trova una cricca, ritornala *)
		else check (succ inizio) n (Graph g)							(*altrimenti richiama check sul prossimo nodo*)

exception InputNotCorrect
(*funzione main*)
let cricca n (Graph g) =
   if n < 2 then (print_endline("N non puo essere minore di 2"); raise InputNotCorrect)
   else check 1 n (Graph g);;


(*OLD VERSION*
(*N-CRIQUE PATHS FINDER*)
let searchp results inizio n (Graph s)=
                let estendi cammino = 
				List.map (function x -> x::cammino)
                                (List.filter (function x -> not (List.mem x cammino)) (s (List.hd cammino)))
                        in let rec search_aux results lista  = match lista with
                                 [] -> results
                                | cammino::rest -> if (List.length cammino) = n then if (List.mem inizio (s (List.hd cammino))) 
											then search_aux (results @ [cammino]) rest
										     else search_aux results rest
                                                   else search_aux results ((estendi cammino) @ rest)
                              in search_aux results [[inizio]];;

(*COMMENTO:
START:
CHIAMA SEARCH AUX:
   controlla  "inizio":
	se vuoto -> []
	altrimenti controlla: SE inizio è una lista lunga N ALLORA controlla SE l'ultimo nodo è collegato al nodo iniziale del cammino:
											 se SÌ, aggiunge il cammino ai risultati ed esegue search_aux sulla coda
											 ALTRIMENTI non lo aggiunge
        	ALTRIMENTI richiama search_aux passandogli results e il risultato della concatenazione tra ESTENDI cammino e la coda della lista "inizio"

	FUNZIONE ESTENDI:
	riceve cammino
	Filtra la lista dei successori dell'ultimo nodo visitato (ovvero List.hd cammino) selezionando solo quelli non ancora visitati nel cammino, 
	e per ogni nodo x trovato richiama search_aux passando x::cammino come "lista"
*)


exception NotFound;;

let rec check inizio n (Graph g) =
	if (g inizio) = [] then (print_endline("nessuna cricca"); raise NotFound )
	else let cammini = (searchp [] inizio n (Graph g)) in if (List.length cammini) >= (n-1) then (List.hd cammini)
		else check (succ inizio) n (Graph g);;

let cricca n (Graph g) =
   check 1 n (Graph g);;
*)

(*
(*DFS CON PROFONDITÀ LIMITATA A N CON RISULTATI*)
let searchp_base risultati inizio n (Graph s)=
                let estendi cammino = (*if (List.length cammino) = n then stampalista cammino;  *)
				List.map (function x -> x::cammino)
                                (List.filter (function x -> not (List.mem x cammino)) (s (List.hd cammino)))
                        in let rec search_aux risultati lista  = match lista with
                                 [] -> risultati
                                | cammino::rest -> if (List.length cammino) = n then search_aux (risultati @ [cammino]) rest
                                                   else search_aux risultati ((estendi cammino) @ rest)
                              in search_aux risultati [[inizio]];;
*)
*)
