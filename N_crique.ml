(*RICERCA CON PROFONDITÀ LIMITATA A N CON RESULTS*)
let searchp_base results inizio n (Graph s)=
                let estendi cammino = (*if (List.length cammino) = n then stampalista cammino;  *)
				List.map (function x -> x::cammino)
                                (List.filter (function x -> not (List.mem x cammino)) (s (List.hd cammino)))
                        in let rec search_aux results lista  = match lista with
                                 [] -> results
                                | cammino::rest -> if (List.length cammino) = n then search_aux (results @ [cammino]) rest
                                                   else search_aux results ((estendi cammino) @ rest)
                              in search_aux results [[inizio]];;


(*N-CRIQUE PATHS FINDER*)
let searchp results cont inizio n (Graph s)=
    let estendi cammino = 
	List.map (function x -> x::cammino)
        (List.filter (function x -> not (List.mem x cammino)) (s (List.hd cammino)))
             in let rec search_aux results cont lista  = match lista with
                 [] -> []
                 | cammino::rest -> if (List.length cammino) = n then if (List.mem inizio (s (List.hd cammino))) 
									then if n = 2 then []@[cammino]
									else let cammino_sort = (List.sort compare cammino) in 
	    								     let indice = find cammino_sort results in 
											if indice > -1 then 
							       				    let nuovo_cont = ((List.nth cont indice) + 1) in if nuovo_cont = n-1 then ([] @ [cammino_sort])
											   						     else search_aux results (replace cont indice nuovo_cont) rest
						          				(* else if n=2 then []@[cammino_sort ]*)
												else search_aux (results @ [cammino_sort]) (cont @ [1]) rest
									else search_aux results cont rest
                                    else search_aux results cont ((estendi cammino) @ rest)
              in search_aux results cont [[inizio]];;
	      
exception NotFound;;
let rec check inizio n (Graph g) =
	if (g inizio) = [] then (print_endline("nessuna cricca della dimensione inserita"); raise NotFound )
	else let cammini = (searchp [] [] inizio n (Graph g)) in if (List.length cammini) > 0 then (cammini)
		else check (succ inizio) n (Graph g);;

let cricca n (Graph g) =
   check 1 n (Graph g);;


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
	se vuoto -> results
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
