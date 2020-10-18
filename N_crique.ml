(*RICERCA CON PROFONDITÀ LIMITATA A N CON RESULTS*)
let searchp results inizio n (Graph s)=
                let estendi cammino = (*if (List.length cammino) = n then stampalista cammino;  *)
				List.map (function x -> x::cammino)
                                (List.filter (function x -> not (List.mem x cammino)) (s (List.hd cammino)))
                        in let rec search_aux results lista  = match lista with
                                 [] -> results
                                | cammino::rest -> if (List.length cammino) = n then search_aux (results @ [cammino]) rest
                                                   else search_aux results ((estendi cammino) @ rest)
                              in search_aux results [[inizio]];;
(*COMMENTO:
START:
CHIAMA SEARCH AUX:
   controlla  "inizio":
	se vuoto -> results
	altrimenti controlla: SE inizio è una lista lunga N ALLORA aggiunge il cammino ai risultati ed esegue search_aux sulla coda
        	ALTRIMENTI richiama search_aux passandogli results e il risultato della concatenazione tra 
									ESTENDI cammino e la coda della lista "inizio"
	FUNZIONE ESTENDI:
	riceve cammino
	Filtra la lista dei successori dell'ultimo nodo visitato (ovvero List.hd cammino) selezionando solo quelli non ancora visitati nel cammino, 
	e per ogni nodo x trovato richiama search_aux passando x::cammino come "lista"
*)
	(*lista prima chiamato inizio*)
	



(*N-CRIQUE PATH FINDINGS*)
let searchp results inizio n (Graph s)=
                let estendi cammino = (*if (List.length cammino) = n then*) stampalista [inizio];  
				List.map (function x -> x::cammino)
                                (List.filter (function x -> not (List.mem x cammino)) (s (List.hd cammino)))
                        in let rec search_aux results lista  = match lista with
                                 [] -> results
                                | cammino::rest -> if (List.length cammino) = n then if (List.mem inizio (s (List.hd cammino))) 
											then search_aux (results @ [cammino]) rest
										     else search_aux results rest
                                                   else search_aux results ((estendi cammino) @ rest)
                              in search_aux results [[inizio]];;
			


(*CHECK IF N-CRIQUE*)
let rec check inizio n (Graph g) =
	if (g inizio) = [] then raise NotFound 
	else let cammini = (searchp [] inizio n (Graph g)) in if (List.length cammini) >= (n-1) then cammini
		else check (succ inizio) n (Graph g);;
