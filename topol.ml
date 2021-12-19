(* SORTOWANIE TOPOLOGICZNE *)
(*
    Autor: Florian Ficek
    Code review: Wojtek Rzepliński
*)
open List;;
open PMap;;

exception Cykliczne;;

(* 
    prosta funkcja tworząca mapę (graf) dla listy
    każdemu wierzchołkowi pryzpisana jest lista jego sąsiadów
*)
let create_map l = 
    let rec helper l m = 
        match l with
        [] -> m
        |(v,maps)::t ->  helper t (PMap.add v maps m)
    in helper l PMap.empty
;;

let topol l = 
    let visited = ref PMap.empty
    (* 
        mapa mówiący czy wierzchołek był odwiedzony lub dodany
        visited - 0
        added - 1
    *)
    and tsort = ref [] (* wynikowa lista *)
    and graph = ref (create_map l) in
    (* dfs - funkcja przechodząca po całym grafie *)
    let rec dfs w = 
        let is_visited = PMap.mem w !visited in (* sprawdzenie, czy wierzchołek odwiedzony *)
        let check = if is_visited then PMap.find w !visited else 2 (* jeśli odwiedzony to odczytujemy wartość, inaczej wpisujemy 2, które niema znaczenia dla działania programu *)
        in
        match is_visited,check with
        |(true, 0) -> raise Cykliczne
        |(_,_) -> if not (PMap.mem w !visited) then
            (
            visited := PMap.add w 0 !visited; (* przypisanie wierzchołka jako odwiedzonego *)
            if mem w !graph then (* sprawdzenie czy wierczhołek znajduje się w mapie, jeśli tak, rekurencja dfs dla jego sąsiadów *)
                List.iter dfs (PMap.find w !graph);
            visited := PMap.add w 1 !visited; (* przypisanie wierzchołka jako dodanego *)
            tsort := w::(!tsort);
            )
    in
    (
    List.iter (fun (w, _) -> dfs w) l;
    !tsort;
    )
;;

