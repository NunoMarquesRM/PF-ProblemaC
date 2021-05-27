(* Leitura do Input:
    introduz as moedas numa lista,
    quando chega a 0 retorna a lista
*)
let rec read_coins i acc =
  if i == 0 then acc else
    read_coins ( i - 1 ) ( ( Scanf.scanf " %d" ( fun x -> x ) ) :: acc )

(*  Algoritmo gready:
      dá o troco com o numero de moedas mais altas.
      inicia no ponto mais alto até atingir o 0.
      quano atinge o 0 retorna o número de passos efetuados.
*)
let rec gready coins left steps =
if left == 0 then steps else
  match coins with
  | x :: xs ->
    if x > left then gready xs left steps
    else
      gready xs ( left - ( left / x * x  ) ) ( steps + left / x )
  | _ -> 100000

(*  Algoritmo Dynamico:
      A cada iteração tentamos retirar o máximo valor ao troco,
      para tal, usamos cada valor das possiveis moedas e voltamos a
      chamar a função recursiva.
      Ao contrário do algoritmo greedy, o dinamico explora a cada iteração
      as restantes moedas dentro da lista. Desta forma garantimos que
      cenários, por exemplo, troco 6, moedas {1,3,4}, conseguimos encontrar
      o número minimo através de {3,3} e não {4,1,1}
*)
let rec dynamic coins left steps ceiling =
  if steps >= ceiling then 100000 else
  if left == 0 then steps else
    match coins with
    | x :: xs ->
      if x > left then dynamic xs left steps ceiling else
      if left mod x == 0 then steps + left / x else
        let this = dynamic xs ( left - ( left / x * x ) )( steps + left / x ) ceiling in
        let those = dynamic xs left steps ceiling in
        if this < those then this else those
    | _ -> 100000

(*
    Função que procura os casos onde os algoritmos divergem
    Para tal, percorremos um intervalo de número e para cada número
    desse intervalo, comparamos o resultado dos dois algoritmos.
    Caso os resultados divergiam, concluimos que para o dado conjunto de
    moedas os algoritmos não concordam
*)
let rec compare coins i max =
  if i > max then -1 else
  let a = gready coins i 0 in
  let b = dynamic coins i 0 a in
  if a <> b then i else compare coins ( i + 1 ) max

(*
    Função auxiliar usada para encontrar o número limite para procurarmos
    Neste caso, usamos 3x o valor da maior moeda na lista
    O número 1000000 serve meramente para auxiliar a contrução do
    pattern match
*)
let m coins =
  match coins with
  | x :: xs -> x * 3
  | _ -> 1000000

(*  
  Função auxiliar para imprimir o resultado da comparação
  Na procura pela concordância - função compare - caso o algoritmo retorne
  outro número sem ser -1, então conluimos que os algoritmos discordam.
  Em suma, se o resultado do retorno é -1, então estes concordam e imprimimos "YES".
  Na situação oposta, imprimimos o primeiro valor onde os algortimos discordam
*)
let print coins =
  let x = compare coins 1 10000 in
  if x == -1 then print_endline "YES"
  else ( print_int x; print_endline "" )

(*
  Função main. Leitura do número de moedas. 
  Chamada da função da leitura das moedas
  e chamada da função usada para iniciar os cálculos
*)
let _ =
let n = Scanf.scanf " %d" ( fun x -> x ) in
let coins = read_coins n [] in
print coins