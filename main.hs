-- import de libs de trace para debug e para manipulação de char
-- is digit = verifica se o char é um digito | digitToInt tranforma char em um Int

import Debug.Trace (trace)
import Data.Char (isDigit, digitToInt)

-- Função que recebe um char e verifica se esse char está é um elemento da lista de char.

verificaOperadorChar :: Char -> Bool
verificaOperadorChar c = c `elem` ['+', '-', '*', '/']

-- Função que recebe um String e verifica se esse String é um elemento da lista de Strings.

validaOperador :: String -> Bool
validaOperador c = head c `elem` ['+', '-', '*', '/']

-- Função que recebe um simbolo (+ ou / ou - ou *) e dois números e executa a operação

executarOperacaoAritmetica :: String -> Float -> Float -> Float
executarOperacaoAritmetica "+" x y = x + y
executarOperacaoAritmetica "-" x y = x - y
executarOperacaoAritmetica "*" x y = x * y
executarOperacaoAritmetica "/" x y = x / y

-- Função que recebe uma lista de char e transforma ela em um valor Float utilizando o comando "read"

charParaFloat :: [Char] -> Float
charParaFloat str = read str

-- Função que recebe uma String e tenta transformar essa String em um Float utilizando a função reads que pega o string e tenta transformar em uma tupla [(Float, String)]
-- se a tupla resultante for um valor em float e a String vazia, é um número, retorna true, se essa ordem não for cumprida retorna false.

eNumero :: String -> Bool
eNumero str =
  case reads str :: [(Float, String)] of
    [(numero, "")] -> True
    _              -> False

-- Função Recebe uma String e um contador e separa o numero do resto da expressão devolvendo em uma tupla.
-- Exemplo: "2.4+2" = ("2.4","+2")
-- Essa função basicamente remove o numero do incio da expressão e retorna ele separados em uma tupla, impedindo caracteres não permitidos
-- e numeros com mais de dois pontos Ex: 2.22.22

buscaNumero :: String -> Int -> (String, String)
buscaNumero [] _ = ("", "")
buscaNumero (c:cs) cont
  | cont < 2 =
    if isDigit c then
      let (num, rest) = buscaNumero cs cont
      in (c : num, rest)
    else if c == '.' then
      let (num, rest) = buscaNumero cs (cont + 1)
      in (c : num, rest)
    else if verificaOperadorChar c || c == '(' || c == ')' then
      ("", c:cs)
    else
      error "A entrada deve possuir apenas numeros, simbolos de operacoes aritméticas e parênteses."
  | otherwise = error "A entrada não aceita numeros com dois pontos. Exemplo: 2.2.343"


-- Função separa a função entre parênteses do resto da expressão para ser executada
resolveSubexpressao :: [String] -> ([String], [String])
resolveSubexpressao = resolveSubexpressaoAux 0
  where
    resolveSubexpressaoAux :: Int -> [String] -> ([String], [String])
    resolveSubexpressaoAux _ [] = ([], [])
    resolveSubexpressaoAux contador (c:cs)
      | c == "(" =
        let (subexp, resto) = resolveSubexpressaoAux (contador + 1) cs
        in (c : subexp, resto)
      | c == ")" =
        if contador == 0
          then ([], cs)
          else let (subexp, resto) = resolveSubexpressaoAux (contador - 1) cs
                in (c : subexp, resto)
      | otherwise =
        let (subexp, resto) = resolveSubexpressaoAux contador cs
        in (c : subexp, resto)


-- Função que faz a resolução da expressao
-- 1° linha define as entradas e retorna o resultado, sendo em ordem a lista de String da expressão, a lista de numeros e a lista de operadores
-- 2° linha retorna onúmero na cabeça da lista se a lista de expressão = [] e a lista de operadores = 0
-- 3° linha caso a lista de expressão estiver vazio, a função chama ela mesma recursivamente concatenando o resultadoda operação na lista de numeros e 
-- removendo os dois numeros da operação e removendo também o operador utilizando
-- 4° linha, se for numero, concatena na lista de numeros.
-- se for "(" remove as operações entre o parêntese resolve separado na função resolveExpressao com a separação feita em resolveSubexpressao, armazena o resultado em resultadoSubexp
-- e o resultado na função de numeros
-- se for um operador, verifica a prioridade, se a prioridade do operador atual for maior que do operador na cabeça da pilha ou se a pilha de operador estiver vazia, apenas concatena o novo operador da pilha de operadores.
-- caso contrario resolve a operação com os dois primeiros numeros e o primeiro operador das pilhas, concatena o resultado na lista de numeros e chama recursivamentea função com a mesma lista de expressão

resolveExpressao :: [String] -> [Float] -> [String] -> Float
resolveExpressao [] numeros [] = head numeros
resolveExpressao [] numeros (op:ops) = resolveExpressao [] (executarOperacaoAritmetica op (head (tail numeros)) (head numeros) : drop 2 numeros) ops
resolveExpressao (c:cs) numeros operadores
  | eNumero c =
    let numero = charParaFloat c
    in resolveExpressao cs (numero : numeros) operadores
  | c == "(" =
    let (subexp, resto) = resolveSubexpressao cs
        resultadoSubexp = resolveExpressao subexp [] []
    in resolveExpressao resto (resultadoSubexp : numeros) operadores
  | validaOperador c =
    if null operadores || (prioridade c > prioridade (head operadores))
      then resolveExpressao cs numeros (c:operadores)
      else let resultado = executarOperacaoAritmetica (head operadores) (head (tail numeros)) (head numeros)
           in resolveExpressao (c:cs) (resultado : drop 2 numeros) (tail operadores)
  | otherwise = error "Caractere invalido na expressao"
  where
    prioridade :: String -> Int
    prioridade "+" = 1
    prioridade "-" = 1
    prioridade "*" = 2
    prioridade "/" = 2
    prioridade _ = 0

-- Função principal, onde a expressão será calculada
-- filter (/= ' ') expressao : Remove os expaços em branco que houverem na expressão
avalia :: String -> Float
avalia expressao = if inconsistenciasLista exp []
                    then resolveExpressao exp [] []
                    else error "Expressao Invalida"
                   where
                      exp = quebraEntrada (filter (/= ' ') expressao)

inconsistenciasLista :: [String] -> [String] -> Bool
inconsistenciasLista (c:cs) [] = inconsistenciasLista cs [c]
inconsistenciasLista [] _ = True
inconsistenciasLista (c:cs) (h:t)
      | c == "(" && (h == ")" || eNumero h) = False
      | otherwise = inconsistenciasLista cs (c:(h:t))

-- Função que quebra a expressão em uma lista de Strings. Ex: "2+4*(4/2)" = ["2","+","4","*","(","4","/","2",")"]
quebraEntrada :: String -> [String]
quebraEntrada "" = []
quebraEntrada (c:cs)
  | isDigit c =
      let (numero, resto) = buscaNumero (c:cs) 0
      in numero : quebraEntrada resto
  | verificaOperadorChar c = [c] : quebraEntrada cs
  | c == '(' = [c] : quebraEntrada cs
  | c == ')' = [c] : quebraEntrada cs
  | otherwise = error "Caractere invalido na expressao."
