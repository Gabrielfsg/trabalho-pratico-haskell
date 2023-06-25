import Data.Char (isDigit, digitToInt)

type Tipo = String
type Lexema = String
type Token = (Tipo,Lexema)

avalia:: String -> Float 
avalia "" = error "Entre com uma expressão matématica válida."
avalia expressao = let  
                      exp =  filter (/=' ') expressao
                      exp'=  tokenizar exp
                      (res,_) = evalExp exp'
                   in
                      praFloat res

-- Analisador Léxico
tokenizar :: String -> [Token]
tokenizar [] = []
tokenizar (c:cs) | isDigit c = 
                    let (numero,resto) = buscarNum (c:cs) 0
                    in ("NUM",numero) : tokenizar resto
                 | c `elem` ['+','-','/','*'] = ("OP",[c]) : tokenizar cs
                 | c == '(' = ("ABRE_PAR","(") : tokenizar cs
                 | c == ')' = ("FECHA_PAR",")") : tokenizar cs
                 | otherwise = ("Error",[c]) : tokenizar cs

buscarNum:: String -> Int -> (String,String)
buscarNum [] _ = ("",[])  
buscarNum (n:ns) cont | isDigit n = let (num,resto) = buscarNum ns cont
                                    in (n: num,resto) 
                      | n == '.' && cont < 1 = let (num,resto) = buscarNum ns (cont + 1)
                                               in (n: num,resto)
                      | otherwise = ("",n:ns)  

-- Analisador Sintático
{-   
    exp -> mult restoAdd
    restoAdd -> '+' mult restoAdd 
                | '-' mult restoAdd | lambda
    mult -> uno restoMult
    restoMult -> '*' uno restoMult
                |  '/' uno restoMult 
                | lambda
    uno -> '+' uno | '-' uno | fator
    fator -> num  | '(' exp ')'
-}


evalExp :: [Token] -> (String,[Token])
evalExp [] = ("",[])
evalExp tokens = let  
                    (y,tokens') = mult tokens
                    (y',tokens'') = restoAdd y tokens' 
                 in 
                    (y',tokens'')

restoAdd :: (String) -> [Token] -> (String,[Token])
restoAdd res [] = (res,[])
restoAdd res tokens@(token:_) =
                            let 
                                (tipo,lex) = token
                            in if tipo == "OP" && (lex == "+" || lex == "-") then 
                                    let 
                                        tokens' = consome tokens "OP"
                                        (y,tokens'') = mult tokens'
                                        (y',tokens''') = restoAdd y tokens''
                                    in executarOperacao lex res y' tokens'''  
                                else
                                    (res,tokens)
                       
                   
mult :: [Token] -> (String,[Token])
mult [] = ("",[])              
mult tokens@(token: _) = let (res,tokens') = (uno tokens)
                             (res',tokens'') = restoMult res tokens'
                         in (res',tokens'')
                    
restoMult :: String -> [Token] -> (String,[Token])
restoMult res [] = (res,[])
restoMult res tokens@(token:tokens') | 
                                tipo == "OP"  && (lex == "*" || lex == "/")= 
                                    let 
                                        tokens'' = consome tokens "OP"
                                        (y,tokens''') = uno tokens''
                                        (y',tokens'''') = restoMult y tokens'''
                                    in executarOperacao lex res y' tokens''''
                                | otherwise = (res,tokens)
                                where 
                                    (tipo,lex) = token
                        
                                    
                        
uno :: [Token] -> (String,[Token])
uno tokens = if tipo == "OP" then
                        let
                           (numCru,tokens') = uno tokens
                           num = numeroComSinal lex numCru 
                        in 
                           (num,tokens')
                      else
                        fator tokens
             where
                (tipo,lex) = head tokens

                        
fator :: [Token] -> (String,[Token])
fator tokens = if tipo == "NUM" then
                          let
                            num = lex
                            tokens' = consome tokens "NUM"
                          in 
                            (num,tokens') 
                        else  
                          let 
                            (num,tokens') = evalExp (consome tokens "ABRE_PAR")
                            tokens'' = consome tokens' "FECHA_PAR"
                          in 
                            (num,tokens'')
                where
                    (tipo,lex) = head tokens

-- Consome um token baseado em um token que se espera consumir e retorna o resto
consome:: [Token] -> Tipo -> [Token]
consome [] _ = []
consome ((tipo,lex):ts) tipoEsperado = if tipo == tipoEsperado then  
                                          ts 
                                       else if tipoEsperado == "FECHA_PAR" then
                                          removeTokensSubExp ts 
                                       else if tipo == "Error" then
                                          error ("Caractere inválido: " ++ lex)
                                       else  
                                          error ("Esperado {" ++ tipoEsperado ++ "} e foi recebido {" ++ tipo ++ "}")  

-- "Caractere " ++ lex ++ " é inválido"
--  "Esperado {" : tipoEsperado : "} e foi recebido: " ++ tipo 
-- Remove os tokens de uma expressão em parentesis
removeTokensSubExp ((tipo,lex):ts) | lex == ")" = ts
                                   | otherwise = removeTokensSubExp ts

-- Função auxiliar para executar uma operação aritmética
executarOperacao :: String -> String -> String -> [Token] -> (String,[Token])
executarOperacao "+" x y tokens = (show (praFloat x + praFloat y),tokens)
executarOperacao "-" x y tokens = (show (praFloat x - praFloat y),tokens)
executarOperacao "*" x y tokens = (show (praFloat x * praFloat y),tokens)
executarOperacao "/" x y tokens = (show (praFloat x / praFloat y),tokens)

-- Função retorna um numero como positivo ou negativo
numeroComSinal:: String -> String -> String
numeroComSinal "+" x = show (praFloat x)
numeroComSinal "-" x = show (-praFloat x)

praFloat :: String -> Float
praFloat str = read str :: Float



