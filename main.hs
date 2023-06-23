import Data.Char (isDigit, digitToInt)

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

-- Função auxiliar para verificar se um caractere é um operador
ehOperador :: Char -> Bool
ehOperador c = c `elem` ['+', '-', '*', '/']

-- Função auxiliar para executar uma operação aritmética
executarOperacao :: Char -> Float -> Float -> Float
executarOperacao '+' x y = x + y
executarOperacao '-' x y = x - y
executarOperacao '*' x y = x * y
executarOperacao '/' x y = x / y

type Tipo = String
type Lexema = String
type Token = (Tipo,Lexema)

tiposToken x | x == 1 = "NUM"
             | x == 2 = "OP"
             | x == 3 = "ABRE_PAR"
             | x == 4 = "FECHAR_PAR"
             | otherwise = "Error" 


-- avalia:: String -> Float
avalia expressao = tokenizar exp
                    where exp = filter (/=' ') expressao

-- tokenizar :: String -> [Token]
tokenizar (c:cs) | isDigit c = [t] : buscarNum (c:cs)
                        
                 | c `elem` ['+','-','/','*'] = [t] : ("OP",c)
                 | c == '(' = "ABRE_PAR" = [t] : ("ABRE_PAR",'(')
                 | c == ')' = "FECHA_PAR" = [t] : ("FECHAR_PAR",')')
                 | otherwise = error "Caractere" ++ "'" ++ c ++ "'" ++ "Inválido"

buscarNum (n:ns)


-- evalExp :: String -> Float
-- evalExp "" = 0.0
-- evalExp expressao = restoAdd (mult expressao)
-- mult expressao = restoMult (uno expressao)