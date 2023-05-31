import Data.Char (isDigit, digitToInt)

-- Função auxiliar para verificar se um caractere é um operador
ehOperador :: Char -> Bool
ehOperador c = c `elem` ['+', '-', '*', '/']

-- Função auxiliar para executar uma operação aritmética
executarOperacao :: Char -> Float -> Float -> Float
executarOperacao '+' x y = x + y
executarOperacao '-' x y = x - y
executarOperacao '*' x y = x * y
executarOperacao '/' x y = x / y

-- Função principal que avalia a expressão
avalia :: String -> Float
avalia expressao = avaliarOperacoes expressaoSemEspacos [] []
  where
    -- Remove espaços em branco desnecessários
    expressaoSemEspacos = filter (/= ' ') expressao

    -- Função auxiliar para extrair um número da expressão
    extrairNumero :: String -> (Float, String)
    extrairNumero "" = (0.0, "")
    extrairNumero cs =
      let (numStr, resto) = span isDigit cs
       in (read numStr, resto)

    -- Função auxiliar para extrair uma subexpressão entre parênteses
    extrairSubexpressao :: String -> (String, String)
    extrairSubexpressao = extrairSubexpressaoAux 0
      where
        extrairSubexpressaoAux :: Int -> String -> (String, String)
        extrairSubexpressaoAux _ "" = ("", "")
        extrairSubexpressaoAux contador (c:cs)
          | c == '(' =
            let (subexp, resto) = extrairSubexpressaoAux (contador + 1) cs
             in (c : subexp, resto)
          | c == ')' =
            if contador == 0
              then ("", cs)
              else let (subexp, resto) = extrairSubexpressaoAux (contador - 1) cs
                    in (c : subexp, resto)
          | otherwise =
            let (subexp, resto) = extrairSubexpressaoAux contador cs
             in (c : subexp, resto)

    -- Função auxiliar para avaliar as operações
    avaliarOperacoes :: String -> [Float] -> [Char] -> Float
    avaliarOperacoes "" operandos [] = head operandos
    avaliarOperacoes "" operandos (op:ops) = avaliarOperacoes "" (executarOperacao op (head (tail operandos)) (head operandos) : drop 2 operandos) ops
    avaliarOperacoes (c:cs) operandos operadores
      | isDigit c =
        let (numero, resto) = extrairNumero (c:cs)
         in avaliarOperacoes resto (numero : operandos) operadores
      | c == '(' =
        let (subexp, resto) = extrairSubexpressao cs
            resultadoSubexp = avaliarOperacoes subexp [] []
         in avaliarOperacoes resto (resultadoSubexp : operandos) operadores
      | ehOperador c =
        if null operadores || (precedencia c > precedencia (head operadores))
          then avaliarOperacoes cs operandos (c:operadores)
          else let novoOperando = executarOperacao (head operadores) (head (tail operandos)) (head operandos)
                in avaliarOperacoes (c:cs) (novoOperando : drop 2 operandos) (tail operadores)
      | otherwise = error "Caractere inválido na expressão"
      where
        precedencia :: Char -> Int
        precedencia '+' = 1
        precedencia '-' = 1
        precedencia '*' = 2
        precedencia '/' = 2
        precedencia _ = 0
