import Control.DeepSeq
import Control.Exception
import System.IO

main = do
  -- Abre o arquivo no modo leitura
  h <- openFile "../file-samples/file.txt" ReadMode
  contents <- hGetContents h

  -- É necessário realizarmos alguma operaçao com a variavel contents para que o arquivo seja lido (antes de fechar).
  -- Portanto forçamos a leitura do arquivo completo com a função force
  evaluate $ force contents

  -- Após efetuar alguma operação com a varaiavel contents, podemos fechar o arquivo
  hClose h
  
  putStrLn $
    unwords $
      map head $
        filter (not . null) $
          map words $
            lines contents

  hClose h
