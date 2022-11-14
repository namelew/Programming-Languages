data Cliente = OrgGov String
            |  Empresa String Integer String String
            |  Individuo Pessoa Bool
            deriving Show

data Pessoa = Pessoa String String Genero
            deriving Show

data Genero = Masculino | Feminino | Outro
            deriving Show

nomeCliente'' :: Cliente -> String
nomeCliente'' (OrgGov nome)                        = nome
nomeCliente'' (Empresa nome _ _ _)                 = nome
nomeCliente'' (Individuo (Pessoa pn sn _) _) = pn ++ " " ++ sn