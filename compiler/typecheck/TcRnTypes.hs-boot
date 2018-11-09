module TcRnTypes where

import IOEnv

type TcM = TcRn

type TcRnIf a b = IOEnv (Env a b)

type TcRn = TcRnIf TcGblEnv TcLclEnv

data Env a b
data TcLclEnv
data TcGblEnv
