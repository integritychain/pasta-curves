{-# LANGUAGE NoImplicitPrelude, Safe #-}

module Constants (pallasPrime, vestaPrime) where

import Prelude (Integer)


-- | pallas field modulus https://neuromancer.sk/std/other/Pallas
pallasPrime :: Integer
pallasPrime = 0x40000000000000000000000000000000224698fc094cf91b992d30ed00000001


-- | vesta field modulus https://neuromancer.sk/std/other/Vesta
vestaPrime :: Integer
vestaPrime = 0x40000000000000000000000000000000224698fc0994a8dd8c46eb2100000001
