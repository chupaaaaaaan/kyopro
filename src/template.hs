{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import My.IO

main :: IO ()
main = do
    n <- int1
    as <- intlist
    xys <- int2list n

    return ()
