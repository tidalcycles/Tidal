-- run the following in ghci..

import Sound.OSC.FD as O

-- gets a randomly allocated port, might be useful..
-- udp <- udpServer "127.0.0.1" 0
-- udpPort udp

udp <- udpServer "127.0.0.1" 6012

r <- openUDP  "127.0.0.1" 6011

-- execute an arbitrary statement
sendMessage r $ Message "/eval" [string "return 10"]
m <- recvMessage udp
m

-- evaluate a definition
sendMessage r $ Message "/eval" [string "let x = 10"]
m <- recvMessage udp
m

-- evaluate a binding statement
sendMessage r $ Message "/eval" [string "y <- return 1"]
m <- recvMessage udp
m

-- evaluate a tidal statment
sendMessage r $ Message "/eval" [string "d1 $ s \"bd\" # n x"]
m <- recvMessage udp
m

-- error
sendMessage r $ Message "/eval" [string "d1 $ suond \"bd\""]
m <- recvMessage udp
m

-- ask the type of an expression
sendMessage r $ Message "/type" [string "s \"bd\""]
m <- recvMessage udp
m

sendMessage r $ Message "/ping" []
m <- recvMessage udp
m

-- receive cps values
sendMessage r $ Message "/cps" []
m <- recvMessage udp
m
