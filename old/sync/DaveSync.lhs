> import Network.Netclock.Client
> import Sound.OpenSoundControl
> import Sound.OSC.FD

> --daveip = "127.0.0.1";
> daveip = "192.168.0.3";
> daveport = 4000
> adeip = "10.0.0.3";
> adeport = 1777;
> --daveip = "158.223.59.96";

> tpb = 1

> main :: IO ()
> main = do dave <- openUDP daveip daveport
>           clocked "davesync" "127.0.0.1" "127.0.0.1" tpb $ onTick dave

> onTick :: UDP -> BpsChange -> Int -> IO ()
> onTick dave current ticks = 
>     do putStrLn "tickdave"
>        let m = Message "/sync" [Int 8, Float ((changeBps current) * 60)]
>        sendOSC dave m

> onTickAde :: UDP -> BpsChange -> Int -> IO ()
> onTickAde ade current ticks = 
>     do let n = Message "/PureEvents/Beat" [Int ticks]
>        sendOSC ade n
