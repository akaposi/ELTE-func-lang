module MTFHazi where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.State
import Control.Monad

-- Vegyük az alábbi környezetet
data Env = MkEnv {
  isRootUser :: Bool, -- Root-e a felhasználó
  userName   :: String, -- felhasználónév
  sudoers    :: [String], -- azon felhasználónevek, akiknek van admin engedélyük
  users      :: [(String, String)], -- felhasználónevek és jelszók
  pwd        :: String, -- jelenlegi mappa ami nyitva van
  homeDir    :: String  -- a felhasználó munkadirectoryja
                 }

-- READER
-- Definiáljuk az isSudoable függvényt, amely megnézi, hogy a jelen felhasználónak lehet-e admin engedélye.
-- Akkor lehet engedálye, ha jelenleg root felhasználól vagyunk, vagy a sudoers listában benne van a felhasználó neve.
isSudoable :: Reader Env Bool
isSudoable = undefined

-- Definiáljuk a getPassword függvény amely visszaadja a felhasználó jelszavát (ha van)
getPassword :: Reader Env (Maybe String)
getPassword = undefined

-- Definiáljuk a runAs függvényt amely megpróbál egy Readert egy felhasználónként lefuttatni.
-- Az első paraméter a felhasználó név a második a jelszó. Ha nincs ilyen felhasználó vagy a jelszó nem helyes adjunk vissza nothingot
runAs :: String -> String -> Reader Env a -> Reader Env (Maybe a)
runAs = undefined

-- Definiáljuk az inHomeDir függvényt amely egy readert lefuttat úgy, hogy a munkadirectory a felhasználó homedirectoryja legyen
inHomeDir :: Reader Env a -> Reader Env a
inHomeDir = undefined

-- Definiáljuk a defineUsers amely felhasználókat definiál az adott névvel és jelszókkal.
-- Ha egy felhasználó már létezik és jelenleg adminok vagyunk, akkor a nem admin felhasználókat írjuk fölül, egyébként azokat ignoráljuk.
defineUsers :: [(String, String)] -> Reader Env a -> Reader Env a
defineUsers = undefined

-- WRITER
-- Vegyük az alábbi típusokat
type IPAdress = (Int, Int, Int, Int) -- mindegyik szám 0 és 255 között van és egy IP cím egy oktetét jelzi decimálisan, pl 127.0.0.1 = (127, 0, 0, 1)
data Action = LogIn | LogOut | DoMagic deriving (Eq, Show)

-- Definiáljuk az xMagic függvényt, amely a 127.0.0.1-es IP címre loggol X darab DoMagic akciót
xMagic :: Int -> Writer [(IPAdress, Action)] ()
xMagic = undefined

-- Definiáljuk a logAround függvény, amely egy IP címet kap paraméterül, loggol egy belépést az adott IP címmel, lefuttatja a második paraméterben kapott writert
-- majd loggol egy kilépést az adott IP címmel
logAround :: IPAdress -> Writer [(IPAdress, Action)] a -> Writer [(IPAdress, Action)] a
logAround = undefined

-- Definiáljuk a countMagic függvényt, amely megszámolja hogy a paraméterül kapott writerbe melyik IP cím loggolta a legtöbb DoMagic actiont
countMagic :: Writer [(IPAdress, Action)] () -> Writer [(IPAdress, Action)] IPAdress
countMagic = undefined


-- Definiáljuk a trySudo függvényt, amely lefuttatja a paraméterül kapott műveletet, ha a felhasználó admin / benne van a sudoers fileban
-- Ha nincs ne futassuk le a műveletet és írjuk a logba hogy "$X is not in the sudoers file, this incident will be reported"
-- A környezetet bemeneti paraméterként kapjuk
trySudo :: Env -> Writer [String] () -> Writer [String] ()
trySudo = undefined

-- EXCEPT
-- Definiáljuk a safeFactorial függvényt amely rekurzívan elvégzi a faktoriális függvényt. Ha a paraméterül kapott szám < 0 dobjunk errort!
safeFactorial :: Int -> Except String Int
safeFactorial = undefined

-- Definiáljuk a lessSafeFactorial függvényt amely azon értékekre, amelyekre a safeFactorial error-t dobna, adjon vissza -1-et.
-- Ehhez használjuk fel az előző függvényt és a catchError függvényt.
lessSafeFactoriall :: Int -> Except String Int
lessSafeFactoriall = undefined

-- Definiáljuk újra a trySudo függvényt, de loggolás helyett dobjunk errort ha nem admin a felhaszáló
trySudoE :: Env -> Except String a -> Except String a
trySudoE = undefined
