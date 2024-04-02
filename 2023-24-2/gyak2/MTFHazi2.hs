module MTFHazi2 where

import MTFHazi
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.State
import Control.Monad
import Control.Monad.IO.Class

-- Minden feladat során az alábbi stackben fogunk dolgozni
-- Állapotváltozási környezet, benne egy [(String, String)] típusu értékkel (felhasználónév és jelszó listapár)
-- Olvasási környezet, benne egy (String, String) típusú értékkel (felhasználónév és home directory)
-- Írási környezet, benne egy [String] típusú értékkel (üzenetek logja)
-- Hibakezelési környezet, String típusú hibaüzenetekkel

-- Lehet megkötéseket a feladat során használni, csak itt kell explicit a sorrendet megadni
runStackToIO :: StateT [(String, String)] (ReaderT (String, String) (WriterT [String] (ExceptT String IO))) a -> IO (Either String ((a, [(String, String)]), [String]))
runStackToIO m = runExceptT (runWriterT (runReaderT (runStateT m []) ("root", "/")))

-- Definiálj egy createUser függvényt, amely egy felhasználónevet és egy jelszót beolvas STDIN-ról, majd hozzáadja azt a felhasználók listájához.
-- Definiálj egy adminCheck függvényt, amely megvizsgálja, hogy a jelenlegi felhasználó admin-e. Ha nem, írja ki a logba, hogy "Non-Admin User" és dobjon tetszőleges tartalmú hibaüzenetet.
-- Definiálj egy deleteUser függvényt, amely egy paraméterül kapott nevű felhasználót kitöröl az állapotból. Előfeltételként vizsgáljuk meg, hogy a felhasználó admin-e az előző függvény segítségével. Ha a kitörlendő felhasználó nem létezik, írjuk a logba.
