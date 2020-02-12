{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Sudoku.Gui where
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (catch)
import Control.Monad (forM_, when)
import Data.GI.Base
import Data.GI.Base.GError (gerrorMessage, GError(..))
import Data.IORef
import Data.Maybe (fromJust)
import GI.Gtk.Enums
import GI.Gtk.Objects.Box
import GI.Gtk.Objects.Builder
import GI.Gtk.Objects.Button
import GI.Gtk.Objects.CssProvider
import GI.Gtk.Objects.Grid
import GI.Gtk.Objects.Image
import GI.Gtk.Objects.Overlay
import GI.Gtk.Objects.Popover
import GI.Gtk.Objects.StyleContext
import GI.Gtk.Objects.Window
import GI.Pango.Objects.FontMap
import GI.Pango.Structs.FontDescription
import Paths_sudokuhs
import Sudoku.Internal.Bindings
import Sudoku.Internal.Bridge
import Sudoku.Internal.Sudoku
import qualified Data.Text as T
import qualified GI.Gtk.Functions as Gtk (main, init, mainQuit)

-- DEFS ------------------------------------------------------------------------

data Env = Env { lastCell :: IORef Button
               , pulse    :: Int
               , cssPath  :: T.Text
               , cssPrio  :: Int
               , uiPath   :: T.Text
               , fontPath :: T.Text
               , logoPath :: T.Text }

data App = App { keypad :: Popover
               , board  :: Grid
               , window :: Window }

data Color = Red | Green -- colors must be supported in CSS

type Font = (FontMap, FontDescription)

dfltEnv :: IO Env
dfltEnv = do
    lc <- newIORef undefined :: IO (IORef Button)
    cp <- T.pack <$> getDataFileName "ui/gui.css"
    up <- T.pack <$> getDataFileName "ui/app.ui"
    ip <- T.pack <$> getDataFileName "ui/sudokuicons.ttf"
    lp <- T.pack <$> getDataFileName "ui/sudoku.png"
    return $ Env
        { lastCell = lc
        , pulse    = 350000
        , cssPath  = cp
        , cssPrio  = 600 -- css priority
        , uiPath   = up
        , fontPath = ip
        , logoPath = lp }

-- Helper Funcs ----------------------------------------------------------------

-- | Applies app.ui to app window
applyCss :: T.Text -> Int -> Window -> IO ()
applyCss fp p win = do
    screen <- windowGetScreen win
    css <- cssProviderNew
    cssProviderLoadFromPath css fp
    styleContextAddProviderForScreen screen css (fromIntegral p)

-- | Construct GObject type from builder
unsafeBuildObj :: (GObject o)
    => (ManagedPtr o -> o)
    -> Builder
    -> T.Text
    -> IO o
unsafeBuildObj t builder wId = builderGetObject builder wId
    >>= unsafeCastTo t . fromJust

-- | Add Sudoku.ttf to target button
addBtnFont :: Font -> Button -> IO ()
addBtnFont fnt btn = #createPangoContext btn >>= \x -> do
    #loadFont (fst fnt) x $ snd fnt
    return ()

-- | Map an action over each button in the Sudoku table, discarding the result
traverseTable :: Grid ->  [Point] -> (Button -> IO ()) -> IO ()
traverseTable g ps f = do
    forM_ ps \(fromIntegral -> i, fromIntegral -> j) -> do
        cell <- #getChildAt g j i >>= unsafeCastTo Button . fromJust
        f cell

-- | Flash Color onto the cells in ps for pulse duration
flashColor :: Int -> Grid -> [Point] -> Color -> IO ()
flashColor pulse' g ps c = traverseTable g ps \x -> do
    style <- #getStyleContext x
    #addClass style $ sel c
    forkIO $ threadDelay pulse' >> #removeClass style (sel c)
    return ()
    where
        sel = \case
            Red -> "red"
            Green -> "green"

-- Build UI --------------------------------------------------------------------

-- | Install Sudoku.ttf to current app isntance
buildFont :: T.Text -> IO Font
buildFont fp = do
    pass <- currentAppFontAddFile fp
    when (not pass) $ error "err adding font file"
    fontMap <- carioFontMapGetDefault
    fdesc <- fontDescriptionFromString "Sudoku"
    return (fontMap, fdesc)

-- | Constructs the Sudoku table
buildBoard :: IORef Button -> Builder -> IO Grid
buildBoard lcRef b = do
    kp <- unsafeBuildObj Popover b "keypad"
    board' <- unsafeBuildObj Grid b "board"
    forM_ cSpace \(i, j) -> do
        cellT <- getLbl i j
        cell <- new Button [ #expand := True
                           , #relief := ReliefStyleNone
                           , #name   := "cell" ]
        on cell #clicked $ do
            writeIORef lcRef cell
            style <- #getStyleContext cell
            #addClass style "selected"
            cellBtn kp cell
        style <- #getStyleContext cell
        #addClass style cellT
        #attach board' cell (fromIntegral i) (fromIntegral j) 1 1
    return board'
        where
            getLbl x y = let
                i = getBlock (y, x)
                in if odd $ i
                    then return "oddCell"
                    else return "evenCell"

buildOverlayLayout :: Grid -> Builder -> IO ()
buildOverlayLayout g b = do
    overlay <- unsafeBuildObj Overlay b "init_overlay"
    diffBox <- unsafeBuildObj Box b "popup_menu"
    #addOverlay overlay diffBox
    difficultyBtns b g
    #show overlay

buildLogo :: T.Text -> Builder -> IO ()
buildLogo fp b = do
    logo <- unsafeBuildObj Image b "logo"
    set logo [#file := fp]

buildCtrlLayout :: Int -> Grid -> Builder -> IO ()
buildCtrlLayout pulse' g b = do
    newGame_ <- unsafeBuildObj Button b "new_game_btn"
    check_ <- unsafeBuildObj Button b "check_btn"
    solve_ <- unsafeBuildObj Button b "solve_btn"
    on newGame_ #clicked $ newGameBtn b
    on solve_ #clicked $ solveBtn pulse' g
    on check_ #clicked $ checkBtn pulse' g
    return ()

buildKeypad :: IORef Button -> Int -> Grid -> Builder -> IO Popover
buildKeypad lcRef pulse' g b = do
    kp <- unsafeBuildObj Popover b "keypad"
    win <- unsafeBuildObj Window b "main"
    let prefix = "kp_"
    forM_ [1..sudokuSz] \x -> do
        btn <- unsafeBuildObj Button b . T.pack $ prefix <> show x
        on btn #clicked $ kpDigitBtn kp x
    checkBtn' <- unsafeBuildObj Button b "kp_check"
    clearBtn <- unsafeBuildObj Button b "kp_clear"
    cheatBtn <- unsafeBuildObj Button b "kp_cheat"
    on checkBtn' #clicked $ kpCheckBtn pulse' kp g
    on clearBtn #clicked $ kpClearBtn kp
    on cheatBtn #clicked $ kpCheatBtn pulse' kp g
    on kp #closed $ forkIO do
            lc <- readIORef lcRef
            style <- #getStyleContext lc
            #removeClass style "selected"
            threadDelay pulse'
            #setFocus win (Nothing :: Maybe Button)
        >> return ()
    return kp

buildWindow :: Builder -> IO Window
buildWindow b = do
    win <- unsafeBuildObj Window b "main"
    on win #destroy Gtk.mainQuit
    return win

buildApp :: Env -> IO App
buildApp e = do
    builder <- builderNewFromFile (uiPath e)
    board' <- buildBoard (lastCell e) builder
    buildOverlayLayout board' builder
    buildLogo (logoPath e) builder
    buildCtrlLayout (pulse e) board' builder
    buildFont (fontPath e)
    kp <- buildKeypad (lastCell e) (pulse e) board' builder
    win <- buildWindow builder
    applyCss (cssPath e) (cssPrio e) win
    return $ App kp board' win

-- Button Controls -------------------------------------------------------------

cellBtn :: Popover -> Button -> IO ()
cellBtn kp btn = do
    #setRelativeTo kp $ Just btn
    #popup kp

kpDigitBtn :: Popover -> Int -> IO ()
kpDigitBtn p v = do
    parent <- #getRelativeTo p >>= unsafeCastTo Button
    set parent [ #label := T.pack . show $ v ]
    #popdown p

kpCheckBtn :: Int -> Popover -> Grid -> IO ()
kpCheckBtn pulse' p g = do
    cell <- keypadCell g p
    gridConflicts g cell >>= \case
        [] -> flashColor pulse' g [(fst cell)] Green
        xs -> flashColor pulse' g xs Red
    #popdown p
    return ()

kpClearBtn :: Popover -> IO ()
kpClearBtn p = do
    parent <- #getRelativeTo p >>= unsafeCastTo Button
    set parent [#label := T.empty]
    #popdown p

kpCheatBtn :: Int -> Popover -> Grid -> IO ()
kpCheatBtn pulse' p g = do
    kpClearBtn p
    sltn <- gridSolve g
    cord <-fst <$> keypadCell g p
    case sltn of
        Nothing -> flashColor pulse' g cSpace Red
        Just x -> case lookup cord x of
            Nothing -> flashColor pulse' g [cord] Green
            Just x' -> gridUpdate g [(cord, x')]
                >> flashColor pulse' g [cord] Green
    #popdown p

solveBtn :: Int ->  Grid -> IO ()
solveBtn pulse' g = gridSolve g >>= \case
    Nothing -> flashColor pulse' g cSpace Red
    Just [] -> flashColor pulse' g cSpace Green
    Just xs -> gridUpdate g xs >> flashColor pulse' g cSpace Green

checkBtn :: Int -> Grid -> IO ()
checkBtn pulse' g = do
    board' <- gridArray g
    case allConflicts board' of
        [] -> flashColor pulse' g cSpace Green
        xs -> flashColor pulse' g xs Red

difficultyBtns :: Builder -> Grid -> IO ()
difficultyBtns b g = do
    menuBox <- unsafeBuildObj Box b "popup_menu"
    easyBtn <- unsafeBuildObj Button b "easy_btn"
    normalBtn <- unsafeBuildObj Button b "normal_btn"
    hardBtn <- unsafeBuildObj Button b "hard_btn"
    on easyBtn #clicked do
        #hide menuBox
        gridRegen g 30
    on normalBtn #clicked do
        #hide menuBox
        gridRegen g 50
    on hardBtn #clicked do
        #hide menuBox
        gridRegen g 70
    return ()

newGameBtn :: Builder -> IO ()
newGameBtn b = do
    menuBox <- unsafeBuildObj Box b "popup_menu"
    #show menuBox

--------------------------------------------------------------------------------

-- | Run the app
main :: IO ()
main = do
    Gtk.init Nothing
    env <- dfltEnv
    app <- buildApp env
    let win = window app
    #setFocus win (Nothing :: Maybe Button)
    #showAll win
    #setFocus win (Nothing :: Maybe Button)
    Gtk.main
    `catch` (\(e::GError) -> gerrorMessage e >>= putStrLn . T.unpack)
