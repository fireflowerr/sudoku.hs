{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sudoku.Internal.Bindings where
import Data.IORef
import Data.GI.Base.BasicTypes
import GI.Pango.Objects.FontMap
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Unsafe.Coerce

-- enable loading custom font into widgets

data FcConfig = FcConfig

data PangoFontDescription = PangoFontDescription

foreign import ccall "FcConfigGetCurrent" configGetCurrent :: IO (Ptr FcConfig)

foreign import ccall "FcConfigAppFontAddFile" configAppFontAddFile
    :: Ptr FcConfig -> CString -> IO Bool

foreign import ccall "pango_cairo_font_map_get_default" carioFontMapGetDefault'
    :: IO (Ptr a)

currentAppFontAddFile :: String -> IO Bool
currentAppFontAddFile s = do
    cfg <- configGetCurrent
    str <- newCAString s
    configAppFontAddFile cfg str

carioFontMapGetDefault :: IO FontMap
carioFontMapGetDefault = do
    ptr <- carioFontMapGetDefault'
    fptr <- newForeignPtr_ ptr
    cs <- newIORef Nothing
    return . unsafeCoerce $ ManagedPtr fptr Nothing cs
