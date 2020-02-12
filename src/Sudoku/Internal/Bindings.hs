{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Sudoku.Internal.Bindings where
import Data.GI.Base.BasicTypes
import Data.IORef
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import GI.Pango.Objects.FontMap
import Unsafe.Coerce
import qualified Data.Text as T

-- enable loading custom font into widgets

data FcConfig = FcConfig

data PangoFontDescription = PangoFontDescription
foreign import ccall "FcConfigGetCurrent" configGetCurrent :: IO (Ptr FcConfig)
foreign import ccall "FcConfigAppFontAddFile" configAppFontAddFile
    :: Ptr FcConfig -> CString -> IO Bool

foreign import ccall "pango_cairo_font_map_get_default" carioFontMapGetDefault'
    :: IO (Ptr a)

currentAppFontAddFile :: T.Text -> IO Bool
currentAppFontAddFile s = do
    cfg <- configGetCurrent
    str <- newCAString . T.unpack $ s
    configAppFontAddFile cfg str

carioFontMapGetDefault :: IO FontMap
carioFontMapGetDefault = do
    ptr <- carioFontMapGetDefault'
    fptr <- newForeignPtr_ ptr
    cs <- newIORef Nothing
    return . unsafeCoerce $ ManagedPtr fptr Nothing cs
