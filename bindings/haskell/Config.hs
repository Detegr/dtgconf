module Config (withConfig,withLoadedConfig,addToConfig,saveConfig,getSection,getConfig,ConfigSection,ConfigItem,Config) where

{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Control.Monad
import Data.String.Utils

data CConfigItem = CConfigItem
  {
      key :: CString,
      val :: CString
  } deriving Show

type ConfigItem = (String, Maybe String)

instance Storable CConfigItem where
  alignment _ = 8
  sizeOf _ = 16
  peek ptr = do
    a <- peekByteOff ptr 0
    b <- peekByteOff ptr 8
    return $ CConfigItem a b
  poke ptr (CConfigItem a b)= do
    pokeByteOff ptr 0 a
    pokeByteOff ptr 8 b

data CConfigSection = CConfigSection
  {
      cname           :: CString,
      citemsCount     :: CUInt,
      callocatedItems :: CUInt,
      citems          :: Ptr (Ptr CConfigItem)
  } deriving Show

type ConfigSection = (String, [ConfigItem])

instance Storable CConfigSection where
  alignment _ = 8
  sizeOf _ = 24
  peek ptr = do
    a <- peekByteOff ptr 0
    b <- peekByteOff ptr 8
    c <- peekByteOff ptr 12
    d <- peekByteOff ptr 16
    return $ CConfigSection a b c d
  poke ptr (CConfigSection a b c d) = do
    pokeByteOff ptr 0  a
    pokeByteOff ptr 8  b
    pokeByteOff ptr 12 c
    pokeByteOff ptr 16 d

data CConfig = CConfig 
  {
      sectionCount      :: CUInt,
      allocatedSections :: CUInt,
      sections          :: Ptr (Ptr CConfigSection)
  } deriving Show

type Config = [ConfigSection]

instance Storable CConfig where
  alignment _ = 8
  sizeOf _ = 16
  peek ptr = do
    a <- peekByteOff ptr 0
    b <- peekByteOff ptr 4
    c <- peekByteOff ptr 8
    return $ CConfig a b c
  poke ptr (CConfig a b c) = do
    pokeByteOff ptr 0 a
    pokeByteOff ptr 4 b
    pokeByteOff ptr 8 c

foreign import ccall unsafe "config_init" initConfig :: Ptr CConfig -> IO()
foreign import ccall unsafe "config_load" loadConfig :: Ptr CConfig -> CString -> IO()
foreign import ccall unsafe "config_find_section" findSection :: Ptr CConfig -> CString -> IO(Ptr CConfigSection)
foreign import ccall unsafe "config_add" configAdd :: Ptr CConfig -> CString -> CString -> CString -> IO()
foreign import ccall unsafe "config_free" freeConfig :: Ptr CConfig -> IO()
foreign import ccall unsafe "config_save" saveConfigInternal :: Ptr CConfig -> CString -> IO()

withConfig :: (Ptr CConfig -> IO a) -> IO()
withConfig f = alloca $ \p -> initConfig p >> f p >> freeConfig p

withLoadedConfig :: String -> (Ptr CConfig -> IO a) -> IO a
withLoadedConfig s f = alloca $ \p -> do
  withCString s (\cstr -> loadConfig p cstr)
  val <- f p
  freeConfig p
  return val

getSection :: Ptr CConfig -> String -> IO [ConfigItem]
getSection c needle = do
  withCString needle $ \n -> do
    section <- findSection c n
    if section == nullPtr then return []
    else peek section >>= cConfigSectionToConfigSection

cConfigItemToItem :: CConfigItem -> IO ConfigItem
cConfigItemToItem i = do
  k <- peekCString (key i)
  case val i == nullPtr of
    True  -> return $ (k, Nothing)
    False -> do
      v <- peekCString (val i)
      return $ (k, Just v)

cConfigSectionToConfigSection :: CConfigSection -> IO [ConfigItem]
cConfigSectionToConfigSection s = do
  name <- peekCString (cname s)
  itemptrs <- peekArray (fromIntegral . citemsCount $ s) (citems s)
  mapM (cConfigItemToItem <=< peek) itemptrs

getConfig :: Ptr CConfig -> IO [[ConfigItem]]
getConfig c = do
  pc <- peek c
  arrptrs <- peekArray (fromIntegral . sectionCount $ pc) $ sections pc
  mapM (cConfigSectionToConfigSection <=< peek) arrptrs

withNullableCString :: String -> (CString -> IO()) -> IO()
withNullableCString s f =
  case length s of
    0 -> f nullPtr
    _ -> withCString s $ \cs -> f cs

addToConfig :: Ptr CConfig -> String -> String -> String -> IO()
addToConfig c sect key val =
  withNullableCString sect $ \csect ->
    withNullableCString key $ \ckey ->
      withNullableCString val $ \cval ->
        configAdd c csect ckey cval

saveConfig :: Ptr CConfig -> String -> IO()
saveConfig c filename = do
  withCString filename $ \f -> do
    saveConfigInternal c f
