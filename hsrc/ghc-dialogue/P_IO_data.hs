{-# LANGUAGE CPP #-}
module P_IO_data {-(module XStuff, module P_IO_data)-} where
import XStuff
import ShowFun()
import Prelude hiding (IOError)
import System.Directory(XdgDirectory)
#ifdef VERSION_old_time
import System.Time(ClockTime,CalendarTime)
#endif
#ifdef VERSION_time
import Data.Time(UTCTime,ZonedTime)
#endif

data Request =	-- file system requests:
			  ReadFile      String         
			| WriteFile     String String
			| AppendFile    String String
			| ReadBinFile   String 
			| WriteBinFile  String Bin
			| AppendBinFile String Bin
			| DeleteFile    String
			| StatusFile    String
                 -- useful for IO with GHC >=6.12
 			| ReadBinaryFile  String         
 			| WriteBinaryFile String String
                -- Files at standard locations for application data
                        | ReadXdgFile XdgDirectory String
                        | WriteXdgFile XdgDirectory String String
		-- channel system requests:
			| ReadChan	String 
			| AppendChan    String String
			| ReadBinChan   String 
			| AppendBinChan String Bin
			| StatusChan    String
		-- environment requests:
			| Echo          Bool
			| GetArgs
			| GetEnv        String
			| SetEnv        String String
		-- optional
			| ReadChannels	[String]
			| ReadBinChannels [String]
			| CreateProcess	Dialogue
			| CreateDirectory String String
			| OpenFile	String Bool
			| OpenBinFile	String Bool
			| CloseFile	File
			| ReadVal	File
			| ReadBinVal	File
			| WriteVal	File Char
			| WriteBinVal	File Bin
		-- hbc bonus
			| Sleep		Double
			| ChangeDirectory String
			| GetTime -- returns ClockTime -- /TH 2002-04-14
			| DeleteDirectory String
			| System	String
	                | ReadDirectory String
	                | XCommand    (XDisplay,XWId,XCommand)
	                | GetAsyncInput
			| GetCpuTime
			| GetProgName
                        | GetLocalTime -- return CalendarTime
			| SigAction Int SigAct
			| Exit Int
			| ReadFileScattered String [Int]
			| Select       [Descriptor]
			| SocketRequest SocketRequest
			| XRequest     (XDisplay,XWId,XRequest)
			| ReadFileFast String         
			| RenameFile String String
			| GetCurrentDirectory

		        | GetModificationTime FilePath -- returns ClockTime
                        | GetCurrentTime               -- returns UTCTime
                        | GetZonedTime                 -- returns ZonedTime
{-
			-- Haskell 1.3 I/O
			| H_OpenFile String Int		-- return Fil
			| H_Close File			-- return Success
			| H_FileSize File		-- return IntResp
			| H_IsEOF File			-- return IntResp
			| H_SetBuffering File Int	-- return Success
			| H_GetBuffering File		-- return IntResp
			| H_Flush File			-- return Success
			| H_Seek File Int Int		-- return IntResp
			| H_GetFlags File		-- return IntResp
			| H_GetChar File		-- return IntResp
			| H_UnGetChar File Int		-- return Success
			| H_PutChar File Int		-- return Success
			| H_PutString File String	-- return Success
			| H_GetFile File		-- return Str
			| H_Select ([File], [File], [Double]) -- return SelectResp, last list is a Maybe
			| H_CCall _CPointer [_CUnion] _CUnion -- return CCallResp
			| __RunAnswer __Answer		-- never returns
			| H_GetTime			-- returns GetTimeResp
			| H_GetErrno			-- returns IntResp
-}
                      deriving (Show,Read)

{-
data __Answer = __Answer EXISTVAR

instance Text __Answer where
    showsType _ = showString "__Answer"


data _CUnion  =		  _CUInt Int
			| _CUDouble Double 
			| _CUString String 
			| _CUPointer _CPointer
#ifdef P_IO_data
			| _CUByteVector _ByteVector._ByteVector
#endif
		deriving (Eq, Text)

ISO(_CPointer , _CPointer , Int) deriving (Eq, Text)
-}
data SigAct   =		  SAIgnore
			| SADefault
			| SACatch (String -> Dialogue)
		deriving (Show,Read)

data Response =		  Success
			| Str String 
			| Bn  Bin
			| Failure IOError
			| Tag [(String, Char)]
			| BinTag [(String, Bin)]
	                | StrList [String]
			| Fil File
			| Dbl Double
	                | AsyncInput AsyncInput
			| SocketResponse SocketResponse
			| XResponse XResponse
			| IntResp Int
			| SelectResp [([File], [File], [Double])]
			| SigActResp SigAct
--			| CCallResp _CUnion
--			| GetTimeResp Double Bool String Int
#ifdef VERSION_old_time
			| ClockTime ClockTime -- response to GetTime --
			| CalendarTime CalendarTime -- response to GetLocalTime
#endif
#ifdef VERSION_time
                        | UTCTime UTCTime         -- response to GetCurrentTime
                        | ZonedTime ZonedTime     -- response to GetZonedTime
#endif
		deriving (Show,Read)

data IOError =		  WriteError   String
			| ReadError    String
			| SearchError  String
			| FormatError  String
			| OtherError   String
		deriving (Show,Read)


type Bin = Unused
type Dialogue = [Response] -> [Request] -- not useful anymore
--type _CUnion = Unused
type File = Unused

type Unused = ()

#ifdef VERSION_old_time
instance Read ClockTime -- !!!
#endif
