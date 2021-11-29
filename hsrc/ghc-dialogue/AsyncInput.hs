{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}
{- Obsolete OPTIONS -optc-I/usr/X11R6/include -optc-DNON_POSIX_SOURCE -fvia-C -}
--
module AsyncInput
   (doSocketRequest,doSelect,getAsyncInput',initXCall,XCallState)
  where
import P_IO_data({-Request(..),-}Response(..))
import Xtypes
import Sockets as S
import DLValue
import Unsafe.Coerce -- !!!
--import ResourceIds
import Utils(swap)

import XCallTypes
import StructFuns
import Xlib
import EncodeEvent
import Marshall
import MyForeign
import GHC.Exts(addrToAny# )
import GHC.Ptr(FunPtr(..))

--import Ap
import HbcUtils(lookupWithDefault)
import Data.Maybe(mapMaybe)
import Control.Monad(when)
import Data.Traversable(traverse)

import PQueue

import Data.IORef(newIORef,readIORef,writeIORef,IORef)
import System.Posix.DynamicLinker as DL

import PackedString(unpackPS,lengthPS{-,packCBytesST,psToByteArray-})

default (Int)

#include "newstructfuns.h"

H_STRUCTTYPE(fd_set)

allocaInt = allocaElem (0::Int)

type AiTable = [(Fd,Descriptor)]

type IOVar a = IORef a

newIOVar = newIORef
readIOVar = readIORef
writeIOVar = writeIORef

type MsTime = Int

data XCallState = XCallState
      Cfd_set
      (IOVar AiTable)
      (IOVar (PQueue MsTime (MsTime,Timer)))
      (IOVar Time)

initXCall =
 XCallState
   <$> newPtr
  <*> newIOVar []
  <*> newIOVar empty
  <*> newIOVar 0

type Fd = Int32

foreign import ccall "unistd.h read" cread :: Fd -> Addr -> CSize -> IO CSize
foreign import ccall "unistd.h write" cwrite :: Fd -> CString -> CSize -> IO CSize
foreign import ccall "sys/socket.h" accept :: Fd -> CsockAddr -> Addr -> IO Fd

getAsyncInput' (XCallState fds aitable tq tno) =
 AsyncInput <$> do
   ai <- readIOVar aitable
   let timeLeft =
	  do tqv <- readIOVar tq
	     case inspect tqv of
	       Nothing -> return Nothing
	       Just ((t,_),_) ->
		 do ms <- mstime
		    return . Just . max 0 $ t - ms
   
   let doSelect = do
	  timeout <- traverse newTimeVal =<< timeLeft
          readfds <- newPtr
--	  _casm_ ``bcopy(%0,%1,sizeof(fd_set));'' fds readfds
	  bcopy_fdset fds readfds
	  n <- select readfds timeout
	  maybe (return ()) freePtr timeout
	  let findFd fd = do
		  s <- fromC <$>
--		       _casm_ ``%r=FD_ISSET(%0,(fd_set*)%1);'' fd readfds
		       fd_isset fd readfds
		  if not s then findFd (fd+1) 
		    else let d = lookAi fd in case d of
		       DisplayDe _ -> mkEvent fd d
		       SocketDe _ ->
		         let bufsize = 2000
			 in alloca bufsize $ \ buf ->
			 do got <- tryP "read" (>=0) $ cread fd buf (fromIntegral bufsize)
			    str <- unmarshallString' (CString buf) (fromIntegral got)
			    return (d,SocketRead str)
		       LSocketDe _ ->
		         allocaInt $ \ addrlen ->
			 do addr <- newsockAddr
			    sfd <- tryP "accept" (>=0) $ 
				--_ccall_ ACCEPT fd addr addrlen
				accept fd addr addrlen
			    sf <- getfilep sfd "r+"
			    --buf <- stToIO $ newCharArray (1,1000)
			    --tryP "hostName" (==0) $ _ccall_ hostName addr buf
			    --peer <- cstring <$> mutByteArr2Addr buf
			    let peer = ""
			    return (d,SocketAccepted (So sf) peer)

		       _ -> error "getAsyncInput3"
          e <- if n == -1 then error "select" 
	       else if n > 0 then findFd 0
	            else do tno <- removetimeq tq
	                    return (TimerDe tno,TimerAlarm)
	  freePtr readfds
	  return e

       mkEvent fd d = do
           (window,fev) <- {-motionCompress display =<<-} getNextEvent display
	   return (descriptor,XEvent (window,fev))
         where descriptor@(DisplayDe display) = d
         
       lookAi = lookupWithDefault ai (error "getAsyncInput2")
       dispde x@(fd,DisplayDe _) = Just x
       dispde _ = Nothing
   case mapMaybe dispde ai of
       [] -> doSelect
       (fd,d@(DisplayDe display)):_ -> do
         q <- xPending display
         if q>0 then mkEvent fd d else doSelect

newTimeVal tleft =
  do timeout <- newPtr
     SET(timeVal,Int,timeout,tv_usec,(tleft `mod` 1000) * 1000)
     SET(timeVal,Int,timeout,tv_sec,(tleft `div` 1000))
     return timeout

foreign import ccall "sys/select.h select" cselect :: Int -> Cfd_set -> Cfd_set -> Cfd_set -> CtimeVal -> IO Int32
foreign import ccall "unistd.h" getdtablesize :: IO Int

select :: Cfd_set -> Maybe CtimeVal -> IO Int32
select readfds timeout = start where
 start = do
    tablesize <- getdtablesize
    n <- cselect tablesize readfds nullPtr nullPtr (maybe nullPtr id timeout)
{-
      case timeout of
        Nothing -> 
           _casm_ ``%r=select(getdtablesize(),%0,NULL,NULL,NULL);'' readfds
	Just t -> 
           _casm_ ``%r=select(getdtablesize(),%0,NULL,NULL,%1);'' readfds t
-}
    
    if n /= -1 then return n else do
       e <- errno
       if e == CCONST(EINTR) --- || e == CCONST(EAGAIN)
         then start -- again
         else return n
--}
foreign import ccall "asyncinput.h get_errno" errno :: IO Int
--errno :: IO Int
--errno = _casm_ ``%r=errno;''

foreign import ccall "sys/socket.h" listen :: Fd -> Int32 -> IO Int
foreign import ccall "sys/socket.h" socket :: Int32 -> Int32 -> Int32 -> IO Fd
foreign import ccall "stdio.h" fopen :: CString -> CString -> IO Int -- hmm
foreign import ccall "stdio.h" fclose :: Int -> IO Int

foreign import ccall "asyncinput.h" in_connect :: CString -> Int32 -> Int32 -> IO Int32
foreign import ccall "asyncinput.h" in_bind :: Int32 -> Int32 -> IO Fd
foreign import ccall "asyncinput.h" get_stdin :: IO Int

doSocketRequest (XCallState  fds  aitable tq tno) sr =
 case sr of
   CreateTimer interval first -> do
          tqv <- readIOVar tq
          tnov <- readIOVar tno
	  now <- mstime
	  writeIOVar tq (insert tqv (now+first,(interval,Ti tnov)))
	  writeIOVar tno (tnov+1)
	  returnS (Timer (Ti tnov))
   DestroyTimer t -> do
          tqv <- readIOVar tq
	  writeIOVar tq (remove tqv t)
	  return Success
   OpenSocket host port -> do
     chost <- marshallString host
     s <- tryP "in_connect" (>=0) $ 
--           _casm_ ``%r=in_connect(%0,%1,SOCK_STREAM);'' chost port
           in_connect chost (fromIntegral port) (fromIntegral CCONST(SOCK_STREAM))
     sf <- getfilep s "r+"
     freePtr chost
     returnS (Socket (So sf))
   OpenLSocket port -> do
      s <- tryP "in_bind" (>=0) $ if port == 0 
--         then _casm_ ``%r=socket(AF_INET,SOCK_STREAM,0);''
           then socket (fromIntegral CCONST(AF_INET)) (fromIntegral CCONST(SOCK_STREAM)) 0
--	   else _casm_ ``%r=in_bind(%0,SOCK_STREAM);'' port
	   else in_bind (fromIntegral port) (fromIntegral CCONST(SOCK_STREAM))
      tryP "listen" (==0) $ listen s 5
      SocketResponse . LSocket . LSo <$> getfilep s "r+"
   WriteSocket s str -> writeSocket s str
   WriteSocketPS s str ->
      do writeSocket s (unpackPS str) -- grr!
         returnS (Wrote (lengthPS str)) -- grr!
{-
     do
      fd <- fileno s
      r <- tryP "WriteSocket[out]" (>=0) $ 
        _casm_ ``%r=write(fileno((FILE*)%0),%1,%2);'' s (psToByteArray str) (lengthPS str)
--        bawrite fd (psToByteArray str) (lengthPS str)
      return (SocketResponse (Wrote r))
-}
   CloseSocket (So s) -> close s
   CloseLSocket (LSo s) -> close s
   GetStdinSocket -> SocketResponse . Socket . So <$>
--        _casm_ ``%r=stdin;''
	get_stdin
   GetSocketName (So s) -> socketname s
   GetLSocketName (LSo s) -> socketname s
   StartProcess cmd doIn doOut doErr -> startProcess cmd doIn doOut doErr
   DLOpen path -> do dh <- dlopen path [RTLD_LAZY]
                     case dh of
                       Null -> failu =<< dlerror
                       _ -> returnS $ S.DLHandle (DL dh)
   DLClose (DL dh) -> do dlclose dh ; return Success
   DLSym (DL dh) name ->
       do FunPtr fp <- dlsym dh name
          case addrToAny# fp of
            (# hval #) -> returnS . DLVal $ DLValue (unsafeCoerce hval)
   OpenFileAsSocket name mode -> do
     cname <- marshallString name
     cmode <- marshallString mode
     s <- tryP "OpenSocketAsFile[fopen]" (/=0) $ fopen cname cmode
     freePtr cname
     freePtr cmode
     returnS $ (Socket . So) s
   _ -> error ("Not implemented: "++show sr)
 where returnS = return . SocketResponse
       close s = do
          fclose s
	  return Success
       socketname s =
          allocaInt $ \ lenp ->
          do sa <- newsockAddr
--	     tryP "GetLSocketName" (==0) $ _ccall_ GETSOCKNAME (s::Int) sa lenp
	     tryP "GetLSocketName" (==0) $ getsockname s sa lenp
	     len <- peek lenp
	     strp <- GETC(sockAddr,char *,CString,sa,sa_data)
--	     Str . unpackPS <$> (stToIO $ packCBytesST len strp)
	     Str <$> unmarshallString' strp len

writeSocket (So s) str =
   do let n = length str
      fd <- get_fileno s
      cstr <- marshallString' str n
      tryP "WriteSocket[out]" (>=0) $ 
--        _casm_ ``%r=write(fileno((FILE*)%0),%1,%2);'' s cstr n
        cwrite fd cstr (fromIntegral n)
      freePtr cstr
      return Success

foreign import ccall "sys/socket.h" getsockname :: Int -> CsockAddr -> Addr -> IO Int
foreign import ccall "stdio.h" fdopen :: Int32 -> CString -> IO Int

getfilep s mode = tryP "fdopen" (/=0) $
  do cmode <- marshallString (mode::String)
--     _ccall_ fdopen (s::Int) cmode :: IO Int
     fdopen s cmode :: IO Int

foreign import ccall "string.h" strerror :: Int -> IO CString

tryP e p io = do
    r <- io
    if p (r{-::Int-}) then return r else do
--      cstr <- _casm_ ``%r=strerror(errno);''
      cstr <- strerror =<< errno
      s <- unmarshallString cstr
      failu (e++": "++ s)


peekq tq = do
   tqv <- readIOVar tq
   return (inspect tqv)

removetimeq tq = do
   tqv <- readIOVar tq
   case inspect tqv of
     Just ((first,v@(interval,tnov)),tqv') -> do
        let tqv2 = if interval == 0 then tqv'
	           else insert tqv' (first+interval,v)
        writeIOVar tq tqv2
	return tnov
	
foreign import ccall "sys/time.h" gettimeofday :: CtimeVal -> Addr -> IO ()

mstime = do
    now <- newtimeVal
--    _casm_ ``gettimeofday(%0,NULL);'' now
    gettimeofday now nullAddr
    s <- GET(timeVal,Int,now,tv_sec)
    us <- GET(timeVal,Int,now,tv_usec)
    return (s * 1000 + us `div` 1000 :: Int)


foreign import ccall "asyncinput.h" fdzero :: Cfd_set -> IO ()
foreign import ccall "asyncinput.h fdset" fd_set :: Fd -> Cfd_set -> IO ()
foreign import ccall "asyncinput.h fdisset" fd_isset :: Fd -> Cfd_set -> IO Int
foreign import ccall "asyncinput.h" bcopy_fdset :: Cfd_set -> Cfd_set -> IO ()

doSelect :: XCallState -> [Descriptor] -> IO Response
doSelect (XCallState fds aitable _ _) dl =
 do
  fdzero fds
  ait <- concat <$> mapM descriptor dl
  writeIOVar aitable ait
  mapM_ fdset (map fst ait)
  return Success
 where descriptor d = case d of
	   LSocketDe (LSo s) -> withd $ get_fileno s
	   SocketDe (So s) ->  withd $ get_fileno s
	   OutputSocketDe (So s) ->  withd $ get_fileno s
	   DisplayDe ({-Display-} d) -> withd $ 
--	         _casm_ ``%r=((Display*)%0)->fd;'' d
		 xConnectionNumber d -- hmm
	   TimerDe _ -> return []
           _ -> do putStr "Unexpected descriptor: ";print d;return []
          where withd m = m >>= \fd -> return [(fd,d)]

       fdset :: Fd -> IO ()
       --fdset s =  _casm_ ``FD_SET(%0,(fd_set*)%1);'' s fds
       fdset s =  fd_set s fds

foreign import ccall "asyncinput.h" get_fileno :: Int -> IO Fd
{-

-- Register problems with FD_ZERO under Redhat 6.1 Linux-i386...
--fdzero :: Cfd_set -> IO ()
--fdzero fds = _casm_ ``{ fd_set *s=%0; FD_ZERO(s);}'' fds
--fdzero fds = _ccall_ FD_ZERO fds

mutByteArr2Addr :: MutableByteArray RealWorld Int -> IO  Addr
mutByteArr2Addr arr  = _casm_ `` %r=(void *)%0; '' arr
--}

foreign import ccall "unistd.h" fork :: IO Int
foreign import ccall "unistd.h" execl :: CString -> CString -> CString -> CString -> Int -> IO Int
foreign import ccall "unistd.h" pipe :: Addr -> IO Int
foreign import ccall "unistd.h" dup :: Fd -> IO Fd
foreign import ccall "asyncinput.h" disable_timers :: IO ()

startProcess cmd doIn doOut doErr =
  do inPipe <- optPipe doIn
     outPipe <- optPipe doOut
     errPipe <- optPipe doErr
--     pid <- _ccall_ fork
     pid <- fork
     case pid::Int of
       -1 -> failu "fork" -- use tryP instead
       0 -> do -- child process
	       -- Disable virtual timer, used by the GHC RTS
	       disable_timers
	       optDupIn 0 inPipe
	       optDupOut 1 outPipe
	       optDupOut 2 errPipe
	       binsh <- marshallString "/bin/sh"
	       sh <- marshallString "sh"
	       dashc <- marshallString "-c"
	       ccmd <- marshallString cmd
	       execl binsh sh dashc ccmd (0::Int)
	       --_ccall_ _exit 1
	       failu "execl"
       _ -> do -- parent process
	       inS <- optPipeIn inPipe
	       outS <- optPipeOut outPipe
	       errS <- optPipeOut errPipe
	       return $ SocketResponse $ ProcessSockets inS outS errS
  where
    optPipe False = return Nothing
    optPipe True = Just `fmap` newPipe
    newPipe = do pa <- newArray 2
		 ok <- pipe (addrOf (pa::CInt32))
		 [p0,p1] <- readArray pa 2
		 freePtr pa
		 when (ok/=0) $ failu "pipe" -- use tryP instead
		 return (p0,p1)

    optDupIn d = optDupOut d . fmap swap
    optDupOut d = maybe (return ()) (dupOut d)
    dupOut d (p0,p1) = do cclose d
			  dup p1
			  cclose p0
			  cclose p1
			  return ()

    optPipeIn = optPipeS "w" . fmap swap
    optPipeOut = optPipeS "r"
    optPipeS m = maybe (return Nothing) (fmap Just . pipeS m)
    pipeS m (p0,p1) = do cclose p1
			 fmap So (getfilep p0 m)
			 

foreign import ccall "unistd.h close" cclose :: Int32 -> IO Int32
