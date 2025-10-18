--------------------------------------------------------------------------------
--
with Interfaces.C;
with Interfaces.C.Strings;
with Types; use Types;
with C_Constants;

package Posix is

  type Byte is new Types.Byte;
  type Short is new Integer_2;
  type Unsigned_Short is new Word;
  type Int is new Integer_4;

--------------------------------------------------------------------
  type Mode_T is range 0..2_147_483_647;
  for Mode_T'size use 32;
  
  type Size_T is new Integer_4;


  type Pid_T is range -1 .. 2_147_483_647;
  for Pid_T'size use 32;

  O_RDONLY   : constant Int := Int(C_Constants.O_RDONLY);    -- open for reading only
  O_WRONLY   : constant Int := Int(C_Constants.O_WRONLY);    -- open for writing only
  O_RDWR     : constant Int := Int(C_Constants.O_RDWR);      -- open for reading and writing
  O_CREAT    : constant Int := Int(C_Constants.O_CREAT);     -- no file? create it
  O_EXCL     : constant Int := Int(C_Constants.O_EXCL);      -- lock file (see below)
  O_NOCTTY   : constant Int := Int(C_Constants.O_NOCTTY);    -- if tty, don't acquire it
  O_TRUNC    : constant Int := Int(C_Constants.O_TRUNC);     -- file exists? truncate it
  O_APPEND   : constant Int := Int(C_Constants.O_APPEND);    -- file exists? move to end
  O_NONBLOCK : constant Int := Int(C_Constants.O_NONBLOCK);  -- if pipe, don't wait for data
  O_SYNC     : constant Int := Int(C_Constants.O_SYNC);      -- don't cache writes
  O_ASYNC    : constant Int := Int(C_Constants.O_ASYNC);     -- async. IO via SIGIO
  -- not on mac O_DIRECT   : constant Int := Int(C_Constants.O_DIRECT);    -- direct disk access
  -- not on mac O_LARGEFILE: constant Int := Int(C_Constants.O_LARGEFILE); -- not implemented in Linux (yet)
  O_DIRECTORY: constant Int := Int(C_Constants.O_DIRECTORY); -- error if file isn't a dir
  O_NOFOLLOW : constant Int := Int(C_Constants.O_NOFOLLOW);  -- if sym link, open link itself  

   ------------ lock files start --------------
     
   function Lockf(Fd : Int; Cmd : Int; Siz : Size_T) return Int;
   
   F_ULOCK : constant Int := Int(C_Constants.F_ULOCK);   
   F_LOCK  : constant Int := Int(C_Constants.F_LOCK);   
   F_TLOCK : constant Int := Int(C_Constants.F_TLOCK);   
   F_TEST  : constant Int := Int(C_Constants.F_TEST);   
   
   --------------- lockfiles stop --------------

  function Getpid return Pid_T;
  function Setsid return Pid_T;
  function Fork return Pid_T;
  Fork_Failed : exception;
  
  function Umask(Mask : Mode_T) return Mode_T;
  procedure Do_Exit(Status : int);
  procedure Daemonize ;

  procedure Perror (Msg : String ) ;
  function Open(Path : Interfaces.C.Strings.Chars_Ptr ; flags : int ; mode : mode_t) return Int; 
  function Close(Fd : Int) return Int; 
  function Errno return Int;  
  function Write(Fd : Int; Buf : Interfaces.C.Strings.Chars_Ptr; Count : Size_T) return Size_T;
  
  subtype Buffer_String_Type is String(1..2_048);
  type Buffer_String_Pointer is access all Buffer_String_Type;     
  
  function Read (Fd : Int; Buf : Buffer_String_Pointer; Count : Size_T) return Size_T;
  
private

  pragma Import (C, Getpid ,"getpid");
  pragma Import (C, Setsid, "setsid");
  pragma Import (C, Fork, "fork");
  pragma Import (C, Umask, "umask");
  pragma Import (C, Do_Exit, "_exit");

  pragma Import (C, open, "open");
  pragma Import (C, close, "close");
  --int creat(const char *pathname, mode_t mode);
  pragma Import (C, Errno, "__get_errno");   
--ssize_t write(int fd, const void *buf, size_t count); 
  pragma Import(C, Write, "write");
  pragma Import(C, Lockf, "lockf");
  
  --ssize_t read(int fd, void *buf, size_t count);
  pragma Import(C, Read, "read");
  
end Posix;


