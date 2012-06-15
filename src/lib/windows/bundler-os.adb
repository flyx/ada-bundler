with Ada.Directories;

with Interfaces.C.Strings;

with System;

package body Bundler.OS is
   type HResult is mod 2**32;
   for HResult'Size use 32;
   type DWord   is mod 2**32;
   for DWord'Size use 32;
   
   Flag_Create : constant DWord := 0; -- TODO
   
   type CSIDL is (Desktop         ,
                  Internet        ,
                  Programs        ,
                  Controls        ,
                  Printers        ,
                  Personal        ,
                  Favorites       ,
                  Startup         ,
                  Recent          ,
                  SendTo          ,
                  BitBucket       ,
                  Startmenu       ,
                  MyDocuments     ,
                  MyMusic         ,
                  MyVideo         ,
                  DesktopDirectory,
                  Drives          ,
                  Network         ,
                  Nethood         ,
                  Fonts           ,
                  Templates       ,
                  Common_Startmenu,
                  Common_Programs ,
                  Common_Startup  ,
                  Common_DesktopDirectory,
                  AppData          ,
                  Printhood        ,
                  Local_AppData    ,
                  AltStartup       ,
                  Common_AltStartup,
                  Common_Favorites ,
                  Internet_Cache   ,
                  Cookies          ,
                  History          ,
                  Common_AppData   ,
                  Windows          ,
                  System_Path      ,
                  Program_Files    ,
                  MyPictures       ,
                  Profile          ,
                  SystemX86        ,
                  Program_FilesX86 ,
                  Program_Files_Common,
                  Program_Files_CommonX86,
                  Common_Templates ,
                  Common_Documents ,
                  Common_Admintools,
                  Admintools       ,
                  Connections      ,
                  Common_Music     ,
                  Common_Pictures  ,
                  Common_Video     ,
                  Resources        ,
                  Resources_Localized,
                  Common_OEM_Links,
                  CDBurn_Area     ,
                  ComputersNearMe ,
                  Profiles);
   for CSIDL use (Desktop           => 16#0000#,
                  Internet          => 16#0001#,
                  Programs          => 16#0002#,
                  Controls          => 16#0003#,
                  Printers          => 16#0004#,
                  Personal          => 16#0005#,
                  Favorites         => 16#0006#,
                  Startup           => 16#0007#,
                  Recent            => 16#0008#,
                  SendTo            => 16#0009#,
                  BitBucket         => 16#000A#,
                  Startmenu         => 16#000B#,
                  MyDocuments       => 16#000C#,
                  MyMusic           => 16#000D#,
                  MyVideo           => 16#000E#,
                  DesktopDirectory  => 16#0010#,
                  Drives            => 16#0011#,
                  Network           => 16#0012#,
                  Nethood           => 16#0013#,
                  Fonts             => 16#0014#,
                  Templates         => 16#0015#,
                  Common_Startmenu  => 16#0016#,
                  Common_Programs   => 16#0017#,
                  Common_Startup    => 16#0018#,
                  Common_DesktopDirectory => 16#0019#,
                  AppData           => 16#001A#,
                  Printhood         => 16#001B#,
                  Local_AppData     => 16#001C#,
                  AltStartup        => 16#001D#,
                  Common_AltStartup => 16#001E#,
                  Common_Favorites  => 16#001F#,
                  Internet_Cache    => 16#0020#,
                  Cookies           => 16#0021#,
                  History           => 16#0022#,
                  Common_AppData    => 16#0023#,
                  Windows           => 16#0024#,
                  System_Path       => 16#0025#,
                  Program_Files     => 16#0026#,
                  MyPictures        => 16#0027#,
                  Profile           => 16#0028#,
                  SystemX86         => 16#0029#,
                  Program_FilesX86  => 16#002A#,
                  Program_Files_Common    => 16#002B#,
                  Program_Files_CommonX86 => 16#002C#,
                  Common_Templates  => 16#002D#,
                  Common_Documents  => 16#002E#,
                  Common_Admintools => 16#002F#,
                  Admintools        => 16#0030#,
                  Connections       => 16#0031#,
                  Common_Music      => 16#0035#,
                  Common_Pictures   => 16#0036#,
                  Common_Video      => 16#0037#,
                  Resources         => 16#0038#,
                  Resources_Localized => 16#0039#,
                  Common_OEM_Links  => 16#003A#,
                  CDBurn_Area       => 16#003B#,
                  ComputersNearMe   => 16#003D#,
                  Profiles          => 16#003E#);
   for CSIDL'Size use Interfaces.C.int'Size;
   
   function SHGetFolderPath (hwndOwner : System.Address; Folder : CSIDL;
                             Token     : System.Address; Flags  : DWord;
                             Path      : access Interfaces.C.Strings.chars_ptr)
                            return HResult;
   pragma Import (StdCall, SHGetFolderPath, "SHGetFolderPathA");
   
   Exec_Dir : constant String := Ada.Directories.Current_Directory;

   function Configuration_Dir return String is
   begin
      return Exec_Dir & "/config/";
   end Configuration_Dir;
      
   function Data_Dir return String is
   begin
      return Exec_Dir & "/data/";
   end Data_Dir;
   
   function User_Application_Folder return String is
      C_Path : access Interfaces.C.Strings.chars_ptr
        := new Interfaces.C.Strings.chars_ptr;
      Result : HResult;
   begin
      Result := SHGetFolderPath (System.Null_Address, Appdata,
                                 System.Null_Address, 0, C_Path);
      declare
         Path : String := Interfaces.C.Strings.Value (C_Path.all);
      begin
         Interfaces.C.Strings.Free (C_Path.all);
         return Path;
      end;
   end User_Application_Folder;
   
   function User_Configuration_Dir return String is
   begin
      return User_Application_Folder;
   end User_Configuration_Dir;
   
   function User_Data_Dir return String is
   begin
      return User_Application_Folder;
   end User_Data_Dir;
   
   function User_Cache_Dir return String is
   begin
      return "TODO/";
   end User_Cache_Dir;
   
   function User_Runtime_Dir return String is
   begin
      return "TODO/";
   end User_Runtime_Dir;

end Bundler.OS;